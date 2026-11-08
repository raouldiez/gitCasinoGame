# Baccarat Simulator — Shiny UI
# Run from the repo folder:  shiny::runApp()   (or open in RStudio and click "Run App")
# Needs: install.packages("shiny")   (bslib comes with it)

library(shiny)
# Suit symbols are Unicode; make sure R treats text as UTF-8 (already the default on
# Windows with R >= 4.2 and on macOS).
if (!isTRUE(l10n_info()[["UTF-8"]])) invisible(try(Sys.setlocale("LC_CTYPE", "C.UTF-8"), silent = TRUE))
source("baccarat.R", local = TRUE, encoding = "UTF-8")

# ---- Helpers ----------------------------------------------------------------
# Default for a blank input: numericInput gives NA (not NULL) when the box is empty
`%||%` <- function(a, b) if (is.null(a) || (length(a) == 1 && is.na(a))) b else a
RED_SUITS <- c(2L, 3L)  # hearts, diamonds
SUIT_HTML <- c("&spades;", "&hearts;", "&diams;", "&clubs;")  # locale-proof card faces

card_html <- function(id, tag = FALSE, small = FALSE) {
  suit <- (id - 1L) %/% 13L + 1L
  rank <- card_rank(id)
  cls <- paste("pcard", if (suit %in% RED_SUITS) "red", if (small) "small")
  tg <- if (tag) {
    t <- DRAGON_TAGS[CARD_VALUE[rank] + 1L]
    span(class = paste("tag", if (t > 0) "pos" else if (t < 0) "neg"),
         if (t > 0) paste0("+", t) else t)
  }
  div(class = cls, span(class = "rank", RANK_LABEL[rank]),
      span(class = "suit", HTML(SUIT_HTML[suit])), tg)
}

# Start of shoe: the first card is turned face up, then that many cards (10 for
# 10/J/Q/K) are burned face down. Only the face-up card counts for the Dragon count.
burn_html <- function(shoe, show_tag = FALSE) {
  first <- shoe[1]; n <- BURN_VALUE[card_rank(first)]
  div(class = "burn",
      div(class = "burn-title", "New shoe: burn cards"),
      div(class = "burn-row",
          div(class = "burn-up", card_html(first, tag = show_tag), div(class = "burn-cap", "Face up")),
          div(class = "burn-down",
              div(class = "backs", lapply(seq_len(n), function(i) div(class = "pcard back"))),
              div(class = "burn-cap", sprintf("%d burned face down", n)))),
      div(class = "burn-note", sprintf(
        "%s is worth %d, so %d card%s go%s to the discard tray. Place your bets and press Deal.",
        RANK_LABEL[card_rank(first)], n, n, if (n == 1) "" else "s", if (n == 1) "es" else "")))
}

hand_cards <- function(shoe, h, k) {
  idx <- h$first[k]:(h$first[k] + h$used[k] - 1L)
  list(player = shoe[idx[c(1, 3, if (h$p_cards[k] == 3) 5)]],
       banker = shoe[idx[c(2, 4, if (h$b_cards[k] == 3) h$used[k])]])
}

# Running count after k hands (k = 0: only the exposed burn card)
rc_after <- function(shoe, h, k) {
  if (k == 0) return(h$rc[1])
  idx <- h$first[k]:(h$first[k] + h$used[k] - 1L)
  h$rc[k] + sum(DRAGON_TAGS[CARD_VALUE[card_rank(shoe[idx])] + 1L])
}
cards_left_after <- function(h, k) if (k == 0) h$cards_left[1] else h$cards_left[k] - h$used[k]

bead_plate_html <- function(winner, rows = 6, min_cols = 12) {
  n <- length(winner); cols <- max(min_cols, ceiling(n / rows))
  m <- matrix(NA_character_, rows, cols); if (n) m[seq_len(n)] <- winner
  tags$table(class = "road bead-plate", lapply(seq_len(rows), function(r)
    tags$tr(lapply(seq_len(cols), function(c) {
      w <- m[r, c]
      tags$td(if (!is.na(w)) span(class = paste("bead", w), w))
    }))))
}

big_road_html <- function(winner, rows = 6, min_cols = 20) {
  br <- big_road(winner, rows)
  cols <- max(min_cols, if (nrow(br)) max(br$col) + 2 else 0)
  tags$table(class = "road big", lapply(0:(rows - 1), function(r)
    tags$tr(lapply(0:(cols - 1), function(c) {
      i <- which(br$col == c & br$row == r)
      tags$td(if (length(i)) span(class = paste("ring", br$winner[i]),
                                  if (br$ties[i] > 0) span(class = "tie", br$ties[i])))
    }))))
}

RESULT_TEXT <- c(P = "Player wins", B = "Banker wins", T = "Tie")

# "Banker wins by 6", plus Natural / Dragon 7 / Panda 8 tags (the margin settles the Dragon Bonus)
result_text <- function(h, k, specials = TRUE, game = "standard") {
  m <- abs(h$p_total[k] - h$b_total[k])
  txt <- if (h$winner[k] == "T") "Tie" else sprintf("%s by %d", RESULT_TEXT[[h$winner[k]]], m)
  if (!specials) return(txt)
  ez <- game == "ez"   # Dragon 7 / Panda 8 are only events at an EZ table
  tag <- if (ez && h$dragon7[k]) "Dragon 7" else if (ez && h$panda8[k]) "Panda 8" else if (h$natural[k]) "Natural"
  if (is.null(tag)) txt else paste(txt, "–", tag)
}

# ---- UI -----------------------------------------------------------------------
css <- "
.felt { background: radial-gradient(ellipse at center, #1f6b45 0%, #134a2f 100%);
        border-radius: 18px; padding: 22px; color: #f4f1e8; min-height: 300px; }
.side { text-align: center; }
.side h4 { letter-spacing: .15em; font-weight: 600; margin-bottom: 10px; }
.side .total { font-size: 2.6rem; font-weight: 700; line-height: 1; margin-top: 10px; }
.cards { display: flex; gap: 8px; justify-content: center; min-height: 104px; }
.pcard { width: 70px; height: 100px; background: #fffdf7; border-radius: 8px; color: #1b1b1b;
         box-shadow: 0 2px 6px rgba(0,0,0,.35); display: flex; flex-direction: column;
         align-items: center; justify-content: center; position: relative; font-family: Georgia, serif; }
.pcard .rank { font-size: 1.6rem; font-weight: 700; } .pcard .suit { font-size: 1.6rem; line-height: 1; }
.pcard.red { color: #c0392b; }
.pcard.back { background: repeating-linear-gradient(45deg, #8b1e1e 0 4px, #a8322c 4px 8px);
              border: 3px solid #fffdf7; }
.burn { text-align: center; display: grid; gap: 10px; justify-items: center; }
.burn-title { font-size: 1.3rem; font-weight: 700; letter-spacing: .08em; }
.burn-row { display: flex; gap: 28px; align-items: flex-start; flex-wrap: wrap; justify-content: center; }
.burn-up, .burn-down { display: grid; gap: 8px; justify-items: center; }
.backs { display: flex; } .backs .pcard + .pcard { margin-left: -46px; }
.burn-cap { font-size: .85rem; color: #d9e6dc; margin-top: 14px; }
.burn-note { font-size: .95rem; color: #e8efe9; }
.pcard.small { width: 46px; height: 66px; } .pcard.small .rank, .pcard.small .suit { font-size: 1.05rem; }
.pcard .tag { position: absolute; bottom: -22px; font: 600 .8rem system-ui; color: #ddd; }
.pcard .tag.pos { color: #8fe3a5; } .pcard .tag.neg { color: #ffb4a8; }
.banner { text-align: center; font-size: 1.5rem; font-weight: 700; margin-top: 16px; min-height: 2.2rem; }
.banner .special { color: #ffd166; margin-left: .5rem; }
.stat { font-size: .8rem; text-transform: uppercase; letter-spacing: .08em; color: #6c757d; }
.statv { font-size: 1.4rem; font-weight: 700; }
.pos-net { color: #2e7d32; } .neg-net { color: #c62828; }
table.road { border-collapse: collapse; } table.road td { width: 22px; height: 22px; border: 1px solid #e3e3e3; text-align: center; padding: 0; }
.bead { display: inline-block; width: 18px; height: 18px; border-radius: 50%; color: #fff; font: 700 11px/18px system-ui; }
.bead.B { background: #c62828; } .bead.P { background: #1565c0; } .bead.T { background: #2e7d32; }
.ring { display: inline-block; width: 16px; height: 16px; border-radius: 50%; border: 3px solid; position: relative; }
.ring.B { border-color: #c62828; } .ring.P { border-color: #1565c0; }
.ring .tie { position: absolute; top: -9px; right: -9px; font: 700 10px system-ui; color: #2e7d32; }
.road-wrap { overflow-x: auto; padding-bottom: 4px; }
.count-box { background: #fff8e1; border-radius: 10px; padding: 10px 14px; }
.hot { background: #ffd166; color: #3d2c00; border-radius: 6px; padding: 2px 8px; font-weight: 700; }
.legend span { margin-right: 10px; }
.credits { font-size: .8rem; color: #6c757d; text-align: center; padding: 18px 16px 24px; max-width: 900px; margin: 0 auto; }
.credits a { color: inherit; }
.feedback-ok { color: #2e7d32; font-weight: 700; } .feedback-bad { color: #c62828; font-weight: 700; }
"

SIDE_CHOICES <- c("Banker" = "B", "Player" = "P", "Opposite of last result" = "opposite",
                  "Same as last result" = "same")

# Money-management inputs shared by Play (auto-bet) and Simulate; ids get a prefix.
money_inputs <- function(prefix) tagList(
  radioButtons(paste0(prefix, "side"), "Bet on", SIDE_CHOICES),
  # width = "100%": numericInput's default 300px width overflows a two-column row
  div(class = "d-flex gap-2",
      numericInput(paste0(prefix, "base"), "Base bet", 10, min = 1, step = 5, width = "100%"),
      numericInput(paste0(prefix, "max"), "Table max", 1000, min = 1, step = 100, width = "100%")),
  numericInput(paste0(prefix, "steps"), "Max progression steps", 6, min = 1, max = 15, width = "100%"),
  div(class = "d-flex gap-2",
      numericInput(paste0(prefix, "sl"), "Stop-loss per shoe", 0, min = 0, step = 50, width = "100%"),
      numericInput(paste0(prefix, "tp"), "Take-profit per shoe", 0, min = 0, step = 50, width = "100%")),
  p(class = "small text-muted", "Stop-loss and take-profit of 0 mean off. Chop progression always bets ",
    "against the last result; Dragon count always bets the Dragon 7.")
)

bet_input <- function(id, label, pays) {
  div(class = "d-flex align-items-center gap-2 mb-1",
      div(style = "width: 110px", strong(label), br(), span(class = "text-muted small", pays)),
      div(style = "flex: 1", numericInput(id, NULL, 0, min = 0, step = 5, width = "100%")))
}

ui <- bslib::page_navbar(
  title = "Baccarat Simulator",
  fillable = FALSE,   # let the page scroll instead of squeezing panels to fit the window
  theme = bslib::bs_theme(version = 5, primary = "#1f6b45"),
  header = tags$head(tags$style(HTML(css))),
  # one HTML string: tags() would put a space between each link and the full stop after it
  footer = div(class = "credits", HTML(paste0(
    "Baccarat Simulator by Raoul R. Diez, DODT (2016; v2 2026, built with help from Claude by Anthropic). ",
    "Dragon 7 count: Eliot Jacobson, Ph.D., ",
    "<a href='https://wizardofodds.com/games/baccarat/' target='_blank'>via Wizard of Odds</a>. ",
    "Exact odds checked against figures published by Michael Shackleford (Wizard of Odds). ",
    "<a href='https://github.com/raouldiez/gitCasinoGame' target='_blank'>Source on GitHub</a>."))),

  # ---- Play -------------------------------------------------------------------
  bslib::nav_panel("Play",
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(width = 320,
        radioButtons("p_game", "Game", c("Standard (5% commission)" = "standard",
                                         "EZ Baccarat (no commission)" = "ez")),
        numericInput("p_bankroll", "Starting bankroll", 1000, min = 10, step = 100),
        checkboxInput("a_on", strong("Auto-bet with a system"), FALSE),
        conditionalPanel("!input.a_on",
          bet_input("bet_P", "Player", "1:1"),
          bet_input("bet_B", "Banker", "0.95:1 / EZ 1:1"),
          bet_input("bet_T", "Tie", "8:1"),
          # Dragon Bonus is offered in both games; Dragon 7 and Panda 8 only in EZ Baccarat
          bet_input("bet_DP", "Dragon Bonus Player", "1:1 to 30:1"),
          bet_input("bet_DB", "Dragon Bonus Banker", "1:1 to 30:1"),
          conditionalPanel("input.p_game == 'ez'",
            bet_input("bet_D", "Dragon 7", "40:1"),
            bet_input("bet_N", "Panda 8", "25:1"))),
        conditionalPanel("!input.a_on", tags$details(class = "small mb-2",
          tags$summary("Dragon Bonus pay table"),
          tags$table(class = "table table-sm mb-0", tags$tbody(
            lapply(list(c("Natural win", "1 to 1"), c("Natural tie", "Push"),
                        c("Win by 9", "30 to 1"), c("Win by 8", "10 to 1"), c("Win by 7", "6 to 1"),
                        c("Win by 6", "4 to 1"), c("Win by 5", "2 to 1"), c("Win by 4", "1 to 1"),
                        c("Win by 0–3, or any other result", "Loses")),
                   function(r) tags$tr(tags$td(r[1]), tags$td(r[2]))))))),
        conditionalPanel("input.a_on",
          selectInput("a_system", "System", SYSTEMS),
          uiOutput("a_help"),
          conditionalPanel("input.a_system == 'Dragon count'",
            sliderInput("a_thr", "Bet Dragon when true count ≥", 0, 10, 4, step = 0.5)),
          money_inputs("a_"),
          numericInput("a_hands", "Autoplay this many hands", 20, min = 1, max = 500),
          div(class = "d-flex gap-2 mb-2",
              actionButton("a_go", "Autoplay", class = "btn-success flex-fill"),
              actionButton("a_stop", "Stop", class = "btn-outline-danger flex-fill")),
          sliderInput("a_speed", "Seconds per hand", 0.1, 3, 0.6, step = 0.1)),
        div(class = "d-flex gap-2",
            actionButton("deal", "Deal", class = "btn-primary flex-fill"),
            actionButton("p_new", "New shoe", class = "btn-outline-secondary")),
        checkboxInput("p_showcount", "Show Dragon count", FALSE)
      ),
      uiOutput("a_status"),
      bslib::layout_columns(col_widths = c(3, 3, 3, 3),
        div(div(class = "stat", "Bankroll"), uiOutput("p_bank", class = "statv")),
        div(div(class = "stat", "Session net"), uiOutput("p_net", class = "statv")),
        div(div(class = "stat", "Hand"), textOutput("p_hand", container = div)),
        div(div(class = "stat", "Cards left"), textOutput("p_left", container = div))
      ),
      div(class = "felt",
        bslib::layout_columns(col_widths = c(6, 6),
          div(class = "side", h4("PLAYER"), uiOutput("p_pcards"), uiOutput("p_ptotal")),
          div(class = "side", h4("BANKER"), uiOutput("p_bcards"), uiOutput("p_btotal"))),
        uiOutput("p_banner")),
      uiOutput("p_count"),
      bslib::layout_columns(col_widths = c(5, 7),
        bslib::card(bslib::card_header("Bead plate"), div(class = "road-wrap", uiOutput("p_bead"))),
        bslib::card(bslib::card_header("Big road"), div(class = "road-wrap", uiOutput("p_big")))),
      bslib::card(bslib::card_header("Recent hands"), tableOutput("p_history"))
    )),

  # ---- Simulate ---------------------------------------------------------------
  bslib::nav_panel("Simulate",
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(width = 300,
        sliderInput("s_shoes", "Shoes to simulate", 100, 5000, 1000, step = 100),
        sliderInput("s_decks", "Decks", 1, 8, 8),
        sliderInput("s_cut", "Cards behind cut card", 6, 60, 14),
        radioButtons("s_game", "Game", c("Standard" = "standard", "EZ Baccarat" = "ez"), inline = TRUE),
        numericInput("s_seed", "Random seed (blank = random)", NA),
        checkboxGroupInput("s_systems", "Systems to compare", SYSTEMS,
                           selected = c("Flat", "Martingale", "Paroli", "Fibonacci",
                                        "Chop progression (2016)", "Dragon count")),
        money_inputs("s_"),
        sliderInput("s_thresh", "Dragon bet when true count ≥", 0, 10, 4, step = 0.5),
        actionButton("s_run", "Run simulation", class = "btn-primary w-100")
      ),
      uiOutput("s_summary"),
      bslib::card(bslib::card_header("House edge: simulated vs exact (%)"), tableOutput("s_edge")),
      bslib::card(bslib::card_header("Betting systems"), tableOutput("s_strat"), uiOutput("s_skipnote"),
                  p(class = "small text-muted", "Return = net result ÷ total amount wagered. ",
                    "Worst drawdown = largest drop from a previous high.")),
      bslib::card(bslib::card_header("Report"), plotOutput("s_plot", height = "720px")),
      bslib::card(bslib::card_header("Dragon 7 by true count"), tableOutput("s_dragon")),
      bslib::card(bslib::card_header(textOutput("s_cf_title", inline = TRUE)),
        uiOutput("s_cf_tiles"),
        bslib::layout_columns(col_widths = c(7, 5),
          div(plotOutput("s_cf_plot", height = "260px")),
          div(strong("Shoes by number of qualifying hands"), tableOutput("s_cf_dist"),
              strong("Where in the shoe they come"), tableOutput("s_cf_stage"))),
        p(class = "small text-muted", "A qualifying hand is one where the true count before the deal is at or ",
          "above the threshold, so the Dragon count strategy would bet it. Stage of the shoe is measured by the ",
          "share of cards already dealt.")),
      bslib::card(bslib::card_header("Dragon 7s per shoe"),
        uiOutput("s_dps_tiles"),
        bslib::layout_columns(col_widths = c(7, 5),
          div(strong("How many Dragon 7s a shoe gets"),
              p(class = "small text-muted mb-1", "Bars: simulated shoes. Dots: what independent hands predict with the same average."),
              plotOutput("s_dps_plot", height = "260px")),
          div(strong("Shoes by number of Dragon 7s"), div(style = "max-height: 300px; overflow: auto", tableOutput("s_dps_dist")))),
        strong(textOutput("s_dps_rec_title", inline = TRUE)),
        p(class = "small text-muted mb-1", "Dragon true count before each hand of the record shoe; red dots are its Dragon 7s, ",
          "the dashed line is the bet threshold. A Dragon 7 usually uses up 4s to 7s, which pulls the count down right after it; ",
          "the record shoe can still come at any count."),
        plotOutput("s_dps_rec", height = "240px"))
    )),

  # ---- Count trainer -------------------------------------------------------------
  bslib::nav_panel("Count trainer",
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(width = 300,
        p("Keep the Dragon 7 running count as hands are dealt. ",
          "Tags: 4, 5, 6, 7 = −1;  8, 9 = +2;  A, 2, 3, 10, J, Q, K = 0. ",
          "The first burn card is shown face up and counts."),
        p(class = "small text-muted", "True count = running count ÷ decks remaining. ",
          "Bet the Dragon when the true count is +4 or higher."),
        actionButton("t_next", "Next hand", class = "btn-primary w-100 mb-2"),
        checkboxInput("t_auto", "Auto-deal", FALSE),
        sliderInput("t_speed", "Seconds per hand", 0.5, 6, 2.5, step = 0.5),
        sliderInput("t_every", "Quiz me every N hands (0 = never)", 0, 10, 5),
        checkboxInput("t_tags", "Show tags under cards (practice mode)", FALSE),
        actionButton("t_new", "New shoe", class = "btn-outline-secondary w-100")
      ),
      bslib::layout_columns(col_widths = c(3, 3, 3, 3),
        div(div(class = "stat", "Hand"), textOutput("t_hand", container = div)),
        div(div(class = "stat", "Cards left"), textOutput("t_left", container = div)),
        div(div(class = "stat", "Decks left"), textOutput("t_decks", container = div)),
        div(div(class = "stat", "Score"), textOutput("t_score", container = div))
      ),
      div(class = "felt",
        bslib::layout_columns(col_widths = c(6, 6),
          div(class = "side", h4("PLAYER"), uiOutput("t_pcards")),
          div(class = "side", h4("BANKER"), uiOutput("t_bcards"))),
        div(style = "height: 26px")),
      bslib::card(
        bslib::card_header("Your count"),
        uiOutput("t_prompt"),
        div(class = "d-flex gap-2 align-items-end flex-wrap",
            numericInput("t_rc", "Running count", NA, width = "150px"),
            numericInput("t_tc", "True count (optional)", NA, step = 0.5, width = "170px"),
            actionButton("t_check", "Check", class = "btn-primary"),
            actionButton("t_reveal", "Reveal", class = "btn-outline-secondary")),
        uiOutput("t_feedback"))
    ))
)

# ---- Server -------------------------------------------------------------------
server <- function(input, output, session) {

  # ---- Play ----
  play <- reactiveValues(shoe = NULL, h = NULL, k = 0, bank = 1000, start = 1000,
                         log = NULL, last = NULL)
  # Auto-bet: the progression keeps its state between hands; it restarts each shoe,
  # and stop-loss / take-profit apply to the current shoe (same as in Simulate).
  auto <- reactiveValues(prog = NULL, shoe_net = 0, stopped = FALSE, remaining = 0, running = FALSE, msg = NULL)
  auto_cfg <- reactive(system_config(
    input$a_system, input$a_side, base = max(1, input$a_base %||% 10),
    table_max = max(1, input$a_max %||% 1000), max_steps = max(1, input$a_steps %||% 6),
    stop_loss = input$a_sl %||% 0, take_profit = input$a_tp %||% 0, threshold = input$a_thr))
  reset_auto <- function() {
    auto$prog <- make_progression(isolate(input$a_system), isolate(max(1, input$a_steps %||% 6)))
    auto$shoe_net <- 0; auto$stopped <- FALSE
  }
  observeEvent(list(input$a_on, input$a_system, input$a_steps), reset_auto())
  output$a_help <- renderUI(p(class = "small text-muted", SYSTEM_HELP[[input$a_system]]))

  new_play_shoe <- function() {
    play$shoe <- new_shoe(); play$h <- play_shoe(play$shoe); play$k <- 0; play$last <- NULL
    reset_auto()
  }
  observe({ isolate({ new_play_shoe(); play$bank <- play$start <- input$p_bankroll }) })
  observeEvent(input$p_bankroll, { play$bank <- play$start <- input$p_bankroll; play$log <- NULL },
               ignoreInit = TRUE)
  observeEvent(input$p_new, { new_play_shoe(); play$log <- NULL })

  BET_NAME <- c(P = "Player", B = "Banker", T = "Tie", D = "Dragon 7", N = "Panda 8",
                DP = "Dragon Bonus Player", DB = "Dragon Bonus Banker")
  PAY_COL  <- c(P = "Player", B = "Banker", T = "Tie", D = "Dragon7", N = "Panda8",
                DP = "DragonBonusP", DB = "DragonBonusB")
  # Dragon 7 is an EZ Baccarat bet, so a Dragon count system sits out at a standard table
  dragon_unavailable <- function(side) identical(side, "D") && identical(input$p_game, "standard")
  stop_autoplay <- function(msg = NULL) {
    auto$remaining <- 0; auto$running <- FALSE; if (!is.null(msg)) auto$msg <- msg
  }

  do_deal <- function() {
    if (play$k >= nrow(play$h)) {
      # Start the next shoe and stop on the burn, so it is always seen before hand 1
      showNotification("Shoe finished. New shoe: burn cards shown, press Deal for the first hand.",
                       type = "message")
      new_play_shoe(); return(TRUE)
    }
    k <- play$k + 1
    winners <- play$h$winner[seq_len(k - 1)]
    last <- tail(c(NA_character_, winners[winners != "T"]), 1)
    bets <- c(P = 0, B = 0, T = 0, D = 0, N = 0, DP = 0, DB = 0)
    if (isTRUE(input$a_on)) {
      if (!auto$stopped) {
        d <- system_decide(auto_cfg(), auto$prog, last, play$h$tc[k])
        if (d$stake > 0 && !dragon_unavailable(d$side)) bets[[d$side]] <- d$stake
      }
    } else {
      offered <- c("P", "B", "T", "DP", "DB", if (input$p_game == "ez") c("D", "N"))
      for (b in offered) bets[[b]] <- input[[paste0("bet_", b)]] %||% 0
      bets[is.na(bets) | bets < 0] <- 0
    }
    if (sum(bets) > play$bank) {
      stop_autoplay("Stopped: the next bet is more than your bankroll.")
      showNotification("Total bets exceed your bankroll.", type = "error"); return(FALSE)
    }
    play$k <- k
    pay <- unlist(settle(play$h[k, ], input$p_game))[PAY_COL[names(bets)]]
    names(pay) <- names(bets)
    net <- sum(bets * pay)
    play$bank <- play$bank + net
    play$last <- list(net = net, bet = sum(bets))

    if (isTRUE(input$a_on)) {
      side <- names(bets)[bets > 0]
      if (length(side) && pay[[side]] != 0) auto$prog$update(pay[[side]] > 0)
      w <- play$h$winner[k]
      if (w != "T" && !is.na(last) && w != last) auto$prog$side_changed()
      auto$shoe_net <- auto$shoe_net + net
      cfg <- auto_cfg()
      if (!auto$stopped && cfg$stop_loss > 0 && auto$shoe_net <= -cfg$stop_loss) {
        auto$stopped <- TRUE; stop_autoplay("Stop-loss reached: no more bets this shoe.")
      } else if (!auto$stopped && cfg$take_profit > 0 && auto$shoe_net >= cfg$take_profit) {
        auto$stopped <- TRUE; stop_autoplay("Take-profit reached: no more bets this shoe.")
      }
    }

    hc <- hand_cards(play$shoe, play$h, k)
    placed <- bets[bets > 0]
    play$log <- rbind(data.frame(
      Hand = as.integer(k),
      Player = paste(card_label(hc$player), collapse = " "), P = as.integer(play$h$p_total[k]),
      Banker = paste(card_label(hc$banker), collapse = " "), B = as.integer(play$h$b_total[k]),
      Result = result_text(play$h, k, game = input$p_game),
      `Bet on` = if (length(placed)) paste(BET_NAME[names(placed)], placed, collapse = ", ") else "–",
      Net = sprintf("%+.2f", net), check.names = FALSE), play$log)
    TRUE
  }
  observeEvent(input$deal, do_deal())

  # Autoplay: deal one hand per tick until the count runs out or a limit stops it
  observeEvent(input$a_go, {
    auto$msg <- NULL; auto$remaining <- max(0, input$a_hands %||% 0); auto$running <- auto$remaining > 0
  })
  observeEvent(input$a_stop, stop_autoplay("Autoplay stopped."))
  observeEvent(input$a_on, if (!isTRUE(input$a_on)) stop_autoplay())
  # One hand per timer tick. The loop depends only on auto$running, which changes when autoplay
  # starts or stops. (Depending on auto$remaining, which every tick changes, made the observer
  # re-run at once instead of waiting for the timer, so all hands were dealt in one burst and
  # the screen, including Session net, only refreshed at the end.)
  observe({
    req(auto$running)
    invalidateLater(max(100, (input$a_speed %||% 0.6) * 1000))
    isolate({
      if (auto$remaining <= 0) return()
      ok <- do_deal()
      auto$remaining <- if (isTRUE(ok)) auto$remaining - 1 else 0
      if (auto$remaining == 0) {
        auto$running <- FALSE
        if (is.null(auto$msg)) auto$msg <- "Autoplay finished."
      }
    })
  })
  output$a_status <- renderUI({
    if (!isTRUE(input$a_on)) return(NULL)
    k <- play$k + 1; winners <- play$h$winner[seq_len(play$k)]
    last <- tail(c(NA_character_, winners[winners != "T"]), 1)
    nxt <- if (auto$stopped) "No more bets this shoe (limit reached)"
           else if (k > nrow(play$h)) "Shoe finished"
           else {
             d <- system_decide(auto_cfg(), auto$prog, last, play$h$tc[k])
             if (dragon_unavailable(d$side) || (input$a_system == "Dragon count" && input$p_game == "standard"))
               "Sitting out: Dragon 7 is only offered in EZ Baccarat"
             else if (d$stake > 0) sprintf("Next bet: %s %s", BET_NAME[[d$side]], format(d$stake))
             else "Next hand: sitting out"
           }
    div(class = "count-box mb-2 d-flex gap-3 flex-wrap",
        strong(input$a_system), span(nxt),
        span(sprintf("Shoe result %+.2f", auto$shoe_net)),
        if (auto$remaining > 0) span(class = "hot", sprintf("Autoplay: %d left", auto$remaining)),
        if (!is.null(auto$msg) && auto$remaining == 0) em(auto$msg))
  })

  output$p_bank <- renderUI(sprintf("%.2f", play$bank))
  output$p_net <- renderUI({
    n <- play$bank - play$start
    span(class = if (n >= 0) "pos-net" else "neg-net", sprintf("%+.2f", n))
  })
  output$p_hand <- renderText(sprintf("%d of %d", play$k, nrow(play$h)))
  output$p_left <- renderText(cards_left_after(play$h, play$k))

  side_cards <- function(which) renderUI({
    if (play$k == 0) return(NULL)   # the burn is shown across the table instead
    hc <- hand_cards(play$shoe, play$h, play$k)
    div(class = "cards", lapply(hc[[which]], card_html))
  })
  output$p_pcards <- side_cards("player"); output$p_bcards <- side_cards("banker")
  output$p_ptotal <- renderUI(if (play$k) div(class = "total", play$h$p_total[play$k]))
  output$p_btotal <- renderUI(if (play$k) div(class = "total", play$h$b_total[play$k]))
  output$p_banner <- renderUI({
    k <- play$k; if (k == 0) return(burn_html(play$shoe, show_tag = isTRUE(input$p_showcount)))
    h <- play$h
    ez <- input$p_game == "ez"
    sp <- if (ez && h$dragon7[k]) "DRAGON 7" else if (ez && h$panda8[k]) "PANDA 8" else if (h$natural[k]) "Natural"
    div(class = "banner", result_text(h, k, specials = FALSE), if (!is.null(sp)) span(class = "special", sp),
        if (play$last$bet > 0) div(style = "font-size: 1.1rem; font-weight: 500",
          if (play$last$net == 0) "Push: your bet is returned"
          else sprintf("You %s %.2f", if (play$last$net > 0) "won" else "lost", abs(play$last$net))),
        if (k == nrow(h)) div(style = "font-size: 1rem", "Cut card is out: that was the last hand of the shoe."))
  })
  output$p_count <- renderUI({
    if (!isTRUE(input$p_showcount)) return(NULL)
    rc <- rc_after(play$shoe, play$h, play$k); left <- cards_left_after(play$h, play$k)
    tc <- rc / (left / 52)
    div(class = "count-box my-2", sprintf("Running count %+d  ·  True count %+.1f  ", rc, tc),
        if (tc >= 4) span(class = "hot", "Dragon bet favorable"))
  })
  output$p_bead <- renderUI(bead_plate_html(play$h$winner[seq_len(play$k)]))
  output$p_big  <- renderUI(big_road_html(play$h$winner[seq_len(play$k)]))
  output$p_history <- renderTable(head(play$log, 10), striped = TRUE, spacing = "s")

  # ---- Simulate ----
  sim <- eventReactive(input$s_run, {
    validate(need(length(input$s_systems) > 0, "Pick at least one system."))
    withProgress(message = "Dealing shoes...", value = 0, {
      seed <- if (is.na(input$s_seed)) NULL else input$s_seed
      h <- simulate_shoes(input$s_shoes, decks = input$s_decks, cut_card = input$s_cut,
                          seed = seed, progress = FALSE)
      incProgress(0.2)
      systems <- default_systems(input$s_systems, side = input$s_side,
        base = max(1, input$s_base %||% 10), table_max = max(1, input$s_max %||% 1000),
        max_steps = max(1, input$s_steps %||% 6), stop_loss = input$s_sl %||% 0,
        take_profit = input$s_tp %||% 0, threshold = input$s_thresh)
      # Dragon 7 is EZ-only, so the Dragon count has nothing to bet at a standard table
      skipped <- input$s_game == "standard" && "Dragon count" %in% names(systems)
      if (skipped) systems[["Dragon count"]] <- NULL
      validate(need(length(systems) > 0, "Dragon count needs EZ Baccarat: pick another system or switch the game to EZ."))
      names(systems)[names(systems) == "Dragon count"] <- sprintf("Dragon count (TC ≥ %g)", input$s_thresh)
      runs <- list()
      for (nm in names(systems)) {   # one at a time so the progress bar moves
        incProgress(0.7 / length(systems), message = paste("Scoring", nm))
        runs[[nm]] <- play_strategy(h, systems[[nm]], game = input$s_game)
      }
      cs <- as.data.frame(do.call(rbind, lapply(runs, `[[`, "summary")))
      attr(cs, "runs") <- runs
      incProgress(0.05, message = "Computing exact odds...")
      edge <- house_edge(h, input$s_game, decks = input$s_decks)
      cf <- count_frequency_summary(h, input$s_thresh)
      dps <- dragons_per_shoe(h, input$s_thresh)
      list(h = h, cs = cs, edge = edge, game = input$s_game, skipped = skipped, cf = cf, dps = dps)
    })
  })
  output$s_summary <- renderUI({
    if (input$s_run == 0) return(div(class = "alert alert-light",
      "Choose settings and press Run simulation. 1,000 shoes with six systems takes about 5–10 seconds."))
    s <- sim(); tb <- table(factor(s$h$winner, c("B", "P", "T")))
    div(class = "alert alert-success", sprintf(
      "%s hands in %s shoes · Banker %.2f%% · Player %.2f%% · Tie %.2f%% · Dragon 7 %.2f%% · Panda 8 %.2f%%",
      format(nrow(s$h), big.mark = ","), format(max(s$h$shoe), big.mark = ","),
      100 * tb[["B"]] / nrow(s$h), 100 * tb[["P"]] / nrow(s$h), 100 * tb[["T"]] / nrow(s$h),
      100 * mean(s$h$dragon7), 100 * mean(s$h$panda8)))
  })
  output$s_edge <- renderTable(setNames(transform(sim()$edge, bet = c(Player = "Player", Banker = "Banker",
      Tie = "Tie", Dragon7 = "Dragon 7", Panda8 = "Panda 8", DragonBonusP = "Dragon Bonus Player",
      DragonBonusB = "Dragon Bonus Banker")[bet]), c("Bet", "Simulated", "± 95% CI", "Exact")),
                               digits = 3, striped = TRUE)
  output$s_skipnote <- renderUI(if (isTRUE(sim()$skipped)) p(class = "small text-warning",
    "Dragon count was left out: the Dragon 7 bet is only offered in EZ Baccarat. Switch the game to EZ to include it."))
  output$s_strat <- renderTable({
    cs <- sim()$cs
    data.frame(System = rownames(cs), `Hands bet` = format(cs$hands_bet, big.mark = ","),
               Wagered = format(round(cs$wagered), big.mark = ","),
               Net = format(round(cs$net), big.mark = ","),
               `Return %` = sprintf("%+.2f", cs$return_pct),
               `Worst drawdown` = format(round(cs$worst_drawdown), big.mark = ","), check.names = FALSE)
  }, striped = TRUE)
  output$s_plot <- renderPlot({ s <- sim(); plot_report(s$h, game = s$game, cs = s$cs) }, res = 96)
  # ---- How often each shoe reaches the Dragon true count threshold ----
  output$s_cf_title <- renderText(if (input$s_run == 0) "How often a shoe reaches the Dragon count threshold"
    else sprintf("How often a shoe reaches a Dragon true count of +%g or higher", sim()$cf$threshold))
  output$s_cf_tiles <- renderUI({
    cf <- sim()$cf
    tile <- function(k, v) div(class = "col", div(class = "stat", k), div(class = "statv", v))
    div(class = "row g-2 mb-2",
      tile("Shoes that reach it", sprintf("%.1f%%", cf$pct_shoes_reaching)),
      tile("Qualifying hands per shoe", sprintf("%.1f avg · %g median", cf$mean_hands_per_shoe, cf$median_hands_per_shoe)),
      tile("Per shoe, when reached", sprintf("%.1f hands", cf$mean_hands_when_reached)),
      tile("First reached (median)", if (is.na(cf$median_first_hand)) "–" else sprintf("hand %g", cf$median_first_hand)),
      tile("Share of all hands", sprintf("%.1f%%", cf$pct_hands)))
  })
  output$s_cf_plot <- renderPlot({
    cf <- sim()$cf; d <- cf$distribution
    op <- par(mar = c(4.2, 4.2, 1, 1)); on.exit(par(op))
    b <- barplot(d$pct_shoes, names.arg = d$hands_per_shoe, col = c("#c8553d", rep("#2a6f97", 5)), border = NA,
                 ylim = c(0, max(d$pct_shoes) * 1.18), xlab = "Qualifying hands in the shoe", ylab = "% of shoes")
    text(b, d$pct_shoes, sprintf("%.1f%%", d$pct_shoes), pos = 3, cex = 0.85)
  }, res = 96)
  output$s_cf_dist <- renderTable(setNames(sim()$cf$distribution, c("Qualifying hands", "Shoes", "% of shoes")),
                                  striped = TRUE, spacing = "xs", digits = 1)
  output$s_cf_stage <- renderTable(setNames(sim()$cf$by_stage, c("Stage of the shoe", "% of qualifying hands")),
                                   striped = TRUE, spacing = "xs", digits = 1)

  # ---- Dragon 7s per shoe ----
  output$s_dps_tiles <- renderUI({
    d <- sim()$dps
    tile <- function(k, v, s = NULL) div(class = "col", div(class = "stat", k), div(class = "statv", v),
                                          if (!is.null(s)) div(class = "small text-muted", s))
    div(class = "row g-2 mb-2",
      tile("Most in one shoe", d$max_dragons, sprintf("shoes with that many: %s", format(d$record_shoes, big.mark = ","))),
      tile("Average per shoe", sprintf("%.2f", d$avg_dragons), sprintf("median %g", d$median_dragons)),
      tile("Shoes with none", sprintf("%.1f%%", d$pct_zero)),
      tile("Shoes with 5 or more", sprintf("%.1f%%", d$pct_5plus)),
      tile("Panda 8 per shoe", sprintf("%.2f", d$avg_panda), sprintf("most in one shoe: %d", d$max_panda)),
      tile(sprintf("Dragons at TC ≥ +%g", d$threshold), sprintf("%.1f%%", d$pct_dragons_at_threshold),
           sprintf("of all Dragons, on %.1f%% of hands", d$pct_hands_at_threshold)))
  })
  output$s_dps_plot <- renderPlot({
    d <- sim()$dps; x <- d$distribution[d$distribution$dragons <= d$max_dragons, ]
    op <- par(mar = c(4.2, 4.2, 1, 1)); on.exit(par(op))
    b <- barplot(x$pct_shoes, names.arg = x$dragons, border = NA,
                 col = ifelse(x$dragons == d$max_dragons, "#c8553d", "#2a6f97"),
                 ylim = c(0, max(x$pct_shoes, x$independent_pct) * 1.18),
                 xlab = "Dragon 7s in the shoe", ylab = "% of shoes")
    points(b, x$independent_pct, pch = 19, cex = 0.9)
    text(b, pmax(x$pct_shoes, x$independent_pct), format(x$shoes, big.mark = ","), pos = 3, cex = 0.75)
  }, res = 96)
  output$s_dps_dist <- renderTable({
    x <- sim()$dps$distribution
    # short headings: this table sits in a narrow column
    data.frame(Dragons = x$dragons, Shoes = format(x$shoes, big.mark = ","),
               `%` = sprintf("%.2f", x$pct_shoes), `Indep. %` = sprintf("%.2f", x$independent_pct),
               `Panda 8s` = format(x$panda_shoes, big.mark = ","), check.names = FALSE)
  }, striped = TRUE, spacing = "xs", align = "r")
  output$s_dps_rec_title <- renderText({
    d <- sim()$dps
    sprintf("The record shoe (#%s): %d Dragon 7s in %d hands", format(d$record_shoe, big.mark = ","),
            d$max_dragons, nrow(d$record))
  })
  output$s_dps_rec <- renderPlot({
    d <- sim()$dps; r <- d$record
    op <- par(mar = c(4.2, 4.2, 1, 1)); on.exit(par(op))
    yl <- range(c(r$true_count, 0, d$threshold)) + c(-1, 1)
    plot(r$hand, r$true_count, type = "l", lwd = 2, col = "#2a6f97", ylim = yl,
         xlab = "Hand", ylab = "Dragon true count", las = 1)
    abline(h = 0, col = "grey60"); abline(h = d$threshold, lty = 2, col = "grey40")
    hit <- r$dragon7
    points(r$hand[hit], r$true_count[hit], pch = 21, bg = "#c8553d", col = "white", cex = 1.6)
  }, res = 96)

  output$s_dragon <- renderTable(setNames(dragon_by_count(sim()$h),
    c("True count", "Hands", "% of hands", "EV %", "± 95% CI")), striped = TRUE)

  # ---- Trainer ----
  tr <- reactiveValues(shoe = NULL, h = NULL, k = 0, quiz = FALSE, right = 0, asked = 0, fb = NULL)
  new_trainer_shoe <- function() {
    tr$shoe <- new_shoe(); tr$h <- play_shoe(tr$shoe); tr$k <- 0; tr$quiz <- FALSE; tr$fb <- NULL
  }
  observe(isolate(new_trainer_shoe()))
  observeEvent(input$t_new, new_trainer_shoe())

  next_hand <- function() {
    if (tr$quiz) return()
    if (tr$k >= nrow(tr$h)) {
      tr$fb <- div(class = "feedback-ok", "End of shoe. Press New shoe to continue."); return()
    }
    tr$k <- tr$k + 1; tr$fb <- NULL
    if (input$t_every > 0 && tr$k %% input$t_every == 0) {
      tr$quiz <- TRUE; updateCheckboxInput(session, "t_auto", value = FALSE)
    }
  }
  observeEvent(input$t_next, next_hand())
  observe({
    req(input$t_auto)
    invalidateLater(input$t_speed * 1000)
    isolate(next_hand())
  })

  truth <- reactive({
    rc <- rc_after(tr$shoe, tr$h, tr$k); left <- cards_left_after(tr$h, tr$k)
    list(rc = rc, tc = rc / (left / 52), left = left)
  })
  observeEvent(input$t_check, {
    t <- truth()
    if (is.na(input$t_rc)) { tr$fb <- div(class = "feedback-bad", "Enter your running count first."); return() }
    rc_ok <- input$t_rc == t$rc
    tc_ok <- is.na(input$t_tc) || abs(input$t_tc - t$tc) <= 0.5
    tr$asked <- tr$asked + 1; tr$right <- tr$right + (rc_ok && tc_ok)
    tr$fb <- div(
      div(class = if (rc_ok) "feedback-ok" else "feedback-bad",
          if (rc_ok) "Running count correct!" else sprintf("Not quite: the running count is %+d.", t$rc)),
      if (!is.na(input$t_tc)) div(class = if (tc_ok) "feedback-ok" else "feedback-bad",
          sprintf("True count %+.2f (%d cards = %.1f decks left).", t$tc, t$left, t$left / 52)),
      if (t$tc >= 4) div(span(class = "hot", "Dragon bet is favorable here")))
    tr$quiz <- FALSE
    updateNumericInput(session, "t_rc", value = NA); updateNumericInput(session, "t_tc", value = NA)
  })
  observeEvent(input$t_reveal, {
    t <- truth(); tr$quiz <- FALSE
    tr$fb <- div(sprintf("Running count %+d · True count %+.2f", t$rc, t$tc),
                 if (t$tc >= 4) span(class = "hot ms-2", "Dragon bet favorable"))
  })

  output$t_hand  <- renderText(sprintf("%d of %d", tr$k, nrow(tr$h)))
  output$t_left  <- renderText(cards_left_after(tr$h, tr$k))
  output$t_decks <- renderText(sprintf("%.1f", cards_left_after(tr$h, tr$k) / 52))
  output$t_score <- renderText(if (tr$asked) sprintf("%d / %d", tr$right, tr$asked) else "–")
  t_side <- function(which) renderUI({
    if (tr$k == 0) {
      if (which == "player") return(div(class = "cards", div(class = "text-center",
        div("Burn card (counts):"), card_html(tr$shoe[1], tag = isTRUE(input$t_tags)))))
      return(div(class = "cards", em("Press Next hand to start")))
    }
    hc <- hand_cards(tr$shoe, tr$h, tr$k)
    div(class = "cards", lapply(hc[[which]], card_html, tag = isTRUE(input$t_tags)))
  })
  output$t_pcards <- t_side("player"); output$t_bcards <- t_side("banker")
  output$t_prompt <- renderUI(if (tr$quiz) div(class = "alert alert-warning py-2",
    "Quiz time! What is the running count now?"))
  output$t_feedback <- renderUI(tr$fb)
}

shinyApp(ui, server)
