# Baccarat Simulator — Shiny UI
# Run from the repo folder:  shiny::runApp()   (or open in RStudio and click "Run App")
# Needs: install.packages("shiny")   (bslib comes with it)

library(shiny)
# Suit symbols are Unicode; make sure R treats text as UTF-8 (already the default on
# Windows with R >= 4.2 and on macOS).
if (!isTRUE(l10n_info()[["UTF-8"]])) invisible(try(Sys.setlocale("LC_CTYPE", "C.UTF-8"), silent = TRUE))
source("baccarat.R", local = TRUE, encoding = "UTF-8")

# ---- Helpers ----------------------------------------------------------------
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
.feedback-ok { color: #2e7d32; font-weight: 700; } .feedback-bad { color: #c62828; font-weight: 700; }
"

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

  # ---- Play -------------------------------------------------------------------
  bslib::nav_panel("Play",
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(width = 300,
        radioButtons("p_game", "Game", c("Standard (5% commission)" = "standard",
                                         "EZ Baccarat (no commission)" = "ez")),
        numericInput("p_bankroll", "Starting bankroll", 1000, min = 10, step = 100),
        bet_input("bet_P", "Player", "1:1"),
        bet_input("bet_B", "Banker", "0.95:1 / EZ 1:1"),
        bet_input("bet_T", "Tie", "8:1"),
        bet_input("bet_D", "Dragon 7", "40:1"),
        bet_input("bet_N", "Panda 8", "25:1"),
        div(class = "d-flex gap-2",
            actionButton("deal", "Deal", class = "btn-primary flex-fill"),
            actionButton("p_new", "New shoe", class = "btn-outline-secondary")),
        checkboxInput("p_showcount", "Show Dragon count", FALSE)
      ),
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
        sliderInput("s_thresh", "Dragon bet when true count ≥", 0, 10, 4, step = 0.5),
        checkboxGroupInput("s_strats", "Strategies", names(default_strategies()),
                           selected = names(default_strategies())),
        actionButton("s_run", "Run simulation", class = "btn-primary w-100")
      ),
      uiOutput("s_summary"),
      bslib::layout_columns(col_widths = c(5, 7),
        bslib::card(bslib::card_header("House edge: simulated vs exact (%)"), tableOutput("s_edge")),
        bslib::card(bslib::card_header("Strategies (units)"), tableOutput("s_strat"))),
      bslib::card(bslib::card_header("Report"), plotOutput("s_plot", height = "720px")),
      bslib::card(bslib::card_header("Dragon 7 by true count"), tableOutput("s_dragon"))
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
  new_play_shoe <- function() {
    play$shoe <- new_shoe(); play$h <- play_shoe(play$shoe); play$k <- 0; play$last <- NULL
  }
  observe({ isolate({ new_play_shoe(); play$bank <- play$start <- input$p_bankroll }) })
  observeEvent(input$p_bankroll, { play$bank <- play$start <- input$p_bankroll; play$log <- NULL },
               ignoreInit = TRUE)
  observeEvent(input$p_new, { new_play_shoe(); play$log <- NULL })

  observeEvent(input$deal, {
    if (play$k >= nrow(play$h)) {
      showNotification("Shoe finished. Starting a new shoe.", type = "message"); new_play_shoe()
    }
    bets <- c(P = input$bet_P, B = input$bet_B, T = input$bet_T, D = input$bet_D, N = input$bet_N)
    bets[is.na(bets) | bets < 0] <- 0
    if (sum(bets) > play$bank) {
      showNotification("Total bets exceed your bankroll.", type = "error"); return()
    }
    play$k <- play$k + 1; k <- play$k
    pay <- unlist(settle(play$h[k, ], input$p_game))
    net <- sum(bets * pay[c("Player", "Banker", "Tie", "Dragon7", "Panda8")])
    play$bank <- play$bank + net
    play$last <- list(net = net, bet = sum(bets))
    hc <- hand_cards(play$shoe, play$h, k)
    play$log <- rbind(data.frame(
      Hand = as.integer(k),
      Player = paste(card_label(hc$player), collapse = " "), P = as.integer(play$h$p_total[k]),
      Banker = paste(card_label(hc$banker), collapse = " "), B = as.integer(play$h$b_total[k]),
      Result = paste0(RESULT_TEXT[[play$h$winner[k]]],
                      if (play$h$dragon7[k]) " – Dragon 7" else if (play$h$panda8[k]) " – Panda 8" else ""),
      Bet = sum(bets), Net = sprintf("%+.2f", net), check.names = FALSE), play$log)
  })

  output$p_bank <- renderUI(sprintf("%.2f", play$bank))
  output$p_net <- renderUI({
    n <- play$bank - play$start
    span(class = if (n >= 0) "pos-net" else "neg-net", sprintf("%+.2f", n))
  })
  output$p_hand <- renderText(sprintf("%d of %d", play$k, nrow(play$h)))
  output$p_left <- renderText(cards_left_after(play$h, play$k))

  side_cards <- function(which) renderUI({
    if (play$k == 0) return(div(class = "cards", em("Place your bets and press Deal")))
    hc <- hand_cards(play$shoe, play$h, play$k)
    div(class = "cards", lapply(hc[[which]], card_html))
  })
  output$p_pcards <- side_cards("player"); output$p_bcards <- side_cards("banker")
  output$p_ptotal <- renderUI(if (play$k) div(class = "total", play$h$p_total[play$k]))
  output$p_btotal <- renderUI(if (play$k) div(class = "total", play$h$b_total[play$k]))
  output$p_banner <- renderUI({
    k <- play$k; if (k == 0) return(div(class = "banner"))
    h <- play$h
    sp <- if (h$dragon7[k]) "DRAGON 7" else if (h$panda8[k]) "PANDA 8" else if (h$natural[k]) "Natural"
    div(class = "banner", RESULT_TEXT[[h$winner[k]]], if (!is.null(sp)) span(class = "special", sp),
        if (play$last$bet > 0) div(style = "font-size: 1.1rem; font-weight: 500",
          sprintf("You %s %.2f", if (play$last$net >= 0) "won" else "lost", abs(play$last$net))),
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
    validate(need(length(input$s_strats) > 0, "Pick at least one strategy."))
    withProgress(message = "Dealing shoes...", value = 0, {
      seed <- if (is.na(input$s_seed)) NULL else input$s_seed
      h <- simulate_shoes(input$s_shoes, decks = input$s_decks, cut_card = input$s_cut,
                          seed = seed, progress = FALSE)
      incProgress(0.3, message = "Scoring strategies...")
      strats <- default_strategies()
      thr <- input$s_thresh
      strats[["Dragon count TC>=4"]] <- function(s) strategy_dragon_count(s, threshold = thr)
      strats <- strats[input$s_strats]
      names(strats)[names(strats) == "Dragon count TC>=4"] <- sprintf("Dragon count TC>=%g", thr)
      cs <- compare_strategies(h, strats, game = input$s_game)
      incProgress(0.6, message = "Computing exact odds...")
      edge <- house_edge(h, input$s_game, decks = input$s_decks)
      list(h = h, cs = cs, edge = edge, game = input$s_game)
    })
  })
  output$s_summary <- renderUI({
    if (input$s_run == 0) return(div(class = "alert alert-light",
      "Choose settings and press Run simulation. 1,000 shoes takes about 5 seconds."))
    s <- sim(); tb <- table(factor(s$h$winner, c("B", "P", "T")))
    div(class = "alert alert-success", sprintf(
      "%s hands in %s shoes · Banker %.2f%% · Player %.2f%% · Tie %.2f%% · Dragon 7 %.2f%% · Panda 8 %.2f%%",
      format(nrow(s$h), big.mark = ","), format(max(s$h$shoe), big.mark = ","),
      100 * tb[["B"]] / nrow(s$h), 100 * tb[["P"]] / nrow(s$h), 100 * tb[["T"]] / nrow(s$h),
      100 * mean(s$h$dragon7), 100 * mean(s$h$panda8)))
  })
  output$s_edge <- renderTable(setNames(sim()$edge, c("Bet", "Simulated", "± 95% CI", "Exact")),
                               digits = 3, striped = TRUE)
  output$s_strat <- renderTable({
    cs <- sim()$cs
    data.frame(Strategy = rownames(cs), `Hands bet` = format(cs$hands_bet, big.mark = ","),
               Wagered = format(cs$units_wagered, big.mark = ","), Net = cs$net_units,
               `Return %` = cs$return_pct, `Worst drawdown` = cs$worst_drawdown, check.names = FALSE)
  }, striped = TRUE)
  output$s_plot <- renderPlot({ s <- sim(); plot_report(s$h, game = s$game, cs = s$cs) }, res = 96)
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
