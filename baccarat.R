# ........................................................
# Title .................. Baccarat Simulator v2
# Author ................. Raoul R. Diez (original 2016), v2 rewrite 2026
# Description ............ Fast, rule-accurate Punto Banco simulator with:
#   * integer card engine (no string parsing), burn + cut-card handling
#   * standard (5% commission) and EZ Baccarat (Dragon 7 push) payouts
#   * side bets: Tie 8:1, Dragon 7 40:1, Panda 8 25:1
#   * exact 8-deck odds by full enumeration (to validate the simulator)
#   * Dragon 7 card counting (Jacobson tags) with true count per hand
#   * pluggable betting strategies with bankroll tracking
#   * bead-plate display and summary plots (base R only, no packages)
#
# Quick start:
#   source("baccarat.R")
#   play_shoe(new_shoe(), verbose = TRUE)             # watch one shoe
#   h <- simulate_shoes(2000, seed = 42)              # simulate
#   house_edge(h, game = "ez")                        # observed vs exact
#   compare_strategies(h, game = "standard")          # betting systems
#   dragon_by_count(h)                                # does counting work?
#   plot_report(h, file = "report.png")
# ........................................................

# ---- Cards ---------------------------------------------------------------
# A card is an integer id 1..52: rank = ((id - 1) %% 13) + 1, suit = (id - 1) %/% 13 + 1
RANK_LABEL  <- c("A", 2:10, "J", "Q", "K")
SUIT_LABEL  <- c("♠", "♥", "♦", "♣")
CARD_VALUE  <- c(1:9, 0, 0, 0, 0)   # baccarat point value by rank
BURN_VALUE  <- c(1:10, 10, 10, 10)  # face value used to size the burn

# Jacobson's Dragon 7 count, indexed by point value 0..9 (+1 offset):
#            0   1  2  3   4   5   6   7  8  9
DRAGON_TAGS <- c(0, 0, 0, 0, -1, -1, -1, -1, 2, 2)

card_rank <- function(id) ((id - 1L) %% 13L) + 1L
card_label <- function(id) paste0(RANK_LABEL[card_rank(id)], SUIT_LABEL[(id - 1L) %/% 13L + 1L])

new_shoe <- function(decks = 8) sample(rep.int(1:52, decks))

# ---- Drawing rules ---------------------------------------------------------
# BANKER_DRAW[banker_total + 1, k]: k = player's third-card value + 1 (1..10),
# or k = 11 when the Player stood. TRUE means Banker draws.
BANKER_DRAW <- local({
  m <- matrix(FALSE, 10, 11)
  for (b in 0:9) {
    m[b + 1, 11] <- b <= 5
    for (p3 in 0:9) {
      m[b + 1, p3 + 1] <- switch(as.character(b),
        "0" = , "1" = , "2" = TRUE,
        "3" = p3 != 8,
        "4" = p3 %in% 2:7,
        "5" = p3 %in% 4:7,
        "6" = p3 %in% 6:7,
        FALSE)
    }
  }
  m
})

# ---- Play one shoe -------------------------------------------------------
# shoe     : integer card ids (from new_shoe) in deal order
# cut_card : cards behind the cut card; when it comes out, one more hand is dealt
# tags     : count tags by point value 0..9 (default: Dragon 7 count)
play_shoe <- function(shoe = new_shoe(), cut_card = 14, burn = TRUE,
                      tags = DRAGON_TAGS, verbose = FALSE, as_df = TRUE) {
  N    <- length(shoe)
  val  <- CARD_VALUE[card_rank(shoe)]
  pos  <- 1L
  rc   <- 0

  if (burn) {
    nburn <- BURN_VALUE[card_rank(shoe[1])]
    rc    <- rc + tags[val[1] + 1]          # only the first burn card is exposed
    if (verbose) cat(sprintf("Burn card %s -> burning %d more face down\n",
                             card_label(shoe[1]), nburn))
    pos <- pos + 1L + nburn
  }

  max_h <- ceiling(N / 4)
  out <- list(
    hand = integer(max_h), p_total = integer(max_h), b_total = integer(max_h),
    p_cards = integer(max_h), b_cards = integer(max_h), p3 = integer(max_h),
    winner = character(max_h), natural = logical(max_h),
    dragon7 = logical(max_h), panda8 = logical(max_h),
    cards_left = integer(max_h), rc = numeric(max_h), tc = numeric(max_h),
    first = integer(max_h), used = integer(max_h)   # position of the hand in the shoe
  )

  k <- 0L
  cut_seen <- FALSE
  repeat {
    left <- N - pos + 1L
    if (left < 6L) break
    k <- k + 1L
    out$cards_left[k] <- left
    out$rc[k] <- rc
    out$tc[k] <- rc / (left / 52)

    i  <- pos
    p  <- (val[i] + val[i + 2L]) %% 10L
    b  <- (val[i + 1L] + val[i + 3L]) %% 10L
    used <- 4L
    pn <- 2L; bn <- 2L; p3 <- NA_integer_
    natural <- p >= 8L || b >= 8L

    if (!natural) {
      if (p <= 5L) {
        p3 <- val[i + used]; used <- used + 1L; pn <- 3L
        p  <- (p + p3) %% 10L
        col <- p3 + 1L
      } else {
        col <- 11L
      }
      if (BANKER_DRAW[b + 1L, col]) {
        b <- (b + val[i + used]) %% 10L; used <- used + 1L; bn <- 3L
      }
    }

    idx <- i:(i + used - 1L)
    rc  <- rc + sum(tags[val[idx] + 1L])
    pos <- pos + used

    w <- if (p > b) "P" else if (b > p) "B" else "T"
    out$hand[k] <- k; out$p_total[k] <- p; out$b_total[k] <- b
    out$p_cards[k] <- pn; out$b_cards[k] <- bn; out$p3[k] <- p3
    out$winner[k] <- w; out$natural[k] <- natural
    out$first[k] <- i; out$used[k] <- used
    out$dragon7[k] <- w == "B" && bn == 3L && b == 7L
    out$panda8[k]  <- w == "P" && pn == 3L && p == 8L

    if (verbose) {
      pc <- shoe[idx[c(1, 3, if (pn == 3) 5)]]
      bc <- shoe[idx[c(2, 4, if (bn == 3) used)]]
      tag <- if (out$dragon7[k]) "  DRAGON 7" else if (out$panda8[k]) "  PANDA 8" else ""
      cat(sprintf("%3d  P: %-13s = %d   B: %-13s = %d   %s%s%s\n", k,
                  paste(card_label(pc), collapse = " "), p,
                  paste(card_label(bc), collapse = " "), b,
                  c(P = "Player", B = "Banker", T = "Tie")[[w]],
                  if (natural) " (natural)" else "", tag))
    }

    if (cut_seen) break
    if (N - pos + 1L <= cut_card) {
      cut_seen <- TRUE
      if (verbose) cat("---- cut card out: one more hand ----\n")
    }
  }

  out <- lapply(out, `[`, seq_len(k))
  out$p3[out$p_cards == 2L] <- NA_integer_
  if (verbose) {
    cat("\n"); print(table(factor(out$winner, c("B", "P", "T"))))
    cat("\n"); print_bead_plate(out$winner)
  }
  if (!as_df) return(out)   # raw list: much faster for bulk simulation
  invisible(as.data.frame(out, stringsAsFactors = FALSE))
}

# ---- Simulate many shoes -------------------------------------------------
# shoes: optional preloaded shoes (matrix with one shoe per row, or list).
#        Using `seed` is the lighter-weight way to make runs reproducible.
simulate_shoes <- function(n_shoes = 1000, decks = 8, cut_card = 14, seed = NULL,
                           shoes = NULL, tags = DRAGON_TAGS, progress = interactive()) {
  if (!is.null(seed)) set.seed(seed)
  if (is.matrix(shoes)) shoes <- split(shoes, row(shoes))
  if (!is.null(shoes)) n_shoes <- length(shoes)
  if (progress) pb <- txtProgressBar(0, n_shoes, style = 3)
  res <- vector("list", n_shoes)
  for (s in seq_len(n_shoes)) {
    sh <- if (is.null(shoes)) new_shoe(decks) else shoes[[s]]
    res[[s]] <- play_shoe(sh, cut_card = cut_card, tags = tags, as_df = FALSE)
    if (progress) setTxtProgressBar(pb, s)
  }
  if (progress) close(pb)
  cols <- names(res[[1]])
  h <- lapply(setNames(cols, cols), function(cn) unlist(lapply(res, `[[`, cn), use.names = FALSE))
  h <- as.data.frame(h, stringsAsFactors = FALSE)
  h$shoe <- rep(seq_len(n_shoes), vapply(res, function(r) length(r$hand), 1L))
  h[c("shoe", setdiff(names(h), "shoe"))]
}

# ---- Payouts -------------------------------------------------------------
# Which side bets each game offers. Both games carry the Dragon Bonus on Player or Banker;
# EZ Baccarat (no commission) adds Dragon 7 (40:1) and Panda 8 (25:1).
SIDE_BETS <- list(standard = c("DragonBonusP", "DragonBonusB"),
                  ez       = c("DragonBonusP", "DragonBonusB", "Dragon7", "Panda8"))

# Dragon Bonus: natural winner 1 to 1, natural tie push; a non-natural winner pays by margin.
# Everything else (including a non-natural tie) loses.
DRAGON_BONUS_PAY <- c(`4` = 1, `5` = 2, `6` = 4, `7` = 6, `8` = 10, `9` = 30)

# Net per 1 unit on the Dragon Bonus for `side` ("P" or "B"). A natural ends the hand with
# two cards each, so a natural winner always holds the 8 or 9.
dragon_bonus <- function(h, side = c("P", "B")) {
  side <- match.arg(side)
  if (!all(c("p_total", "b_total", "natural") %in% names(h))) return(rep(NA_real_, nrow(h)))
  margin <- if (side == "P") h$p_total - h$b_total else h$b_total - h$p_total
  win_pay <- ifelse(margin >= 4, DRAGON_BONUS_PAY[as.character(pmin(pmax(margin, 4), 9))], -1)
  ifelse(h$natural, ifelse(margin > 0, 1, ifelse(margin == 0, 0, -1)),
         ifelse(margin > 0, win_pay, -1))
}

# Net result per 1 unit wagered on each bet, for every hand.
settle <- function(h, game = c("standard", "ez")) {
  game <- match.arg(game)
  P <- h$winner == "P"; B <- h$winner == "B"; T <- h$winner == "T"
  banker_win <- if (game == "standard") 0.95 else ifelse(h$dragon7, 0, 1)
  data.frame(
    Player  = ifelse(P, 1, ifelse(B, -1, 0)),
    Banker  = ifelse(B, banker_win, ifelse(P, -1, 0)),
    Tie     = ifelse(T, 8, -1),
    Dragon7 = ifelse(h$dragon7, 40, -1),
    Panda8  = ifelse(h$panda8, 25, -1),
    DragonBonusP = unname(dragon_bonus(h, "P")),
    DragonBonusB = unname(dragon_bonus(h, "B"))
  )
}

# ---- Exact odds by enumeration ---------------------------------------------
# Enumerates every ordered 6-card sequence by point value (10^6 rows) and weights it
# by its exact probability off the top of a fresh shoe. Unused cards marginalise out.
# Results are cached per deck count, since the enumeration takes a second or two.
.exact_cache <- new.env()
exact_odds <- function(decks = 8) {
  key <- as.character(decks)
  if (is.null(.exact_cache[[key]])) .exact_cache[[key]] <- .exact_odds_compute(decks)
  .exact_cache[[key]]
}

.exact_odds_compute <- function(decks) {
  n <- c(16, rep(4, 9)) * decks              # cards of each point value 0..9
  N <- sum(n)
  g <- as.matrix(expand.grid(rep(list(0:9), 6)))
  prob <- rep(1, nrow(g))
  for (k in 1:6) {
    prior <- if (k == 1) 0 else rowSums(g[, 1:(k - 1), drop = FALSE] == g[, k])
    prob <- prob * (n[g[, k] + 1] - prior) / (N - k + 1)
  }
  p <- (g[, 1] + g[, 3]) %% 10; b <- (g[, 2] + g[, 4]) %% 10
  nat <- p >= 8 | b >= 8
  pdraw <- !nat & p <= 5
  pf <- ifelse(pdraw, (p + g[, 5]) %% 10, p)
  col <- ifelse(pdraw, g[, 5] + 1, 11)
  bdraw <- !nat & BANKER_DRAW[cbind(b + 1, col)]
  bf <- ifelse(bdraw, (b + ifelse(pdraw, g[, 6], g[, 5])) %% 10, b)
  w <- ifelse(pf > bf, "P", ifelse(bf > pf, "B", "T"))
  d7 <- w == "B" & bdraw & bf == 7
  p8 <- w == "P" & pdraw & pf == 8
  pr <- function(x) sum(prob[x])
  probs <- c(Banker = pr(w == "B"), Player = pr(w == "P"), Tie = pr(w == "T"),
             Dragon7 = pr(d7), Panda8 = pr(p8))
  db_ev <- function(margin) {   # Dragon Bonus expected value for one side
    pay <- ifelse(nat, ifelse(margin > 0, 1, ifelse(margin == 0, 0, -1)),
                  ifelse(margin >= 4, DRAGON_BONUS_PAY[as.character(pmin(pmax(margin, 4), 9))], -1))
    sum(prob * pay)
  }
  edge <- function(x) -x  # house edge = -EV
  ev <- c(
    Player_std  = probs["Player"] - probs["Banker"],
    Banker_std  = 0.95 * probs["Banker"] - probs["Player"],
    Player_ez   = probs["Player"] - probs["Banker"],
    Banker_ez   = (probs["Banker"] - probs["Dragon7"]) - probs["Player"],
    Tie         = 8 * probs["Tie"] - (1 - probs["Tie"]),
    Dragon7     = 40 * probs["Dragon7"] - (1 - probs["Dragon7"]),
    Panda8      = 25 * probs["Panda8"] - (1 - probs["Panda8"]),
    DragonBonusP = db_ev(pf - bf),
    DragonBonusB = db_ev(bf - pf)
  )
  names(ev) <- sub("\\..*", "", names(ev))
  list(prob = probs, house_edge = edge(ev))
}

# ---- Observed vs exact house edge -------------------------------------------
house_edge <- function(h, game = c("standard", "ez"), decks = 8) {
  game <- match.arg(game)
  # only the bets this game actually offers
  s  <- settle(h, game)[c("Player", "Banker", "Tie", SIDE_BETS[[game]])]
  ex <- exact_odds(decks)$house_edge
  key <- c(Player = paste0("Player_", if (game == "standard") "std" else "ez"),
           Banker = paste0("Banker_", if (game == "standard") "std" else "ez"),
           Tie = "Tie", Dragon7 = "Dragon7", Panda8 = "Panda8",
           DragonBonusP = "DragonBonusP", DragonBonusB = "DragonBonusB")[names(s)]
  data.frame(
    bet          = names(s),
    sim_edge_pct = round(-colMeans(s) * 100, 3),
    ci95_pct     = round(1.96 * apply(s, 2, sd) / sqrt(nrow(s)) * 100, 3),
    exact_pct    = round(ex[key] * 100, 3),
    row.names = NULL
  )
}

# ---- Betting strategies ---------------------------------------------------
# A strategy is function(state) -> list(bet = "P"/"B"/"T"/"D"/"none", units = n).
# state: last (last non-tie winner this shoe, or NA), streak (its length),
# losses (consecutive losing bets), tc (Dragon true count before this hand).
strategy_flat_banker <- function(state) list(bet = "B", units = 1)
strategy_flat_player <- function(state) list(bet = "P", units = 1)

# Raoul's 2016 system: bet opposite the last result, +1 unit per loss, give up
# after 3 losses and wait for the side to change. Within a streak of length s,
# s - 1 bets have already lost, so the stake is simply s units.
strategy_chop_progression <- function(state, max_losses = 3) {
  if (is.na(state$last) || state$streak > max_losses) return(list(bet = "none", units = 0))
  list(bet = if (state$last == "B") "P" else "B", units = state$streak)
}

# Follow the streak: bet the same side as the last result.
strategy_follow <- function(state) {
  if (is.na(state$last)) return(list(bet = "B", units = 1))
  list(bet = state$last, units = 1)
}

# Martingale on Banker: double after each loss, reset after win or 6 losses.
strategy_martingale <- function(state, max_steps = 6) {
  list(bet = "B", units = 2^(state$losses %% max_steps))
}

# Count the Dragon: bet the Dragon 7 only when the true count is +4 or more.
strategy_dragon_count <- function(state, threshold = 4) {
  if (state$tc >= threshold) list(bet = "D", units = 1) else list(bet = "none", units = 0)
}

play_strategy <- function(h, strategy, game = c("standard", "ez")) {
  game <- match.arg(game)
  if (inherits(strategy, "betting_system")) return(run_system(h, strategy, game))
  # Plain vectors/matrix: indexing data-frame columns inside the loop is ~5x slower
  pay <- as.matrix(settle(h, game)[c("Player", "Banker", "Tie", "Dragon7")])
  payout_col <- c(P = 1L, B = 2L, T = 3L, D = 4L)
  new_shoe_at <- c(TRUE, h$shoe[-1] != h$shoe[-nrow(h)])
  tc <- h$tc; winner <- h$winner
  n <- nrow(h)
  net <- wagered <- numeric(n)
  for (i in seq_len(n)) {
    if (new_shoe_at[i]) {
      last <- NA_character_; streak <- 0; losses <- 0
    }
    d <- strategy(list(last = last, streak = streak, losses = losses, tc = tc[i]))
    if (d$bet != "none" && d$units > 0) {
      r <- pay[i, payout_col[[d$bet]]] * d$units
      wagered[i] <- d$units
      net[i] <- r
      if (r < 0) losses <- losses + 1 else if (r > 0) losses <- 0
    }
    w <- winner[i]
    if (w != "T") {
      streak <- if (!is.na(last) && w == last) streak + 1 else 1
      last <- w
    }
  }
  summarise_run(net, wagered)
}

# Shared result format for function strategies and configurable systems.
summarise_run <- function(net, wagered) {
  cum <- cumsum(net)
  list(net = net, wagered = wagered, cumulative = cum,
       summary = c(hands_bet = sum(wagered > 0), wagered = sum(wagered),
                   net = round(sum(net), 2),
                   return_pct = round(100 * sum(net) / max(1, sum(wagered)), 3),
                   worst_drawdown = round(min(cum - cummax(pmax(cum, 0))), 2)))
}

default_strategies <- function() list(
  "Flat Banker"            = strategy_flat_banker,
  "Flat Player"            = strategy_flat_player,
  "Chop progression (2016)" = strategy_chop_progression,
  "Follow the streak"      = strategy_follow,
  "Martingale Banker"      = strategy_martingale,
  "Dragon count TC>=4"     = strategy_dragon_count
)

compare_strategies <- function(h, strategies = default_strategies(), game = c("standard", "ez")) {
  game <- match.arg(game)
  runs <- lapply(strategies, play_strategy, h = h, game = game)
  tab <- as.data.frame(do.call(rbind, lapply(runs, `[[`, "summary")))
  attr(tab, "runs") <- runs   # per-hand detail, e.g. attr(x, "runs")[[1]]$cumulative
  tab
}

# ---- Configurable betting systems ---------------------------------------------
# A system = which side to bet x how to size the bet, plus money-management limits.
# Progressions count wins and losses in units and ignore commission, as players do.
# Pushes (ties on Player/Banker, EZ Dragon 7 on Banker) leave the progression unchanged.
SYSTEMS <- c("Flat", "Martingale", "Paroli", "Fibonacci", "D'Alembert", "1-3-2-6",
             "Labouchere", "Oscar's Grind", "Chop progression (2016)", "Dragon count")

SYSTEM_HELP <- c(
  "Flat"                    = "Same bet every hand.",
  "Martingale"              = "Double after each loss; back to 1 unit after a win or after the step limit.",
  "Paroli"                  = "Double after each win; back to 1 unit after a loss or after 3 wins in a row.",
  "Fibonacci"               = "Move one step up 1,1,2,3,5,8... after a loss, two steps back after a win.",
  "D'Alembert"              = "Add 1 unit after a loss, take 1 off after a win.",
  "1-3-2-6"                 = "Bet 1, 3, 2, 6 units on consecutive wins; back to 1 after a loss or after the 6.",
  "Labouchere"              = "Line 1-2-3: bet first + last; cross both off on a win, add the bet to the line on a loss.",
  "Oscar's Grind"           = "Aim for +1 unit per series: raise 1 unit after a win, never past what finishes the series.",
  "Chop progression (2016)" = "Raoul's 2016 system: bet against the last result, +1 unit per loss, sit out after 3 losses until the side changes.",
  "Dragon count"            = "Flat Dragon 7 side bet only when the true count reaches the threshold."
)

# side: "B", "P", "opposite" (bet against the last result), "same" (bet the last result).
# Chop progression always bets "opposite"; Dragon count always bets the Dragon 7.
# stop_loss / take_profit: stop betting for the rest of the shoe once the shoe's
# result reaches -stop_loss or +take_profit (0 = off).
system_config <- function(system = "Flat", side = c("B", "P", "opposite", "same"), base = 10,
                          table_max = 1000, max_steps = 6, stop_loss = 0, take_profit = 0,
                          threshold = 4) {
  system <- match.arg(system, SYSTEMS)
  side <- match.arg(side)
  if (system == "Chop progression (2016)") side <- "opposite"
  if (system == "Dragon count") side <- "D"
  structure(list(system = system, side = side, base = base, table_max = table_max,
                 max_steps = max_steps, stop_loss = stop_loss, take_profit = take_profit,
                 threshold = threshold), class = "betting_system")
}

# Progression state machine. units() is the next bet size in units (0 = sit out);
# update(won) is called after every decided (non-push) bet.
make_progression <- function(system, max_steps = 6) {
  fib <- c(1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987)
  k <- 0; line <- c(1, 2, 3); u <- 1; p <- 0; sitout <- FALSE
  reset <- function() { k <<- 0; line <<- c(1, 2, 3); u <<- 1; p <<- 0; sitout <<- FALSE }
  units <- function() switch(system,
    "Martingale" = , "Paroli" = 2^k,
    "Fibonacci" = fib[k + 1],
    "D'Alembert" = 1 + k,
    "1-3-2-6" = c(1, 3, 2, 6)[k + 1],
    "Labouchere" = if (length(line) == 1) line else line[1] + line[length(line)],
    "Oscar's Grind" = u,
    "Chop progression (2016)" = if (sitout) 0 else 1 + k,
    1)
  update <- function(won) {
    bet <- units()
    switch(system,
      "Martingale" = { k <<- if (won) 0 else k + 1; if (k >= max_steps) k <<- 0 },
      "Paroli"     = { k <<- if (won) k + 1 else 0; if (k >= 3) k <<- 0 },
      "Fibonacci"  = { k <<- if (won) max(0, k - 2) else k + 1
                       if (k >= min(max_steps, length(fib))) k <<- 0 },
      "D'Alembert" = { k <<- if (won) max(0, k - 1) else k + 1; if (k >= max_steps) k <<- 0 },
      "1-3-2-6"    = { k <<- if (won) (k + 1) %% 4 else 0 },
      "Labouchere" = { line <<- if (won) line[-c(1, length(line))] else c(line, bet)
                       if (!length(line) || length(line) > 3 + max_steps) line <<- c(1, 2, 3) },
      "Oscar's Grind" = { if (won) { p <<- p + bet
                                     if (p >= 1) { u <<- 1; p <<- 0 } else u <<- min(u + 1, 1 - p)
                          } else p <<- p - bet },
      "Chop progression (2016)" = { k <<- if (won) 0 else k + 1
                                    if (k >= 3) { k <<- 0; sitout <<- TRUE } },
      NULL)
    invisible()
  }
  side_changed <- function() sitout <<- FALSE
  list(units = units, update = update, reset = reset, side_changed = side_changed)
}

# Decide the next bet: list(side = "P"/"B"/"D" or NA, stake = money amount).
system_decide <- function(cfg, prog, last, tc) {
  side <- switch(cfg$side,
    B = "B", P = "P",
    opposite = if (is.na(last)) NA_character_ else if (last == "B") "P" else "B",
    same = if (is.na(last)) "B" else last,
    D = if (tc >= cfg$threshold) "D" else NA_character_)
  u <- prog$units()
  if (is.na(side) || u <= 0) return(list(side = NA_character_, stake = 0))
  list(side = side, stake = min(u * cfg$base, cfg$table_max))
}

run_system <- function(h, cfg, game = c("standard", "ez")) {
  game <- match.arg(game)
  pay <- as.matrix(settle(h, game)[c("Player", "Banker", "Tie", "Dragon7")])
  payout_col <- c(P = 1L, B = 2L, T = 3L, D = 4L)
  new_shoe_at <- c(TRUE, h$shoe[-1] != h$shoe[-nrow(h)])
  tc <- h$tc; winner <- h$winner
  prog <- make_progression(cfg$system, cfg$max_steps)
  n <- nrow(h)
  net <- wagered <- numeric(n)
  for (i in seq_len(n)) {
    if (new_shoe_at[i]) { last <- NA_character_; prog$reset(); shoe_net <- 0; stopped <- FALSE }
    if (!stopped) {
      d <- system_decide(cfg, prog, last, tc[i])
      if (d$stake > 0) {
        r <- pay[i, payout_col[[d$side]]]
        net[i] <- r * d$stake; wagered[i] <- d$stake
        if (r != 0) prog$update(r > 0)
        shoe_net <- shoe_net + net[i]
        if ((cfg$stop_loss > 0 && shoe_net <= -cfg$stop_loss) ||
            (cfg$take_profit > 0 && shoe_net >= cfg$take_profit)) stopped <- TRUE
      }
    }
    w <- winner[i]
    if (w != "T") { if (!is.na(last) && w != last) prog$side_changed(); last <- w }
  }
  summarise_run(net, wagered)
}

# Every system with the same side and money settings, ready for compare_strategies().
default_systems <- function(systems = SYSTEMS, ...) {
  setNames(lapply(systems, function(s) system_config(s, ...)), systems)
}

# ---- Analysis helpers -----------------------------------------------------
# Dragon 7 EV by true-count bucket: shows where the side bet turns positive.
dragon_by_count <- function(h, breaks = c(-Inf, -2, 0, 2, 4, 6, Inf)) {
  bucket <- cut(h$tc, breaks, right = FALSE)
  r <- ifelse(h$dragon7, 40, -1)
  data.frame(
    true_count = levels(bucket),
    hands      = as.vector(table(bucket)),
    pct_hands  = round(100 * as.vector(table(bucket)) / nrow(h), 2),
    ev_pct     = round(100 * as.vector(tapply(r, bucket, mean)), 2),
    ci95_pct   = round(100 * 1.96 * as.vector(tapply(r, bucket, sd)) /
                         sqrt(as.vector(table(bucket))), 2)
  )
}

# Streak (run) lengths ignoring ties, per shoe.
streak_lengths <- function(h) {
  unlist(lapply(split(h$winner, h$shoe), function(w) rle(w[w != "T"])$lengths))
}

# Per-shoe summary similar to the 2016 simulplay() output.
shoe_summary <- function(h) {
  do.call(rbind, lapply(split(h, h$shoe), function(s) {
    r <- rle(s$winner[s$winner != "T"])
    data.frame(shoe = s$shoe[1], hands = nrow(s),
               Banker = sum(s$winner == "B"), Player = sum(s$winner == "P"),
               Tie = sum(s$winner == "T"), Dragon7 = sum(s$dragon7),
               Panda8 = sum(s$panda8),
               BRun = max(c(0, r$lengths[r$values == "B"])),
               PRun = max(c(0, r$lengths[r$values == "P"])))
  }))
}

# How often each shoe reaches the Dragon true count threshold (TC before the hand >= threshold,
# the same test the Dragon count strategy uses to place its bet).
# Returns one row per shoe: hands dealt, hands at/above the threshold, their share,
# first hand that qualified (NA if none) and the shoe's highest true count.
count_frequency <- function(h, threshold = 4) {
  at <- h$tc >= threshold
  by_shoe <- split(seq_len(nrow(h)), h$shoe)
  do.call(rbind, lapply(by_shoe, function(i) {
    q <- which(at[i])
    data.frame(shoe = h$shoe[i[1]], hands = length(i), hands_at = length(q),
               pct_hands = 100 * length(q) / length(i),
               first_hand = if (length(q)) h$hand[i][q[1]] else NA_integer_,
               max_tc = max(h$tc[i]))
  }))
}

# Summary of count_frequency(): how many shoes reach the threshold, how often, how early,
# a distribution of qualifying hands per shoe, and where in the shoe they fall.
count_frequency_summary <- function(h, threshold = 4,
                                    buckets = c(0, 1, 3, 6, 11, 21, Inf)) {
  cf <- count_frequency(h, threshold)
  at <- h$tc >= threshold
  labels <- c("0", "1-2", "3-5", "6-10", "11-20", "21+")
  dist <- table(cut(cf$hands_at, buckets, labels = labels, right = FALSE))
  # Stage of the shoe by share of cards already dealt before the hand
  # each shoe's own starting size: cards left before its first hand (after the burn)
  n_cards <- ave(h$cards_left, h$shoe, FUN = max)
  dealt <- 1 - h$cards_left / n_cards
  stage <- cut(dealt, c(-Inf, 1/3, 2/3, Inf), labels = c("First third", "Middle third", "Last third"))
  list(
    threshold = threshold,
    shoes = nrow(cf),
    pct_shoes_reaching = 100 * mean(cf$hands_at > 0),
    mean_hands_per_shoe = mean(cf$hands_at),
    median_hands_per_shoe = median(cf$hands_at),
    mean_hands_when_reached = if (any(cf$hands_at > 0)) mean(cf$hands_at[cf$hands_at > 0]) else 0,
    pct_hands = 100 * mean(at),
    median_first_hand = median(cf$first_hand, na.rm = TRUE),
    distribution = data.frame(hands_per_shoe = labels, shoes = as.vector(dist),
                              pct_shoes = round(100 * as.vector(dist) / nrow(cf), 1)),
    by_stage = data.frame(stage = levels(stage),
                          pct_of_qualifying_hands = round(100 * as.vector(table(stage[at])) / max(1, sum(at)), 1)),
    per_shoe = cf
  )
}

# Bead plate: results in columns of 6, as on a casino scoreboard.
print_bead_plate <- function(winner, rows = 6) {
  n <- length(winner); cols <- ceiling(n / rows)
  m <- matrix(".", rows, cols)
  m[seq_len(n)] <- winner
  for (r in seq_len(rows)) cat(paste(m[r, ], collapse = " "), "\n")
}

# Big Road scoreboard: each streak goes down a column; a new column starts when the
# side changes. Streaks longer than the board turn right ("dragon tail"). Ties are
# not placed; they are counted on the cell before them.
# Returns data.frame(col, row, winner, ties), 0-based positions.
big_road <- function(winner, rows = 6) {
  cells <- list(); occupied <- new.env()
  key <- function(c, r) paste(c, r)
  last <- NA; run_col <- -1; col <- -1; row <- -1; pending_ties <- 0
  for (w in winner) {
    if (w == "T") {
      if (length(cells)) cells[[length(cells)]]$ties <- cells[[length(cells)]]$ties + 1
      else pending_ties <- pending_ties + 1
      next
    }
    if (!is.na(last) && w == last) {
      if (row + 1 < rows && is.null(occupied[[key(col, row + 1)]])) row <- row + 1
      else col <- col + 1
    } else {
      run_col <- run_col + 1; col <- run_col; row <- 0
    }
    occupied[[key(col, row)]] <- TRUE
    cells[[length(cells) + 1]] <- list(col = col, row = row, winner = w, ties = pending_ties)
    pending_ties <- 0; last <- w
  }
  if (!length(cells)) return(data.frame(col = integer(), row = integer(), winner = character(), ties = integer()))
  do.call(rbind, lapply(cells, as.data.frame, stringsAsFactors = FALSE))
}

# ---- Plots ---------------------------------------------------------------
plot_report <- function(h, file = NULL, game = "standard", cs = NULL) {
  if (!is.null(file)) png(file, width = 1600, height = 1200, res = 150)
  op <- par(mfrow = c(2, 2), mar = c(4.5, 4.5, 3, 1)); on.exit(par(op))

  # 1. Strategies
  if (is.null(cs)) cs <- compare_strategies(h, game = game)   # pass cs to reuse results
  runs <- attr(cs, "runs")
  cols <- rep(c("#2a6f97", "#c8553d", "#6a994e", "#8a5a9e", "#e09f3e", "#333333",
                "#17becf", "#b5651d", "#d63384", "#7f7f7f"), length.out = length(runs))
  rng <- range(unlist(lapply(runs, `[[`, "cumulative")))
  plot(NA, xlim = c(1, nrow(h)), ylim = rng, xlab = "Hand", ylab = "Net result",
       main = "Betting systems vs the house")
  abline(h = 0, col = "grey70")
  for (i in seq_along(runs)) lines(runs[[i]]$cumulative, col = cols[i], lwd = 1.5)
  legend("bottomleft", names(runs), col = cols, lwd = 2, bty = "n", cex = 0.75)

  # 2. Dragon by count
  d <- dragon_by_count(h)
  x <- barplot(d$ev_pct, names.arg = d$true_count, col = ifelse(d$ev_pct > 0, "#6a994e", "#c8553d"),
               ylim = range(c(d$ev_pct - d$ci95_pct, d$ev_pct + d$ci95_pct, 0), na.rm = TRUE),
               xlab = "Dragon true count", ylab = "EV per unit (%)", main = "Dragon 7 EV by true count")
  arrows(x, d$ev_pct - d$ci95_pct, x, d$ev_pct + d$ci95_pct, angle = 90, code = 3, length = 0.04)
  abline(h = 0)

  # 3. Streaks vs theory
  s <- streak_lengths(h)
  obs <- table(factor(pmin(s, 12), 1:12)) / length(s)
  pB <- exact_odds()$prob; q <- pB["Banker"] / (pB["Banker"] + pB["Player"])
  # mixture of geometric run lengths for B and P runs
  k <- 1:12
  theo <- 0.5 * (1 - q) * q^(k - 1) + 0.5 * q * (1 - q)^(k - 1)
  theo[12] <- 1 - sum(theo[1:11])
  y <- barplot(rbind(obs, theo), beside = TRUE, col = c("#2a6f97", "grey75"),
               names.arg = c(1:11, "12+"), xlab = "Streak length (ties ignored)",
               ylab = "Share of streaks", main = "Streaks: observed vs pure chance")
  legend("topright", c("Simulated", "Independent hands"), fill = c("#2a6f97", "grey75"), bty = "n")

  # 4. Outcome mix per shoe
  ss <- shoe_summary(h)
  hist(ss$Banker - ss$Player, breaks = 30, col = "#e09f3e", border = "white",
       xlab = "Banker wins minus Player wins, per shoe", main = "Shoe-to-shoe swing")
  abline(v = mean(ss$Banker - ss$Player), lwd = 2)

  if (!is.null(file)) dev.off()
  invisible(cs)
}
