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
    cards_left = integer(max_h), rc = numeric(max_h), tc = numeric(max_h)
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
    Panda8  = ifelse(h$panda8, 25, -1)
  )
}

# ---- Exact odds by enumeration ---------------------------------------------
# Enumerates every ordered 6-card sequence by point value (10^6 rows) and weights it
# by its exact probability off the top of a fresh shoe. Unused cards marginalise out.
exact_odds <- function(decks = 8) {
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
  edge <- function(x) -x  # house edge = -EV
  ev <- c(
    Player_std  = probs["Player"] - probs["Banker"],
    Banker_std  = 0.95 * probs["Banker"] - probs["Player"],
    Player_ez   = probs["Player"] - probs["Banker"],
    Banker_ez   = (probs["Banker"] - probs["Dragon7"]) - probs["Player"],
    Tie         = 8 * probs["Tie"] - (1 - probs["Tie"]),
    Dragon7     = 40 * probs["Dragon7"] - (1 - probs["Dragon7"]),
    Panda8      = 25 * probs["Panda8"] - (1 - probs["Panda8"])
  )
  names(ev) <- sub("\\..*", "", names(ev))
  list(prob = probs, house_edge = edge(ev))
}

# ---- Observed vs exact house edge -------------------------------------------
house_edge <- function(h, game = c("standard", "ez"), decks = 8) {
  game <- match.arg(game)
  s  <- settle(h, game)
  ex <- exact_odds(decks)$house_edge
  key <- c(Player = paste0("Player_", if (game == "standard") "std" else "ez"),
           Banker = paste0("Banker_", if (game == "standard") "std" else "ez"),
           Tie = "Tie", Dragon7 = "Dragon7", Panda8 = "Panda8")
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
  pay <- settle(h, game)
  payout_col <- c(P = "Player", B = "Banker", T = "Tie", D = "Dragon7")
  n <- nrow(h)
  net <- wagered <- numeric(n)
  for (i in seq_len(n)) {
    if (i == 1 || h$shoe[i] != h$shoe[i - 1]) {
      last <- NA_character_; streak <- 0; losses <- 0
    }
    d <- strategy(list(last = last, streak = streak, losses = losses, tc = h$tc[i]))
    if (d$bet != "none" && d$units > 0) {
      r <- pay[[payout_col[[d$bet]]]][i] * d$units
      wagered[i] <- d$units
      net[i] <- r
      if (r < 0) losses <- losses + 1 else if (r > 0) losses <- 0
    }
    w <- h$winner[i]
    if (w != "T") {
      streak <- if (!is.na(last) && w == last) streak + 1 else 1
      last <- w
    }
  }
  cum <- cumsum(net)
  list(net = net, wagered = wagered, cumulative = cum,
       summary = c(hands_bet = sum(wagered > 0), units_wagered = sum(wagered),
                   net_units = round(sum(net), 1),
                   return_pct = round(100 * sum(net) / max(1, sum(wagered)), 3),
                   worst_drawdown = round(min(cum - cummax(pmax(cum, 0))), 1)))
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

# Bead plate: results in columns of 6, as on a casino scoreboard.
print_bead_plate <- function(winner, rows = 6) {
  n <- length(winner); cols <- ceiling(n / rows)
  m <- matrix(".", rows, cols)
  m[seq_len(n)] <- winner
  for (r in seq_len(rows)) cat(paste(m[r, ], collapse = " "), "\n")
}

# ---- Plots ---------------------------------------------------------------
plot_report <- function(h, file = NULL, game = "standard") {
  if (!is.null(file)) png(file, width = 1600, height = 1200, res = 150)
  op <- par(mfrow = c(2, 2), mar = c(4.5, 4.5, 3, 1)); on.exit(par(op))

  # 1. Strategies
  cs <- compare_strategies(h, game = game)
  runs <- attr(cs, "runs")
  cols <- c("#2a6f97", "#c8553d", "#6a994e", "#8a5a9e", "#e09f3e", "#333333")
  rng <- range(unlist(lapply(runs, `[[`, "cumulative")))
  plot(NA, xlim = c(1, nrow(h)), ylim = rng, xlab = "Hand", ylab = "Net units",
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
