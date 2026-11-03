# Run with: Rscript tests.R
source("baccarat.R")
ok <- function(cond, msg) { if (!isTRUE(cond)) stop("FAIL: ", msg); cat("ok  ", msg, "\n") }

# Build a stacked shoe from ranks (suit = spades) so we control every card.
# Card order: P1 B1 P2 B2 [P3] [B3]. No burn, huge cut card so exactly one hand is dealt.
stack <- function(...) c(c(...), rep(13L, 40))   # padded with Kings
one <- function(...) play_shoe(stack(...), burn = FALSE, cut_card = 1000)[1, ]

h <- one(9, 13, 13, 13)                 # Player natural 9
ok(h$winner == "P" && h$natural && h$p_cards == 2 && h$b_cards == 2, "natural 9 stands, no draws")

h <- one(6, 13, 13, 13, 5)              # P 6 stands, B 0 draws a 5
ok(h$p_cards == 2 && h$b_cards == 3 && h$b_total == 5 && h$winner == "P", "player stands on 6, banker draws on 0")

h <- one(13, 3, 13, 13, 8, 4)           # P 0 draws 8; B 3 must stand on player 8
ok(h$p_total == 8 && h$b_cards == 2 && h$winner == "P", "banker 3 stands when player third card is 8")

h <- one(13, 6, 13, 13, 6, 1)           # P 0 draws 6; B 6 draws on 6
ok(h$b_cards == 3 && h$b_total == 7 && h$winner == "B" && h$dragon7, "banker 6 draws on player 6 -> Dragon 7")

h <- one(13, 6, 13, 13, 5, 1)           # P 0 draws 5; B 6 stands
ok(h$b_cards == 2, "banker 6 stands on player third card 5")

h <- one(3, 13, 13, 13, 5, 7)           # P 3 draws 5 = 8; B 0 draws 7 = 7 -> Panda
ok(h$panda8 && h$winner == "P", "three-card Player 8 beating Banker = Panda 8")

h <- one(7, 13, 13, 13, 7)              # P 7 stands; B 0 draws 7 -> Tie 7 (not a Dragon win)
ok(h$winner == "T" && !h$dragon7, "three-card banker 7 tying is not a Dragon")

# Exact odds versus published 8-deck figures (Wizard of Odds / Jacobson)
ex <- exact_odds(8)
ok(abs(ex$prob["Banker"] - 0.458597) < 1e-6, sprintf("exact Banker  %.6f", ex$prob["Banker"]))
ok(abs(ex$prob["Player"] - 0.446247) < 1e-6, sprintf("exact Player  %.6f", ex$prob["Player"]))
ok(abs(ex$prob["Tie"]    - 0.095156) < 1e-6, sprintf("exact Tie     %.6f", ex$prob["Tie"]))
ok(abs(ex$house_edge["Dragon7"] - 0.07611) < 1e-4, sprintf("Dragon 7 house edge %.4f%%", 100 * ex$house_edge["Dragon7"]))
ok(abs(ex$house_edge["Banker_std"] - 0.010579) < 1e-5, sprintf("Banker (5%% commission) edge %.4f%%", 100 * ex$house_edge["Banker_std"]))

# Simulation agrees with exact odds within sampling error
h <- simulate_shoes(3000, seed = 1, progress = FALSE)
fr <- prop.table(table(h$winner))
for (k in c("B", "P", "T")) {
  e <- ex$prob[c(B = "Banker", P = "Player", T = "Tie")[[k]]]
  se <- sqrt(e * (1 - e) / nrow(h))
  ok(abs(fr[[k]] - e) < 4 * se, sprintf("simulated %s %.4f vs exact %.4f", k, fr[[k]], e))
}

# Preloaded shoes (matrix, as in the 2016 cylinder()) give identical results to the same seed
set.seed(7); m <- rbind(new_shoe(), new_shoe())
a <- simulate_shoes(shoes = m, progress = FALSE)
set.seed(7); b <- simulate_shoes(2, progress = FALSE)
ok(identical(a$winner, b$winner), "preloaded matrix of shoes reproduces seeded run")

# Chop progression matches the 2016 bettingsys() on even-money outcomes
source_env <- new.env(); sys.source("BaccaratAnalysisPreloadNC.R", source_env)
one_shoe <- h[h$shoe == 1, ]
runs <- rle(one_shoe$winner[one_shoe$winner != "T"])$lengths
old <- source_env$bettingsys(runs)
# old system ignores commission and ties, so settle at even money with ties removed
nt <- one_shoe[one_shoe$winner != "T", ]
r  <- play_strategy(nt, strategy_chop_progression)
# Re-score at even money: Banker bets also paid 1:1
even <- sum(sign(r$net) * r$wagered)
ok(even == old, sprintf("chop progression = 2016 bettingsys (%g units)", old))

cat("\nAll tests passed.\n")
