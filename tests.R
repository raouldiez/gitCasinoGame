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

# ---- Configurable betting systems ----
hs <- simulate_shoes(300, seed = 11, progress = FALSE)
old <- play_strategy(hs, strategy_chop_progression)
new <- play_strategy(hs, system_config("Chop progression (2016)", base = 1, table_max = Inf))
ok(identical(old$net, new$net), "Chop progression system = original 2016 strategy, hand for hand")
ok(identical(play_strategy(hs, strategy_flat_banker)$net,
             play_strategy(hs, system_config("Flat", "B", base = 1))$net), "Flat Banker system = flat strategy")

# Hand-made sequences (one shoe, no Dragon/Panda): stake sizes checked by hand
seqh <- function(w) data.frame(shoe = 1L, winner = w, tc = 0, dragon7 = FALSE, panda8 = FALSE)
stakes <- function(w, ...) play_strategy(seqh(w), system_config(...), game = "standard")$wagered
nets   <- function(w, ...) play_strategy(seqh(w), system_config(...), game = "standard")$net

ok(identical(stakes(c("P", "P", "B", "T", "P"), "Martingale", "B"), c(10, 20, 40, 10, 10)),
   "Martingale doubles after losses, resets after a win, holds on a tie")
ok(identical(nets(c("P", "P", "B", "T", "P"), "Martingale", "B"), c(-10, -20, 38, 0, -10)),
   "Martingale results include 5% commission")
ok(identical(stakes(c("P", "P", "P", "P"), "Martingale", "B", table_max = 30), c(10, 20, 30, 30)),
   "table maximum caps the Martingale stake")
ok(identical(stakes(rep("P", 4), "Martingale", "B", max_steps = 3), c(10, 20, 40, 10)),
   "Martingale restarts at 1 unit after max_steps losses")
ok(identical(stakes(c("B", "P", "P"), "Labouchere", "P"), c(40, 50, 50)),
   "Labouchere: 1+3, then 1+4 after a loss, then 2+3 after a win")
ok(identical(stakes(c("B", "B", "P", "P", "P"), "Oscar's Grind", "P"), c(10, 10, 10, 20, 10)),
   "Oscar's Grind raises after a win only while behind, resets at +1 unit")
ok(identical(stakes(c("P", "P", "P", "P", "P"), "1-3-2-6", "P"), c(10, 30, 20, 60, 10)),
   "1-3-2-6 runs the sequence on wins and restarts after the 6")
ok(identical(stakes(c("P", "P", "B", "P"), "Paroli", "P"), c(10, 20, 40, 10)),
   "Paroli doubles after wins, resets after a loss")
ok(identical(stakes(c("B", "B", "P", "P"), "Fibonacci", "P"), c(10, 10, 20, 10)),
   "Fibonacci: up one step per loss, back two per win")
ok(identical(stakes(c("B", "B", "P", "P"), "D'Alembert", "P"), c(10, 20, 30, 20)),
   "D'Alembert: +1 unit per loss, -1 per win")
ok(identical(stakes(c("B", "P", "B", "P"), "Flat", "opposite"), c(0, 10, 10, 10)) &&
   identical(nets(c("B", "P", "B", "P"), "Flat", "opposite"), c(0, 10, 9.5, 10)),
   "'opposite' bets against the last result (wins every hand of a chop), sits out the first hand")
ok(identical(stakes(c("B", "B", "B", "B"), "Flat", "P", stop_loss = 20), c(10, 10, 0, 0)),
   "stop-loss ends betting for the rest of the shoe")
ok(identical(stakes(c("P", "P", "P", "P"), "Flat", "P", take_profit = 20), c(10, 10, 0, 0)),
   "take-profit ends betting for the rest of the shoe")

# ---- Dragon Bonus (standard game side bet) ----
ok(abs(ex$house_edge["DragonBonusP"] - 0.0265) < 5e-5 && abs(ex$house_edge["DragonBonusB"] - 0.0937) < 5e-5,
   sprintf("Dragon Bonus 8-deck edges: Player %.4f%%, Banker %.4f%% (published 2.65%%, 9.37%%)",
           100 * ex$house_edge["DragonBonusP"], 100 * ex$house_edge["DragonBonusB"]))
db <- function(...) { h <- one(...); c(P = settle(h)$DragonBonusP, B = settle(h)$DragonBonusB) }
ok(identical(db(9, 13, 13, 7), c(P = 1, B = -1)), "natural winner pays 1 to 1 (Player 9 vs Banker 7)")
ok(identical(db(4, 4, 4, 4), c(P = 0, B = 0)), "natural tie (8-8) pushes both sides")
ok(identical(db(13, 13, 13, 13, 9, 13), c(P = 30, B = -1)), "non-natural win by 9 pays 30 to 1")
# Cards below: P1 B1 P2 B2 P3 B3; Kings are 0, so both hands start at 0 and both draw.
ok(identical(db(13, 13, 13, 13, 8, 13), c(P = 10, B = -1)), "non-natural win by 8 pays 10 to 1 (8 vs 0)")
ok(identical(db(13, 13, 13, 13, 1, 7), c(P = -1, B = 4)), "Banker non-natural win by 6 pays 4 to 1 (7 vs 1)")
ok(identical(db(13, 13, 13, 13, 6, 2), c(P = 1, B = -1)), "non-natural win by 4 pays 1 to 1 (6 vs 2)")
ok(identical(db(13, 13, 13, 13, 5, 2), c(P = -1, B = -1)), "non-natural win by 3 loses (5 vs 2)")
ok(identical(db(13, 13, 13, 13, 5, 5), c(P = -1, B = -1)), "non-natural tie loses both sides (5 vs 5)")
ok(identical(house_edge(simulate_shoes(50, seed = 1, progress = FALSE), "standard")$bet,
             c("Player", "Banker", "Tie", "DragonBonusP", "DragonBonusB")) &&
   identical(house_edge(simulate_shoes(50, seed = 1, progress = FALSE), "ez")$bet,
             c("Player", "Banker", "Tie", "DragonBonusP", "DragonBonusB", "Dragon7", "Panda8")),
   "Both games offer Dragon Bonus; EZ adds Dragon 7 and Panda 8")
cat("\nAll tests passed.\n")
