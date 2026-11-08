# gitCasinoGame
A simulation of Baccarat (Punto Banco) in R, used to study its central tendencies, patterns,
betting systems and side-bet card counting.

## User interfaces

Both interfaces have the same three tabs:

* **Play**: each shoe starts by showing the burn: the first card face up, then that many
  cards face down (10 for 10/J/Q/K). Deal hand by hand and bet on Player, Banker, Tie,
  Dragon 7 and Panda 8, with a bead plate, Big Road and an optional Dragon count. Turn on
  **Auto-bet** to let a betting system place the bets, and **Autoplay** to run it for N hands.
* **Simulate**: set shoes, decks, cut card, game and seed, then compare simulated vs exact
  house edge, betting systems, and Dragon 7 return by true count.

### Tables and side bets

| | Standard (5% commission) | EZ Baccarat (no commission) |
|---|---|---|
| Player / Banker / Tie | ✓ | ✓ (Banker pushes on a Dragon 7) |
| Dragon Bonus, Player or Banker side | ✓ | ✓ |
| Dragon 7 (40 to 1), Panda 8 (25 to 1) | | ✓ |

**Dragon Bonus pay table:** natural win 1 to 1; natural tie push; non-natural win by 9: 30 to 1,
by 8: 10 to 1, by 7: 6 to 1, by 6: 4 to 1, by 5: 2 to 1, by 4: 1 to 1. A win by 0–3 points or a
non-natural tie loses. Exact 8-deck house edge: **2.65% on the Player side and 9.37% on the Banker side**
(`exact_odds()`, which matches the published figures for 4, 6 and 8 decks). Results show the winning
margin ("Banker wins by 6") because the margin settles the bet.

### Betting systems

Every system is a choice of **side** (Banker, Player, opposite of the last result, or same as the
last result) combined with a **bet-sizing rule**: Flat, Martingale, Paroli, Fibonacci, D'Alembert,
1-3-2-6, Labouchère, Oscar's Grind, the 2016 chop progression, or the Dragon count. Settings:
base bet, table maximum, maximum progression steps, and a stop-loss and take-profit per shoe.
Ties (and, in EZ Baccarat, a Banker bet on a Dragon 7) are pushes and leave the progression unchanged.

```r
cfg <- system_config("Martingale", side = "B", base = 10, table_max = 1000, max_steps = 6)
run <- play_strategy(h, cfg, game = "standard")
compare_strategies(h, default_systems(side = "opposite", stop_loss = 200))
```
* **Count trainer**: practise the Dragon 7 count with timed auto-deal, periodic quizzes and
  an optional practice mode that shows each card's tag.

### Shiny app (`app.R`)

The Shiny app runs on the tested R engine in `baccarat.R`.

```r
install.packages("shiny")   # once
shiny::runApp()             # from the repo folder, or open app.R in RStudio and click Run App
```

![Shiny app](screenshot-shiny.png)

### Browser version (`web/index.html`)

A single self-contained page with the same engine ported to JavaScript. It reproduces the same
exact odds and needs no R. Open the file in any browser, or enable GitHub Pages to host it.

## v2: `baccarat.R`

A rewrite of the 2016 simulator. It uses base R only and needs no packages.

* **Accurate and checked.** `exact_odds()` enumerates every 8-deck deal and reproduces the published
  probabilities (Banker 0.458597, Player 0.446247, Tie 0.095156). `tests.R` checks the drawing rules
  hand by hand and confirms the simulator agrees with the exact numbers.
* **Faster.** Cards are integers rather than strings, so it runs about 5× faster than the 2016 code
  (1,000 shoes in about 0.7 s).
* **Real payouts.** Supports standard Baccarat (5% commission) and EZ Baccarat (Banker pushes on Dragon 7).
  Side bets are Tie 8:1, Dragon 7 40:1 and Panda 8 25:1.
* **Casino procedure.** Burn cards are sized by the first card. When the cut card comes out, one more
  hand is dealt.
* **Dragon 7 counting.** Uses Eliot Jacobson's tags (4–7 = −1, 8–9 = +2). The running and true count
  are recorded before every hand.
* **Betting systems.** Strategies plug in as functions, and the 2016 chop progression is included.
  Each run records units wagered, net result, return and worst drawdown.
* **Display and plots.** Card-by-card shoe display with a bead plate, plus a four-panel report.

```r
source("baccarat.R")
play_shoe(verbose = TRUE)                 # watch one shoe dealt
h <- simulate_shoes(5000, seed = 42)      # one row per hand
house_edge(h, game = "ez")                # simulated vs exact house edge
compare_strategies(h)                     # betting systems head to head
dragon_by_count(h)                        # Dragon 7 EV by true count
count_frequency_summary(h, threshold = 4) # how often each shoe reaches TC >= +4
dragons_per_shoe(h)                       # most Dragon 7s in a shoe, average, record shoe
shoe_summary(h)                           # per-shoe table like 2016 simulplay()
plot_report(h, file = "report.png")
```

To write your own strategy, give it a function that takes `state` (`last`, `streak`, `losses`, `tc`)
and returns `list(bet = "P"/"B"/"T"/"D"/"none", units = n)`. Then run it with `play_strategy(h, fn)`.

### Results from 20,000 shoes (1.6M hands)

![report](report.png)

| Strategy | Return per unit wagered |
|---|---|
| Flat Banker | −1.09% |
| Flat Player | −1.20% |
| Chop progression (2016) | −1.13% |
| Follow the streak | −1.13% |
| Martingale Banker | −0.98% (but needs 3× the action and has the deepest drawdown) |
| Dragon 7 only at true count ≥ +4 | **+7.6%** (on about 10% of hands) |

Progressions and pattern-following can't change the house edge, because streak lengths match
independent hands exactly. Counting the Dragon 7 side bet is the only approach here that wins.

Run the tests with `Rscript tests.R`.

## 2016 files

`BaccaratAnalysis.R`, `BaccaratAnalysisPreload.R` and `BaccaratAnalysisPreloadNC.R` are the original
simulator, kept for reference. They were patched to run on R ≥ 4.2 (the `class(s) == "matrix"` check),
so they no longer crash on shoes with no Banker or Player run or with a single run, and naturals now
record their real point spread.

## Credits

* **Raoul R. Diez, DODT**: author. Doctor in Organizational Development and Transformation;
  Bachelor's degree in Computer Science, with post-baccalaureate graduate studies in Computer Science.
  Wrote the original simulator in 2016 (`BaccaratAnalysis*.R`) as a study in probability and
  simulation, including the idea of tracking 8s and 9s for the Dragon 7 bet, and directed the 2026 v2 rewrite.
* **Eliot Jacobson, Ph.D.**: the Dragon 7 card count used throughout (tags 4–7 = −1, 8–9 = +2;
  bet at a true count of +4 or more), from his analysis *Card Counting the Dragon Side Bet in
  EZ Baccarat* (2011), published on [Wizard of Odds](https://wizardofodds.com/games/baccarat/).
  More of his work is at [apheat.net](https://www.apheat.net/).
* **Michael Shackleford (Wizard of Odds)**: published 8-deck baccarat probabilities and house
  edges, which `tests.R` uses to check `exact_odds()` and the simulator.
* **Claude (Anthropic)**: helped write v2: the engine, tests, Shiny app and browser version.
  Those commits are marked `Co-Authored-By: Claude`.
* **Tools**: [R](https://www.r-project.org/), [Shiny](https://shiny.posit.co/) and
  [bslib](https://rstudio.github.io/bslib/). The browser version uses the Cormorant Garamond and IBM Plex
  fonts from [Google Fonts](https://fonts.google.com/) (SIL Open Font License).

The betting systems (Martingale, Paroli, Fibonacci, D'Alembert, 1-3-2-6, Labouchère, Oscar's Grind)
are traditional systems in the public domain. They are included to show that bet sizing cannot
overcome the house edge, not as recommendations.
