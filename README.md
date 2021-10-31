Glass Stepping Stones
================

From [Zach Wissner-Gross](https://twitter.com/xaqwg) and [The
Riddler](https://fivethirtyeight.com/features/can-you-survive-squid-game-riddler/):

![Zach’s explanation of the challenge](challenge.png)

## Solution:

I’ve taken a simulation-based approach to estimating the number of
survivors. My code can be found in `squid_game.R` and in the
`README.Rmd` file that produced what you’re reading now.

**(Spoiler Alert)**

In this episode, some of the players are fortunate enough to have a
former glass factory worker posit his guesses before they do, taking his
leaps based on the optical and acoustic differences between tempered and
normal glass.

In these simulations, I’ve assumed the glassmaker’s judgments to be
infallible - he, and thus and all players behind him, will survive with
100% probability. However, his placement in the order of players is
random.

Below are the results of my simulation with, and without, the help of
this player.

### Probabilities that **x** players survive:

<img src="README_files/figure-gfm/plot-1-1.png" title="Overlapping frequency distributions of the number of survivors in this simulation, where one group has the advantage of having the glassmaker with them. This shows that on average, 7 players will survive without him, and around 10 will survive with his help." alt="Overlapping frequency distributions of the number of survivors in this simulation, where one group has the advantage of having the glassmaker with them. This shows that on average, 7 players will survive without him, and around 10 will survive with his help."  />

### Cumulative probability that at least **x** survive:

<img src="README_files/figure-gfm/plot-2-1.png" title="Line chart of the cumulative probability distribution of survivors in the two groups, where one group has the advantage of having the glassmaker with them. This shows that with a glassmaker, there is a 50.5% chance that 10 or more players survive - but that without him, only an 11.9% chance." alt="Line chart of the cumulative probability distribution of survivors in the two groups, where one group has the advantage of having the glassmaker with them. This shows that with a glassmaker, there is a 50.5% chance that 10 or more players survive - but that without him, only an 11.9% chance."  />
