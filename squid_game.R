library(purrr)
library(dplyr)
library(ggplot2)

# to run this script and ensure you have the right packages,
# open the `squiddler.Rproj` file and run `renv::restore()`

sim <- function(n_tiles = 18,
                n_players = 16,
                player_017 = FALSE) {
  # generate correct sequence of tiles
  tiles_actual <- sample(c(TRUE, FALSE), n_tiles, replace = TRUE)
  # generate initial guesses for all players
  tiles_guessed <- matrix(
    data = sample(c(TRUE, FALSE), n_tiles * n_players, replace = TRUE),
    nrow = n_players,
    ncol = n_tiles,
    byrow = TRUE
  )
  
  # if there is a glassmaker (player 017) present,
  # they will know the actual sequence of tiles,
  # but will be placed randomly in the order of players guessing
  if (player_017) {
    player_017_index <- sample(n_players, 1)
    tiles_guessed[player_017_index, ] <- tiles_actual
  }
  
  # iterate down, from i = player 1 to the last player
  for (i in 1:n_players) {
    # iterate across, from j = tile 1 to the last tile
    for (j in 1:n_tiles) {
      # if the current player's guess on tile j is correct,
      # set all subsequent players' guesses on tile j to the correct one
      if (tiles_guessed[i, j] == tiles_actual[j]) {
        tiles_guessed[(min(i + 1, n_players)):n_players, j] <-
          tiles_actual[j]
      } else {
      # otherwise, eliminate the current player (break from the j loop),
      # but first, set all subsequent players' guesses on tile j to the correct one
        tiles_guessed[(min(i + 1, n_players)):n_players, j] <-
          tiles_actual[j]
        tiles_guessed[i, (min(j + 1, n_tiles)):(n_tiles)] <- NA
        break
      }
    }
  }
  
  # count the number of survivors (the number of correct guesses on the final tile)
  n_survivors <-
    sum(tiles_guessed[, n_tiles] == tiles_actual[n_tiles], na.rm = T)
  
  # return a named list with n_survivors and player_017 (whether or not player 017 was present)
  return(list(n_survivors = n_survivors, player_017 = player_017))
  
}

# 
# # run sims ----------------------------------------------------------------
# 
# results <- dplyr::bind_rows({
#   # run the sim 500K times without player 017
#   500000 %>% purrr::rerun(sim(
#     n_players = 16,
#     n_tiles = 18,
#     player_017 = FALSE
#   )) %>% bind_rows()
# },
# {
#   # run the sim 500K times with player 017
#   500000 %>% purrr::rerun(sim(
#     n_players = 16,
#     n_tiles = 18,
#     player_017 = TRUE
#   )) %>% bind_rows()
# })
# 
# # plot 1: probabilities of x survivors ------------------------------------
# 
# # create new tibble of the means by group (player017, !player_017),
# # and the (SPOILER) actual number of survivors on the show
# means <- results %>%
#   group_by(player_017) %>%
#   summarize(x = mean(n_survivors)) %>%
#   ungroup() %>%
#   mutate(y = 0) %>%
#   mutate(color = c("#f8b702", "#fc4c82")) %>%
#   bind_rows(tibble(x = 3, y = 0, color = "#0c4549"))
# 
# results %>%
#   count(n_survivors, player_017) %>%
#   mutate(p = n / 500000) %>%
#   ggplot(aes(x = n_survivors, y = p, fill = player_017)) +
#   geom_col(
#     position = 'identity',
#     alpha = 0.8,
#     width = 1,
#     color = "white",
#     size = 0.25
#   ) +
#   scale_fill_manual(values = c("#f8b702", "#fc4c82")) +
#   scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
#   scale_x_continuous(breaks = 0:16) +
#   geom_point(data = means,
#              aes(x = x, y = y),
#              size = 7,
#              color = "white") +
#   geom_point(data = means, aes(x = x, y = y, color = color), size = 5) +
#   scale_color_identity() +
#   annotate(
#     geom = "curve",
#     x = 3,
#     y = 0.08,
#     xend = (means %>% filter(!player_017) %>% pull(x)) - 0.05,
#     yend = 0.005,
#     curvature = -.3,
#     arrow = arrow(length = unit(2, "mm")),
#     color = "white",
#     size = 1
#   ) +
#   annotate(
#     geom = "label",
#     x = 2,
#     y = 0.085,
#     label = "Mean survivors\nwithout glassmaker",
#     fill = "white",
#     color = "#c69201",
#     family = "mono",
#     label.padding = unit(0.5, "lines"),
#     fontface = "bold",
#     size = 4
#   ) +
#   annotate(
#     geom = "label",
#     x = (means %>% filter(!player_017) %>% pull(x)) + 0.35,
#     y = 0.01,
#     label = (means %>% filter(!player_017) %>% pull(x) %>% round(2)),
#     family = "mono",
#     color = "#c69201",
#     size = 6,
#     fontface = "bold"
#   ) +
#   annotate(
#     geom = "curve",
#     x = 14,
#     y = 0.12,
#     xend = (means %>% filter(player_017) %>% pull(x)),
#     yend = 0.005,
#     curvature = .3,
#     arrow = arrow(length = unit(2, "mm")),
#     color = "white",
#     size = 1
#   ) +
#   annotate(
#     geom = "label",
#     x = 14,
#     y = 0.12,
#     label = "Mean survivors\nwith glassmaker",
#     fill = "white",
#     color = "#e24475",
#     family = "mono",
#     label.padding = unit(0.5, "lines"),
#     fontface = "bold",
#     size = 4
#   ) +
#   annotate(
#     geom = "label",
#     x = (means %>% filter(player_017) %>% pull(x)) + 1,
#     y = 0.01,
#     angle = 25,
#     label = (means %>% filter(player_017) %>% pull(x) %>% round(2)),
#     family = "mono",
#     color = "#fc4c82",
#     size = 6,
#     fontface = "bold"
#   ) +
#   annotate(
#     geom = "curve",
#     x = 1,
#     y = 0.03,
#     xend = 2.95,
#     yend = 0.005,
#     curvature = -.3,
#     arrow = arrow(length = unit(2, "mm")),
#     color = "white",
#     size = 1
#   ) +
#   annotate(
#     geom = "label",
#     x = 0.5,
#     y = 0.03,
#     label = "Actual number\nof survivors",
#     color = "#0c4549",
#     fill = "white",
#     family = "mono",
#     label.padding = unit(0.5, "lines"),
#     fontface = "bold",
#     size = 4
#   ) +
#   xlab("Survivors") +
#   ylab("Probability x Survive") +
#   ggtitle("Glass Stepping Stones", subtitle = "16 players, 18 pairs of glass, 1 million simulations") +
#   theme_minimal() +
#   theme(
#     text = element_text(family = 'mono',  color = "white"),
#     plot.title = element_text(family = 'Game Of Squids', size = rel(1.5)),
#     plot.subtitle = element_text(size = rel(1.1), face = "bold"),
#     axis.text = element_text(size = rel(1.2), color = "white"),
#     axis.title = element_text(
#       size = rel(1.2),
#       color = "white",
#       face = "bold"
#     ),
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.x = element_blank(),
#     panel.grid.minor.y = element_blank(),
#     plot.background = element_rect(fill = "#0c4549"),
#     legend.position = 'none'
#   )
# 
# 
# # plot 2: cumulative probabilities (at least x survivors) ----------------------------------------
# 
# p2_data <- results %>%
#   count(n_survivors, player_017) %>%
#   mutate(p = n / 500000) %>%
#   group_by(player_017) %>%
#   arrange(desc(n_survivors)) %>%
#   mutate(p_cumulative = cumsum(p)) %>%
#   bind_rows(tibble(
#     n_survivors = 0,
#     player_017 = TRUE,
#     p_cumulative = 1
#   )) %>%
#   ungroup()
# 
# p_10_without <-
#   p2_data %>% filter(!player_017, n_survivors == 10) %>% pull(p_cumulative) %>% round(3) * 100
# p_10_with <-
#   p2_data %>% filter(player_017, n_survivors == 10) %>% pull(p_cumulative) %>% round(3) * 100
# 
# p2_data %>%
#   ggplot(aes(x = n_survivors, y = p_cumulative, color = player_017)) +
#   geom_line(size = 2) +
#   geom_point(color = "white", size = 5) +
#   geom_point(size = 4) +
#   scale_color_manual(values = c("#f8b702", "#fc4c82")) +
#   scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
#   scale_x_continuous(breaks = 0:16) +
#   annotate(
#     geom = "text",
#     x = 6.5,
#     y = 0.125,
#     label = paste0(
#       "Only an ",
#       p_10_without,
#       "% chance\n10+ players survive\nwithout a glassmaker"
#     ),
#     color = "#f8b702",
#     family = "mono",
#     fontface = "bold",
#     size = 4,
#     hjust = "right"
#   ) +
#   annotate(
#     geom = "curve",
#     x = 6.75,
#     y = 0.125,
#     xend = 9.8,
#     yend = p_10_without / 100,
#     curvature = 0.1,
#     arrow = arrow(length = unit(2, "mm")),
#     color = "#f8b702",
#     size = 1
#   ) +
#   annotate(
#     geom = "text",
#     x = 11.5,
#     y = 0.625,
#     label = paste0(p_10_with, "% chance 10+ survive\nwith the help of a glassmaker"),
#     color = "#fc6f9b",
#     family = "mono",
#     fontface = "bold",
#     size = 4,
#     hjust = "left"
#   ) +
#   annotate(
#     geom = "curve",
#     x = 11.25,
#     y = 0.625,
#     xend = 10.15,
#     yend = p_10_with / 100 + 0.02,
#     curvature = 0.1,
#     arrow = arrow(length = unit(2, "mm")),
#     color = "#fc4c82",
#     size = 1
#   ) +
#   xlab("Survivors") +
#   ylab("Probability at Least X Survive") +
#   ggtitle("Glass Stepping Stones", subtitle = "16 players, 18 pairs of glass panes, 1 million simulations") +
#   theme_minimal() +
#   theme(
#     text = element_text(family = 'mono',  color = "white"),
#     plot.title = element_text(family = 'Game Of Squids', size = rel(1.5)),
#     plot.subtitle = element_text(size = rel(1.1), face = "bold"),
#     axis.text = element_text(size = rel(1.2), color = "white"),
#     axis.title = element_text(
#       size = rel(1.2),
#       color = "white",
#       face = "bold"
#     ),
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.x = element_blank(),
#     panel.grid.minor.y = element_blank(),
#     plot.background = element_rect(fill = "#0c4549"),
#     legend.position = 'none'
#   )
