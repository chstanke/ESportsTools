#' Top Players by Kill-Death Ratio
#'
#' Identifies players with the highest kill-death ratios.
#'
#' @param df A data frame containing player performance data.
#' @param n Number of top players to return.
#'
#' @return A data frame of players ranked by kill-death ratio.
#'
#' @examples
#' data(player_stats)
#' kd_leaders(player_stats, n = 5)
#'
#' @export
kd_leaders <- function(df, n = 5) {
  library(dplyr)

  df %>%
    group_by(player) %>%
    summarise(
      kd_ratio = sum(kills) / sum(deaths),
      .groups = "drop"
    ) %>%
    arrange(desc(kd_ratio)) %>%
    head(n)
}
