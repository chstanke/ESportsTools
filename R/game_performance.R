#' Game Performance Summary
#'
#' Computes summary statistics for each game, including average score,
#' total hours played, and average kill-death ratio.
#'
#' @param df A data frame containing player performance data.
#'
#' @return A data frame summarizing performance metrics by game.
#'
#' @examples
#' data(player_stats)
#' game_performance(player_stats)
#'
#' @export
game_performance <- function(df) {
  library(dplyr)

  df %>%
    group_by(game) %>%
    summarise(
      avg_score = mean(score),
      total_hours = sum(hours_played),
      avg_kd = sum(kills) / sum(deaths),
      .groups = "drop"
    ) %>%
    arrange(desc(avg_score))
}
