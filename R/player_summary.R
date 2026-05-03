#' Player Performance Summary
#'
#' Computes summary statistics for each player, including average score,
#' total hours played, total kills, and kill-death ratio.
#'
#' @param df A data frame containing player performance data.
#'
#' @return A data frame with one row per player and summary metrics.
#'
#' @examples
#' data(player_stats)
#' player_summary(player_stats)
#'
#' @export
player_summary <- function(df) {
  library(dplyr)

  df %>%
    group_by(player) %>%
    summarise(
      avg_score = mean(score),
      total_hours = sum(hours_played),
      total_kills = sum(kills),
      kd_ratio = sum(kills) / sum(deaths),
      .groups = "drop"
    ) %>%
    arrange(desc(avg_score))
}
