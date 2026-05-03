#' Top Players by Performance
#'
#' Returns the top players ranked by average score.
#'
#' @param df A data frame containing player performance data.
#' @param n Number of top players to return.
#'
#' @return A data frame of the top n players ranked by score.
#'
#' @examples
#' data(player_stats)
#' top_players(player_stats, n = 5)
#'
#' @export
top_players <- function(df, n = 5) {
  library(dplyr)

  df %>%
    group_by(player) %>%
    summarise(
      avg_score = mean(score),
      kd_ratio = sum(kills) / sum(deaths),
      .groups = "drop"
    ) %>%
    arrange(desc(avg_score)) %>%
    head(n)
}
