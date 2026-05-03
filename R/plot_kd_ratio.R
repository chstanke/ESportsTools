#' Plot Kill-Death Ratio by Player
#'
#' Creates a bar chart showing kill-death ratios for each player.
#'
#' @param df A data frame containing player performance data.
#'
#' @return A ggplot2 object displaying K/D ratios.
#'
#' @examples
#' data(player_stats)
#' plot_kd_ratio(player_stats)
#'
#' @export
plot_kd_ratio <- function(df) {
  library(dplyr)
  library(ggplot2)

  kd_data <- df %>%
    group_by(player) %>%
    summarise(kd = sum(kills) / sum(deaths), .groups = "drop")

  ggplot(kd_data, aes(x = reorder(player, kd), y = kd)) +
    geom_col() +
    coord_flip() +
    theme_minimal() +
    labs(
      title = "K/D Ratio by Player",
      x = "Player",
      y = "K/D Ratio"
    )
}
