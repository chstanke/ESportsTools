#' Plot Player Score Distribution
#'
#' Creates a boxplot showing the distribution of player scores.
#'
#' @param df A data frame containing player performance data.
#'
#' @return A ggplot2 object displaying score distribution.
#'
#' @examples
#' data(player_stats)
#' plot_player_scores(player_stats)
#'
#' @export
plot_player_scores <- function(df) {
  library(ggplot2)

  ggplot(df, aes(x = player, y = score)) +
    geom_boxplot() +
    theme_minimal() +
    labs(
      title = "Player Score Distribution",
      x = "Player",
      y = "Score"
    )
}
