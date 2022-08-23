#' Plot Prediction Probability
#' @description Takes the output predictions from predict_games() and plots the predicted outcomes for the home teams
#' @param predictions dataframe with .pred_class, .pred_1, .pred_0 and home_team columns. Output from predict_games() function
#' @param pct_over_coinflip logical. If TRUE, probabilities are returned to represent how far over a 50-50 guess the prediction is. For example, a value of 0 on the plot x axis would mean that the home and away team have a 50% chance of winning the game.
#' @importFrom dplyr mutate case_when
#' @importFrom ggplot2 ggplot geom_col aes scale_fill_manual scale_x_continuous labs theme_bw theme element_text
#' @return ggplot object with home teams on the y axis and probabilities of the correct prediction on the x axis
#' @export
#'
#' @examples
get_prob_plot <- function(
    predictions       = NULL,
    pct_over_coinflip = FALSE
    ) {

  # if no predictions dataframe is entered, get predictions for the upcoming week
  if(is.null(predictions)) {

    predictions <- nflpredictr::predict_games()

  }

  # If plot should be returned as the probability over 0.50 that prediction is correct
  if(pct_over_coinflip == TRUE) {

    # prediction probabilities plot
    game_probs <-
      predictions %>%
      dplyr::mutate(
        probability = dplyr::case_when(
          .pred_class == "1" ~ (.pred_1) - 0.5,
          .pred_class == "0" ~ (.pred_0) - 0.5
        ),
        fill        = dplyr::case_when(
          .pred_class == "1" ~ "Home win",
          .pred_class == "0" ~ "Home loss"
        ),
      ) %>%
      ggplot2::ggplot() +
      ggplot2::geom_col(aes(x = probability, y = home_team, fill = fill)) +
      ggplot2::scale_fill_manual(values = c("red3", "forestgreen")) +
      ggplot2::scale_x_continuous(
        limits = c(0, 0.5),
        breaks = seq(0, 0.5, by = 0.10),
        expand = c(0.01, 0),
        labels = scales::percent
      ) +
      ggplot2::labs(
        x    = "Probability",
        y    = "Home team",
        fill = "Prediction"
      ) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        # plot.title   = ggplot2::element_text(face = "bold", size = 14),
        axis.title   = ggplot2::element_text(face = "bold", size = 12),
        axis.text    = ggplot2::element_text(size = 10),
        legend.text  = ggplot2::element_text(size = 10),
        legend.title = ggplot2::element_text(face = "bold", hjust = 0.5, size = 10)
      )

    return(game_probs)

  # Return entire probabilities plot
  } else {

  # prediction probabilities plot
  game_probs <-
    predictions %>%
    dplyr::mutate(
      probability = dplyr::case_when(
        .pred_class == "1" ~ .pred_1,
        .pred_class == "0" ~ .pred_0
      ),
      fill        = dplyr::case_when(
        .pred_class == "1" ~ "Home win",
        .pred_class == "0" ~ "Home loss"
      )
    ) %>%
    ggplot2::ggplot() +
    ggplot2::geom_col(aes(x = probability, y = home_team, fill = fill)) +
    ggplot2::scale_fill_manual(values = c("red3", "forestgreen")) +
    ggplot2::scale_x_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, by = 0.1),
      expand = c(0.01, 0),
      labels = scales::percent
      ) +
    ggplot2::labs(
      x    = "Probability",
      y    = "Home team",
      fill = "Prediction"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title   = ggplot2::element_text(face = "bold", size = 12),
      axis.text    = ggplot2::element_text(size = 10),
      legend.text  = ggplot2::element_text(size = 10),
      legend.title = ggplot2::element_text(face = "bold", hjust = 0.5, size = 10)
    )

  return(game_probs)

  }

}






