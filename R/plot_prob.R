#' Plot Prediction Probability
#' @description Takes the output predictions from predict_games() and plots the predicted outcomes for the home teams
#' @param predictions dataframe with .pred_class, .pred_1, .pred_0 and home_team columns. Output from predict_games() function
#' @param bar_plot logical. If TRUE, a bar plot is generated. Default is FALSE and will return a lollipop plot
#' @param pct_over_coinflip logical. If TRUE, probabilities are returned to represent how far over a 50-50 guess the prediction is. For example, a value of 0 on the plot x axis would mean that the home and away team have a 50% chance of winning the game.
#' @param prob_alpha Whether the lollipop or bars on plot should be more/less transparent based on the model class probability. Default is TRUE, alpha is used on plot elements
#' @importFrom dplyr mutate case_when
#' @importFrom ggplot2 ggplot geom_col aes scale_fill_manual scale_x_continuous labs theme_bw theme element_text guides element_blank scale_color_manual
#' @return ggplot object with team matchups on the y axis and probabilities of the correct prediction on the x axis
#' @export
plot_prob <- function(
    predictions       = NULL,
    bar_plot          = FALSE,
    pct_over_coinflip = FALSE,
    prob_alpha        = TRUE
    ) {

  # if no predictions dataframe is entered, get predictions for the upcoming week
  if(is.null(predictions)) {

    predictions <- nflpredictr::predict_games()

  }

  # If plot should be returned as the probability over 0.50 that prediction is correct
  if(pct_over_coinflip == TRUE) {

    if(bar_plot == TRUE) {
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
          winning_team = dplyr::case_when(
            .pred_class == "1" ~ home_team,
            .pred_class == "0" ~ away_team
          ),
          win_type = dplyr::case_when(
            winning_team == home_team ~ "Home win",
            winning_team == away_team ~ "Away win"
          ),
          clean_matchup = paste0(away_team, " @ ", home_team)
        )

      # check if plots should use probability for alpha
      if(prob_alpha == TRUE) {

        # prediction probabilities plot with probability alpha
        game_probs_plot <-
          game_probs %>%
          ggplot2::ggplot() +
          ggplot2::geom_col(aes(x = probability,
                                y = stats::reorder(clean_matchup, probability),
                                fill = win_type,
                                alpha = probability)
          )

      } else {

        # prediction probabilities plot w/o probability alpha
        game_probs_plot <-
          game_probs %>%
          ggplot2::ggplot() +
          ggplot2::geom_col(aes(x = probability,
                                y = stats::reorder(clean_matchup, probability),
                                fill = win_type)
          )

      }

      # prediction probabilities plot
      game_probs_plot <-
        game_probs_plot +
        ggplot2::geom_text(
          ggplot2::aes(x = probability, y = clean_matchup, label = winning_team),
          color = "black", size = 4, hjust = 0, nudge_x = 0.01) +
        ggplot2::scale_fill_manual(values = c("dodgerblue3", "forestgreen")) +
        ggplot2::scale_x_continuous(
          limits = c(0, 0.5),
          breaks = seq(0, 0.5, by = 0.10),
          expand = c(0, 0),
          labels = scales::percent
        ) +
        ggplot2::labs(
          title    = paste0("NFL Game Predictions"),
          subtitle = paste0("Week: ", game_probs$week[1], "\nSeason: ", game_probs$season[1]),
          x        = "Probability",
          y        = "Matchups",
          fill     = "Outcome"
        ) +
        ggplot2::guides(alpha = "none") +
        ggplot2::theme_bw() +
        ggplot2::theme(
          plot.title       = ggplot2::element_text(size = 14, face = "bold", hjust = 0.5),
          plot.subtitle    = ggplot2::element_text(size = 12, hjust = 0.5),
          axis.title       = ggplot2::element_text(face = "bold", size = 12),
          axis.text        = ggplot2::element_text(size = 10),
          legend.text      = ggplot2::element_text(size = 12),
          legend.title     = ggplot2::element_text(face = "bold", hjust = 0.5, size = 12),
          axis.ticks       = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank()
        )

      return(game_probs_plot)

    } else {

      # prediction probabilities
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
          winning_team = dplyr::case_when(
            .pred_class == "1" ~ home_team,
            .pred_class == "0" ~ away_team
          ),
          win_type = dplyr::case_when(
            winning_team == home_team ~ "Home win",
            winning_team == away_team ~ "Away win"
          ),
          clean_matchup = paste0(away_team, " @ ", home_team)
        )

      # check if plots should use probability for alpha
      if(prob_alpha == TRUE) {

        # prediction probabilities plot with probability alpha
        game_probs_plot <-
          game_probs %>%
          ggplot2::ggplot() +
          ggplot2::geom_point(aes(x = probability, y = stats::reorder(clean_matchup, probability),
                                  col = win_type,
                                  alpha = probability), size = 4) +
          ggplot2::geom_segment(aes(x = 0, xend = probability, y = clean_matchup, yend = clean_matchup,
                                    col = win_type,
                                    alpha = probability), size = 1.5)

      } else {

        # prediction probabilities plot w/o probability alpha
        game_probs_plot <-
          game_probs %>%
          ggplot2::ggplot() +
          ggplot2::geom_point(aes(x = probability, y = stats::reorder(clean_matchup, probability),
                                  col = win_type
          ), size = 4) +
          ggplot2::geom_segment(aes(x = 0, xend = probability, y = clean_matchup, yend = clean_matchup,
                                    col = win_type), size = 1.5)


      }

      # prediction probabilities plot
      game_probs_plot <-
        game_probs_plot +
        ggplot2::geom_text(
          ggplot2::aes(x = probability, y = clean_matchup, label = winning_team),
          color = "black", size = 4, hjust = 0, nudge_x = 0.02) +
        ggplot2::scale_color_manual(values = c("dodgerblue3", "forestgreen")) +
        ggplot2::scale_x_continuous(
          limits = c(0, 0.5),
          breaks = seq(0, 0.5, by = 0.10),
          expand = c(0, 0),
          labels = scales::percent
        ) +
        ggplot2::labs(
          title    = paste0("NFL Game Predictions"),
          subtitle = paste0("Week: ", game_probs$week[1], "\nSeason: ", game_probs$season[1]),
          x        = "Probability",
          y        = "Matchups",
          col      = "Outcome"
        ) +
        ggplot2::guides(alpha = "none") +
        ggplot2::theme_bw() +
        ggplot2::theme(
          plot.title       = ggplot2::element_text(size = 14, face = "bold", hjust = 0.5),
          plot.subtitle    = ggplot2::element_text(size = 12, hjust = 0.5),
          axis.title       = ggplot2::element_text(face = "bold", size = 12),
          axis.text        = ggplot2::element_text(size = 10),
          legend.text      = ggplot2::element_text(size = 12),
          legend.title     = ggplot2::element_text(face = "bold", hjust = 0.5, size = 12),
          axis.ticks       = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank()
        )

      return(game_probs_plot)

    }

  # Return entire probabilities plot
  } else {

    if(bar_plot == TRUE) {

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
          ),
          winning_team = dplyr::case_when(
            .pred_class == "1" ~ home_team,
            .pred_class == "0" ~ away_team
          ),
          win_type = dplyr::case_when(
            winning_team == home_team ~ "Home win",
            winning_team == away_team ~ "Away win"
          ),
          clean_matchup = paste0(away_team, " @ ", home_team)
        )

      # check if plots should use probability for alpha
      if(prob_alpha == TRUE) {

        # prediction probabilities plot with probability alpha
        game_probs_plot <-
          game_probs %>%
          ggplot2::ggplot() +
          ggplot2::geom_col(aes(x = probability,
                                y = stats::reorder(clean_matchup, probability),
                                fill = win_type,
                                alpha = probability)
          )

      } else {

        # prediction probabilities plot w/o probability alpha
        game_probs_plot <-
          game_probs %>%
          ggplot2::ggplot() +
          ggplot2::geom_col(aes(x = probability,
                                y = stats::reorder(clean_matchup, probability),
                                fill = win_type)
          )

      }

      # prediction probabilities plot
      game_probs_plot <-
        game_probs_plot +
        ggplot2::geom_text(
        ggplot2::aes(x = probability, y = clean_matchup, label = winning_team),
          color = "black", size = 4, hjust = 0, nudge_x = 0.01) +
        ggplot2::scale_fill_manual(values = c("dodgerblue3", "forestgreen")) +
        ggplot2::scale_x_continuous(
          limits = c(0, 1),
          breaks = seq(0, 1, by = 0.1),
          expand = c(0, 0),
          labels = scales::percent
        ) +
        ggplot2::labs(
          title    = paste0("NFL Game Predictions"),
          subtitle = paste0("Week: ", game_probs$week[1], "\nSeason: ", game_probs$season[1]),
          x        = "Probability",
          y        = "Matchups",
          fill     = "Outcome"
        ) +
        ggplot2::guides(alpha = "none") +
        ggplot2::theme_bw() +
        ggplot2::theme(
          plot.title       = ggplot2::element_text(size = 14, face = "bold", hjust = 0.5),
          plot.subtitle    = ggplot2::element_text(size = 12, hjust = 0.5),
          axis.title       = ggplot2::element_text(face = "bold", size = 12),
          axis.text        = ggplot2::element_text(size = 10),
          legend.text      = ggplot2::element_text(size = 12),
          legend.title     = ggplot2::element_text(face = "bold", hjust = 0.5, size = 12),
          axis.ticks       = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank()
        )

      return(game_probs_plot)

    } else {

      # prediction probabilities
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
          ),
          winning_team = dplyr::case_when(
            .pred_class == "1" ~ home_team,
            .pred_class == "0" ~ away_team
          ),
          win_type = dplyr::case_when(
            winning_team == home_team ~ "Home win",
            winning_team == away_team ~ "Away win"
          ),
          clean_matchup = paste0(away_team, " @ ", home_team)
        )

      # check if plots should use probability for alpha
      if(prob_alpha == TRUE) {

        # prediction probabilities plot with probability alpha
        game_probs_plot <-
          game_probs %>%
          ggplot2::ggplot() +
          ggplot2::geom_point(aes(x = probability, y = stats::reorder(clean_matchup, probability),
                                  col = win_type,
                                  alpha = probability), size = 4) +
          ggplot2::geom_segment(aes(x = 0, xend = probability, y = clean_matchup, yend = clean_matchup,
                                    col = win_type,
                                    alpha = probability), size = 1.5)

      } else {

        # prediction probabilities plot w/o probability alpha
        game_probs_plot <-
          game_probs %>%
          ggplot2::ggplot() +
          ggplot2::geom_point(aes(x = probability, y = stats::reorder(clean_matchup, probability),
                                  col = win_type
          ), size = 4) +
          ggplot2::geom_segment(aes(x = 0, xend = probability, y = clean_matchup, yend = clean_matchup,
                                    col = win_type), size = 1.5)


      }

      # prediction probabilities plot
      game_probs_plot <-
        game_probs_plot +
        ggplot2::geom_text(
        ggplot2::aes(x = probability, y = clean_matchup, label = winning_team),
          color = "black", size = 4, hjust = 0, nudge_x = 0.02) +
        ggplot2::scale_color_manual(values = c("dodgerblue3", "forestgreen")) +
        ggplot2::scale_x_continuous(
          limits = c(0, 1),
          breaks = seq(0, 1, by = 0.1),
          expand = c(0, 0),
          labels = scales::percent
        ) +
        ggplot2::labs(
          title = paste0("NFL Game Predictions"),
          subtitle = paste0("Week: ", game_probs$week[1], "\nSeason: ", game_probs$season[1]),
          x    = "Probability",
          y    = "Matchups",
          col  = "Outcome"
        ) +
        ggplot2::guides(alpha = "none") +
        ggplot2::theme_bw() +
        ggplot2::theme(
          plot.title       = ggplot2::element_text(size = 14, face = "bold", hjust = 0.5),
          plot.subtitle    = ggplot2::element_text(size = 12, hjust = 0.5),
          axis.title       = ggplot2::element_text(face = "bold", size = 12),
          axis.text        = ggplot2::element_text(size = 10),
          legend.text      = ggplot2::element_text(size = 12),
          legend.title     = ggplot2::element_text(face = "bold", hjust = 0.5, size = 12),
          axis.ticks       = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank()
        )

      return(game_probs_plot)

    }

  }

}
