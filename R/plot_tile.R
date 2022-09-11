#' Plot home and away win probabilities on a grid for a given week
#' @description Takes the output predictions from predict_games() and plots the prediction probabilities for both teams for a given week.
#' @param predictions dataframe with win, home_win_prob, away_win_prob and home_team columns. Output from predict_games() function
#' @param favored_only logical. whether to only plot the favored teams win probabilities, or both team outcomes. Default is FALSE, to return win probability for both teams.
#' @return ggplot object with matchups on the Y axis and the X axis split by home and away teams. Darker red colors indicate a lower win probability and dark green colors indicate a higher win probability.
#' @importFrom dplyr mutate case_when
#' @importFrom ggplot2 ggplot geom_tile aes scale_fill_manual scale_x_continuous labs theme_minimal theme element_text  guides element_blank scale_fill_gradientn
#' @importFrom scales percent
#' @importFrom RColorBrewer brewer.pal
#' @importFrom grDevices colorRampPalette
#' @importFrom stats reorder
#' @export
plot_tile <- function(
    predictions       = NULL,
    favored_only      = FALSE
) {

  # if no predictions dataframe is entered, get predictions for the upcoming week
  if(is.null(predictions)) {

    predictions <- nflpredictr::predict_games()

  }

  if(favored_only == TRUE) {


    # prediction probabilities
    game_probs <-
      predictions %>%
      dplyr::mutate(
        probability = dplyr::case_when(
          win == "1" ~ home_win_prob,
          win == "0" ~ 1-(home_win_prob)
        ),
        prob_pct    = paste0(100*round(probability, 6), " %"),
        fill        = dplyr::case_when(
          win == "1" ~ "Home win",
          win == "0" ~ "Home loss"
        ),
        winning_team = dplyr::case_when(
          win == "1" ~ home_team,
          win == "0" ~ away_team
        ),
        win_type = dplyr::case_when(
          winning_team == home_team ~ "Home",
          winning_team == away_team ~ "Away"
        ),
        clean_matchup = paste0(away_team, " @ ", home_team)
      )

    # color pallette
    colpal <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(11, "YlGn"))

    # tile plot
    tile_plot <-
      game_probs %>%
      ggplot2::ggplot() +
      ggplot2::geom_tile(aes(
        x    = win_type,
        y    = stats::reorder(clean_matchup, probability),
        fill = probability)) +
      ggplot2::scale_fill_gradientn(
        colours = colpal(16),
        breaks  = seq(0.5, 1, by = 0.1),
        labels  = scales::percent
      ) +
      ggplot2::geom_text(
        ggplot2::aes(
          x     = win_type,
          y     = stats::reorder(clean_matchup, probability),
          label = winning_team
        ),
        color = "black", size = 4, hjust = 0, nudge_x = 0.01) +
      ggplot2::labs(
        fill = "Win probability",
        y    = "Matchups",
        x    = "Favored Team"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title       = ggplot2::element_text(size = 14, face = "bold", hjust = 0.5),
        plot.subtitle    = ggplot2::element_text(size = 12, hjust = 0.5),
        axis.title       = ggplot2::element_text(face = "bold", size = 12),
        axis.text        = ggplot2::element_text(size = 10),
        legend.text      = ggplot2::element_text(size = 10),
        legend.title     = ggplot2::element_text(face = "bold", hjust = 0.5, size = 10),
        axis.ticks       = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank()
      )

    return(tile_plot)

  } else {

    colpal <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(11, "RdYlGn"))

    # prediction probabilities
    game_probs <-
      predictions %>%
      dplyr::mutate(
        clean_matchup = paste0(away_team, " @ ", home_team),
        winning_team = dplyr::case_when(
          win == "1" ~ home_team,
          win == "0" ~ away_team
        )
      ) %>%
      tidyr::pivot_longer(cols = c(home_team, away_team)) %>%
      dplyr::mutate(
        probability = dplyr::case_when(
          name == "home_team" ~ home_win_prob,
          name == "away_team" ~ away_win_prob
        ),
        name = dplyr::case_when(
          name == "home_team" ~ "Home team",
          name == "away_team" ~ "Away team"
        )
      )

    tile_plot <-
      game_probs %>%
      ggplot2::ggplot() +
      ggplot2::geom_tile(aes(
        x    = name,
        y    = stats::reorder(clean_matchup, probability),
        fill = probability)
      ) +
      ggplot2::scale_fill_gradientn(
        colours = colpal(16),
        breaks  = seq(0, 1, by = 0.1),
        labels  = scales::percent
      ) +
      ggplot2::geom_text(
        ggplot2::aes(
          x     = name,
          y     = stats::reorder(clean_matchup, probability),
          label = value
        ),
        color = "black", size = 4, hjust = 0, nudge_x = 0.01) +
      ggplot2::labs(
        fill = "Win probability",
        y    = "Matchups",
        x    = ""
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title       = ggplot2::element_text(size = 14, face = "bold", hjust = 0.5),
        plot.subtitle    = ggplot2::element_text(size = 12, hjust = 0.5),
        axis.title       = ggplot2::element_text(face = "bold", size = 12),
        axis.text        = ggplot2::element_text(size = 10),
        legend.text      = ggplot2::element_text(size = 10),
        legend.title     = ggplot2::element_text(face = "bold", hjust = 0.5, size = 10),
        axis.ticks       = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank()
      )

    return(tile_plot)

}
}

