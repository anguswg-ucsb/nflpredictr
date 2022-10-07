#' @title Request predictions for past and upcoming NFL games from the NFL Win Predictor API
#' @description Retrieve NFL game predictions for a given year and week
#' @param year character or numeric year of season to get predictions for
#' @param week numeric value for the week of the season to predict
#' @param base_url character base URL to NFL Win Predictor API. Default is: "http://68.183.25.9:8000/predict-new-data?"
#' @importFrom dplyr tibble arrange select bind_rows
#' @importFrom magrittr `%>%`
#' @importFrom httr POST content
#' @return tibble with a row for each matchup between teams for the desired week. There is basic identifying information on each game and three columns representing the model predicted outcomes: .pred_class, .pred_1, .pred_0. .pred_class gives a 1 if the home team is predicted to win, and a 0 if the home team is predicted to lose. The .pred_1 and .pred_0 columns indicate the probability of either class occurring.
#' @examples
#' # Predict week 8 of the 2021 NFL season
#' predict_games(year = 2021, week = 8)
#' @export
predict_games <- function(
    year     = NULL,
    week     = NULL,
    base_url = "http://68.183.25.9:8000/predict-new-data?"
    ) {

  # stop if week 1, TODO, fix DO droplet API
  if(week == 1) {
    stop(paste0("Invalid 'week' argument\n'week' must be > 1"))
  }

  # if no year is entered, default to current season
  if(is.null(year)) {
    year <- get_year()
  }

  # if no week is entered, default to current week
  if(is.null(week)) {
    week <- get_week()
  }

  # Construct API URL
  url <- paste0(base_url, "year=", year, "&pred_week=", week)

  # message to user
  message(paste0("\n\nSending request to nflwinpredictor API...\nRequest URL:\n", url))

  # Post request to API and format responce into dataframe
  win_api <-
    url %>%
    httr::POST() %>%
    httr::content(as = "parsed")

  # if an error is returned from the API
  if (any(grepl("Internal server error", win_api) == TRUE)) {

    stop(
      paste0("\nInvalid inputs:\nyear = ", year, "\nweek = ", week)
         )
  }

  # convert parsed list to tibble, arrange by highest probability of winning
  win_api <-
    win_api %>%
    dplyr::bind_rows() %>%
    dplyr::tibble() %>%
    dplyr::arrange(-home_win_prob )

  return(win_api)


  # # if NA year given,  default to current season
  # if(is.na(year)) {
  #   year <- get_year()
  # }
  #
  # # if NA week is entered, default to current week
  # if(is.na(week)) {
  #   week <- get_week()
  # }
  #
  # # if no year is entered, default to current season
  # if(is.null(year)) {
  #
  #   year <- get_year()
  #
  # }
  #
  # # if no week is entered, default to current week
  # if(is.null(week)) {
  #
  #   week <- get_week()
  #
  # }

  # # scrape new data to input into model
  # scrape_data <- scrape_games(
  #   year      = year,
  #   pred_week = week
  # )
  #
  # # generate predictions
  # prediction_df <-
  #   win_model %>%
  #   tune::augment(new_data = scrape_data) %>%
  #   dplyr::select(
  #     season, week, game_id,
  #     home_team     = team,
  #     away_team     = opponent,
  #     win           = .pred_class,
  #     home_win_prob = .pred_1,
  #     away_win_prob = .pred_0
  #   )
  #
  # return(prediction_df)

  # library(devtools)
  # year = NULL
  # week = NULL
  # source("R/get_year.R")
  # source("R/get_week.R")
  # source("R/get_week_dates.R")
  # source("R/utils.R")
  # source("R/get_nfl_teams.R")
  # source("R/get_vegas.R")
  # source("R/win_model_data.R")
  # library(tidyverse)
  # library(tune)
  # win_model <- readRDS("win_model_logistic_reg.rds")
  # usethis::use_data(win_model, internal = TRUE, overwrite = T)


}
