#' @title Request predictions from NFL Win Predictor API
#' @description Returns numeric year of the NFL season for a given date
#' @param year character or numeric year of season to get predictions for
#' @param week numeric value for the week of the season to predict
#' @param base_url character string defining the base API endpoint URL. Default is predict-new-data endpoint
#' @importFrom httr POST content
#' @importFrom dplyr tibble arrange
#' @importFrom magrittr `%>%`
#' @return numeric indicating the season year
#' @export
predict_games <- function(
    year = NULL,
    week = NULL,
    base_url = "http://68.183.25.9:8000/predict-new-data?"
    ) {

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
    dplyr::arrange(-.pred_1)

  return(win_api)

}

