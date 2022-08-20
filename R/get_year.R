#' @title Find the year of the NFL season for a given date
#' @description Returns numeric year of the NFL season for a given date
#' @param date character string date, default is NULL and date will set to the date the function was run
#' @return numeric indicating the season year
get_year <- function(date = NULL) {

  if(is.null(date)) {

    # current date when function is run in VM
    date <- Sys.Date()

  }

  # Current year
  year  <- as.numeric(substr(date, 1, 4))

  # Current month
  month <- as.numeric(substr(date, 6, 7))

  # If month is in part of season after Jan 1
  if (month %in% c(1, 2, 3, 4)) {

    year <- year - 1
  }

  return(year)
}
