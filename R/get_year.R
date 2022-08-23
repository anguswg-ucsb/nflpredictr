#' @title Find the year of the NFL season for a given date
#' @description Returns numeric year of the NFL season for a given date
#' @param date character string date, default is NULL and date will set to the date the function was run
#' @importFrom lubridate ymd
#' @return numeric indicating the season year
get_year <- function(date = NULL) {

  # if no date is entered, default ins NULL
  if(is.null(date)) {

    # current date when function is run in VM
    date <- Sys.Date()

  }

  # check that date is in correct format
  check_date <- lubridate::ymd(date, quiet = TRUE)

  # error check
  if(is.na(check_date)) {
    stop("Incorrect date format, date must be in YYYY-MM-DD format")
  }

  # Current year
  year  <- as.numeric(substr(date, 1, 4))

  # Current month
  month <- as.numeric(substr(date, 6, 7))

  # If month is in part of season after Jan 1
  if (month %in% c(1, 2, 3, 4)) {

    year <- year - 1

  }

  # return year
  return(year)

}

