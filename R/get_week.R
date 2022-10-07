#' @title Find the week of the NFL season according to a date
#' @description returns week of the NFL season by a date, if no date is entered, default is to return the current week of the season.
#' @param date character date, YYYY-MM-DD. Default is NULL and date will set to the date the function was run
#' @importFrom magrittr `%>%`
#' @importFrom lubridate ymd
#' @importFrom rvest read_html html_nodes html_table
#' @importFrom janitor clean_names
#' @importFrom dplyr mutate case_when select summarise group_by ungroup arrange n lag filter
#' @importFrom stats setNames
#' @return character string of the current week of the NFL season
get_week <- function(date = NULL) {

  # if no date is entered, set to date function is run
  if(is.null(date)) {

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

    year     <- year - 1

  }

  # year function is run
  sys_year <- get_year()

  # Construct URL
  url  <- url(
    paste0("https://www.pro-football-reference.com/years/", year ,"/games.htm"),
    "rb"
  )

  # Read HTML page using URL
  page <- rvest::read_html(url)

  # close URL connection
  close(url)
  # closeAllConnections()

  # Extract HTML nodes for table
  page_nodes <-
    page %>%
    rvest::html_nodes("table")

  # Extract season games
  page_table <- rvest::html_table(
    page_nodes[[1]],
    header  = T,
    fill    = T,
    convert = F
  ) %>%
    janitor::clean_names() %>%
    dplyr::mutate(
      home = case_when(
        x == ""  ~ 1,
        x == "@" ~ 0,
        x == "N" ~ 0
      )
    ) %>%
    dplyr::select(1:3) %>%
    stats::setNames(c("week", "day", "datex"))

  # remove headers for each week
  page_table <- page_table[!grepl("Week", page_table$week), ]

  # remove empty rows
  page_table <- page_table[!apply(page_table == "", 1, any),]

  # if year is less than current year, dates are correctly formatted, no need to parse dates
  if(year < sys_year) {

    # remove headers for each week
    page_table <- page_table[!grepl("Playoffs", page_table$week), ]

    # Create a clean date and min max of dates for each week of games
    page_table <-
      page_table %>%
      dplyr::group_by(week) %>%
      dplyr::mutate(
        min_date = min(datex),
        max_date = max(datex)
      ) %>%
      dplyr::ungroup()

    # start and end of each NFL week, extract current week
    current_week <-
      page_table %>%
      dplyr::group_by(week, min_date, max_date) %>%
      dplyr::summarise() %>%
      dplyr::ungroup() %>%
      dplyr::arrange(min_date) %>%
      dplyr::mutate(
        week = 1:n()
      ) %>%
      dplyr::mutate(
        min_date      = as.Date(min_date),
        max_date      = as.Date(max_date),
        current_date  = date,
        min_date      = dplyr::lag(max_date) + 1,
        min_date      = dplyr::case_when(
          is.na(min_date) ~ max_date - 1,
          TRUE            ~ min_date
        )
      ) %>%
      dplyr::filter(date >= min_date, date <= max_date) %>%
      .$week

  } else {

    # Create a clean date and min max of dates for each week of games
    page_table <-
      page_table %>%
      dplyr::mutate(
        year     = year,
        new_year = dplyr::case_when(
          grepl("January|February|March", datex) ~ year + 1,
          TRUE                                   ~ year
        ),
        date = as.Date(paste0(new_year, "-", substr(datex, 6, 10)))
        # date = as.Date(paste(datex, new_year), format='%Y-%m-%d')
        # date = gsub(sub('(-.*)\\-|-.*', '\\1', datex), new_year, datex)
        # date = as.Date(paste(datex, new_year), format='%b %d %Y')
      ) %>%
      dplyr::group_by(week) %>%
      dplyr::mutate(
        min_date = min(date),
        max_date = max(date)
      ) %>%
      dplyr::ungroup()

    # start and end of each NFL week, extract current week
    current_week <-
      page_table %>%
      dplyr::group_by(week, min_date, max_date) %>%
      dplyr::summarise() %>%
      dplyr::ungroup() %>%
      dplyr::arrange(min_date) %>%
      dplyr::mutate(
        current_date  = date,
        min_date      = dplyr::lag(max_date) + 1,
        min_date      = dplyr::case_when(
          is.na(min_date) ~ max_date - 1,
          TRUE            ~ min_date
        )
      ) %>%
      dplyr::filter(date >= min_date, date <= max_date) %>%
      .$week

  }

  # if week is outside week range, set to week 1 of the current season
  if(length(current_week) == 0) {

    current_week <- 1

  }

  # if current week returns a preseason week, set current_week = 1
  if(grepl("Pre", current_week)) {

    current_week <- 1

  }

  # ensure week is a numeric
  current_week <- as.numeric(current_week)

  return(current_week)

}
