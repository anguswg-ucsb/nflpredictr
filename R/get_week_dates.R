#' @title Find dates coorsponding to weeks of a given NFL season
#' @description Returns a tibble with the starting and ending dates of each week in a given NFL season.
#' @param date character date, YYYY-MM-DD. Default is NULL and date will set to the date the function was run (i.e. the current season)
#' @importFrom magrittr `%>%`
#' @importFrom lubridate ymd
#' @importFrom rvest read_html html_nodes html_table
#' @importFrom janitor clean_names
#' @importFrom dplyr mutate case_when select summarise group_by ungroup arrange n lag filter
#' @importFrom stats setNames
#' @return tibble showing the weeks of the desired NFL season with start_week and end_week columns indicating the dates that a week starts and ends, respectively.
#' @export
get_week_dates <- function(date = NULL) {

  # if no date is entered, set to date function is run
  if(is.null(date)) {

    date <- Sys.Date()

  }

  message(paste0("Date input: ", date))

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

  message(paste0("Retrieving dates of NFL weeks: ", year))

  # Construct URL
  url  <- paste0("https://www.pro-football-reference.com/years/", year ,"/games.htm")

  # Read HTML page using URL
  page <- rvest::read_html(url)

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

  # remove extra header rows by "Week" string
  page_table <- page_table[!grepl("Week", page_table$week), ]

  # remove extra header rows by "Day" string
  page_table <- page_table[!grepl("Day", page_table$day), ]

  # remove extra header rows by "Date" string
  page_table <- page_table[!grepl("Date", page_table$datex), ]

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
    week_dates <-
      page_table %>%
      dplyr::arrange(min_date) %>%
      dplyr::group_by(week, min_date, max_date) %>%
      dplyr::slice(1) %>%
      dplyr::select(-datex) %>%
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
          is.na(min_date) ~ max_date - 7,
          TRUE            ~ min_date
        )
      ) %>%
      dplyr::rename(week_start = min_date, week_end = max_date) %>%
      dplyr::filter(!grepl("Pre", week))

    # dates for week 1
    week_one <-
      week_dates %>%
      dplyr::filter(week == 1)

    # Set start of week 1 to July 1st of that year
    week_one$week_start <- as.Date(paste0(year, "-07-01"))

    # remove preseason weeks & week 1, bind rows from week_one w/ the altered week_start, arrange by week
    week_dates <-
      week_dates %>%
      dplyr::filter(!grepl("Pre", week), week != 1) %>%
      dplyr::bind_rows(week_one) %>%
      dplyr::mutate(week= as.numeric(week)) %>%
      dplyr::arrange(week) %>%
      dplyr::mutate(season = year) %>%
      dplyr::relocate(season, week) %>%
      dplyr::select(-day)

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
        date = as.Date(paste(datex, new_year), format='%b %d %Y')
      ) %>%
      dplyr::group_by(week) %>%
      dplyr::mutate(
        min_date = min(date),
        max_date = max(date)
      ) %>%
      dplyr::ungroup()


    # start and end of each NFL week, extract current week
    week_dates <-
      page_table %>%
      dplyr::arrange(min_date) %>%
      dplyr::group_by(week, min_date, max_date) %>%
      dplyr::slice(1) %>%
      dplyr::select(-date, -datex) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(min_date) %>%
      dplyr::mutate(
        min_date      = as.Date(min_date),
        max_date      = as.Date(max_date),
        current_date  = date,
        min_date      = dplyr::lag(max_date) + 1,
        min_date      = dplyr::case_when(
          is.na(min_date) ~ max_date - 7,
          TRUE            ~ min_date
        )
      ) %>%
      dplyr::select(week, week_start = min_date, week_end = max_date, current_date)

    # dates for week 1
    week_one <-
      week_dates %>%
      dplyr::filter(!grepl("Pre", week)) %>%
      dplyr::filter(week == "1")

    # Set start of week 1 to July 1st of that year
    week_one$week_start <- as.Date(paste0(year, "-07-01"))

    # remove preseason weeks & week 1, bind rows from week_one w/ the altered week_start, arrange by week
    week_dates <-
      week_dates %>%
      dplyr::filter(!grepl("Pre", week), week != "1") %>%
      dplyr::bind_rows(week_one) %>%
      dplyr::mutate(week= as.numeric(week)) %>%
      dplyr::arrange(week) %>%
      dplyr::mutate(season = year) %>%
      dplyr::relocate(season, week)

  }

  # if week is outside week range, set to week 1 of the current season
  if(nrow(week_dates) == 0) {

    stop(paste0("No data avaliable ---> check webpage in browser at URL: ", url))

  }

  return(week_dates)

}
