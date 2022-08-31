#' @title Scrape internet for Vegas Odds for current NFL season
#' @description Returns a dataframe with NFL odds for either the current week of the NFL season, or all available weeks.
#' @param all_weeks logical, whether to return all available odds or just the current weeks odds. Default is TRUE, returns just the current week odds
#' @return tibble with a row for each game with the odds columns from the perspective of the home team (spread, win, over under total)
#' @importFrom magrittr `%>%`
#' @importFrom rvest read_html html_nodes html_table
#' @importFrom janitor clean_names
#' @importFrom dplyr mutate case_when select summarise group_by ungroup arrange n filter relocate
#' @importFrom stats setNames na.omit
#' @importFrom progress progress_bar
#' @export
get_vegas <- function(
    all_weeks = FALSE
    ) {

  # URL to NFl odds tables
  vegas_url <- "https://vegas-odds.com/nfl/odds/"

  message(paste0("Retrieving Las Vegas betting odds...\nURL: ", vegas_url))

  vegas_page <- rvest::read_html(vegas_url)

  # vegas odds table nodes
  vegas_tbls <-
    vegas_page %>%
    rvest::html_nodes("table")

  # current week
  current_week  <- get_week()

  # dates associated with weeks of NFL season
  week_dates    <- get_week_dates()

  # current week date
  max_week_date <-
    week_dates %>%
    dplyr::filter(week == current_week)

  # empty list to add odds tables to
  odds_lst <- list()

  # progress bar
  pb <- progress::progress_bar$new(total = length(vegas_tbls))

  # loop through each table containing the odds for each game
  for (i in 1:length(vegas_tbls)) {

    # progress bar ticker
    pb$tick()

    tbl_date <- parse_table(page_table = vegas_tbls[[i]])

    # If only current week is desired
    if(all_weeks == FALSE) {

      # if parsed date is within range of current week, make odds table
      if(tbl_date >= max_week_date$week_start & tbl_date <= max_week_date$week_end) {

        vegas_odds <- make_odds_table(
          page_table = vegas_tbls[[i]],
          date_table = week_dates
          )

        # if parsed date NOT within range of current week, skip to next iteration
      } else {

        next

      }

    # If all weeks are desired
    } else {

      # clean odds table
      vegas_odds <- make_odds_table(
        page_table = vegas_tbls[[i]],
        date_table = week_dates
        )

    }

    # add vegas odds dataframe to list of odds tables
    odds_lst[[i]] <- vegas_odds

  }

  # bind rows and arrange by week
  odds_df <-
    odds_lst %>%
    dplyr::bind_rows() %>%
    dplyr::arrange(week)

  return(odds_df)

}

#' @title Scrape a week of NFL matchups for a given season from CBS.com
#' @description Finds the team matchups for a specific week of an NFL season
#' @param year numeric for season of interest
#' @param week numeric for the week that we want to get the team matchups for
#' @param verbose logical, whether to print messages as function runs. Default is to print messages (TRUE)
#' @importFrom dplyr mutate filter select case_when group_by ungroup bind_rows
#' @importFrom rvest read_html html_nodes html_table
#' @return dataframe with the home and away teams for the desired week
#' @examples
#' # Get NFL matchups for a given season and week
#' matchups <- get_matchups(
#'                         year = 2022,
#'                         week = 1
#'                        )
#' matchups
#' @export
get_matchups <- function(
    year        = NULL,
    week        = NULL,
    verbose     = TRUE
) {

  if(is.null(week)) {
    week = 1
  }

  # date when function is run
  current_year <- get_year()   # current_date <- Sys.Date()

  if(is.null(year)) {

    # Current year
    year  <- current_year

    if(verbose == TRUE) {

      message(paste0("No year entered, defaulting to current season - ", year))

    }

  }

  # If the year is greater than the current season, make year = current_year
  if(year > current_year) {

    # current year
    year  <- current_year

    if(verbose == TRUE) {
      message(paste0("Year entered was greater than current season, defaulting to current season - ", year))
    }

    # if year is before 2000, set year = 2000
  } else if(year < 2016) {

    year <-  2016

    if(verbose == TRUE) {

      message(paste0("Year entered was less than 2000 season, defaulting to ", year, "season"))

    }
  }

  # Check and make sure week is a valid week of season
  if(week < 1) {

    if(verbose == TRUE) {
      # setting week to 2 if week is less than 2
      message(paste0("Week ", week ," invalid\nWeek must be within valid week range: 1 - upcoming week\nSetting week = 1"))
    }
    week <-  1

  }

  # If season is past 2021 and prediction week is greater than 18, set week to 18
  if(year >= 2021 & week > 18) {

    if(verbose == TRUE) {
      # setting week to max week after 2020 season (18)
      message(paste0("Week ", week ," invalid\nWeek must be within valid week range: 1 - 18\nSetting week = 18"))
    }

    week <- 18

  }

  # If season is before 2021 and prediction week is greater than 17, set week to 17
  if(year < 2021 & week > 17) {

    if(verbose == TRUE) {
      # setting week to max week before 2021 season (17)
      message(paste0("Week ", week ," invalid\nWeek must be within valid week range: 1 - 17\nSetting week = 17"))
    }
    week <- 17

  }

 # regular season URL
 url <- paste0("https://www.cbssports.com/nfl/schedule/", year, "/regular/", week, "/")

 if(verbose == TRUE) {

   message(paste0("Retrieving matchups:\nSeason:", year, "\nWeek: ", week))

 }

  # Read HTML page using URL
  page <- rvest::read_html(url)

  # Extract HTML nodes for table
  page_nodes <-
    page %>%
    rvest::html_nodes("table")

  # empty list to add to in loop
  tbl_lst <- list()

  # loop through each table on CBS page and clean and add to list
  for (i in 1:length(page_nodes)) {

    # Extract each table of games representing the days games are played
    page_table <- rvest::html_table(
      page_nodes[[i]],
      header  = F,
      fill    = T,
      convert = T
    )

    # clean up table and get desired schedule for week
    page_table <-
      page_table %>%
      dplyr::select(away_team = X1, home_team = X2) %>%
      dplyr::filter(!grepl("Away", away_team, ignore.case = TRUE)) %>%
      dplyr::mutate(
        away_team = gsub("[[:punct:]]", "", away_team),
        home_team = gsub("[[:punct:]]", "", home_team)
      ) %>%
      dplyr::mutate(
        away_abb = dplyr::case_when(
          grepl(" ", away_team) ~ gsub("(*UCP)[^;-](?<!\\b\\p{L})", "", away_team, perl=TRUE),
          TRUE                  ~ toupper(substr(away_team, 1, 3))
        ),
        home_abb = dplyr::case_when(
          grepl(" ", home_team) ~ gsub("(*UCP)[^;-](?<!\\b\\p{L})", "", home_team, perl=TRUE),
          TRUE                  ~ toupper(substr(home_team, 1, 3))
        )
      ) %>%
      dplyr::mutate(
        season    = year,
        week      = week,
        away_team = dplyr::case_when(
          away_abb == "LC"  ~ "LAC",
          away_abb == "LR"  ~ "LA",
          away_abb == "NJ"  ~ "NYJ",
          away_abb == "NG"  ~ "NYG",
          away_abb == "JAC" ~ "JAX",
          TRUE              ~ away_abb
        ),
        home_team = dplyr::case_when(
          home_abb == "LC"  ~ "LAC",
          home_abb == "LR"  ~ "LA",
          home_abb == "NJ"  ~ "NYJ",
          home_abb == "NG"  ~ "NYG",
          home_abb == "JAC" ~ "JAX",
          TRUE              ~ home_abb
        ),
        game_id   = dplyr::case_when(
          week < 10  ~ paste0(year, "_0", week, "_", away_team, "_",  home_team),
          week >= 10 ~ paste0(year, "_", week, "_", away_team, "_",  home_team)
        )
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(season, week, game_id, home_team, away_team)

    tbl_lst[[i]] <- page_table
  }


  # Bind rows of list
  upcoming_games <- dplyr::bind_rows(tbl_lst)

  return(upcoming_games)

}

#' @title Extract date from scraped vegas-lines.com page
#' @param page_table xml_node containing a table entry with betting information for a matchup
#' @return character string representing date of odds table
#' @importFrom magrittr `%>%`
#' @importFrom rvest html_table
#' @importFrom janitor clean_names
#' @importFrom dplyr mutate across
#' @importFrom tidyr pivot_longer
#' @importFrom stats na.omit
parse_table <- function(page_table) {

  odds_tbl <-
    page_table %>%
    rvest::html_table(header = T) %>%
    stats::na.omit() %>%
    janitor::clean_names() %>%
    dplyr::mutate(dplyr::across(where(is.numeric), as.character))

  # parse date time column
  parse_date <- as.Date(
    gsub(" ", "", strsplit(odds_tbl$date_time[1], "@")[[1]][1]),
    format = "%m/%d/%y"
  )

  return(parse_date)

}

#' @title Clean single betting table from scraped vegas-odds.com page
#' @param page_table xml_node containing a table entry with betting information for a matchup
#' @param date_table a dataframe detailing the starting and ending dates of each week of an NFL season, the output from get_week_dates() function. Default is NULL and current season date table will be pulled
#' @param all_weeks logical, whether to return all weeks on vegas-odds.com page or only the current week. Default is FALSE, returns only the current week of odds.
#' @return tibble with a tidied table with odds data
#' @importFrom magrittr `%>%`
#' @importFrom rvest html_table
#' @importFrom janitor clean_names
#' @importFrom dplyr mutate across case_when left_join select n filter relocate bind_cols
#' @importFrom tidyr pivot_longer
#' @importFrom stats na.omit
make_odds_table <- function(
    page_table = NULL,
    date_table = NULL,
    all_weeks  = FALSE
    ) {

  if(is.null(page_table)) {

    # dates associated with weeks of NFL season
    stop(paste0("Invalid 'page_table' argument\nRequires a HTML table from vegas-odds.com"))

  }

  if(is.null(date_table)) {

    # dates associated with weeks of NFL season
    date_table    <- get_week_dates()

  }

  # load html table of game odds
  odds_tbl <-
    page_table %>%
    rvest::html_table(header = T) %>%
    stats::na.omit() %>%
    janitor::clean_names() %>%
    dplyr::mutate(dplyr::across(where(is.numeric), as.character))

  # parse date time column
  parse_date <- as.Date(
    gsub(" ", "", strsplit(odds_tbl$date_time[1], "@")[[1]][1]),
    format="%m/%d/%y"
  )

  # add parse_date as a date column, join team abbreviations with team name and keep relevent columns
  odds_tbl <-
    odds_tbl %>%
    dplyr::mutate(
      date = parse_date
    ) %>%
    dplyr::left_join(
      get_nfl_teams(),
      by = c("team" = "team_name")
    ) %>%
    dplyr::select(-date_time, -team)

  # date of the game from website
  odate <- odds_tbl$date[1]

  # match dates of games with week of season
  odds_week <-
    week_dates %>%
    dplyr::mutate(
      date        = odate,
      check_week  = dplyr::case_when(
        date >= week_start & date <= week_end ~ TRUE,
        TRUE                                  ~ FALSE
      )
    ) %>%
    dplyr::filter(check_week == TRUE) %>%
    dplyr::select(season, week, date)

  # get schedule for season/week of odds table
  matchups <- get_matchups(
    year    = odds_week$season,
    week    = odds_week$week,
    verbose = FALSE
  )

  # home and away info
  game_location <-
    matchups %>%
    tidyr::pivot_longer(
      cols      = c(home_team, away_team),
      names_to  = "homeaway",
      values_to = "team_abb") %>%
    dplyr::filter(team_abb %in% odds_tbl$team_abb) %>%
    dplyr::select(season, week, game_id, homeaway, team_abb)

  # extract betting odds data
  odds_tbl <-
    odds_tbl %>%
    dplyr::left_join(
      game_location,
      by = c("team_abb")
    ) %>%
    # dplyr::mutate(
    #   homeaway = c("away_team", "home_team")
    # ) %>%
    dplyr::relocate(team = team_abb, homeaway) %>%
    dplyr::mutate(
      spread = dplyr::case_when(
        grepl("EVEN", spread) ~ 0,
        TRUE ~ as.numeric(sub("\\s.*","", spread))
      ),
      total = as.numeric(sub("\\D*(\\d+).*", "\\1", total)),
      win   = as.numeric(win)
    )

  # home team
  home <-
    odds_tbl %>%
    dplyr::filter(homeaway == "home_team") %>%
    # dplyr::relocate(home_team = team) %>%
    dplyr::select(season, week, game_id, home_team = team, date, spread, win, total)

  # away team
  away <-
    odds_tbl %>%
    dplyr::filter(homeaway == "away_team") %>%
    dplyr::select(away_team = team)

  # Bind columns of home and away teams into single row
  vegas_odds <-
    home %>%
    dplyr::bind_cols(away) %>%
    dplyr::mutate(
      favored = dplyr::case_when(
        spread > 0  ~ 0,  # If spread is positive, away team is favored,
        spread < 0  ~ 1,  # If spread is negative, home team is favored
        spread == 0 ~ 0
      )
    ) %>%
    dplyr::relocate(season, week, game_id, date, home_team, away_team, favored)

  return(vegas_odds)

}

