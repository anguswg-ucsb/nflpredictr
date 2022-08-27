#' @title Scrape internet for Vegas Odds for current NFL season
#' @description Returns a dataframe with NFL odds for either the current week of the NFL season, or all available weeks.
#' @param all_weeks logical, whether to return all available odds or just the current weeks odds. Default is TRUE, returns just the current week odds
#' @return tibble with a row for each game with the odds columns from the perspective of the home team (spread, win, over under total)
#' @importFrom magrittr `%>%`
#' @importFrom rvest read_html html_nodes html_table
#' @importFrom janitor clean_names
#' @importFrom dplyr mutate case_when select summarise group_by ungroup arrange n lag filter
#' @importFrom stats setNames na.omit
#' @export
get_vegas <- function(all_weeks = FALSE) {

  # url to NFl odds tables
  vegas_url <- "https://vegas-odds.com/nfl/odds/"
  # vegas_url <- "https://sportsbook.draftkings.com/leagues/football/nfl"

  vegas_page <- rvest::read_html(vegas_url)

  # vegas odds table nodes
  vegas_tbls <-
    vegas_page %>%
    rvest::html_nodes("table")

  # current week
  current_week  <- get_week()

  # dates associated with weeks of NFL season
  week_dates <- get_week_dates()

  # current week date
  max_week_date <-
    week_dates %>%
    dplyr::filter(week == current_week)

  # empty list to add odds tables to
  odds_lst <- list()

  # loop through each table containing the odds for each game
  for (i in 1:length(vegas_tbls)) {

    odds_tbl <-
      vegas_tbls[[i]] %>%
      rvest::html_table(header = T) %>%
      stats::na.omit() %>%
      janitor::clean_names() %>%
      dplyr::mutate(across(where(is.numeric), as.character))

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
        nfl_teams(),
        by = c("team" = "team_name")
      ) %>%
      dplyr::select(-date_time, -team) %>%
      dplyr::mutate(
        homeaway = c("home_team", "away_team")
      ) %>%
      dplyr::relocate(team = team_abb, homeaway) %>%
      dplyr::mutate(
        spread = dplyr::case_when(
          grepl("EVEN", spread) ~ 0,
          TRUE ~ as.numeric(sub("\\s.*","", spread))
        ),
        total = as.numeric(sub("\\D*(\\d+).*", "\\1", total)),
        win   = as.numeric(win)
      ) %>%
      dplyr::ungroup()

    # home team
    home <-
      odds_tbl %>%
      dplyr::filter(homeaway == "home_team") %>%
      dplyr::relocate(home_team = team) %>%
      dplyr::select(home_team, date, spread, win, total)

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
      dplyr::relocate(date, home_team, away_team, favored)

    # date of the game from website
    odds_date <- vegas_odds$date[1]

    # match dates of games with week of season
    match_dates <-
      week_dates %>%
      dplyr::mutate(
        date        = odds_date,
        check_week  = dplyr::case_when(
          date >= week_start & date <= week_end ~ TRUE,
          TRUE                                    ~ FALSE
        )
      ) %>%
      dplyr::filter(check_week == TRUE) %>%
      dplyr::select(season, week, date)

    # add season, week, and game_id columns
    vegas_odds <-
      vegas_odds %>%
      dplyr::left_join(
        match_dates,
        by = "date"
      ) %>%
      dplyr::mutate(
        game_id = dplyr::case_when(
          week < 10  ~ paste0(season, "_0", week, "_", away_team, "_", home_team),
          week >= 10 ~ paste0(season, "_", week, "_", away_team, "_", home_team)
          )
      ) %>%
      dplyr::relocate(season, week, game_id, date)

    # add vegas odds dataframe to list of odds tables
    odds_lst[[i]] <- vegas_odds

  }

  # bind rows
  odds_df <- dplyr::bind_rows(odds_lst)

  # if all available odds is desired, set all_weeks to FALSE. Default is to return current week of odds (FALSE)
  if(all_weeks == TRUE) {

    return(odds_df)

  # Defaults to current week of games odds
  } else {

    # filter to just current week
    odds_df <-
      odds_df %>%
      dplyr::filter(week == current_week) # dplyr::filter(date >= max_week_date$week_start, date <= max_week_date$week_end)

    return(odds_df)

  }

}
