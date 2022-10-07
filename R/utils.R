#' @title Calculate win percentages
#' @description Calculates overall, home, and away win percentages
#' @param df dataframe with home/away teams, week, and game outcome
#' @param verbose logical, if TRUE, prints log message. Default is TRUE
#' @importFrom tidyr pivot_longer fill
#' @importFrom dplyr select group_by arrange mutate lag case_when ungroup filter n across bind_rows relocate `%>%`
#' @importFrom stringr str_split_fixed
#' @return dataframe
get_win_pct <- function(df, verbose = TRUE) {

  # Enable log messages
  if(verbose == TRUE) {
    message(paste0("Generating home/win totals and win % ..."))
  }

  sched <-
    df %>%
    tidyr::pivot_longer(
      cols      = c(home_team, away_team),
      names_to  = "home_away",
      values_to = "team"
    )

  # number of rest days between games
  rest_df <-
    sched %>%
    dplyr::select(season, week, game_id,team, home_away, gameday) %>%
    dplyr::group_by(season, team) %>%
    dplyr::arrange(week, .by_group = T) %>%
    dplyr::mutate(
      lag_gameday = dplyr::lag(gameday),
      rest_days   = as.numeric(round(difftime(gameday, lag_gameday), 0))
    ) %>%
    dplyr::select(season, week, game_id, team, rest_days) %>%
    replace(is.na(.), 7)


  wins_df <-
    sched %>%
    dplyr::select(game_id, season, week, team, home_away, home_score, away_score) %>%
    dplyr::group_by(game_id, home_away) %>%
    dplyr::mutate(
      win = case_when(
        home_away == "home_team"  & home_score > away_score ~ 1,
        home_away == "away_team" & away_score > home_score ~ 1,
        away_score == home_score ~ 0,
        TRUE ~ 0
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(team) %>%
    dplyr::arrange(week, .by_group = T) %>%
    dplyr::mutate(
      games     = 1:n(),
      win_total = cumsum(win),
      win_pct   = win_total/games
    ) %>%
    dplyr::ungroup()

  # Home game win pct %
  home_games <-
    wins_df %>%
    dplyr::filter(home_away == "home_team") %>%
    dplyr::group_by(season, team) %>%
    dplyr::arrange(week, .by_group = T) %>%
    dplyr::mutate(
      ngames         = 1:n(),
      home_win_total = cumsum(win),
      home_win_pct   = home_win_total/ngames
    )

  # Away game win pct %
  away_games <-
    wins_df %>%
    dplyr::filter(home_away == "away_team") %>%
    dplyr::group_by(season, team) %>%
    dplyr::arrange(week, .by_group = T) %>%
    dplyr::mutate(
      ngames         = 1:n(),
      away_win_total = cumsum(win),
      away_win_pct   = away_win_total/ngames
    )

  # Final wins dataframe
  wins <-
    home_games %>%
    dplyr::bind_rows(away_games) %>%
    dplyr::group_by(season, team) %>%
    dplyr::arrange(week, .by_group = T) %>%
    tidyr::fill(home_win_pct, away_win_pct, .direction = "down") %>%
    # dplyr::mutate(
    #   home_win_pct = zoo::na.locf(home_win_pct, na.rm = F),
    #   away_win_pct = zoo::na.locf(away_win_pct, na.rm = F)
    # ) %>%
    replace(is.na(.), 0) %>%
    dplyr::ungroup() %>%
    dplyr::select(season, week, game_id, team, home_away, win, home_score, away_score,
                  home_win = home_win_total,
                  away_win = away_win_total,
                  win_pct, home_win_pct,away_win_pct) %>%
    dplyr::mutate(dplyr::across(where(is.numeric), round, 3)) %>%
    dplyr::left_join(
      rest_df,
      by = c("season", "week", "team", "game_id")
    ) %>%
    dplyr::relocate(season, week, game_id, team, home_away, rest_days) %>%
    dplyr::mutate(
      split_game_id = substr(game_id, 9, 20)
    ) %>%
    dplyr::ungroup()

  # Replace changed team names from game ID
  wins$split_game_id <- gsub("OAK", "LV", wins$split_game_id)
  wins$split_game_id <- gsub("SD", "LAC", wins$split_game_id)
  wins$split_game_id <- gsub("STL", "LA", wins$split_game_id)

  wins$team <- gsub("OAK", "LV", wins$team)
  wins$team <- gsub("SD", "LAC", wins$team)
  wins$team <- gsub("STL", "LA", wins$team)

  wins <-
    wins %>%
    dplyr::mutate(
      home_team     =  stringr::str_split_fixed(split_game_id, "_", 2)[,2],
      away_team     =  stringr::str_split_fixed(split_game_id, "_", 2)[,1]
    ) %>%
    dplyr::mutate(
      opponent  = case_when(
        home_team == team ~ away_team,
        away_team == team ~ home_team
      )
    ) %>%
    dplyr::select(-split_game_id, -home_team, -away_team) %>%
    dplyr::relocate(season,week, game_id, team, opponent, home_away, rest_days) %>%
    dplyr::filter(team != "")


  return(wins)

}

#' @title Calculate NFL Elo Ratings
#' @description Calculates Elo ratings for an NFL season
#' @param nfl_season dataframe with home/away teams, week, home/away team scores, and game outcomes
#' @importFrom elo elo.run
#' @importFrom dplyr select group_by group_split mutate left_join case_when ungroup n across bind_rows `%>%`
#' @return dataframe
get_nfl_elo <- function(nfl_season) {

  df <-
    nfl_season %>%
    dplyr::mutate(
      wins_home = home_score > away_score
    )

  nfl_er <- elo::elo.run(wins_home ~ team + opponent, data = df, k = 20) %>%
    as.data.frame() %>%
    dplyr::group_by(team.A) %>%
    dplyr::mutate(
      r_id = 1:n()
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      team_join = paste0(team.A, "_", team.B, "_", r_id)
    )

  nfl_elo <-
    df %>%
    dplyr::group_by(team) %>%
    dplyr::mutate(
      r_id = 1:n()
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      team_join = paste0(team, "_", opponent, "_", r_id)
    ) %>%
    dplyr::left_join(
      dplyr::select(nfl_er, team_join, elo_team = elo.A, elo_opponent = elo.B),
      by = "team_join"
    ) %>%
    dplyr::select(-team_join, -home_score, -away_score, -wins_home, -r_id)

  home_rating <-
    nfl_elo %>%
    dplyr::group_by(team) %>%
    dplyr::group_split()

  home_elo <- lapply(home_rating, FUN = function(x) {
    rate <-
      x %>%
      dplyr::select(season, week, game_id, win, team, elo = elo_team)
  }) %>%
    dplyr::bind_rows()

  away_rating <-
    nfl_elo %>%
    dplyr::group_by(opponent) %>%
    dplyr::group_split()

  away_elo <- lapply(away_rating, FUN = function(x) {
    rate <-
      x %>%
      dplyr::select(season, week, game_id, win, team = opponent, elo = elo_opponent)
  }) %>%
    dplyr::bind_rows()

  final_elo <- dplyr::bind_rows(home_elo, away_elo)

  return(final_elo)

}

#' @title Process HTML table nodes from profootballreference.com
#' @description Cleans pages with game details from profootballreference.com
#' @param page xml_document created by using rvest::read_html on desired page
#' @importFrom rvest html_nodes html_table
#' @importFrom janitor clean_names
#' @importFrom stats setNames
#' @importFrom dplyr select group_by arrange mutate lag case_when ungroup summarise n relocate `%>%`
#' @return data.frame
process_page <- function(page) {

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
    )

  # remove headers for each week
  page_table <- page_table[!grepl("Week", page_table$week), ]

  # remove empty rows
  page_table <- page_table[!apply(page_table == "Playoffs", 1, any),]

  # fix playoff week names
  fix_weeks <-
    page_table %>%
    dplyr::group_by(week) %>%
    dplyr::mutate(
      min_date = min(as.Date(date))
    ) %>%
    dplyr::group_by(week, min_date) %>%
    dplyr::summarise() %>%
    dplyr::ungroup() %>%
    dplyr::arrange(min_date) %>%
    dplyr::mutate(
      num_week = 1:n()
    )

  # fix weeks to numbers
  page_table <-
    page_table %>%
    dplyr::left_join(
      dplyr::select(fix_weeks, week, num_week),
      by = "week"
    ) %>%
    dplyr::select(-week) %>%
    dplyr::relocate(week = num_week)

  return(page_table)

}

#' @title Scrape data needed to make a prediction for a given week
#' @description Retrieves the necessary model input data from profootballreference.com and returns a dataframe that can be inputted into models
#' @param year numeric for season of interest
#' @param pred_week numeric for the week that we want to get the team matchups for
#' @importFrom tidyr pivot_longer
#' @importFrom rvest read_html html_nodes html_table
#' @importFrom janitor clean_names
#' @importFrom stats setNames
#' @importFrom dplyr select group_by arrange mutate lag case_when ungroup summarise n slice across relocate filter bind_rows `%>%`
#' @importFrom stringr str_split_fixed
#' @return dataframe with necessary model inputs
scrape_games <- function(
    year      = NULL,
    pred_week = NULL
) {

  # if no input given for prediction week, set to predict for week 1
  if(is.null(pred_week)) {

    pred_week = 1

  }

  # current date when function is run in VM
  current_date <- Sys.Date()

  # current NFL season year
  current_year <- get_year()

  # if no input year is given, set prediction year to current season
  if(is.null(year)) {

    # default returns year of current season
    year <- current_year

  }

  # If input year is greater than the current season, set year to current_year
  if(year >= current_year) {

    # current week of NFL season
    current_week <- get_week()

    # default returns year of current season
    year  <- current_year

    # check if pred_week is too far in the future to make prediction
    if(pred_week > current_week) {

      # set pred_week to current week of NFL season
      pred_week <- current_week

    }

  }

  # if input year is before 2016, set year to 2016 (web scrape lookback limitation)
  if(year < 2016) {

    year <- 2016

    message(paste0("Year entered was less than 2000 season, defaulting to ", year, " season"))
  }

  # Check and make sure pred_week is a valid week of season
  if(pred_week < 1) {

    # setting week to 2 if pred_week is less than 2
    message(paste0("Week ",  pred_week, " invalid\nWeek must be within valid week range: 1 - upcoming week\nSetting pred_week = 1"))

    pred_week = 1


  }

  # Take account of added game after 2020 season
  if(year >= 2021 & pred_week > 18) {

    # setting week to max week after 2020 season (18)
    message(paste0("Week ",  pred_week, " invalid\nWeek must be within valid week range: 1 - 18\nSetting pred_week = 18"))

    pred_week = 18

  }

  # Take account of fewer games before 2021 seasons
  if(year < 2021 & pred_week > 17) {

    # setting week to max week before 2021 season (17)
    message(paste0("Week ",  pred_week, " invalid\nWeek must be within valid week range: 1 - 17\nSetting pred_week = 17"))
    pred_week = 17

  }

  # if prediction for week 1 is desired, use last seasons data
  if(pred_week == 1) {

    # Prior season to get metrics leading into week 1 of year
    past_year <- year - 1

    # Construct URL
    url  <- url(
      paste0("https://www.pro-football-reference.com/years/", past_year ,"/games.htm"),
      "rb"
    )

    # Read HTML page using URL
    page <- rvest::read_html(url)

    # close URL connection
    close(url)
    # closeAllConnections()

    # Rename playoff columns as weeks, accounting for added game after 2020 season
    if (past_year >= 2021) {

      # If season is after 2020
      page_table <- process_page(
        page      = page
      ) %>%
        dplyr::filter(week <= 18)

      # Rename playoff columns as weeks, accounting for fewer games before 2021
    } else {

      # if season is before 2021
      page_table <- process_page(
        page      = page
      ) %>%
        dplyr::filter(week <= 17)

    }

    # parse data tables from Pro Football Reference
    outcomes <-
      page_table %>%
      dplyr::left_join(
        dplyr::select(
          get_nfl_teams(),
          team_name, win_team_abb = team_abb
          ),
        by = c("winner_tie" = "team_name")
      ) %>%
      dplyr::left_join(
        dplyr::select(
          get_nfl_teams(),
          team_name, lose_team_abb = team_abb
          ),
        by = c("loser_tie" = "team_name")
      ) %>%
      dplyr::select(week, date,
                    win_team  = win_team_abb,
                    x,
                    lose_team = lose_team_abb,
                    pts_win   = pts,
                    pts_lose  = pts_2,
                    tow, tol)  %>%
      dplyr::mutate(
        home_team = dplyr::case_when(
          x == ""  ~ win_team,
          x == "@" ~ lose_team,
          week == 22 ~ win_team
        ),
        away_team = dplyr::case_when(
          x == ""  ~ lose_team,
          x == "@" ~ win_team,
          week == 22 ~ lose_team
        ),
        pts_win  = as.numeric(pts_win),
        pts_lose = as.numeric(pts_lose),
        game_id  = dplyr::case_when(
          week < 10  ~ paste0(past_year, "_0", week, "_", away_team, "_",  home_team),
          week >= 10 ~ paste0(past_year, "_", week, "_", away_team, "_",  home_team)
        )
      ) %>%
      dplyr::select(-x) %>%
      dplyr::group_by(game_id) %>%
      dplyr::mutate(
        home_pts = dplyr::case_when(
          home_team == win_team ~ max(pts_win, pts_lose),
          home_team != win_team ~ min(pts_win, pts_lose)
        ),
        away_pts = dplyr::case_when(
          home_team == win_team ~ min(pts_win, pts_lose),
          home_team != win_team ~ max(pts_win, pts_lose)
        ),
        home_turnovers = dplyr::case_when(
          home_team == win_team ~ tow,
          home_team != win_team ~ tol
        ),
        away_turnovers = dplyr::case_when(
          home_team == win_team ~ tol,
          home_team != win_team ~ tow
        ),
        season = year
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(season, week, game_id, gameday = date, home_team, away_team,
                    home_score = home_pts, away_score = away_pts,
                    home_turnovers, away_turnovers)

    # calculate win loss percentages
    record <- get_win_pct(outcomes, verbose = FALSE)

    # Create Score differential, home or away team ID, # of rest days
    outcomes <-
      outcomes %>%
      tidyr::pivot_longer(
        cols      = c(home_team, away_team),
        names_to  = "home",
        values_to = "team"
      ) %>%
      dplyr::mutate(
        home        = dplyr::case_when(
          home == "home_team" ~ 1,
          home == "away_team" ~ 0
        ),
        score_diff  = dplyr::case_when(
          home == 1 ~ home_score - away_score,
          home == 0 ~ away_score - home_score
        ),
        home_away   = dplyr::case_when(
          home == 1 ~ "home_team",
          home == 0 ~ "away_team"
        )
      ) %>%
      dplyr::mutate(
        split_game_id = substr(game_id, 9, 20)
      ) %>%
      dplyr::mutate(
        home_team     =  stringr::str_split_fixed(split_game_id, "_", 2)[,2],
        away_team     =  stringr::str_split_fixed(split_game_id, "_", 2)[,1],
        opponent      =  dplyr::case_when(
          home_team == team ~ away_team,
          away_team == team ~ home_team
        )
      ) %>%
      dplyr::select(-split_game_id, -home_team, -away_team) %>%
      dplyr::group_by(team) %>%
      dplyr::arrange(gameday, .by_group = T) %>%
      dplyr::mutate(
        lag_date    = dplyr::lag(gameday),
        rest_days   = as.numeric(round(difftime(gameday, lag_date), 0))
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(-lag_date, -gameday, -home) %>%
      replace(is.na(.), 7) %>%
      dplyr::relocate(season, week, game_id, team, opponent, home_away, rest_days, score_diff, home_score, away_score) %>%
      dplyr::left_join(
        dplyr::select(record, week, game_id, team, win, win_pct, home_win_pct, away_win_pct),
        by = c("week", "game_id", "team")
      )

    # Calculate ELO ratings
    elo <-
      outcomes %>%
      dplyr::filter(home_away == "home_team") %>%
      dplyr::select(season, week, game_id, team, opponent, win, home_score, away_score) %>%
      get_nfl_elo()

    # Join ELO ratings back w/ outcomes
    outcomes <-
      outcomes %>%
      dplyr::left_join(
        dplyr::select(elo, week, game_id, team, elo),
        by = c("week", "game_id", "team")
      ) %>%
      dplyr::group_by(team) %>%
      dplyr::arrange(week, .by_group = TRUE) %>%
      dplyr::mutate(
        turnovers = dplyr::case_when(
          home_away == "home_team" ~ as.numeric(home_turnovers),
          home_away == "away_team" ~ as.numeric(away_turnovers)
        ),
        turnovers    = mean(turnovers, na.rm = T),
        score_diff   = mean(score_diff, na.rm = T)
      ) %>%
      dplyr::slice(which.max(week)) %>%
      dplyr::select(season, week, game_id, team, opponent, home_away, win, win_pct,
                    home_win_pct, away_win_pct, rest_days, score_diff, turnovers, elo)  %>%
      dplyr::mutate(dplyr::across(c(win_pct:away_win_pct), round, 4)) %>%
      dplyr::ungroup()

    # Get schedule of upcoming games
    next_game <- get_matchups(
      year        = year,
      week        = pred_week
    )

    # Upcoming home team stats
    home <-
      next_game %>%
      dplyr::left_join(
        dplyr::select(outcomes, -season, -week, -game_id, -opponent, -home_away, -win),
        by = c("home_team" = "team")
      )

    # Upcoming away team stats
    away <-
      outcomes %>%
      dplyr::filter(team %in% home$away_team) %>%
      dplyr::select(-season, -week, -game_id, -opponent, -home_away, -win) %>%
      stats::setNames(
        c("away_team",
          paste0("opp_", names(.)[names(.) != "team"])
        )
      )

    # Join Home team and away team stats leading up to upcoming game, used as input into models
    matchups <-
      home %>%
      dplyr::left_join(
        away,
        by = c("away_team")
      ) %>%
      dplyr::relocate(season, week, game_id, team = home_team, opponent = away_team)

    return(matchups)

    # If prediction week is after week 1, use current seasons data
  } else {

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

    # process page and extract weeks leading up to prediction week
    page_table <- process_page(
      page = page
    ) %>%
      dplyr::filter(week < pred_week) %>%
      stats::setNames(c("week", "day", "date", "time", "winner_tie", "x", "loser_tie", "x_2",
                        "pts", "pts_2", "yds_w", "tow", "yds_l", "tol", "home"))

    # impute NA data for testing function will operate using future weeks of data
    # tmp <-
    #   page_table %>%
    #   dplyr::mutate(across(c(pts:tol), as.numeric)) %>%
    #   dplyr::mutate(across(c(pts:tol), impute)) %>%
    #   dplyr::mutate(
    #     pts = dplyr::case_when(
    #       pts <= pts_2 ~ pts_2 + 2,
    #       TRUE ~ pts
    #       )
    #     )

    # parse data tables from Pro Football Reference
    outcomes <-
      page_table %>%
      dplyr::left_join(
        dplyr::select(get_nfl_teams(), team_name, win_team_abb = team_abb),
        by = c("winner_tie" = "team_name")
      ) %>%
      dplyr::left_join(
        dplyr::select(get_nfl_teams(), team_name, lose_team_abb = team_abb),
        by = c("loser_tie" = "team_name")
      ) %>%
      dplyr::select(week, date,
                    win_team  = win_team_abb,
                    x,
                    lose_team = lose_team_abb,
                    pts_win   = pts,
                    pts_lose  = pts_2,
                    tow, tol)  %>%
      dplyr::mutate(
        home_team = dplyr::case_when(
          x == ""  ~ win_team,
          x == "@" ~ lose_team,
          week == 22 ~ win_team
        ),
        away_team = dplyr::case_when(
          x == ""  ~ lose_team,
          x == "@" ~ win_team,
          week == 22 ~ lose_team
        ),
        pts_win  = as.numeric(pts_win),
        pts_lose = as.numeric(pts_lose),
        game_id  = dplyr::case_when(
          week < 10  ~ paste0(year, "_0", week, "_", away_team, "_",  home_team),
          week >= 10 ~ paste0(year, "_", week, "_", away_team, "_",  home_team)
        )
      ) %>%
      dplyr::select(-x) %>%
      dplyr::group_by(game_id) %>%
      dplyr::mutate(
        home_pts = dplyr::case_when(
          home_team == win_team ~ max(pts_win, pts_lose),
          home_team != win_team ~ min(pts_win, pts_lose)
        ),
        away_pts = dplyr::case_when(
          home_team == win_team ~ min(pts_win, pts_lose),
          home_team != win_team ~ max(pts_win, pts_lose)
        ),
        home_turnovers = dplyr::case_when(
          home_team == win_team ~ tow,
          home_team != win_team ~ tol
        ),
        away_turnovers = dplyr::case_when(
          home_team == win_team ~ tol,
          home_team != win_team ~ tow
        ),
        season = year
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(season, week, game_id, gameday = date, home_team, away_team,
                    home_score = home_pts, away_score = away_pts,
                    home_turnovers, away_turnovers)

    # calculate win loss percentages
    record <- get_win_pct(outcomes, verbose = FALSE)

    # Create Score differential, home or away team ID, # of rest days
    outcomes <-
      outcomes %>%
      tidyr::pivot_longer(
        cols      = c(home_team, away_team),
        names_to  = "home",
        values_to = "team"
      ) %>%
      dplyr::mutate(
        home = dplyr::case_when(
          home == "home_team" ~ 1,
          home == "away_team" ~ 0
        ),
        score_diff = dplyr::case_when(
          home == 1 ~ home_score - away_score,
          home == 0 ~ away_score - home_score
        ),
        home_away  = case_when(
          home == 1 ~ "home_team",
          home == 0 ~ "away_team"
        )
      ) %>%
      dplyr::mutate(
        split_game_id = substr(game_id, 9, 20)
      ) %>%
      dplyr::mutate(
        home_team     =  stringr::str_split_fixed(split_game_id, "_", 2)[,2],
        away_team     =  stringr::str_split_fixed(split_game_id, "_", 2)[,1],
        opponent      =  dplyr::case_when(
          home_team == team ~ away_team,
          away_team == team ~ home_team
        )
      ) %>%
      dplyr::select(-split_game_id, -home_team, -away_team) %>%
      dplyr::group_by(team) %>%
      dplyr::arrange(gameday, .by_group = T) %>%
      dplyr::mutate(
        lag_date    = lag(gameday),
        rest_days   = as.numeric(round(difftime(gameday, lag_date), 0))
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(-lag_date, -gameday, -home) %>%
      replace(is.na(.), 7) %>%
      dplyr::relocate(season, week, game_id, team, opponent, home_away,
                      rest_days, score_diff, home_score, away_score) %>%
      dplyr::left_join(
        dplyr::select(record, week, game_id, team, win, win_pct, home_win_pct, away_win_pct),
        by = c("week", "game_id", "team")
      )

    # Calculate ELO ratings
    elo <-
      outcomes %>%
      dplyr::filter(home_away == "home_team") %>%
      dplyr::select(season, week, game_id, team, opponent, win, home_score, away_score) %>%
      get_nfl_elo()

    # Join ELO ratings back w/ outcomes
    outcomes <-
      outcomes %>%
      dplyr::left_join(
        dplyr::select(elo, week, game_id, team, elo),
        by = c("week", "game_id", "team")
      ) %>%
      dplyr::group_by(team) %>%
      dplyr::arrange(week, .by_group = TRUE) %>%
      dplyr::mutate(
        turnovers = dplyr::case_when(
          home_away == "home_team" ~ as.numeric(home_turnovers),
          home_away == "away_team" ~ as.numeric(away_turnovers)
        ),
        turnovers    = mean(turnovers, na.rm = T),
        score_diff   = mean(score_diff, na.rm = T)
      ) %>%
      dplyr::slice(which.max(week)) %>%
      dplyr::select(season, week, game_id, team, opponent, home_away, win, win_pct,
                    home_win_pct, away_win_pct, rest_days, score_diff, turnovers, elo)  %>%
      dplyr::mutate(across(c(win_pct:away_win_pct), round, 4)) %>%
      dplyr::ungroup()


    # Get schedule of upcoming games
    next_game <- get_matchups(
      year        = year,
      week        = pred_week
    )

    # Upcoming home team stats
    home <-
      next_game %>%
      dplyr::left_join(
        dplyr::select(outcomes, -season, -week, -game_id, -opponent, -home_away, -win),
        by = c("home_team" = "team")
      )

    # Upcoming away team stats
    away <-
      outcomes %>%
      dplyr::filter(team %in% home$away_team) %>%
      dplyr::select(-season, -week, -game_id, -opponent, -home_away, -win) %>%
      stats::setNames(
        c("away_team",
          paste0("opp_", names(.)[names(.) != "team"])
        )
      )

    # Join Home team and away team stats leading up to upcoming game, used as input into models
    matchups <-
      home %>%
      dplyr::left_join(
        away,
        by = c("away_team")
      ) %>%
      dplyr::relocate(season, week, game_id, team = home_team, opponent = away_team)

    return(matchups)
  }

}


