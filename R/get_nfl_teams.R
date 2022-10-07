#' @title Get NFL teams and team abbreviations
#' @description Returns a tibble with all 32 NFL teams and team abbreviations. Includes more than 32 team names due to teams changing names and/or cities.
#' @return tibble of NFL teams and team abbreviations
#' @export
get_nfl_teams <- function() {
  team_df <- data.frame(
    team_name = c("Arizona Cardinals", "Atlanta Falcons" , "Baltimore Ravens",  "Buffalo Bills",
                  "Carolina Panthers", "Chicago Bears",  "Cincinnati Bengals" ,"Cleveland Browns",
                  "Dallas Cowboys",  "Denver Broncos",  "Detroit Lions",  "Green Bay Packers",
                  "Houston Texans","Indianapolis Colts",  "Jacksonville Jaguars", "Kansas City Chiefs",
                  "Las Vegas Raiders",  "Oakland Raiders",  "Los Angeles Chargers", "San Diego Chargers", "Los Angeles Rams", "St. Louis Rams",
                  "Miami Dolphins",
                  "Minnesota Vikings",   "New England Patriots",  "New Orleans Saints", "New York Giants",
                  "New York Jets",       "Philadelphia Eagles",  "Pittsburgh Steelers",  "San Francisco 49ers",
                  "Seattle Seahawks", "Tampa Bay Buccaneers", "Tennessee Titans", "Washington Football Team", "Washington Redskins",
                  "Washington Commanders"),
    team_nick = c("Cardinals",   "Falcons",     "Ravens",      "Bills", "Panthers", "Bears", "Bengals",
                  "Browns", "Cowboys", "Broncos",  "Lions",  "Packers", "Texans", "Colts",
                  "Jaguars", "Chiefs", "Raiders", "Raiders", "Chargers", "Chargers",    "Rams",
                  "Rams", "Dolphins",   "Vikings",  "Patriots",  "Saints",   "Giants", "Jets",  "Eagles",  "Steelers",  "49ers",
                  "Seahawks", "Buccaneers",    "Titans",   "Football Team", "Redskins", "Commanders"),
    team_abb  = c("ARI", "ATL", "BAL", "BUF", "CAR", "CHI", "CIN", "CLE", "DAL", "DEN",
                  "DET", "GB", "HOU", "IND", "JAX", "KC", "LV", "LV", "LAC", "LAC",  "LA", "LA",  "MIA", "MIN", "NE", "NO",
                  "NYG", "NYJ", "PHI", "PIT", "SF", "SEA", "TB", "TEN", "WAS","WAS", "WAS")
  )

  # add joined team nicknames
  # team_df$team_join <- paste0(gsub(" ", "", team_df$team_nick), gsub(" ", "", team_df$team_nick))

  return(team_df)

}
