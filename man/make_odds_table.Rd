% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_vegas.R
\name{make_odds_table}
\alias{make_odds_table}
\title{Clean single betting table from scraped vegas-odds.com page}
\usage{
make_odds_table(page_table = NULL, date_table = NULL, all_weeks = FALSE)
}
\arguments{
\item{page_table}{xml_node containing a table entry with betting information for a matchup}

\item{date_table}{a dataframe detailing the starting and ending dates of each week of an NFL season, the output from get_week_dates() function. Default is NULL and current season date table will be pulled}

\item{all_weeks}{logical, whether to return all weeks on vegas-odds.com page or only the current week. Default is FALSE, returns only the current week of odds.}
}
\value{
tibble with a tidied table with odds data
}
\description{
Clean single betting table from scraped vegas-odds.com page
}
