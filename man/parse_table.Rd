% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_vegas.R
\name{parse_table}
\alias{parse_table}
\title{Extract date from scraped vegas-lines.com page}
\usage{
parse_table(page_table)
}
\arguments{
\item{page_table}{xml_node containing a table entry with betting information for a matchup}
}
\value{
character string representing date of odds table
}
\description{
Extract date from scraped vegas-lines.com page
}
