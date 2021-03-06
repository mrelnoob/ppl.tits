% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/01_01_prepare_data.R
\name{aggreg_by_nest}
\alias{aggreg_by_nest}
\title{Aggregate tits data by nestbox and year}
\usage{
aggreg_by_nest()
}
\value{
A tibble with 16 variables.
}
\description{
The \code{aggreg_by_nest} function aggregates the raw tits data to create a table
summarizing tits' breeding success for each nestbox each year where breeding occurred. As such,
it only keeps information on nestlings/juveniles and disregards the lines about adult tits.
}
\examples{
\dontrun{
nestbox_repro <- aggreg_by_nest()
}
}
