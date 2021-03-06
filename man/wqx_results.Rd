% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wqx.R
\name{wqx_results}
\alias{wqx_results}
\title{Convert Results to WQX Format}
\usage{
wqx_results(ch, projects, locations = NULL, start_date = "1900-01-01",
  end_date = "2099-01-01")
}
\arguments{
\item{ch}{Open connection handle to database}

\item{projects}{Vector of ProjectIDs}

\item{locations}{Vector of LocationIDs (default=NULL to get all locations)}

\item{start_date}{Start date of results to export}

\item{end_date}{End date of results to export}
}
\value{
data frame of results in WQX format
}
\description{
Extracts results from database and returns data frame in WQX format
}

