% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mainFunctions.R
\name{removeInvalidPeriods}
\alias{removeInvalidPeriods}
\title{Remove Invalid Periods}
\usage{
removeInvalidPeriods(dataset, periodsToRemove)
}
\arguments{
\item{dataset}{A dataset}

\item{periodsToRemove}{A data frame of vulture names and information about each vulture. IMPORTANT: read in with readxl::read_excel("/pathtowhoswho", sheet = "periods_to_remove")}
}
\value{
A dataset with invalid periods removed
}
\description{
This function takes in a dataset of vultures and removes data points which contain known invalid periods (hospital etc.) for individuals.
Note that it is important that the periodsToRemove is provided after being read in as a data.frame as it is necessary for this function to work.
}
