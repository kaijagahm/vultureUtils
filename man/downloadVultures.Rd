% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/funs.R
\name{downloadVultures}
\alias{downloadVultures}
\title{Download vulture data}
\usage{
downloadVultures(
  loginObject,
  extraSensors = F,
  removeDup = T,
  dateTimeStartUTC = NULL,
  dateTimeEndUTC = NULL,
  ...
)
}
\arguments{
\item{loginObject}{A Movebank login object, created by passing a username and password to move::movebankLogin. Passed to `login` in move::getMovebankData().}

\item{extraSensors}{Whether to include extra sensors. Defaults to FALSE. Passed to `includeExtraSensors` in move::getMovebankData().}

\item{removeDup}{Whether to remove duplicated timestamps. Defaults to TRUE. Passed to `removeDuplicatedTimestamps` in move::getMovebankData().}

\item{dateTimeStartUTC}{a POSIXct object, in UTC. Will be converted to character assuming UTC. Passed to `timestamp_start` in move::getMovebankData().}

\item{dateTimeEndUTC}{a POSIXct object, in UTC. Will be converted to character assuming UTC. Passed to `timestamp_end` in move::getMovebankData().}

\item{...}{additional arguments to be passed to move::getMovebankData().}
}
\value{
A movestack.
}
\description{
Download vulture project data from the Israel vulture study Movebank repository, with some minor specifications. Note that you must specify your movebank credentials. This is a wrapper function for move::getMovebankData() that includes the study name hard-coded in, so you don't have to type it each time. If you need to get data for a different study, just use move::getMovebankData() directly.
}
