% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mainFunctions.R
\name{getFeedingEdges}
\alias{getFeedingEdges}
\title{Create co-feeding edge list}
\usage{
getFeedingEdges(
  dataset,
  roostPolygons = NULL,
  roostBuffer = 50,
  consecThreshold = 2,
  distThreshold = 50,
  speedThreshUpper = 5,
  speedThreshLower = NULL,
  timeThreshold = "10 minutes",
  idCol = "Nili_id",
  quiet = T,
  includeAllVertices = F,
  daytimeOnly = T,
  return = "edges",
  getLocs = FALSE
)
}
\arguments{
\item{dataset}{The cleaned GPS dataset to be used to create the edge list. This should be the output from `vultureUtils::cleanData()`.}

\item{roostPolygons}{Roost polygons. Must be an sf object with a CRS that matches the dataset CRS. This is used to filter out points with ground speed less than speedThreshLower that might be occurring at roost sites instead of at a carcass site. If NULL, no filtering will be done by polygon intersections.}

\item{roostBuffer}{Number of meters to buffer the roost polygons by before intersecting them. Default is 50 m.}

\item{consecThreshold}{Minimal number of co-occurrences for considering a viable pair of interacting vultures (default is 2 consecutive time steps). Passed to `vultureUtils::spaceTimeGroups()`. Must be numeric.}

\item{distThreshold}{The maximum distance (in meters) at which vultures are considered interacting. Default is 50 for co-feeding. Passed to `vultureUtils::spaceTimeGroups()`. Must be numeric.}

\item{speedThreshUpper}{Upper speed threshold, in m/s. For co-feeding, default is 5 m/s. Passed to `vultureUtils::filterLocs()`. Must be numeric.}

\item{speedThreshLower}{Lower speed threshold, in m/s. For co-feeding, default is NULL. Passed to `vultureUtils::filterLocs()`. Must be numeric.}

\item{timeThreshold}{timeThreshold Passed to spatsoc::group_times. Threshold for grouping times. e.g.: '2 hours', '10 minutes', etc. if not provided, times will be matched exactly. Note that provided threshold must be in the expected format: '## unit'. Default is "10 minutes"}

\item{idCol}{Passed to spaceTimeGroups: the name of the column containing individual vulture ID's. Default is Nili_id.}

\item{quiet}{Whether to silence the warning messages about grouping individuals with themselves inside the time threshold. Default is T. This occurs because if we set our time threshold to e.g. 10 minutes (the default), there are some GPS points that occur closer together than 10 minutes apart (e.g. if we experimentally set the tags to take points every 5 minutes). As a result, we will "group" together the same individual with itself, resulting in some self edges. I currently have a step in the code that simply removes these self edges, so there should be no problem here. But if you set `quiet = F`, you will at least be warned with the message `"Warning: found duplicate id in a timegroup and/or splitBy - does your group_times threshold match the fix rate?"` when this is occurring.}

\item{includeAllVertices}{logical. Whether to include another list item in the output that's a vector of all individuals in the dataset. For use in creating sparse graphs. Default is F.}

\item{daytimeOnly}{T/F, whether to restrict interactions to daytime only. Default is T.}

\item{return}{One of "edges" (default, returns an edgelist, would need to be used in conjunction with includeAllVertices = T in order to include all individuals, since otherwise they wouldn't be included in the edgelist. Also includes timegroup information, which SRI cannot do. One row in this data frame represents a single edge in a single timegroup.); "sri" (returns a data frame with three columns, ID1, ID2, and sri. Includes pairs whose SRI values are 0, which means it includes all individuals and renders includeAllVertices obsolete.); and "both" (returns a list with two components: "edges" and "sri" as described above.)}

\item{getLocs}{Whether or not to return locations where the interactions happened (for edge list only, doesn't make sense for SRI). Default is FALSE. If getLocs is set to TRUE when return = "sri", a message will tell the user that no locations can be returned for SRI.}
}
\value{
An edge list containing the following columns: `timegroup` gives the numeric index of the timegroup during which the interaction takes place. `minTimestamp` and `maxTimestamp` give the beginning and end times of that timegroup. `ID1` is the id of the first individual in this edge, and `ID2` is the id of the second individual in this edge.
}
\description{
Wrapper of getEdges() with defaults for co-feeding edges. Can still be customized!
}
