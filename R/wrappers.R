if(getRversion() >= "2.15.1")  utils::globalVariables(".")

#' Create co-feeding edge list
#' # XXX to do: - make this one single wrapper that can handle feeding, flying, and roosting. - make *all* possible input parameters adjustable here, with reasonable defaults. - make sure documentation for all those parameters is up to date, including a clear indication of which other function they get passed to.
#'
#' Given a dataset of GPS points, a geographic mask (optional), and some roost polygons, create an edge list.
#' @param dataset The GPS dataset to be used to create the edge list.
#' @param mask The object to use to mask the data. Passed to `vultureUtils::maskData()`. Must be an sf object.
#' @param roostPolygons Roost polygons. Must be an sf object with a CRS that matches the dataset CRS.
#' @param inMaskThreshold Proportion of an individual's days tracked that must fall within the mask. Default is 0.33 (one third of days tracked). Passed to `vultureUtils::mostlyInMask()`. Must be numeric.
#' @param consecThreshold Minimal number of co-occurrences for considering a viable pair of interacting vultures (default is 2 consecutive time steps). Passed to `vultureUtils::spaceTimeGroups()`. Must be numeric.
#' @param distThreshold The maximum distance (in meters) at which vultures are considered interacting. Default is 50 for co-feeding. Passed to `vultureUtils::spaceTimeGroups()`. Must be numeric.
#' @return An edge list containing the following columns: `timegroup` gives the numeric index of the timegroup during which the interaction takes place. `minTimestamp` and `maxTimestamp` give the beginning and end times of that timegroup. `ID1` is the trackID of the first individual in this edge, and `ID2` is the trackID of the second individual in this edge.
#' @export
makeNetwork_coFeeding <- function(dataset, mask, roostPolygons, inMaskThreshold = 0.33, consecThreshold = 2, distThreshold = 50, speedThreshUpper = 5, speedThreshLower = NULL){
  # Argument checks

  # Select only points that fall in the mask
  inMask <- vultureUtils::maskData(dataset = dataset, mask = mask, longCol = "location_long.1",
                                   latCol = "location_lat.1", crs = "WGS84")

  # Remove vultures that have less than `inMaskThreshold` of their duration recorded inside the mask.
  longEnoughIndivs <- vultureUtils::mostlyInMask(dataset = dataset, maskedDataset = inMask,
                                                 thresh = inMaskThreshold, dateCol = "dateOnly")
  dataset <- dataset %>% # using datDF because we don't want to actually restrict it to the mask yet
    filter(trackId %in% longEnoughIndivs)

  # Filter: restrict to non-flight interactions
  filteredData <- vultureUtils::filterLocs(df = dataset, speedThreshUpper = speedThreshUpper)

  # Now mask again to remove the out-of-mask points.
  cleanedInMask <- vultureUtils::maskData(dataset = filteredData, mask = mask,
                                          longCol = "location_long.1",
                                          latCol = "location_lat.1",
                                          crs = "WGS84")

  # Exclude any points that fall within a (buffered) roost polygon
  feedingPoints <- cleanedInMask[lengths(sf::st_intersects(cleanedInMask, roostPolygons)) == 0,]

  # Create edge list using spaceTimeGroups
  feedingEdges <- vultureUtils::spaceTimeGroups(dataset = feedingPoints, distThreshold = distThreshold,
                                                consecThreshold = consecThreshold)

  # Return the edge list
  return(feedingEdges)
}

