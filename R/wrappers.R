if(getRversion() >= "2.15.1")  utils::globalVariables(".")

#' Create co-feeding edge list
#' # XXX to do:  - make *all* possible input parameters adjustable here, with reasonable defaults. - make sure documentation for all those parameters is up to date, including a clear indication of which other function they get passed to.
#'
#' Given a dataset of GPS points, a geographic mask (optional), and some roost polygons, create an edge list.
#' @param dataset The GPS dataset to be used to create the edge list.
#' @param mask The object to use to mask the data. Passed to `vultureUtils::maskData()`. Must be an sf object.
#' @param roostPolygons Roost polygons. Must be an sf object with a CRS that matches the dataset CRS.
#' @param inMaskThreshold Proportion of an individual's days tracked that must fall within the mask. Default is 0.33 (one third of days tracked). Passed to `vultureUtils::mostlyInMask()`. Must be numeric.
#' @param consecThreshold Minimal number of co-occurrences for considering a viable pair of interacting vultures (default is 2 consecutive time steps). Passed to `vultureUtils::spaceTimeGroups()`. Must be numeric.
#' @param distThreshold The maximum distance (in meters) at which vultures are considered interacting. Default is 50 for co-feeding. Passed to `vultureUtils::spaceTimeGroups()`. Must be numeric.
#' @param speedThreshUpper Upper speed threshold, in m/s. For co-feeding, default is 5 m/s. Passed to `vultureUtils::filterLocs()`. Must be numeric.
#' @param speedThreshLower Lower speed threshold, in m/s. For co-feeding, default is NULL. Passed to `vultureUtils::filterLocs()`. Must be numeric.
#' @param crs Coordinate Reference System to check for and transform to, for both the GPS data and the mask. Default is "WGS84". This value is passed to `vultureUtils::maskData()`. Must be a valid CRS or character string coercible to CRS.
#' @param longCol The name of the column in the dataset containing longitude values. Defaults to "location_long.1". Passed to `vultureUtils::maskData()`.
#' @param latCol The name of the column in the dataset containing latitude values. Defaults to "location_lat.1". Passed to `vultureUtils::maskData()`.
#' @param dateCol The name of the column in the dataset containing dates. Defaults to "dateOnly". Passed to `vultureUtils::mostlyInMask()`.
#' @return An edge list containing the following columns: `timegroup` gives the numeric index of the timegroup during which the interaction takes place. `minTimestamp` and `maxTimestamp` give the beginning and end times of that timegroup. `ID1` is the trackID of the first individual in this edge, and `ID2` is the trackID of the second individual in this edge.
#' @export
getFeedingEdges <- function(dataset, mask, roostPolygons, inMaskThreshold = 0.33, consecThreshold = 2, distThreshold = 50, speedThreshUpper = 5, speedThreshLower = NULL, crs = "WGS84", longCol = "location_long.1", latCol = "location_lat.1", dateCol = "dateOnly"){
  # Argument checks
  checkmate::assertClass(mask, "sf")
  checkmate::assertDataFrame(dataset)
  checkmate::assertClass(roostPolygons, "sf")
  checkmate::assertNumeric(inMaskThreshold, len = 1, lower = 0, upper = 1, null.ok = TRUE)
  checkmate::assertNumeric(consecThreshold, len = 1)
  checkmate::assertNumeric(distThreshold, len = 1)
  checkmate::assertNumeric(speedThreshUpper, len = 1, null.ok = TRUE)
  checkmate::assertNumeric(speedThreshLower, len = 1, null.ok = TRUE)
  checkmate::assertCharacter(longCol, len = 1)
  checkmate::assertCharacter(latCol, len = 1)
  checkmate::assertCharacter(dateCol, len = 1)
  checkmate::assertSubset(x = c(longCol, latCol, dateCol), choices = names(dataset))

  # If an inMaskThreshold is given (it usually is), then filter to only the individuals that spend at least the threshold proportion of their days within the mask. Otherwise, just pass the dataset through unfiltered.
  if(!is.null(inMaskThreshold)){
    # Select only points that fall in the mask
    inMask <- vultureUtils::maskData(dataset = dataset, mask = mask, longCol = longCol,
                                     latCol = latCol, crs = crs)

    # Remove vultures that have less than `inMaskThreshold` of their duration recorded inside the mask.
    longEnoughIndivs <- vultureUtils::mostlyInMask(dataset = dataset, maskedDataset = inMask,
                                                   thresh = inMaskThreshold, dateCol = dateCol)
    dataset <- dataset %>% # using datDF because we don't want to actually restrict it to the mask yet
      filter(trackId %in% longEnoughIndivs)
  }

  # Restrict to non-flight interactions.
  filteredData <- vultureUtils::filterLocs(df = dataset,
                                           speedThreshUpper = speedThreshUpper,
                                           speedThreshLower = speedThreshLower)

  # Mask again to remove out-of-mask points.
  cleanedInMask <- vultureUtils::maskData(dataset = filteredData, mask = mask,
                                          longCol = longCol,
                                          latCol = latCol,
                                          crs = crs)

  # Exclude any points that fall within a (buffered) roost polygon
  feedingPoints <- cleanedInMask[lengths(sf::st_intersects(cleanedInMask, roostPolygons)) == 0,]

  # Create edge list using spaceTimeGroups
  feedingEdges <- vultureUtils::spaceTimeGroups(dataset = feedingPoints, distThreshold = distThreshold,
                                                consecThreshold = consecThreshold)

  # Return the edge list
  return(feedingEdges)
}

