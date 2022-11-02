if(getRversion() >= "2.15.1")  utils::globalVariables(".")

#' Clean data
#'
#' This function takes in a raw dataset downloaded from movebank, masks it, and performs basic data cleaning. The output from this function feeds directly into `vultureUtils::spaceTimeGroups()`. Steps: 1. Using the `mask` object, get a list of the individuals in `dataset` that spend at least `inMaskThreshold` proportion of their time inside the mask area. 2. Restrict `dataset` to only these individuals. 3. Re-apply the mask to restrict the remaining points to those that fall within `mask`.
#' @param dataset The GPS dataset to be used to create the edge list.
#' @param removeVars Whether or not to remove unnecessary variables from movebank download. Default is T.
#' @param mask The object to use to mask the data. Passed to `vultureUtils::maskData()`. Must be an sf object.
#' @param inMaskThreshold Proportion of an individual's days tracked that must fall within the mask. Default is 0.33 (one third of days tracked). Passed to `vultureUtils::mostlyInMask()`. Must be numeric.
#' @param crs Coordinate Reference System to check for and transform to, for both the GPS data and the mask. Default is "WGS84". This value is passed to `vultureUtils::maskData()`. Must be a valid CRS or character string coercible to CRS.
#' @param longCol The name of the column in the dataset containing longitude values. Defaults to "location_long.1". Passed to `vultureUtils::maskData()`.
#' @param latCol The name of the column in the dataset containing latitude values. Defaults to "location_lat.1". Passed to `vultureUtils::maskData()`.
#' @param dateCol The name of the column in the dataset containing dates. Defaults to "dateOnly". Passed to `vultureUtils::mostlyInMask()`.
#' @param removeVars Whether or not to remove unnecessary variables. Default is T.
#' @param reMask Whether or not to re-mask after removing individuals that spend less than `inMaskThreshold` in the mask area. Default is T.
#' @param quiet Whether to silence the message that happens when doing spatial joins. Default is T.
#' @param ... additional arguments to be passed to any of several functions: `vultureUtils::removeUnnecessaryVars()` (`addlVars`, `keepVars`);
#' @return An edge list containing the following columns: `timegroup` gives the numeric index of the timegroup during which the interaction takes place. `minTimestamp` and `maxTimestamp` give the beginning and end times of that timegroup. `ID1` is the trackID of the first individual in this edge, and `ID2` is the trackID of the second individual in this edge.
#' @export
cleanData <- function(dataset, mask, inMaskThreshold = 0.33, crs = "WGS84", longCol = "location_long.1", latCol = "location_lat.1", dateCol = "dateOnly", removeVars = T, reMask = T, quiet = T, ...){
  # Argument checks
  checkmate::assertClass(mask, "sf")
  checkmate::assertDataFrame(dataset)
  checkmate::assertNumeric(inMaskThreshold, len = 1, lower = 0, upper = 1, null.ok = TRUE)
  checkmate::assertCharacter(longCol, len = 1)
  checkmate::assertCharacter(latCol, len = 1)
  checkmate::assertCharacter(dateCol, len = 1)
  checkmate::assertSubset(x = c(longCol, latCol, dateCol), choices = names(dataset))

  # Basic data quality filters ----------------------------------------------
  # Remove outlier points based on zeroes (Marta's code)
  df <- df %>%
    mutate(outlier = ifelse(.data$external_temperature == 0 & .data$barometric_height == 0 & .data$ground_speed == 0, 1, 0)) %>%
    filter(is.na(.data$outlier) | .data$outlier == 0) %>%
    dplyr::select(-.data$outlier)

  # filter out bad gps data
  df <- df %>%
    dplyr::filter(.data$gps_time_to_fix <= 89)

  # filter out bad heading data
  df <- df %>%
    dplyr::filter(.data$heading < 360 & .data$heading > 0) # only reasonable headings, between 0 and 360.

  # only take locs that have at least 3 satellites
  df <- df %>%
    dplyr::filter(.data$gps_satellite_count >= 3) # must have at least 3 satellites in order to triangulate.

  # Remove unnecessary variables, if desired. ---------------------------
  if(removeVars == T){
    dataset <- vultureUtils::removeUnnecessaryVars(dataset)
  }

  # Filter to in-mask threshold -----------------------------------------
  # If an inMaskThreshold is given (it usually is), then filter to only the individuals that spend at least the threshold proportion of their days within the mask. Otherwise, just pass the dataset through unfiltered.
  if(!is.null(inMaskThreshold)){
    # Select only points that fall in the mask
    if(quiet == TRUE){
      inMask <- suppressMessages(vultureUtils::maskData(dataset = dataset, mask = mask, longCol = longCol,
                                       latCol = latCol, crs = crs))
    }else{
      inMask <- vultureUtils::maskData(dataset = dataset, mask = mask, longCol = longCol,
                                       latCol = latCol, crs = crs)
    }

    # Remove vultures that have less than `inMaskThreshold` of their duration recorded inside the mask.
    if(quiet == TRUE){
      longEnoughIndivs <- suppressMessages(vultureUtils::mostlyInMask(dataset = dataset,
                                                     maskedDataset = inMask,
                                                     thresh = inMaskThreshold,
                                                     dateCol = dateCol))
    }else{
      longEnoughIndivs <- vultureUtils::mostlyInMask(dataset = dataset,
                                                     maskedDataset = inMask,
                                                     thresh = inMaskThreshold,
                                                     dateCol = dateCol)
    }

    dataset <- dataset %>% # using datDF because we don't want to actually restrict it to the mask yet
      dplyr::filter(.data$trackId %in% longEnoughIndivs)
  }

  # Mask again to remove out-of-mask points, if desired
  if(reMask == T){
    if(quiet == TRUE){
      cleanedInMask <- suppressMessages(vultureUtils::maskData(dataset = dataset, mask = mask,
                                              longCol = longCol,
                                              latCol = latCol,
                                              crs = crs))
    }else{
      cleanedInMask <- vultureUtils::maskData(dataset = dataset, mask = mask,
                                              longCol = longCol,
                                              latCol = latCol,
                                              crs = crs)
    }

    return(cleanedInMask)
  }else{
    return(dataset)
  }
}

#' Create an edge list (flexible; must insert parameters.)
#'
#' Given a dataset of GPS points, a geographic mask, and some roost polygons, create an edge list.
#' @param dataset The cleaned GPS dataset to be used to create the edge list. This should be the output from `vultureUtils::cleanData()`.
#' @param roostPolygons Roost polygons. Must be an sf object with a CRS that matches the dataset CRS.
#' @param roostBuffer Number of meters to buffer the roost polygons by before intersecting them.
#' @param consecThreshold Minimal number of co-occurrences for considering a viable pair of interacting vultures. Passed to `vultureUtils::spaceTimeGroups()`. Must be numeric.
#' @param distThreshold The maximum distance (in meters) at which vultures are considered interacting. Passed to `vultureUtils::spaceTimeGroups()`. Must be numeric.
#' @param speedThreshUpper Upper speed threshold, in m/s.
#' @param speedThreshLower Lower speed threshold, in m/s.
#' @param timeThreshold timeThreshold Passed to spatsoc::group_times. Threshold for grouping times. e.g.: '2 hours', '10 minutes', etc. if not provided, times will be matched exactly. Note that provided threshold must be in the expected format: '## unit'. Default is "10 minutes"
#' @param quiet Whether to silence the warning messages about grouping individuals with themselves inside the time threshold. Default is T. This occurs because if we set our time threshold to e.g. 10 minutes (the default), there are some GPS points that occur closer together than 10 minutes apart (e.g. if we experimentally set the tags to take points every 5 minutes). As a result, we will "group" together the same individual with itself, resulting in some self edges. I currently have a step in the code that simply removes these self edges, so there should be no problem here. But if you set `quiet = F`, you will at least be warned with the message `"Warning: found duplicate id in a timegroup and/or splitBy - does your group_times threshold match the fix rate?"` when this is occurring.
#' @return An edge list containing the following columns: `timegroup` gives the numeric index of the timegroup during which the interaction takes place. `minTimestamp` and `maxTimestamp` give the beginning and end times of that timegroup. `ID1` is the trackID of the first individual in this edge, and `ID2` is the trackID of the second individual in this edge.
#' @export
getEdges <- function(dataset, roostPolygons, roostBuffer, consecThreshold, distThreshold, speedThreshUpper, speedThreshLower, timeThreshold = "10 minutes", quiet = T){
  # Argument checks
  checkmate::assertDataFrame(dataset)
  checkmate::assertClass(roostPolygons, "sf")
  checkmate::assertNumeric(roostBuffer, len = 1)
  checkmate::assertNumeric(consecThreshold, len = 1)
  checkmate::assertNumeric(distThreshold, len = 1)
  checkmate::assertNumeric(speedThreshUpper, len = 1, null.ok = TRUE)
  checkmate::assertNumeric(speedThreshLower, len = 1, null.ok = TRUE)
  checkmate::assertCharacter(timeThreshold, len = 1)

  # Restrict to non-flight interactions.
  filteredData <- vultureUtils::filterLocs(df = dataset,
                                           speedThreshUpper = speedThreshUpper,
                                           speedThreshLower = speedThreshLower)

  # Buffer the roost polygons
  roostPolygons <- convertAndBuffer(roostPolygons, dist = roostBuffer)

  # Exclude any points that fall within a (buffered) roost polygon
  points <- filteredData[lengths(sf::st_intersects(filteredData, roostPolygons)) == 0,]

  # If there are no rows left after filtering, return an empty data frame with the appropriate format.
  if(nrow(points) == 0){
    dummy <- data.frame(timegroup = as.integer(),
                        ID1 = as.character(),
                        ID2 = as.character(),
                        distance = as.numeric(),
                        minTimestamp = as.POSIXct(character()),
                        maxTimestamp = as.POSIXct(character()))
    warning("After filtering, the dataset had 0 rows. Returning an empty edge list.")
    return(dummy)
  }else{
    if(quiet == T){
      edges <- suppressWarnings(vultureUtils::spaceTimeGroups(dataset = points,
                                                              distThreshold = distThreshold,
                                                              consecThreshold = consecThreshold,
                                                              timeThreshold = timeThreshold))
    }else{
      edges <- vultureUtils::spaceTimeGroups(dataset = points,
                                             distThreshold = distThreshold,
                                             consecThreshold = consecThreshold,
                                             timeThreshold = timeThreshold)
    }

    # Return the edge list
    return(edges)
  }
}

#' Create co-feeding edge list
#'
#' Given a dataset of GPS points, a geographic mask, and some roost polygons, create an edge list.
#' @param dataset The cleaned GPS dataset to be used to create the edge list. This should be the output from `vultureUtils::cleanData()`.
#' @param roostPolygons Roost polygons. Must be an sf object with a CRS that matches the dataset CRS.
#' @param roostBuffer Number of meters to buffer the roost polygons by before intersecting them. Default is 50 m.
#' @param consecThreshold Minimal number of co-occurrences for considering a viable pair of interacting vultures (default is 2 consecutive time steps). Passed to `vultureUtils::spaceTimeGroups()`. Must be numeric.
#' @param distThreshold The maximum distance (in meters) at which vultures are considered interacting. Default is 50 for co-feeding. Passed to `vultureUtils::spaceTimeGroups()`. Must be numeric.
#' @param speedThreshUpper Upper speed threshold, in m/s. For co-feeding, default is 5 m/s. Passed to `vultureUtils::filterLocs()`. Must be numeric.
#' @param speedThreshLower Lower speed threshold, in m/s. For co-feeding, default is NULL. Passed to `vultureUtils::filterLocs()`. Must be numeric.
#' @param timeThreshold timeThreshold Passed to spatsoc::group_times. Threshold for grouping times. e.g.: '2 hours', '10 minutes', etc. if not provided, times will be matched exactly. Note that provided threshold must be in the expected format: '## unit'. Default is "10 minutes"
#' @param quiet Whether to silence the warning messages about grouping individuals with themselves inside the time threshold. Default is T. This occurs because if we set our time threshold to e.g. 10 minutes (the default), there are some GPS points that occur closer together than 10 minutes apart (e.g. if we experimentally set the tags to take points every 5 minutes). As a result, we will "group" together the same individual with itself, resulting in some self edges. I currently have a step in the code that simply removes these self edges, so there should be no problem here. But if you set `quiet = F`, you will at least be warned with the message `"Warning: found duplicate id in a timegroup and/or splitBy - does your group_times threshold match the fix rate?"` when this is occurring.
#' @return An edge list containing the following columns: `timegroup` gives the numeric index of the timegroup during which the interaction takes place. `minTimestamp` and `maxTimestamp` give the beginning and end times of that timegroup. `ID1` is the trackID of the first individual in this edge, and `ID2` is the trackID of the second individual in this edge.
#' @export
getFeedingEdges <- function(dataset, roostPolygons, roostBuffer = 50, consecThreshold = 2, distThreshold = 50, speedThreshUpper = 5, speedThreshLower = NULL, timeThreshold = "10 minutes", quiet = T){
  # Argument checks
  checkmate::assertDataFrame(dataset)
  checkmate::assertClass(roostPolygons, "sf")
  checkmate::assertNumeric(roostBuffer, len = 1)
  checkmate::assertNumeric(consecThreshold, len = 1)
  checkmate::assertNumeric(distThreshold, len = 1)
  checkmate::assertNumeric(speedThreshUpper, len = 1, null.ok = TRUE)
  checkmate::assertNumeric(speedThreshLower, len = 1, null.ok = TRUE)

  # Restrict to non-flight interactions.
  filteredData <- vultureUtils::filterLocs(df = dataset,
                                           speedThreshUpper = speedThreshUpper,
                                           speedThreshLower = speedThreshLower)

  # Buffer the roost polygons
  roostPolygons <- convertAndBuffer(roostPolygons, dist = roostBuffer)

  # Exclude any points that fall within a (buffered) roost polygon
  feedingPoints <- filteredData[lengths(sf::st_intersects(filteredData, roostPolygons)) == 0,]

  # If there are no rows left after filtering, return an empty data frame with the appropriate format.
  if(nrow(feedingPoints) == 0){
    dummy <- data.frame(timegroup = as.integer(),
                        ID1 = as.character(),
                        ID2 = as.character(),
                        distance = as.numeric(),
                        minTimestamp = as.POSIXct(character()),
                        maxTimestamp = as.POSIXct(character()))
    warning("After filtering, the dataset had 0 rows. Returning an empty edge list.")
    return(dummy)
  }else{
    if(quiet == T){
      feedingEdges <- suppressWarnings(vultureUtils::spaceTimeGroups(dataset = feedingPoints, distThreshold = distThreshold,
                                                                     consecThreshold = consecThreshold,
                                                                     timeThreshold = timeThreshold))
    }else{
      feedingEdges <- vultureUtils::spaceTimeGroups(dataset = feedingPoints, distThreshold = distThreshold,
                                                    consecThreshold = consecThreshold,
                                                    timeThreshold = timeThreshold)
    }

    # Return the edge list
    return(feedingEdges)
  }
}

#' Create co-flight edge list
#'
#' Given a dataset of GPS points, a geographic mask, and some roost polygons, create an edge list.
#' @param dataset The cleaned GPS dataset to be used to create the edge list. This should be the output from `vultureUtils::cleanData()`.
#' @param roostPolygons Roost polygons. Must be an sf object with a CRS that matches the dataset CRS.
#' @param roostBuffer Number of meters to buffer the roost polygons by before intersecting them. Default is 50 m.
#' @param consecThreshold Minimal number of co-occurrences for considering a viable pair of interacting vultures (default is 2 consecutive time steps). Passed to `vultureUtils::spaceTimeGroups()`. Must be numeric.
#' @param distThreshold The maximum distance (in meters) at which vultures are considered interacting. Default is 1000 m for co-flight Passed to `vultureUtils::spaceTimeGroups()`. Must be numeric.
#' @param speedThreshUpper Upper speed threshold, in m/s. For co-flight, default is NULL. Passed to `vultureUtils::filterLocs()`. Must be numeric.
#' @param speedThreshLower Lower speed threshold, in m/s. For co-flight, default is 5 m/s. Passed to `vultureUtils::filterLocs()`. Must be numeric.
#' @param timeThreshold timeThreshold Passed to spatsoc::group_times. Threshold for grouping times. e.g.: '2 hours', '10 minutes', etc. if not provided, times will be matched exactly. Note that provided threshold must be in the expected format: '## unit'. Default is "10 minutes"
#' @param quiet Whether to silence the warning messages about grouping individuals with themselves inside the time threshold. Default is T. This occurs because if we set our time threshold to e.g. 10 minutes (the default), there are some GPS points that occur closer together than 10 minutes apart (e.g. if we experimentally set the tags to take points every 5 minutes). As a result, we will "group" together the same individual with itself, resulting in some self edges. I currently have a step in the code that simply removes these self edges, so there should be no problem here. But if you set `quiet = F`, you will at least be warned with the message `"Warning: found duplicate id in a timegroup and/or splitBy - does your group_times threshold match the fix rate?"` when this is occurring.
#' @return An edge list containing the following columns: `timegroup` gives the numeric index of the timegroup during which the interaction takes place. `minTimestamp` and `maxTimestamp` give the beginning and end times of that timegroup. `ID1` is the trackID of the first individual in this edge, and `ID2` is the trackID of the second individual in this edge.
#' @export
getFlightEdges <- function(dataset, roostPolygons, roostBuffer = 50, consecThreshold = 2, distThreshold = 1000, speedThreshUpper = NULL, speedThreshLower = 5, timeThreshold = "10 minutes", quiet = T){
  # Argument checks
  checkmate::assertDataFrame(dataset)
  checkmate::assertClass(roostPolygons, "sf")
  checkmate::assertNumeric(roostBuffer, len = 1)
  checkmate::assertNumeric(consecThreshold, len = 1)
  checkmate::assertNumeric(distThreshold, len = 1)
  checkmate::assertNumeric(speedThreshUpper, len = 1, null.ok = TRUE)
  checkmate::assertNumeric(speedThreshLower, len = 1, null.ok = TRUE)

  # Restrict to flight interactions.
  filteredData <- vultureUtils::filterLocs(df = dataset,
                                           speedThreshUpper = speedThreshUpper,
                                           speedThreshLower = speedThreshLower)

  # Buffer the roost polygons
  roostPolygons <- convertAndBuffer(roostPolygons, dist = roostBuffer)

  # Exclude any points that fall within a (buffered) roost polygon
  flightPoints <- filteredData[lengths(sf::st_intersects(filteredData, roostPolygons)) == 0,]

  # If there are no rows left after filtering, return an empty data frame with the appropriate format.
  if(nrow(flightPoints) == 0){
    dummy <- data.frame(timegroup = as.integer(),
                        ID1 = as.character(),
                        ID2 = as.character(),
                        distance = as.numeric(),
                        minTimestamp = as.POSIXct(character()),
                        maxTimestamp = as.POSIXct(character()))
    warning("After filtering, the dataset had 0 rows. Returning an empty edge list.")
    return(dummy)
  }else{
    # Create edge list using spaceTimeGroups
    if(quiet == T){
      flightEdges <- suppressWarnings(vultureUtils::spaceTimeGroups(dataset = flightPoints, distThreshold = distThreshold,
                                                                    consecThreshold = consecThreshold,
                                                                    timeThreshold = timeThreshold))
    }else{
      flightEdges <- vultureUtils::spaceTimeGroups(dataset = flightPoints, distThreshold = distThreshold,
                                                   consecThreshold = consecThreshold,
                                                   timeThreshold = timeThreshold)
    }

    # Return the edge list
    return(flightEdges)
  }
}

#' #' Create co-roosting edge list XXX START HERE
#' #'
#' #' Given a dataset of GPS points, a geographic mask, and some roost polygons, create an edge list.
#' #' @param dataset The cleaned GPS dataset to be used to create the edge list. This should be the output from `vultureUtils::cleanData()`.
#' #' @param roostPolygons Roost polygons. Must be an sf object with a CRS that matches the dataset CRS.
#' #' @param roostBuffer Number of meters to buffer the roost polygons by before intersecting them. Default is 50 m.
#' #' @param consecThreshold Minimal number of co-occurrences for considering a viable pair of interacting vultures (default is 2 consecutive time steps). Passed to `vultureUtils::spaceTimeGroups()`. Must be numeric.
#' #' @return An edge list containing the following columns: `timegroup` gives the numeric index of the timegroup during which the interaction takes place. `minTimestamp` and `maxTimestamp` give the beginning and end times of that timegroup. `ID1` is the trackID of the first individual in this edge, and `ID2` is the trackID of the second individual in this edge.
#' #' @export
#' getRoostingEdges <- function(dataset, roostPolygons, roostBuffer = 50,
#'                              consecThreshold = 2){
#'   # Argument checks
#'   checkmate::assertDataFrame(dataset)
#'   checkmate::assertClass(roostPolygons, "sf")
#'   checkmate::assertNumeric(roostBuffer, len = 1)
#'   checkmate::assertNumeric(consecThreshold, len = 1)
#'   checkmate::assertNumeric(distThreshold, len = 1)
#'   checkmate::assertNumeric(speedThreshUpper, len = 1, null.ok = TRUE)
#'   checkmate::assertNumeric(speedThreshLower, len = 1, null.ok = TRUE)
#'
#'
#'
#'   # Return the edge list
#'   return(flightEdges)
#' }

# b. Nocturnal ground interactions (Co-Roosting): Speed<=5m/s;
# Distance threshold - vultures that slept in the same roost on a given date;
# Time threshold - the vultures roosted together on the same date; Roost buffer = 50m.
#
# i. Assigned last locations of each vulture on each night to a roost polygon
# ii. If locations unassigned, found the first location for the morning after (location of where a vulture woke up the next day)
# ---> assigned to a roost polygon
# iii. If still unassigned, found the average location of the last location at night and first location the following morning and then
# ----> assigned to a roost polygon
# iv. If owing to the 50m buffer around roost polygons, a location is assigned to >1 roost, then remove duplicates and
# -----> assign the location to the closer roost polygon
#
