# Main functions (for use by the user)
if(getRversion() >= "2.15.1")  utils::globalVariables(".") # this supposedly helps deal with some of the data masking issues with dplyr/tidyverse syntax.

#' Download vulture data
#'
#' Download vulture project data from the Israel vulture study Movebank repository, with some minor specifications. Note that you must specify your movebank credentials. This is a wrapper function for move::getMovebankData() that includes the study name hard-coded in, so you don't have to type it each time. If you need to get data for a different study, just use move::getMovebankData() directly.
#' @param loginObject A Movebank login object, created by passing a username and password to move::movebankLogin. Passed to `login` in move::getMovebankData().
#' @param extraSensors Whether to include extra sensors. Defaults to FALSE. Passed to `includeExtraSensors` in move::getMovebankData().
#' @param removeDup Whether to remove duplicated timestamps. Defaults to TRUE. Passed to `removeDuplicatedTimestamps` in move::getMovebankData().
#' @param dateTimeStartUTC a POSIXct object, in UTC. Will be converted to character assuming UTC. Passed to `timestamp_start` in move::getMovebankData().
#' @param dateTimeEndUTC a POSIXct object, in UTC. Will be converted to character assuming UTC. Passed to `timestamp_end` in move::getMovebankData().
#' @param addDateOnly Whether to add a dateOnly column, extracted from the timestamp. Default is T.
#' @param dfConvert Whether to convert the returned data to a data frame or not (default is T).
#' @param quiet Whether to silence the warning messages created when duplicate records are dropped. Default is F (warning messages will print).
#' @param ... additional arguments to be passed to move::getMovebankData().
#' @return A movestack.
#' @export
downloadVultures <- function(loginObject, extraSensors = F, removeDup = T,
                             dateTimeStartUTC = NULL, dateTimeEndUTC = NULL,
                             addDateOnly = T,
                             dfConvert = T,
                             quiet = F,
                             ...){
  # argument checks
  checkmate::assertClass(loginObject, "MovebankLogin")
  checkmate::assertLogical(extraSensors, len = 1)
  checkmate::assertLogical(removeDup, len = 1)
  # Do the POSIXct converion if datetimes are character
  if(is.character(dateTimeStartUTC)){
    dateTimeStartUTC <- as.POSIXct(dateTimeStartUTC)
  }
  if(is.character(dateTimeEndUTC)){
    dateTimeEndUTC <- as.POSIXct(dateTimeEndUTC)
  }
  checkmate::assertPOSIXct(dateTimeStartUTC, null.ok = TRUE)
  checkmate::assertPOSIXct(dateTimeEndUTC, null.ok = TRUE)
  checkmate::assertLogical(addDateOnly, len = 1)
  checkmate::assertLogical(quiet, len = 1)

  if(quiet == T){
    dat <- suppressWarnings(suppressMessages(move::getMovebankData(study = "Ornitela_Vultures_Gyps_fulvus_TAU_UCLA_Israel",
                                                                   login = loginObject,
                                                                   includeExtraSensors = FALSE,
                                                                   deploymentAsIndividuals = FALSE,
                                                                   removeDuplicatedTimestamps = TRUE,
                                                                   timestamp_start = dateTimeStartUTC,
                                                                   timestamp_end = dateTimeEndUTC,
                                                                   ...)))
  }else{
    dat <- move::getMovebankData(study = "Ornitela_Vultures_Gyps_fulvus_TAU_UCLA_Israel",
                                 login = loginObject,
                                 includeExtraSensors = FALSE,
                                 deploymentAsIndividuals = FALSE,
                                 removeDuplicatedTimestamps = TRUE,
                                 timestamp_start = dateTimeStartUTC,
                                 timestamp_end = dateTimeEndUTC,
                                 ...)
  }

  if(addDateOnly == T){
    dat$dateOnly <- as.Date(as.character(dat$timestamp))
  }

  if(dfConvert == TRUE){
    dat <- methods::as(dat, "data.frame")
    # NOTE: DO NOT USE as.data.frame() HERE! The object `dat` being converted is a moveStack object (returned from the `move` R package). This is a special S4 class with defined slots (more info here: https://terpconnect.umd.edu/~egurarie/research/NWT/Step01_LoadingMovebankData.html). One of the slots is `trackId`. For reasons I don't fully understand, using `as.data.frame()` on a moveStack does not retain all the slot names--it will return a dataset that lacks the `trackId` column. But using `methods::as(dat, "data.frame")` converts tht `trackId` slot into a column in the data frame, giving us a `trackId` column that is a factor. We NEED the `trackId` column in order to proceed with other functions further down the pipeline, so it's super important not to use as.data.frame() here.
  }

  # If there was only one individual requested, the data frame won't have a `trackId` column because of how the movestack-to-data-frame conversion works. Add one.
  if(!("trackId" %in% names(dat))){
    dat$trackId <- dat$local_identifier
  }

  return(dat)
}




#' Clean data
#'
#' This function takes in a raw dataset downloaded from movebank, masks it, and performs basic data cleaning. The output from this function feeds directly into `vultureUtils::spaceTimeGroups()`.
#' @param dataset The GPS dataset to be used to create the edge list. Must contain columns specified by `longCol`, `latCol`, and `dateCol` args. Must also contain columns "gps_time_to_fix", "heading", "gps_satellite_count", and "ground_speed" because data cleaning is based on this info.
#' @param gpsMaxTime Max time for gps to communicate with satellites. If less than 0, do not filter based on max time. Default is -1.
#' @param precise Higher quality filter (satellites > 4 and hdop < 5). Default is F.
#' @param removeVars Whether or not to remove unnecessary variables from movebank download. Default is T.
#' @param longCol The name of the column in the dataset containing longitude values. Defaults to "location_long.1".
#' @param latCol The name of the column in the dataset containing latitude values. Defaults to "location_lat.1".
#' @param idCol The name of the column in the dataset containing vulture ID's. Defaults to "Nili_id" (assuming you have joined the Nili_ids from the who's who table).
#' @param ... additional arguments to be passed to any of several functions: `vultureUtils::removeUnnecessaryVars()` (`addlVars`, `keepVars`);
#' @param report Default TRUE. Whether to print a report of how many rows/individuals were removed in each of the data cleaning steps.
#' @return An edge list containing the following columns: `timegroup` gives the numeric index of the timegroup during which the interaction takes place. `minTimestamp` and `maxTimestamp` give the beginning and end times of that timegroup. `ID1` is the id of the first individual in this edge, and `ID2` is the id of the second individual in this edge.
#' @export
cleanData <- function(dataset, gpsMaxTime = -1, precise = F, longCol = "location_long.1", latCol = "location_lat.1", idCol = "Nili_id", removeVars = T, report = T, ...){
  # Argument checks
  checkmate::assertDataFrame(dataset)
  checkmate::assertCharacter(longCol, len = 1)
  checkmate::assertCharacter(latCol, len = 1)
  checkmate::assertChoice("gps_time_to_fix", names(dataset))
  checkmate::assertChoice("heading", names(dataset))
  checkmate::assertChoice("gps_satellite_count", names(dataset))
  checkmate::assertChoice("ground_speed", names(dataset))
  checkmate::assertChoice("external_temperature", names(dataset))
  checkmate::assertChoice("barometric_height", names(dataset))
  checkmate::assertClass(dataset$timestamp, "POSIXct")

  # For checking as we go along and getting a report: a little function to calculate rows, columns, and individuals.
  filterNames <- c("Input data")
  reportData <- data.frame()
  init <- getStats(dataset, idCol) # get an initial baseline from the input data.
  reportData <- dplyr::bind_rows(reportData, init)

  # Basic data quality filters ----------------------------------------------
  # Remove outlier points based on zeroes (Marta's code)
  dataset <- vultureUtils::tempHeightSpeedFilter(dataset)
  outliers <- getStats(dataset, idCol) # AAA
  reportData <- dplyr::bind_rows(reportData, outliers)
  filterNames <- append(filterNames, "Removed outliers with zeroes in three columns")

  # filter out bad gps data
  if(gpsMaxTime > 0){
    dataset <- vultureUtils::gpsTimeFilter(dataset, maxTime = gpsMaxTime)
    badTimeToFix <- getStats(dataset, idCol) # AAA
    reportData <- dplyr::bind_rows(reportData, badTimeToFix)
    filterNames <- append(filterNames, "Removed points that took too long to get GPS fix")
  }
  # filter out bad heading data
  dataset <- vultureUtils::headingFilter(dataset)
  badHeading <- getStats(dataset, idCol) # AAA
  reportData <- dplyr::bind_rows(reportData, badHeading)
  filterNames <- append(filterNames, "Removed points with invalid heading data")

  # only take locs that have at least 3 satellites
  dataset <- vultureUtils::satelliteFilter(dataset, minSatellites = 3)
  badSatellites <- getStats(dataset, idCol) # AAA
  reportData <- dplyr::bind_rows(reportData, badSatellites)
  filterNames <- append(filterNames, "Removed points with too few satellites")

  # precise filter: remove points with < 4 satellites and gps_hdop > 5
  if(precise){
    dataset <- vultureUtils::preciseFilter(dataset)
    lowQualityPoints <- getStats(dataset, idCol) # AAA
    reportData <- dplyr::bind_rows(reportData, lowQualityPoints)
    filterNames <- append(filterNames, "Removed points with < 4 satellites and hdop > 5")
  }

  # SPIKY SPEEDS
  values <- vultureUtils::spikySpeedsFilter(dataset, idCol=idCol, longCol=longCol, latCol=latCol)
  dataset <- values$dataset
  spikySpeeds <- values$spikySpeeds
  nightDistance <- getStats(dataset, idCol) # AAA

  reportData <- dplyr::bind_rows(reportData, spikySpeeds)
  reportData <- dplyr::bind_rows(reportData, nightDistance)
  filterNames <- append(filterNames, "Removed spiky speeds")
  filterNames <- append(filterNames, "Removed points that moved too far at night")

  # remove unrealistic "spiky" altitude values (XXX TO DO)
  values <- vultureUtils::spikyAltitudesFilter(dataset, idCol=idCol)
  dataset <- values$dataset
  nAltitudesToNA <- values$nAltitudesToNA

  # Remove unnecessary variables, if desired. ---------------------------
  if(removeVars == T){
    varsRemoved <- vultureUtils::removeUnnecessaryVars(dataset)
    nColsRemoved <- ncol(dataset)-ncol(varsRemoved) # AAA
    dataset <- varsRemoved
  }

  final <- getStats(dataset, idCol)
  reportData <- dplyr::bind_rows(reportData, final)
  filterNames <- append(filterNames, "Final")

  if(report){
    steps <- filterNames
    df <- reportData %>%
      dplyr::mutate(step = steps) %>%
      dplyr::relocate(step) %>%
      dplyr::mutate(rowsLost = dplyr::lag(rows) - rows,
                    propRowsLost = round(rowsLost/dplyr::lag(rows), 3))
    print(df)
  }
  out <- dataset
  return(out %>%
           dplyr::ungroup())
}

#' In Mask Filter
#'
#' This function filters out individuals that have a percentage of points outside of the mask. The percentage is given by
#' inMaskThreshold. Optionally, points falling outside the mask can also be removed with reMask set to true.
#' Steps: 1. Using the `mask` object, get a list of the individuals in `dataset` that spend at least `inMaskThreshold`
#' proportion of their time inside the mask area. 2. Restrict `dataset` to only these individuals. 3. Re-apply the mask to
#' restrict the remaining points to those that fall within `mask`.
#' @param dataset A dataset with columns: longCol, latCol, dateCol, idCol
#' @param mask The object to use to mask the data. Passed to `vultureUtils::maskData()`. Must be an sf object.
#' @param inMaskThreshold Proportion of an individual's days tracked that must fall within the mask. Default is 0.33 (one third of days tracked). If a number >1 is supplied, will be interpreted as number of days that fall in the mask. Passed to `vultureUtils::mostlyInMask()`. Must be numeric.
#' @param crs Coordinate Reference System to check for and transform to, for both the GPS data and the mask. Default is "WGS84". This value is passed to `vultureUtils::maskData()`. Must be a valid CRS or character string coercible to CRS.
#' @param longCol The name of the column in the dataset containing longitude values. Defaults to "location_long.1". Passed to `vultureUtils::maskData()`.
#' @param latCol The name of the column in the dataset containing latitude values. Defaults to "location_lat.1". Passed to `vultureUtils::maskData()`.
#' @param dateCol The name of the column in the dataset containing dates. Defaults to "dateOnly". Passed to `vultureUtils::mostlyInMask()`.
#' @param idCol The name of the column in the dataset containing vulture ID's. Defaults to "Nili_id" (assuming you have joined the Nili_ids from the who's who table).
#' @param reMask Whether or not to re-mask after removing individuals that spend less than `inMaskThreshold` in the mask area. Default is T.
#' @param quiet Whether to silence the message that happens when doing spatial joins. Default is T.
#' @export
inMaskFilter <- function(dataset, mask, inMaskThreshold = 0.33, crs = "WGS84", longCol = "location_long.1", latCol = "location_lat.1", dateCol = "dateOnly", idCol = "Nili_id", reMask = T, quiet = T){
  # Filter to in-mask threshold
  # If an inMaskThreshold is given (it usually is), then filter to only the individuals that spend at least the threshold proportion of their days within the mask. Otherwise, just pass the dataset through unfiltered. # filter INDIVIDUALS:
  checkmate::assertClass(mask, "sf", null.ok = TRUE)
  checkmate::assertNumeric(inMaskThreshold, len = 1, null.ok = TRUE)
  checkmate::assertCharacter(dateCol, len = 1)
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
                                                                      dateCol = dateCol,
                                                                      idCol = idCol))
    }else{
      longEnoughIndivs <- vultureUtils::mostlyInMask(dataset = dataset,
                                                     maskedDataset = inMask,
                                                     thresh = inMaskThreshold,
                                                     dateCol = dateCol,
                                                     idCol = idCol)
    }
    # tally up how many individuals are getting removed

    # remove the individuals
    dataset <- dataset %>%
      dplyr::filter(.data[[idCol]] %in% longEnoughIndivs)
    firstMask <- getStats(dataset, idCol)
  }else{
    firstMask <- c("rows" = NA, "cols" = NA, "indivs" = NA) # AAA
  }


  # Mask again to remove out-of-mask POINTS, if desired
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
    secondMask <- getStats(cleanedInMask, idCol) # AAA
    out <- cleanedInMask
  }else{
    secondMask <- c("rows" = NA, "cols" = NA, "indivs" = NA) # AAA
    out <- dataset
  }
  list("dataset"=out, "firstMask"=firstMask, "secondMask"=secondMask)
}
#' GPS jamming filter
#'
#' This function takes in a dataset and removes points that lie within mask, given that mask is an sf object containing
#' polygons where GPS points are shifted to after jamming.
#' @param dataset A dataset with columns: longCol, latCol, idCol
#' @param mask The object to use to mask the data. Must be an sf object.
#' @param longCol The name of the column in the dataset containing longitude values. Defaults to "location_long.1". Passed to `vultureUtils::maskData()`.
#' @param latCol The name of the column in the dataset containing latitude values. Defaults to "location_lat.1". Passed to `vultureUtils::maskData()`.
#' @param idCol The name of the column in the dataset containing vulture ID's. Defaults to "Nili_id" (assuming you have joined the Nili_ids from the who's who table).
#' @export
gpsJamFilter <- function(dataset, mask, longCol = "location_long.1", latCol = "location_lat.1", idCol = "Nili_id"){
  before <- getStats(dataset, idCol)
  dataset <- vultureUtils::maskData(dataset = dataset, mask = mask, longCol = longCol, latCol = latCol, crsToSet = "WGS84", op = "difference")
  after <- getStats(dataset, idCol)
  lost <- before - after
  # print(lost)
  dataset
}

#' Create an edge list (flexible; must insert parameters.)
#'
#' Given a dataset of GPS points, a geographic mask, and some roost polygons, create an edge list.
#' @param dataset The cleaned GPS dataset to be used to create the edge list. This should be the output from `vultureUtils::cleanData()`. Must include the columns "ground_speed", "dateOnly", and "timestamp", as well as "location_lat" and "location_long" (which will be passed to `spaceTimeGroups`). XXX edit this: GH#58
#' @param roostPolygons Roost polygons. Must be an sf object with a CRS that matches the dataset CRS. This is used to filter out points with ground speed less than speedThreshLower that might be occurring at roost sites instead of at a carcass site. If NULL, no filtering will be done by polygon intersections.
#' @param roostBuffer Number of meters to buffer the roost polygons by before intersecting them.
#' @param consecThreshold Minimal number of co-occurrences for considering a viable pair of interacting vultures. Passed to `vultureUtils::spaceTimeGroups()`. Must be numeric.
#' @param distThreshold The maximum distance (in meters) at which vultures are considered interacting. Passed to `vultureUtils::spaceTimeGroups()`. Must be numeric.
#' @param speedThreshUpper Upper speed threshold, in m/s.
#' @param speedThreshLower Lower speed threshold, in m/s.
#' @param timeThreshold timeThreshold Passed to spatsoc::group_times. Threshold for grouping times. e.g.: '2 hours', '10 minutes', etc. if not provided, times will be matched exactly. Note that provided threshold must be in the expected format: '## unit'. Default is "10 minutes"
#' @param idCol Passed to spaceTimeGroups: the name of the column containing individual vulture ID's. Default is Nili_id.
#' @param quiet Whether to silence the warning messages about grouping individuals with themselves inside the time threshold. Default is T. This occurs because if we set our time threshold to e.g. 10 minutes (the default), there are some GPS points that occur closer together than 10 minutes apart (e.g. if we experimentally set the tags to take points every 5 minutes). As a result, we will "group" together the same individual with itself, resulting in some self edges. I currently have a step in the code that simply removes these self edges, so there should be no problem here. But if you set `quiet = F`, you will at least be warned with the message `"Warning: found duplicate id in a timegroup and/or splitBy - does your group_times threshold match the fix rate?"` when this is occurring.
#' @param includeAllVertices logical. Whether to include another list item in the output that's a vector of all individuals in the dataset. For use in creating sparse graphs. Default is F.
#' @param daytimeOnly T/F, whether to restrict interactions to daytime only. Default is T.
#' @param return One of "edges" (default, returns an edgelist, would need to be used in conjunction with includeAllVertices = T in order to include all individuals, since otherwise they wouldn't be included in the edgelist. Also includes timegroup information, which SRI cannot do. One row in this data frame represents a single edge in a single timegroup.); "sri" (returns a data frame with three columns, ID1, ID2, and sri. Includes pairs whose SRI values are 0, which means it includes all individuals and renders includeAllVertices obsolete.); and "both" (returns a list with two components: "edges" and "sri" as described above.)
#' @param getLocs Whether or not to return locations where the interactions happened (for edge list only, doesn't make sense for SRI). Default is FALSE. If getLocs is set to TRUE when return = "sri", a message will tell the user that no locations can be returned for SRI.
#' @param speedCol Name of the column containing ground speed values. Default is "ground_speed".
#' @return An edge list containing the following columns: `timegroup` gives the numeric index of the timegroup during which the interaction takes place. `minTimestamp` and `maxTimestamp` give the beginning and end times of that timegroup. `ID1` is the id of the first individual in this edge, and `ID2` is the id of the second individual in this edge.
#' @export
getEdges <- function(dataset, roostPolygons = NULL, roostBuffer, consecThreshold, distThreshold, speedThreshUpper, speedThreshLower, timeThreshold = "10 minutes", idCol = "Nili_id", quiet = T, includeAllVertices = F, daytimeOnly = T, return = "edges", getLocs = FALSE, speedCol = "ground_speed"){
  # Argument checks
  checkmate::assertDataFrame(dataset)
  checkmate::assertSubset("sf", class(dataset))
  checkmate::assertClass(roostPolygons, "sf", null.ok = TRUE)
  checkmate::assertNumeric(roostBuffer, len = 1, null.ok = TRUE)
  checkmate::assertNumeric(consecThreshold, len = 1)
  checkmate::assertNumeric(distThreshold, len = 1)
  checkmate::assertNumeric(speedThreshUpper, len = 1, null.ok = TRUE)
  checkmate::assertNumeric(speedThreshLower, len = 1, null.ok = TRUE)
  checkmate::assertCharacter(timeThreshold, len = 1)
  checkmate::assertLogical(daytimeOnly, len = 1)
  checkmate::assertSubset(return, choices = c("edges", "sri", "both"),
                          empty.ok = FALSE)
  checkmate::assertSubset("timestamp", names(dataset)) # for sunrise/sunset calculations.
  checkmate::assertSubset("dateOnly", names(dataset)) # for sunrise/sunset calculations
  checkmate::assertSubset("location_lat", names(dataset)) # passed to spaceTimeGroups. XXX fix with GH#58
  checkmate::assertSubset("location_long", names(dataset)) # passed to spaceTimeGroups. XXX fix with GH#58
  checkmate::assertSubset(idCol, names(dataset)) # passed to spaceTimeGroups.
  checkmate::assertLogical(getLocs, len = 1)

  # Only require ground_speed column when filtering by speed
  if(!is.null(c(speedThreshLower, speedThreshUpper))){
    checkmate::assertSubset(speedCol, names(dataset)) # necessary for filterLocs.
  }


  # Message about getLocs and sri
  if(getLocs & return == "sri"){
    warning("Cannot return interaction locations when return = 'sri'. If you want interaction locations, use return = 'edges' or return = 'both'.")
  }

  # Get all unique individuals before applying any filtering
  if(includeAllVertices){
    uniqueIndivs <- unique(dataset[[idCol]])
  }

  ## FILTER THE POINTS
  # Restrict interactions based on ground speed
  filteredData <- vultureUtils::filterLocs(df = dataset,
                                           speedThreshUpper = speedThreshUpper,
                                           speedThreshLower = speedThreshLower, speedCol = speedCol)

  # If roost polygons were provided, use them to filter out data
  if(!is.null(roostPolygons)){
    # Buffer the roost polygons

    if(!is.null(roostBuffer)){
      roostPolygons <- convertAndBuffer(roostPolygons, dist = roostBuffer)
    }
    # Exclude any points that fall within a (buffered) roost polygon
    points <- filteredData[lengths(sf::st_intersects(filteredData, roostPolygons)) == 0,]
  }else{
    message("No roost polygons provided; points will not be filtered by spatial intersection.")
    points <- filteredData
  }

  # Restrict based on daylight
  if(daytimeOnly){
    times <- suncalc::getSunlightTimes(date = unique(lubridate::date(points$timestamp)), lat = 31.434306, lon = 34.991889,
                                       keep = c("sunrise", "sunset")) %>%
      dplyr::select(date, sunrise, sunset) # XXX the coordinates I'm using here are from the centroid of Israel calculated here: https://rona.sh/centroid. This is just a placeholder until we decide on a more accurate way of doing this.
    points <- points %>%
      # remove leftover sunrise/sunset cols just in case
      {if("sunrise" %in% names(.)) dplyr::select(., -sunrise) else .}%>%
      {if("sunset" %in% names(.)) dplyr::select(., -sunset) else .}%>%
      dplyr::left_join(times, by = c("dateOnly" = "date")) %>%
      dplyr::mutate(daytime = dplyr::case_when(timestamp > .data[["sunrise"]] &
                                                 timestamp < .data[["sunset"]] ~ T,
                                               TRUE ~ F))

    # Filter out nighttimes
    nNightPoints <- nrow(points[points$daytime == F,])
    points <- points %>%
      dplyr::filter(daytime == T)
    nDayPoints <- nrow(points)
    if(quiet == F){
      cat(paste0("Removed ", nNightPoints, " nighttime points, leaving ",
                 nDayPoints, " points.\n"))
    }
  }

  # If there are no rows left after filtering, create an empty data frame with the appropriate format.
  if(nrow(points) == 0){
    # DUMMY EDGELIST, NO SRI TO COMPUTE
    out <- data.frame(timegroup = as.integer(),
                      ID1 = as.character(),
                      ID2 = as.character(),
                      distance = as.numeric(),
                      minTimestamp = as.POSIXct(character()),
                      maxTimestamp = as.POSIXct(character()))
    warning("After filtering, the dataset had 0 rows.")
  }

  ## GET EDGELIST (optionally compute SRI)
  if(nrow(points) != 0){
    ## Do we need to compute SRI?
    if(return == "edges"){ # if SRI is not needed, we can save time by not computing it.
      if(quiet){
        ### EDGES ONLY, QUIET
        out <- suppressMessages(suppressWarnings(vultureUtils::spaceTimeGroups(dataset = points,
                                                                               distThreshold = distThreshold,
                                                                               consecThreshold = consecThreshold,
                                                                               timeThreshold = timeThreshold,
                                                                               sri = FALSE,
                                                                               idCol = idCol)))
      }else{
        ### EDGES ONLY, WARNINGS
        # compute edges without suppressing warnings
        out <- vultureUtils::spaceTimeGroups(dataset = points,
                                             distThreshold = distThreshold,
                                             consecThreshold = consecThreshold,
                                             timeThreshold = timeThreshold,
                                             sri = FALSE,
                                             idCol = idCol)
      }

    }else if(return %in% c("sri", "both")){ # otherwise we need to compute SRI.
      if(quiet){
        ### EDGES AND SRI, QUIET
        # suppress warnings while computing edges and SRI, returning a list of edges+sri
        out <- suppressMessages(suppressWarnings(vultureUtils::spaceTimeGroups(dataset = points,
                                                                               distThreshold = distThreshold,
                                                                               consecThreshold = consecThreshold,
                                                                               timeThreshold = timeThreshold,
                                                                               sri = TRUE,
                                                                               idCol = idCol)))
        if(return == "sri"){
          out <- out["sri"]
        }
      }else{
        ### EDGES AND SRI, WARNINGS
        # compute edges and SRI without suppressing warnings, returning a list of edges+sri
        out <- vultureUtils::spaceTimeGroups(dataset = points,
                                             distThreshold = distThreshold,
                                             consecThreshold = consecThreshold,
                                             timeThreshold = timeThreshold,
                                             sri = TRUE,
                                             idCol = idCol)
        if(return == "sri"){
          out <- out["sri"]
        }
      }
    }
  }

  locsColNames <- c("latID1", "longID1", "latID2", "longID2", "interactionLat", "interactionLong")
  if(!getLocs & return %in% c("edges", "both")){
    if(!is.list(out)){
      out <- out %>%
        dplyr::select(-any_of(locsColNames))
    }else{
      if("edges" %in% names(out)){
        out$edges <- out$edges %>%
          dplyr::select(-any_of(locsColNames))
      }else{
        out <- out %>%
          dplyr::select(-any_of(locsColNames))
      }
    }
  }

  ## APPEND VERTICES
  if(includeAllVertices){
    toReturn <- append(out, list(as.character(uniqueIndivs)))
  }else{
    toReturn <- out
  }

  # If the list only has one object, unlist it one level down.
  if(length(toReturn) == 1){
    toReturn <- toReturn[[1]]
  }

  ## RETURN LIST
  return(toReturn)
}

#' Create co-feeding edge list
#'
#' Wrapper of getEdges() with defaults for co-feeding edges. Can still be customized!
#' @param dataset The cleaned GPS dataset to be used to create the edge list. This should be the output from `vultureUtils::cleanData()`.
#' @param roostPolygons Roost polygons. Must be an sf object with a CRS that matches the dataset CRS. This is used to filter out points with ground speed less than speedThreshLower that might be occurring at roost sites instead of at a carcass site. If NULL, no filtering will be done by polygon intersections.
#' @param roostBuffer Number of meters to buffer the roost polygons by before intersecting them. Default is 50 m.
#' @param consecThreshold Minimal number of co-occurrences for considering a viable pair of interacting vultures (default is 2 consecutive time steps). Passed to `vultureUtils::spaceTimeGroups()`. Must be numeric.
#' @param distThreshold The maximum distance (in meters) at which vultures are considered interacting. Default is 50 for co-feeding. Passed to `vultureUtils::spaceTimeGroups()`. Must be numeric.
#' @param speedThreshUpper Upper speed threshold, in m/s. For co-feeding, default is 5 m/s. Passed to `vultureUtils::filterLocs()`. Must be numeric.
#' @param speedThreshLower Lower speed threshold, in m/s. For co-feeding, default is NULL. Passed to `vultureUtils::filterLocs()`. Must be numeric.
#' @param timeThreshold timeThreshold Passed to spatsoc::group_times. Threshold for grouping times. e.g.: '2 hours', '10 minutes', etc. if not provided, times will be matched exactly. Note that provided threshold must be in the expected format: '## unit'. Default is "10 minutes"
#' @param idCol Passed to spaceTimeGroups: the name of the column containing individual vulture ID's. Default is Nili_id.
#' @param quiet Whether to silence the warning messages about grouping individuals with themselves inside the time threshold. Default is T. This occurs because if we set our time threshold to e.g. 10 minutes (the default), there are some GPS points that occur closer together than 10 minutes apart (e.g. if we experimentally set the tags to take points every 5 minutes). As a result, we will "group" together the same individual with itself, resulting in some self edges. I currently have a step in the code that simply removes these self edges, so there should be no problem here. But if you set `quiet = F`, you will at least be warned with the message `"Warning: found duplicate id in a timegroup and/or splitBy - does your group_times threshold match the fix rate?"` when this is occurring.
#' @param includeAllVertices logical. Whether to include another list item in the output that's a vector of all individuals in the dataset. For use in creating sparse graphs. Default is F.
#' @param daytimeOnly T/F, whether to restrict interactions to daytime only. Default is T.
#' @param return One of "edges" (default, returns an edgelist, would need to be used in conjunction with includeAllVertices = T in order to include all individuals, since otherwise they wouldn't be included in the edgelist. Also includes timegroup information, which SRI cannot do. One row in this data frame represents a single edge in a single timegroup.); "sri" (returns a data frame with three columns, ID1, ID2, and sri. Includes pairs whose SRI values are 0, which means it includes all individuals and renders includeAllVertices obsolete.); and "both" (returns a list with two components: "edges" and "sri" as described above.)
#' @param getLocs Whether or not to return locations where the interactions happened (for edge list only, doesn't make sense for SRI). Default is FALSE. If getLocs is set to TRUE when return = "sri", a message will tell the user that no locations can be returned for SRI.
#' @return An edge list containing the following columns: `timegroup` gives the numeric index of the timegroup during which the interaction takes place. `minTimestamp` and `maxTimestamp` give the beginning and end times of that timegroup. `ID1` is the id of the first individual in this edge, and `ID2` is the id of the second individual in this edge.
#' @export
getFeedingEdges <- function(dataset, roostPolygons = NULL, roostBuffer = 50, consecThreshold = 2, distThreshold = 50, speedThreshUpper = 5, speedThreshLower = NULL, timeThreshold = "10 minutes", idCol = "Nili_id", quiet = T, includeAllVertices = F, daytimeOnly = T, return = "edges", getLocs = FALSE){
  getEdges(dataset, roostPolygons = roostPolygons, roostBuffer = roostBuffer, consecThreshold = consecThreshold, distThreshold = distThreshold, speedThreshUpper = speedThreshUpper, speedThreshLower = speedThreshLower, timeThreshold = timeThreshold, idCol = idCol, quiet = quiet, includeAllVertices = includeAllVertices, daytimeOnly = daytimeOnly, return = return, getLocs = getLocs)
}

#' Create co-flight edge list
#'
#' Wrapper of getEdges() with defaults for co-flight edges. Can still be customized!
#' @param dataset The cleaned GPS dataset to be used to create the edge list. This should be the output from `vultureUtils::cleanData()`.
#' @param roostPolygons Roost polygons. Must be an sf object with a CRS that matches the dataset CRS. This is used to filter out points with ground speed less than speedThreshLower that might be occurring at roost sites instead of at a carcass site. If NULL, no filtering will be done by polygon intersections.
#' @param roostBuffer Number of meters to buffer the roost polygons by before intersecting them. Default is 50 m.
#' @param consecThreshold Minimal number of co-occurrences for considering a viable pair of interacting vultures (default is 2 consecutive time steps). Passed to `vultureUtils::spaceTimeGroups()`. Must be numeric.
#' @param distThreshold The maximum distance (in meters) at which vultures are considered interacting. Default is 1000 m for co-flight Passed to `vultureUtils::spaceTimeGroups()`. Must be numeric.
#' @param speedThreshUpper Upper speed threshold, in m/s. For co-flight, default is NULL. Passed to `vultureUtils::filterLocs()`. Must be numeric.
#' @param speedThreshLower Lower speed threshold, in m/s. For co-flight, default is 5 m/s. Passed to `vultureUtils::filterLocs()`. Must be numeric.
#' @param timeThreshold timeThreshold Passed to spatsoc::group_times. Threshold for grouping times. e.g.: '2 hours', '10 minutes', etc. if not provided, times will be matched exactly. Note that provided threshold must be in the expected format: '## unit'. Default is "10 minutes"
#' @param idCol Passed to spaceTimeGroups: the name of the column containing individual vulture ID's. Default is Nili_id.
#' @param quiet Whether to silence the warning messages about grouping individuals with themselves inside the time threshold. Default is T. This occurs because if we set our time threshold to e.g. 10 minutes (the default), there are some GPS points that occur closer together than 10 minutes apart (e.g. if we experimentally set the tags to take points every 5 minutes). As a result, we will "group" together the same individual with itself, resulting in some self edges. I currently have a step in the code that simply removes these self edges, so there should be no problem here. But if you set `quiet = F`, you will at least be warned with the message `"Warning: found duplicate id in a timegroup and/or splitBy - does your group_times threshold match the fix rate?"` when this is occurring.
#' @param includeAllVertices logical. Whether to include another list item in the output that's a vector of all individuals in the dataset. For use in creating sparse graphs. Default is F.
#' @param daytimeOnly T/F, whether to restrict interactions to daytime only. Default is T.
#' @param return One of "edges" (default, returns an edgelist, would need to be used in conjunction with includeAllVertices = T in order to include all individuals, since otherwise they wouldn't be included in the edgelist. Also includes timegroup information, which SRI cannot do. One row in this data frame represents a single edge in a single timegroup.); "sri" (returns a data frame with three columns, ID1, ID2, and sri. Includes pairs whose SRI values are 0, which means it includes all individuals and renders includeAllVertices obsolete.); and "both" (returns a list with two components: "edges" and "sri" as described above.)
#' @param getLocs Whether or not to return locations where the interactions happened (for edge list only, doesn't make sense for SRI). Default is FALSE. If getLocs is set to TRUE when return = "sri", a message will tell the user that no locations can be returned for SRI.
#' @return An edge list containing the following columns: `timegroup` gives the numeric index of the timegroup during which the interaction takes place. `minTimestamp` and `maxTimestamp` give the beginning and end times of that timegroup. `ID1` is the id of the first individual in this edge, and `ID2` is the id of the second individual in this edge.
#' @export
getFlightEdges <- function(dataset, roostPolygons = NULL, roostBuffer = 50, consecThreshold = 2, distThreshold = 1000, speedThreshUpper = NULL, speedThreshLower = 5, timeThreshold = "10 minutes", idCol = "Nili_id", quiet = T, includeAllVertices = F, daytimeOnly = T, return = "edges", getLocs = FALSE){
  getEdges(dataset, roostPolygons = roostPolygons, roostBuffer = roostBuffer, consecThreshold = consecThreshold, distThreshold = distThreshold, speedThreshUpper = speedThreshUpper, speedThreshLower = speedThreshLower, timeThreshold = timeThreshold, idCol = idCol, quiet = quiet, includeAllVertices = includeAllVertices, daytimeOnly = daytimeOnly, return = return, getLocs = getLocs)
}

#' Create co-roosting edge list
#'
#' Function to create an edgelist based on roost locations. Two modes: either create a straightforward distance-based network based on a distance threshold (analogous to getFeedingEdges and getFlightEdges, except that there's only one point per individual per night); or create a shared-roost network based on roost polygon assignment.
#' @param dataset A data frame. May have one of two formats, depending on `mode`: either ID/date/lat/long (works for `mode` = "distance", or `mode` = "polygon" if `roostPolygons` is also supplied); or ID/date/roostID (work only for `mode` = "polygon"). May have additional columns.
#' @param mode One of "distance" (default) or "polygon". If "distance", creates a distance-based network with edges each night. If "polygon", creates a shared-polygon-based network.
#' @param roostPolygons An sf object: polygons to use to classify points by roost sites. Required if `mode` = "polygon" AND there is not already a `roostID` column in `dataset`. Otherwise NULL is fine.
#' @param distThreshold A distance threshold, in meters, below which points are considered to be "interacting"--applies only to `mode` = "distance". Default is 500m.
#' @param latCol Name of the column in `dataset` containing latitude information. Default is "location_lat".
#' @param longCol Name of the column in `dataset` containing longitude information. Default is "location_long".
#' @param idCol Name of the column in `dataset` containing the ID's of the vultures. Default is "Nili_id".
#' @param dateCol Name of the column in `dataset` containing roost dates. Default is "date".
#' @param roostCol Name of the column in `dataset` containing roost site assignments. Required only if `mode` = "polygon" AND `roostPolygons` is NULL.
#' @param crsToSet CRS to assign to `dataset` if it is not already an sf object. Default is "WGS84".
#' @param crsToTransform CRS to transform the `dataset` to. Default is "32636" for ITM.
#' @param return One of "edges" (default, returns an edgelist, would need to be used in conjunction with includeAllVertices = T in order to include all individuals, since otherwise they wouldn't be included in the edgelist); "sri" (returns a data frame with three columns, ID1, ID2, and sri. Includes pairs whose SRI values are 0, which means it includes all individuals and renders includeAllVertices obsolete.); and "both" (returns a list with two components: "edges" and "sri" as described above.)
#' @param getLocs Whether or not to return locations where the interactions happened (for edge list only, doesn't make sense for SRI). Default is FALSE. If getLocs is set to TRUE when return = "sri", a message will tell the user that no locations can be returned for SRI.
#' @export
getRoostEdges <- function(dataset, mode = "distance", roostPolygons = NULL, distThreshold = 500, latCol = "location_lat", longCol = "location_long", idCol = "Nili_id", dateCol = "date", roostCol = "roostID", crsToSet = "WGS84", crsToTransform = 32636, return = "edges", getLocs = FALSE){
  # Arg checks
  checkmate::assertDataFrame(dataset)
  checkmate::assertSubset(mode, c("distance", "polygon"), empty.ok = F)
  if(!missing(roostPolygons) & !is.null(roostPolygons)){
    checkmate::assertSubset("sf", class(roostPolygons))
    if(is.na(sf::st_crs(roostPolygons))){
      stop("roostPolygons object must have a valid crs.")
    }
  }
  checkmate::assertNumeric(distThreshold, lower = 0, null.ok = FALSE)
  checkmate::assertCharacter(latCol, len = 1)
  checkmate::assertCharacter(longCol, len = 1)
  checkmate::assertCharacter(idCol, len = 1)
  checkmate::assertCharacter(dateCol, len = 1)
  checkmate::assertCharacter(roostCol, null.ok = T, len = 1)
  checkmate::assertSubset(c(latCol, longCol, idCol, dateCol), names(dataset))
  checkmate::assertSubset(return, c("edges", "sri", "both"))
  checkmate::assertLogical(getLocs, len = 1)

  # Message about getLocs and sri
  if(getLocs & return == "sri"){
    warning("Cannot return interaction locations when return = 'sri'. If you want interaction locations, use return = 'edges' or return = 'both'.")
  }

  # Begin computation of edge list
  if(mode == "distance"){
    ## DISTANCE MODE
    # XXXXXXXXXX
    # Set up an sf object for use.
    if("sf" %in% class(dataset)){ # If dataset is an sf object...
      if(is.na(sf::st_crs(dataset))){ # only fill in crs if it is missing
        message(paste0("`dataset` is already an sf object but has no CRS. Setting CRS to ", crsToSet, "."))
        dataset <- sf::st_set_crs(dataset, crsToSet)
      }
    }else if(is.data.frame(dataset)){ # otherwise, if dataset is a data frame...
      # make sure it contains the lat and long cols
      if(nrow(dataset) == 0){
        stop("Dataset passed to vultureUtils::getRoostEdges has 0 rows. Cannot proceed with grouping.")
      }

      # convert to an sf object
      dataset <- dataset %>%
        sf::st_as_sf(coords = c(longCol, latCol), remove = FALSE) %>%
        sf::st_set_crs(crsToSet) # assign the CRS

    }else{ # otherwise, throw an error.
      stop("`dataset` must be a data frame or an sf object.")
    }

    # Save lat and long coords, in case we need them later. Then, convert to UTM.
    dataset <- dataset %>%
      sf::st_transform(crsToTransform) # convert to UTM: we'll need this for calculating distance later.
    dataset <- dataset %>%
      dplyr::mutate(utmE = unlist(purrr::map(dataset$geometry, 1)),
                    utmN = unlist(purrr::map(dataset$geometry, 2))) %>%
      sf::st_drop_geometry() # spatsoc won't work if this is still an sf object.
    # XXXXXXXXXX # should probably make the above into its own function, since it's repeated code.
    # Use spatsoc to compute distance groups using the distance threshold
    data.table::setDT(dataset)
    # spatialGrouped <- spatsoc::group_pts(DT = dataset, threshold = distThreshold,
    #                                      id = idCol, coords = c("utmE", "utmN"),
    #                                      splitBy = dateCol, timegroup = NULL) # XXX I've submitted an issue to hopefully fix this so we don't need NULL (https://github.com/ropensci/spatsoc/issues/44)

    # Get edges
    edges <- spatsoc::edge_dist(DT = dataset, threshold = distThreshold,
                                id = idCol, coords = c("utmE", "utmN"),
                                splitBy = dateCol, timegroup = NULL,
                                fillNA = FALSE, returnDist = TRUE)

    # Compute interaction locations
    ## get locations of each individual at each time group
    locs <- dataset %>%
      tibble::as_tibble() %>%
      dplyr::select(tidyselect::all_of(c(idCol, dateCol, latCol, longCol))) %>%
      dplyr::distinct() %>%
      dplyr::mutate(across(tidyselect::all_of(c(latCol, longCol)), as.numeric))

    # In case there is more than one point per individual per night, get the mean (there really shouldn't be, but you never know)
    meanLocs <- locs %>%
      dplyr::group_by(across(all_of(c(idCol, dateCol)))) %>%
      dplyr::summarize(mnLat = mean(.data[[latCol]], na.rm = T),
                       mnLong = mean(.data[[longCol]], na.rm = T))

    ef <- edges %>%
      dplyr::left_join(meanLocs, by = c("ID1" = idCol, dateCol)) %>%
      dplyr::rename("latID1" = mnLat, "longID1" = mnLong) %>%
      dplyr::left_join(meanLocs, by = c("ID2" = idCol, dateCol)) %>%
      dplyr::rename("latID2" = mnLat, "longID2" = mnLong) %>%
      dplyr::mutate(interactionLat = (latID1 + latID2)/2,
                    interactionLong = (longID1 + longID2)/2)

    if(!(nrow(edges) == nrow(ef))){
      stop("Wrong number of rows!") # XXX need to better address this error.
    }

    edges <- ef

  }else{
    ## POLYGON MODE
    # Polygon assignment is triggered if the roostID column is missing and there are some polygons provided.
    ## POLYGON ASSIGNMENT
    if(!(roostCol %in% names(dataset)) & !is.null(roostPolygons)){ # if no polygons are yet assigned AND a set of polygons is provided...
      # XXXXXXXXXX
      # Set up an sf object for use.
      if("sf" %in% class(dataset)){ # If dataset is an sf object...
        if(is.na(sf::st_crs(dataset))){ # only fill in crs if it is missing
          message(paste0("`dataset` is already an sf object but has no CRS. Setting CRS to ", crsToSet, "."))
          dataset <- sf::st_set_crs(dataset, crsToSet)
        }
      }else if(is.data.frame(dataset)){ # otherwise, if dataset is a data frame...
        # make sure it contains the lat and long cols
        checkmate::assertChoice(latCol, names(dataset))
        checkmate::assertChoice(longCol, names(dataset))

        if(nrow(dataset) == 0){
          stop("Dataset passed to vultureUtils::getRoostEdges has 0 rows. Cannot proceed with grouping.")
        }

        # convert to an sf object
        dataset <- dataset %>%
          sf::st_as_sf(coords = c(longCol, latCol), remove = FALSE) %>%
          sf::st_set_crs(crsToSet) # assign the CRS

      }else{ # otherwise, throw an error.
        stop("`dataset` must be a data frame or an sf object.")
      }
      # XXXXXXXXXX # should probably make the above into its own function, since it's repeated code.
      # if needed, convert the points to the same coordinate system as the polygons
      if(sf::st_crs(dataset) != sf::st_crs(roostPolygons)){
        dataset <- sf::st_transform(dataset, crs = sf::st_crs(roostPolygons))
      }

      # Add an ID number for each roost.
      roostPolygons$roostPolygonID <- 1:nrow(roostPolygons)
      roostPolygons <- roostPolygons[,"roostPolygonID"] # geometry comes along when you do this

      # Join the dataset to the roost polygons.
      polys <- sf::st_join(dataset, roostPolygons) %>%
        sf::st_drop_geometry() %>%
        dplyr::rename({{roostCol}} := roostPolygonID)
    }else if(roostCol %in% names(dataset)){
      polys <- dataset # we can use the dataset as is.
    }else if(!(roostCol %in% names(dataset)) & is.null(roostPolygons)){
      stop(paste0("Column `", roostCol, "` not found in dataset, and no roost polygons provided. Must provide either the name of a column containing roost assignments or a valid set of roost polygons."))
    }

    # Create an edgelist by shared polygon membership
    edges <- polys %>%
      dplyr::filter(!is.na(.data[[roostCol]])) %>% # remove NA roosts
      dplyr::group_by(.data[[dateCol]], .data[[roostCol]]) %>% # each day/roost is treated separately
      dplyr::group_split(.keep = T) %>%
      purrr::map_dfr(~{tidyr::expand_grid("ID1" = .x[[idCol]], .x)}) %>% # for each polygon/day, create all pairs of individuals, and then bind the results back together into a data frame.

      dplyr::rename("ID2" = {{idCol}}) %>%
      dplyr::filter(ID1 < ID2) %>% # remove self and duplicate edges
      dplyr::select(-c("sunrise", "sunset", "sunrise_twilight", "sunset_twilight", "daylight", "is_roost"))
  }

  locsColNames <- c("latID1", "longID1", "latID2", "longID2", "interactionLat", "interactionLong")
  if(!getLocs & !is.null(edges) & mode == "polygon"){
    edges[[latCol]] <- NULL
    edges[[longCol]] <- NULL
    edges[[roostCol]] <- NULL
  }else if(!getLocs & !is.null(edges) & mode != "polygon"){
    edges <- edges %>%
      dplyr::select(-any_of(locsColNames))
  }

  # now we have either distanceEdges or polygonEdges. Now need to determine whether to calculate SRI or not.
  if(return %in% c("both", "sri")){
    # calculate SRI
    dfSRI <- calcSRI(dataset = dataset, edges = edges, idCol = idCol, timegroupCol = dateCol)
    if(return == "sri"){
      return(dfSRI)
    }else if(return == "both"){
      return(list("edges" = edges,
                  "sri" = dfSRI))
    }

  }else{
    return(edges)
  }
}

#' Get roosts (data frame version)
#'
#' With several methods, get the roost locations of a vulture on each night. This is an adaptation of Marta Acácio's get_roosts function that takes a data frame as input instead of several vectors. The function consecutively calculates the night roost based on the following five methods:
#' 1.  It is the last GPS location of the day, and it is at night (and during the "night hours"), and the speed is equal or less than 4m/s (i.e., the bird was considered to not be flying);
#' 2.  It is the last GPS location of the day, and it is at night (and during the "night hours"), and the speed is NA;
#' 3.  It is the first GPS location of the day, and it is at night (and during the "morning hours"), and the speed is equal or less than 4m/s;
#' 4.  It is the first GPS location of the day, and it is at night (and during the "morning hours"), and the speed is NA;
#' 5.  The last GPS location of the day (independently of the light or the hours) is within the defined buffer (pre-defined, 1km) of the first GPS location of the following day.
#' The roost day is assigned in the following way:
#' 1.  If it is the last location of the day, it is that day's night roost;
#' 2.  If it was the first location of the day, it was the previous day's night roost;
#' 3.  If for a particular date, the roost was calculated using more than 1 method, the selected roost is the earliest calculated roost.
#' @param df data frame containing the `id`, `timestamp`, `x`, `y`, and `ground_speed` columns.
#' @param id column containing animal identifiers
#' @param timestamp column of class `as.POSIXct` (timestamps for GPS fixes)
#' @param x column containing longitude, in decimal degrees
#' @param y column containing latitude, in decimal degrees
#' @param ground_speed column containing ground speed of the animal
#' @param speed_units units of speed (either "m/s" or "km/h"). If speed is provided in "km/h" it is transformed to "m/s".
#' @param buffer optional, numerical value indicating the number of kms to consider the buffer for the roost calculation. The pre-defined value is 1km
#' @param twilight optional, numerical value indicating number of minutes to consider the twilight for calculating the day and night positions. If set to 0, the night period starts at sunset and the day period starts at sunrise. The pre-defined value is 61, so the night period starts 61 minutes before sunset and the day period starts 61 minutes after sunrise
#' @param morning_hours optional, vector indicating the range of hours (in UTC) that are considered the morning period. The pre-defined vector is `c(0:12)`
#' @param night_hours optional, vector indicating the range of hours (in UTC) that are considered the night period. The pre-defined vector is `c(13:23)`
#' @param quiet If F (default), prints time warning/progress message. If T, silences this message.
#' @return a data frame of the calculated roosts for every animal.
#' @export
get_roosts_df <- function(df, id = "local_identifier", timestamp = "timestamp", x = "location_long", y = "location_lat", ground_speed = "ground_speed", speed_units = "m/s", buffer = 1, twilight = 61, morning_hours = c(0:12), night_hours = c(13:23), quiet = F){
  # setup for time warning
  if(!quiet){
    cat("\nFinding roosts... this may take a while if your dataset is large.\n")
    start <- Sys.time()
  }

  # Argument checks
  checkmate::assertDataFrame(df)
  checkmate::assertCharacter(id, len = 1)
  checkmate::assertSubset(id, names(df))
  checkmate::assertCharacter(timestamp, len = 1)
  checkmate::assertSubset(timestamp, names(df))
  checkmate::assertCharacter(x, len = 1)
  checkmate::assertSubset(x, names(df))
  checkmate::assertCharacter(y, len = 1)
  checkmate::assertSubset(y, names(df))
  checkmate::assertCharacter(ground_speed, len = 1)
  checkmate::assertSubset(ground_speed, names(df))
  checkmate::assertCharacter(speed_units, len = 1)
  checkmate::assertSubset(speed_units, c("m/s", "km/h"))
  checkmate::assertNumeric(buffer, len = 1)
  checkmate::assertNumeric(twilight, len = 1)
  checkmate::assertNumeric(morning_hours, upper = 24, lower = 0)
  checkmate::assertNumeric(night_hours, upper = 24, lower = 0)
  checkmate::assertNumeric(df[[x]])
  checkmate::assertNumeric(df[[y]])
  checkmate::assertNumeric(df[[ground_speed]])

  # If the data is an sf object, remove the geometry to make it easier to work with. The computations in this function just depend on lat/long columns being present.
  if("sf" %in% class(df)){
    df <- df %>%
      sf::st_drop_geometry()
  }

  # Transform the twilight period into seconds
  twilight_secs <- twilight * 60

  # If the speed is in km/h transform into m/s
  if(speed_units == "km/h"){
    df <- df %>%
      dplyr::mutate({{ground_speed}} := round(.data[[ground_speed]] / 3.6, 3))
  }

  df[[timestamp]] <- as.POSIXct(df[[timestamp]],
                                format = "%Y-%m-%d %H:%M:%S",
                                tz = "UTC")

  if(sum(is.na(df[[timestamp]])) > 0){
    stop("Timestamp needs to be defined as.POSIXct (%Y-%m-%d %H:%M:%S)")
  }

  df$date <- as.Date(df[[timestamp]])

  # Separate into a list of individuals
  indivs <- df %>%
    dplyr::group_by(.data[[id]]) %>%
    dplyr::group_split(.keep = T)

  roosts <- purrr::map_dfr(indivs, ~{
    temp.id <- unique(.x[[id]])

    id.df <- .x %>%
      dplyr::group_by(date) %>%
      dplyr::arrange({{timestamp}}) %>%
      dplyr::mutate(
        row_id = dplyr::case_when(
          dplyr::row_number() == 1 ~ "first",
          dplyr::row_number() == max(dplyr::row_number()) ~ "last"),
        hour = lubridate::hour(.data[[timestamp]])) %>%
      dplyr::filter(row_id %in% c("first", "last")) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(day_diff = round(difftime(dplyr::lead(date), date, units="days")))

    matrix <- as.matrix(id.df[,c(x, y)])
    leadMatrix <- as.matrix(cbind(dplyr::lead(id.df[[x]]),
                                  dplyr::lead(id.df[[y]])))
    distances <- geosphere::distGeo(p1 = matrix, p2 = leadMatrix)*0.001 %>%
      round(., 2)
    id.df$dist_km <- distances
    id.df$dist_km[id.df$day_diff != 1] <- NA

    # Ryan's Code: I think maptools::sunriset can be replaced with suncalc::getSunlightTimes since its used in other places
    # SEE: https://cran.r-project.org/web/packages/suncalc/ https://cran.r-project.org/web/packages/suncalc/suncalc.pdf

    data <- data.frame(date = as.Date(id.df[[timestamp]]), lat = id.df[[y]], lon = id.df[[x]])

    id.df$sunrise <- suncalc::getSunlightTimes(data = data, keep = c("sunrise"))$sunrise
    id.df$sunset <- suncalc::getSunlightTimes(data = data, keep = c("sunset"))$sunset

    # Set the twilight
    id.df$sunrise_twilight <- id.df$sunrise + twilight_secs
    id.df$sunset_twilight <- id.df$sunset - twilight_secs

    id.df <- id.df %>%
      dplyr::mutate(daylight = ifelse(.data[[timestamp]] >= sunrise_twilight &
                                        .data[[timestamp]] <= sunset_twilight,
                                      "day", "night"))

    # Identify the roosts
    id.df <- id.df %>%
      dplyr::mutate(
        is_roost = dplyr::case_when(
          row_id == "last" & daylight == "night" & hour %in% night_hours & ({{ground_speed}} <= 4 |
                                                                              is.na({{ground_speed}})) ~ 1,
          row_id == "first" & daylight == "night" & hour %in% morning_hours & ({{ground_speed}} <= 4 |
                                                                                 is.na({{ground_speed}})) ~ 1,
          dist_km <= buffer ~ 1
        ),
        roost_date = dplyr::case_when(
          is_roost == 1 & row_id == "last" ~ paste(as.character(date)),
          is_roost == 1 & row_id == "first" ~ paste(as.character(date-1))
        ),
        roost_date = as.Date(roost_date)
      )

    temp.id.roosts <- dplyr::filter(id.df, is_roost == 1)

    # If there is more than 1 roost per day, keep the earliest roost (night roost)
    temp.id.roosts <- temp.id.roosts %>%
      dplyr::group_by(roost_date) %>%
      dplyr::arrange({{timestamp}}) %>%
      dplyr::filter(dplyr::row_number() == 1) %>%
      dplyr::ungroup() %>%
      dplyr::select(-c("row_id", "hour"))

    temp.id.roosts <- temp.id.roosts %>%
      dplyr::select({{id}}, date, roost_date, sunrise, sunset, sunrise_twilight, sunset_twilight, daylight, is_roost, location_lat, location_long)

    return(temp.id.roosts)
  })

  # complete the time message
  if(!quiet){
    end <- Sys.time()
    duration <- difftime(end, start, units = "secs")
    cat(paste0("Roost computation completed in ", duration, " seconds."))
  }

  roosts <- roosts %>%
    dplyr::select({{id}}, date, roost_date, sunrise, sunset, sunrise_twilight, sunset_twilight, daylight, is_roost, location_lat, location_long)

  # return
  return(roosts)

}

#' Make temporal slices of an edge list
#'
#' This function takes an edge list (returned by `vultureUtils::get*Edges()`) and slices it temporally. Returns a list of data frames (edge lists). Includes specifications for whether the time slices should start at the beginning or end, and what to do with any odd-length slice if/when the specified time interval doesn't divide evenly into the amount of time given.
#' @param edges a data frame containing edges and their associated `timegroup`s. Returned by `vultureUtils::get*Edges()`
#' @param n an integer value
#' @param unit unit for the interval. Will be combined with the integer value of `interval`. So if `unit` is "days" and `interval` is 5, then your data will be separated into an interval of "5 days".
#' @param from character, either "start" (default) or "end", to indicate whether the time slices should be taken forwards from the beginning of the data ("start") or backwards from the end of the data ("end").
#' @param dropEdgeGroup logical. Whether to get rid of the "stub" group of uneven size at the opposite end of the data. Default is FALSE--the group will be kept, even though it may contain incomplete data. If TRUE, drop the last group (if `from` == "end") or the first group (if `from` == "start")
#' @param dateTimeStart a dateTime object that defines the beginning of the time period to be divided. If not specified, defaults to the earliest `timestamp` in `edges`. Must be in one of the following formats: "YYYY-MM-DD hh:mm:ss" or "YYYY-MM-DD hh:mm" or "YYYY-MM-DD". Hours must use 24 hour time--e.g. 5:00 pm would be 17:00.
#' @param dateTimeEnd a dateTime object that defines the end of the time period to be divided. If not specified, defaults to the latest `timestamp` in `edges`. Must be in one of the following formats: "YYYY-MM-DD hh:mm:ss" or "YYYY-MM-DD hh:mm" or "YYYY-MM-DD". Hours must use 24 hour time--e.g. 5:00 pm would be 17:00.
#' @param id1Col name of the column in `edges` containing the ID of the first individual in a dyad
#' @param id2Col name of the column in `edges` containing the ID of the second individual in a dyad
#' @param minTimestampCol the name of the column to use for assigning the minimum timestamp, if no `dateTimeStart` is provided. Default is "minTimestamp".
#' @param maxTimestampCol the name of the column to use for assigning the maximum timestamp, if no `dateTimeEnd` is provided. Default is "maxTimestamp".
#' @return A list of data frames (i.e. "edgelists")
#' @export
sliceTemporal <- function(edges, n, unit = "days", from = "start",
                          dropEdgeGroup = FALSE,
                          dateTimeStart = NULL,
                          dateTimeEnd = NULL, id1Col = "ID1", id2Col = "ID2",
                          minTimestampCol = "minTimestamp",
                          maxTimestampCol = "maxTimestamp"){

  # Some basic argument checks
  checkmate::assertCharacter(minTimestampCol, len = 1)
  checkmate::assertCharacter(maxTimestampCol, len = 1)
  checkmate::assertClass(edges[[minTimestampCol]], "POSIXct")
  checkmate::assertClass(edges[[maxTimestampCol]], "POSIXct")
  checkmate::assertChoice(maxTimestampCol, names(edges))
  checkmate::assertChoice(minTimestampCol, names(edges))
  checkmate::assertNumeric(n, len = 1)
  checkmate::assertCharacter(from)
  checkmate::assertSubset(from, choices = c("start", "end"), empty.ok = FALSE)
  checkmate::assertLogical(dropEdgeGroup, len = 1)

  # Either assign dateTimeStart and dateTimeEnd, or coerce the user-provided inputs to lubridate datetimes.
  if(is.null(dateTimeStart)){
    dateTimeStart <- min(edges[[minTimestampCol]])
    message(paste0("No start datetime provided. Using earliest timestamp, which is ", dateTimeStart, "."))
  }
  if(is.null(dateTimeEnd)){
    dateTimeEnd <- max(edges[[maxTimestampCol]])
    message(paste0("No end datetime provided. Using latest timestamp, which is ", dateTimeEnd, "."))
  }

  # Parse the start and end times into standard datetime format
  start <- lubridate::parse_date_time(dateTimeStart, orders = c("%Y%m%d %H%M%S", "%Y%m%d %H%M", "%Y%m%d"))
  if(is.na(start)){
    stop("`dateTimeStart` could not be parsed. Please make sure you have used one of the following formats: YYYY-MM-DD hh:mm:ss, YYYY-MM-DD hh:mm, or YYYY-MM-DD.")
  }
  end <- lubridate::parse_date_time(dateTimeEnd, orders = c("%Y%m%d %H%M%S", "%Y%m%d %H%M", "%Y%m%d"))
  if(is.na(end)){
    stop("`dateTimeEnd` could not be parsed. Please make sure you have used one of the following formats: YYYY-MM-DD hh:mm:ss, YYYY-MM-DD hh:mm, or YYYY-MM-DD.")
  }

  # Check that the user-defined time interval is coercible to a duration
  compositeInterval <- paste(n, unit)
  int <- lubridate::as.duration(compositeInterval)
  if(is.na(int)){
    stop("Arguments `n` and `unit` could not be expressed as a duration: lubridate::as.duration() returned NA. Please make sure you have specified `n` as an integer and `unit` as a unit such as `hours` or `days`.")
  }
  checkmate::assertClass(int, "Duration")
  # Note that even though we checked if this was coercible to a duration, we're not actually going to *use* the object `int`, because `cut()` just wants us to pass a character string.

  # Check that the time interval is less than the full length of the data
  fullDuration <- difftime(end, start, units = unit) # in same units as specified by the `unit` arg.
  if(int >= fullDuration){
    stop(paste0("Your time interval, ", compositeInterval, ", is greater than or equal to the full time span of the trimmed dataset: ", fullDuration, ". Please select a shorter time interval."))
  }

  # append the first and last dates to the data frame
  edges <- edges %>%
    tibble::add_row({{minTimestampCol}} := start, .before = 1) %>%
    tibble::add_row({{minTimestampCol}} := end)

  # Now time to separate the dates, using seq and group.
  ## First, we have to determine how many groups we're going to have.
  nGroups <- ceiling(as.numeric(fullDuration)/n)
  nBreaks <- nGroups + 1

  ## Now we can create a sequence, going either backwards or forwards, depending
  if(from == "end"){
    negInterval <- paste0("-", compositeInterval)
    breaksSequence <- rev(seq(from = end, length = nBreaks, by = negInterval))
    edges <- edges %>%
      dplyr::mutate(intervalGroup_lowerBound = cut.POSIXt(.data[[maxTimestampCol]],
                                                          breaks = breaksSequence, right = TRUE))
  }else{
    breaksSequence <- seq(from = start, length = nBreaks, by = compositeInterval)
    edges <- edges %>%
      dplyr::mutate(intervalGroup_lowerBound = cut.POSIXt(.data[[minTimestampCol]],
                                                          breaks = breaksSequence, right = TRUE))
  }

  # Split the data into a list by group
  dataList <- edges %>%
    dplyr::filter(!is.na(.data$ID1)) %>%
    dplyr::ungroup() %>%
    dplyr::select(.data[[id1Col]], .data[[id2Col]], .data$timegroup, .data$intervalGroup_lowerBound) %>%
    dplyr::group_by(.data$intervalGroup_lowerBound) %>%
    dplyr::group_split(.keep = T)

  # Name the list according to the interval groups
  nms <- purrr::map_chr(dataList, ~as.character(.x$intervalGroup_lowerBound[1]))
  names(dataList) <- nms

  # If `dropEdgeGroup` == T, then drop the first/last group
  if(dropEdgeGroup & from == "start"){
    dataList <- dataList[-length(dataList)]
  }else if(dropEdgeGroup & from == "end"){
    dataList <- dataList[-1]
  }

  # Return the final data
  return(dataList)
}

#' Make a single graph from an edge list.
#'
#' A wrapper of igraph::graph_from_data_frame. Takes into account the formatting returned from vultureUtils::get*Edges(). Has two modes--can either take an edgelist (optionally with a full list of vertices) or an SRI data frame.
#' @param mode One of "edgelist" (default; creates a graph from an edgelist) or "sri" (creates a graph from a data frame containing each dyad once, with its SRI value).
#' @param data a data frame containing edges and their associated `timegroup`s. Returned from vultureUtils::get*Edges().
#' @param weighted whether or not the resulting graphs should have weights attached. Default is FALSE (unweighted graph).
#' @param id1Col name of the column in `edges` containing the ID of the first individual in a dyad. Default is "ID1".
#' @param id2Col name of the column in `edges` containing the ID of the second individual in a dyad. Default is "ID2".
#' @param vertices optional. Either NULL (default) or a data frame/vector with vertex names and optional metadata. If a vector is provided, it will be coerced to a data frame (see documentation for igraph::graph_from_data_frame).
#' @return an igraph object. An undirected graph that is either weighted or unweighted, depending on whether `weighted` is T or F.
#' @export
makeGraph <- function(mode = "edgelist", data, weighted = FALSE,
                      id1Col = "ID1", id2Col = "ID2", vertices = NULL){
  # Some basic argument checks
  checkmate::assertSubset(mode, choices = c("edgelist", "sri"), empty.ok = FALSE)
  checkmate::assertLogical(weighted, len = 1)
  checkmate::assertSubset(id1Col, names(data))
  checkmate::assertSubset(id2Col, names(data))
  checkmate::assertCharacter(id1Col, len = 1)
  checkmate::assertCharacter(id2Col, len = 1)

  if(mode == "edgelist"){
    ## EDGELIST MODE
    # Simplify the edgelist to just the columns needed
    edgesSimple <- data %>%
      dplyr::select(.data[[id1Col]], .data[[id2Col]])

    # Make graph differently depending on `weighted`
    if(weighted == FALSE){
      # Make an unweighted graph
      edgesSimple <- edgesSimple %>%
        dplyr::distinct()
      g <- igraph::graph_from_data_frame(d = edgesSimple, directed = FALSE,
                                         vertices = vertices)
    }else{
      # Make a weighted graph
      edgesWeighted <- edgesSimple %>%
        dplyr::mutate(weight = 1) %>%
        dplyr::group_by(.data[[id1Col]],
                        .data[[id2Col]]) %>%
        dplyr::summarize(weight = sum(.data$weight)) %>%
        dplyr::ungroup()
      g <- igraph::graph_from_data_frame(d = edgesWeighted, directed = FALSE,
                                         vertices = vertices)
    }
  }else{
    checkmate::assertSubset("sri", names(data))
    # Simplify the edgelist to just the columns needed
    edgesSimple <- data %>%
      dplyr::select(.data[[id1Col]], .data[[id2Col]], sri) %>%
      dplyr::mutate("weight" = sri) %>% # calling the column "weight" will automatically add a weight attribute to the graph.
      dplyr::mutate(weight = dplyr::na_if(weight, 0)) # convert 0's to NA so the graph will lay out properly.

    ## SRI MODE
    verts <- unique(c(edgesSimple[[id1Col]], edgesSimple[[id2Col]])) # XXX this is hacky and I don't like how it's handled. Come back to this.
    g <- igraph::graph_from_data_frame(d = edgesSimple, directed = FALSE,
                                       vertices = verts)
  }

  # Remove edges that are 0 or NA
  if(weighted){
    g <- igraph::delete.edges(g, igraph::E(g)[igraph::E(g)$weight <= 0|is.na(igraph::E(g)$weight)])
  }

  # return the graph
  return(g)
}

