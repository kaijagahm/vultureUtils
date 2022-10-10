if(getRversion() >= "2.15.1")  utils::globalVariables(".")

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
                             quiet = F){
  # argument checks
  checkmate::assertClass(loginObject, "MovebankLogin")
  checkmate::assertLogical(extraSensors, len = 1)
  checkmate::assertLogical(removeDup, len = 1)
  # Do the POSIXct converion if datetimes are character
  if(is.character(dateTimeStartUTC)){
    dateTimeStartUTC <- as.POSIXct(dateTimeStartUTC)
  }
  if(is.character(dateTimeEndUTC)){
    dateTimeStartUTC <- as.POSIXct(dateTimeEndUTC)
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
                                 timestamp_end = dateTimeEndUTC)))
  }else{
    dat <- move::getMovebankData(study = "Ornitela_Vultures_Gyps_fulvus_TAU_UCLA_Israel",
                                 login = loginObject,
                                 includeExtraSensors = FALSE,
                                 deploymentAsIndividuals = FALSE,
                                 removeDuplicatedTimestamps = TRUE,
                                 timestamp_start = dateTimeStartUTC,
                                 timestamp_end = dateTimeEndUTC)
  }

  if(addDateOnly == T){
    dat$dateOnly <- as.Date(as.character(dat$timestamp))
  }

  if(dfConvert == TRUE){
    dat <- methods::as(dat, "data.frame")
    # NOTE: DO NOT USE as.data.frame() HERE! The object `dat` being converted is a moveStack object (returned from the `move` R package). This is a special S4 class with defined slots (more info here: https://terpconnect.umd.edu/~egurarie/research/NWT/Step01_LoadingMovebankData.html). One of the slots is `trackId`. For reasons I don't fully understand, using `as.data.frame()` on a moveStack does not retain all the slot names--it will return a dataset that lacks the `trackId` column. But using `methods::as(dat, "data.frame")` converts tht `trackId` slot into a column in the data frame, giving us a `trackId` column that is a factor. We NEED the `trackId` column in order to proceed with other functions further down the pipeline, so it's super important not to use as.data.frame() here.
  }

  return(dat)
}

#' Remove unnecessary vars
#'
#' Remove variables we don't need, so the data is smaller
#' @param dataset a dataset to remove variables from. Must be a data frame.
#' @param addlVars a character vector of additional variables to remove
#' @param keepVars a character vector of variables to keep that would otherwise be removed.
#' @return A dataset, with variables removed
#' @export
removeUnnecessaryVars <- function(dataset, addlVars = NULL, keepVars = NULL){
  checkmate::assertDataFrame(dataset) # must be a data frame
  checkmate::assertCharacter(addlVars, null.ok = TRUE)
  checkmate::assertCharacter(keepVars, null.ok = TRUE)

  # combine `addlVars` with the original list of vars to remove
  toRemove <- c(addlVars, "sensor_type_id","taxon_canonical_name","nick_name","earliest_date_born","sensor","optional",
                "sensor_type","mw_activity_count","eobs_accelerations_raw","eobs_acceleration_sampling_frequency_per_axis",
                "eobs_acceleration_axes","argos_valid_location_algorithm","argos_sensor_4","argos_sensor_3","argos_sensor_2",
                "argos_sensor_1","argos_semi_minor","argos_semi_major","argos_pass_duration","argos_orientation","argos_nopc",
                "argos_lat1","argos_lat2","1084088","argos_lon1","argos_lon2","argos_nb_mes","argos_nb_mes_120",
                "eobs_key_bin_checksum","eobs_fix_battery_voltage","eobs_battery_voltage","eobs_status",
                "eobs_start_timestamp","eobs_type_of_fix","eobs_used_time_to_get_fix","eobs_temperature",
                "gps_dop","magnetic_field_raw_x","magnetic_field_raw_y","magnetic_field_raw_z","ornitela_transmission_protocol",
                "tag_voltage","algorithm_marked_outlier","argos_altitude","argos_best_level","argos_lc","argos_iq",
                "argos_gdop","argos_error_radius","argos_calcul_freq","timestamps","height_raw",
                "barometric_pressure","barometric_height","battery_charging_current","eobs_activity","manually_marked_outlier",
                "eobs_activity_samples", "acceleration_raw_y", "battery_charge_percent", "data_decoding_software","gps_vdop","height_above_ellipsoid",
                'acceleration_raw_x','acceleration_raw_z',"acceleration_raw_z","eobs_horizontal_accuracy_estimate","eobs_speed_accuracy_estimate")

  # get rid of any vars we'd like to keep
  toRemove <- toRemove[!(toRemove %in% keepVars)]

  # actually do the removal
  newDataset <- dataset %>%
    dplyr::select(-dplyr::any_of(toRemove))
  return(newDataset)
}

#' Mask dataset
#'
#' Given a mask and a dataset, apply the mask to the dataset and return only locations inside the mask.
#' @param dataset a dataset to mask
#' @param mask an sf object to use as the mask.
#' @param longCol the name of the column in the dataset containing longitude values
#' @param latCol the name of the column in the dataset containing latitude values
#' @param crs (To be passed to `st_set_crs()`). One of (i) character: a string accepted by GDAL, (ii) integer, a valid EPSG value (numeric), or (iii) an object of class crs.
#' @return A masked data set.
#' @export
maskData <- function(dataset, mask, longCol = "location_long", latCol = "location_lat", crs){
  # argument checks
  checkmate::assertClass(mask, "sf")
  checkmate::assertDataFrame(dataset)
  checkmate::assertCharacter(longCol, len = 1)
  checkmate::assertCharacter(latCol, len = 1)

  # check if the dataset is already an sf object
  issf <- checkmate::testClass(dataset, "sf")

  # if not, convert it to an sf object
  if(issf == FALSE){
    checkmate::assertSubset(x = c(longCol, latCol), choices = names(dataset))
    dataset_sf <- sf::st_as_sf(dataset, coords = c(longCol, latCol), remove = FALSE)
    dataset_sf <- sf::st_set_crs(dataset_sf, value = crs)
  }else{
    dataset_sf <- dataset
  }

  # check the CRS: is it the same as the mask CRS?
  same <- sf::st_crs(mask) == sf::st_crs(dataset_sf)
  if(!same){
    dataset_sf <- sf::st_transform(dataset_sf, crs = sf::st_crs(mask))
  }

  # mask the dataset
  masked <- dataset_sf[mask, , op = sf::st_intersects] # XXX i think i can speed this up if i I just use st_intersects directly, maybe?

  # return the masked dataset
  return(masked)
}

#' Which individuals spent time mostly in the masked area?
#'
#' Compare masked and unmasked data. Return IDs of individual birds that spent at least `thresh` proportion of days in the masked area (vs. outside of the masked area). This function should be used after creating a masked dataset with vultureUtils::maskData(). Note: this function does its calculations based on numbers of *days*. Later modifications might allow for other units of time, but for now everything is in days.
#' @param dataset the full dataset, before masking.
#' @param maskedDataset the dataset after being masked to Israel (output of maskIsrael function)
#' @param thresh proportion (between 0 and 1) of a vulture's total tracked days that it spent in Israel
#' @param dateCol the name of the column containing dates (must be the same in `dataset` and `maskedDataset`). Defaults to "dateOnly".
#' @return A vector of trackIds for vultures
#' @export
mostlyInMask <- function(dataset, maskedDataset, thresh = 0.333, dateCol = "dateOnly"){
  # argument checks
  checkmate::assertDataFrame(dataset)
  checkmate::assertDataFrame(maskedDataset)
  checkmate::assertNumeric(thresh, len = 1, lower = 0, upper = 1)
  checkmate::assertCharacter(dateCol, len = 1)
  checkmate::assertSubset("trackId", names(dataset))
  checkmate::assertSubset("trackId", names(maskedDataset))
  checkmate::assertSubset(dateCol, names(dataset))
  checkmate::assertSubset(dateCol, names(maskedDataset))
  # check that the `dateCol` columns actually are dates.
  checkmate::assertClass(dataset %>% dplyr::pull({{dateCol}}), "Date")
  checkmate::assertClass(maskedDataset %>% dplyr::pull({{dateCol}}), "Date")

  # Look at date durations in the full dataset
  dates <- dataset %>%
    dplyr::group_by(.data$trackId) %>%
    dplyr::summarize(duration = as.numeric(max(.data[[dateCol]],
                                               na.rm = T) - min(.data[[dateCol]],
                                                                na.rm = T)))


  # Look at date durations in the masked Israel dataset
  datesInMask <- maskedDataset %>%
    as.data.frame() %>%
    dplyr::group_by(.data$trackId) %>%
    dplyr::summarize(duration =
                       as.numeric(max(.data[[dateCol]], na.rm = T) -
                                    min(.data[[dateCol]], na.rm = T)))

  # Compare the two dates and calculate proportion
  datesCompare <- dplyr::left_join(dates, datesInMask %>%
                                     dplyr::select(.data$trackId,
                                                   "durationInMask" = .data$duration)) %>%
    dplyr::mutate(propInMask = .data$durationInMask/.data$duration) # compute proportion of days spent in the mask area

  whichInMaskLongEnough <- datesCompare %>%
    dplyr::filter(.data$propInMask > thresh) %>%
    dplyr::pull(.data$trackId) %>%
    unique()

  return(whichInMaskLongEnough)
}

#' Filter edge list to exclude too few consecutive occurrences
#'
#' This function takes an edge list and removes edges that don't occur in at least `consecThreshold` consecutive time groups. It expects an edge list containing *ONE-WAY* edges (i.e. with self edges already removed, and with duplicates not included--already reduced to A-B only, not A-B and B-A). If the edge list contains duplicate edges (A-B and B-A), they will be treated as if they were separate edges. Data must already include `timegroup`s.
#' @param edgeList edge list to work with, containing only A-B edges. No self edges, no B-A edges.
#' @param consecThreshold in how many consecutive time groups must the two individuals interact in order to be included? Default is 2.
#' @param id1Col Character. Name of the column containing the ID of the first individual
#' @param id2Col Character. Name of the column containing the ID of the second individual
#' @param timegroupCol Character. Name of the column containing time groups (integer values), returned by spatsoc functions
#' @param returnGroups whether to return the indices used to group runs of consecutive time slices for each dyad. Default is FALSE.
#' @return An edge list (data frame) containing only edges that occurred in at least `consecThreshold` consecutive time groups.
#' @export
consecEdges <- function(edgeList, consecThreshold = 2, id1Col = "ID1", id2Col = "ID2", timegroupCol = "timegroup", returnGroups = FALSE){
  # argument checks
  checkmate::assertDataFrame(edgeList)
  checkmate::assertNumeric(consecThreshold, len = 1)
  checkmate::assertCharacter(id1Col, len = 1)
  checkmate::assertCharacter(id2Col, len = 1)
  checkmate::assertChoice(id1Col, names(edgeList))
  checkmate::assertChoice(id2Col, names(edgeList))
  checkmate::assertCharacter(timegroupCol, len = 1)
  checkmate::assertChoice(timegroupCol, names(edgeList))
  checkmate::assertInteger(edgeList[[timegroupCol]])
  checkmate::assertLogical(returnGroups, len = 1)

  # do the filtering
  consec <- edgeList %>%
    # for each edge, arrange by timegroup
    dplyr::group_by(.data[[id1Col]], .data[[id2Col]]) %>%
    dplyr::arrange(.data[[timegroupCol]], .by_group = TRUE) %>%

    # create a new index grp that groups rows into consecutive runs
    dplyr::mutate("grp" = cumsum(c(1, diff(.data[[timegroupCol]]) != 1))) %>%
    dplyr::ungroup() %>%

    # group by the new `grp` column and remove any `grp`s that have less than `consecThreshold` rows (i.e. less than `consecThreshold` consecutive time groups for that edge)
    dplyr::group_by(.data[[id1Col]], .data[[id2Col]], .data$grp) %>%
    dplyr::filter(dplyr::n() >= consecThreshold) %>%
    dplyr::ungroup()

  # only return the group column if it's asked for.
  if(returnGroups == FALSE){
    consec <- consec %>%
      dplyr::ungroup() %>%
      dplyr::select(-.data$grp)
    return(consec)
  }else{
    return(consec)
  }
}

#' Filter locs
#'
#' Filter dataset for reasonableness
#' @param df a data frame to filter
#' @param speedThreshLower a single numeric value, the lower limit for ground speed to be included (m/s)
#' @param speedThreshUpper a single numeric value, the upper limit for ground speed to be included (m/s)
#' @return A list of filtered data frames.
#' @export
filterLocs <- function(df, speedThreshLower = NULL, speedThreshUpper = NULL){
  # argument checks
  checkmate::assertDataFrame(df)
  checkmate::assertChoice("gps_time_to_fix", names(df))
  checkmate::assertChoice("heading", names(df))
  checkmate::assertChoice("gps_satellite_count", names(df))
  checkmate::assertChoice("ground_speed", names(df))
  checkmate::assertNumeric(speedThreshLower, null.ok = TRUE, len = 1)
  checkmate::assertNumeric(speedThreshUpper, null.ok = TRUE, len = 1)

  # filter out bad gps data
  df <- df %>%
    dplyr::filter(.data$gps_time_to_fix <= 89)

  # filter out bad heading data
  df <- df %>%
    dplyr::filter(.data$heading < 360 & .data$heading > 0) # only reasonable headings, between 0 and 360.

  # only take locs that have at least 3 satellites
  df <- df %>%
    dplyr::filter(.data$gps_satellite_count >= 3) # must have at least 3 satellites in order to triangulate.

  # Apply speed filters
  # if no speed thresholds are set, warn that we're not applying filtering.
  if(is.null(speedThreshLower) & is.null(speedThreshUpper)){
    warning("No speed thresholds set, so data will not be filtered for speed.")
  }

  # if at least one threshold is set, apply filtering
  if(!is.null(speedThreshLower)){
    df <- df %>%
      dplyr::filter(.data$ground_speed > speedThreshLower)
  }
  if(!is.null(speedThreshUpper)){
    df <- df %>%
      dplyr::filter(.data$ground_speed < speedThreshUpper)
  }
  return(df)
}

#' Buffer feeding sites
#'
#' Buffer feeding sites by a specified number of meters
#' @param feedingSites a data frame or an sf object
#' @param feedingBuffer how much to buffer the feeding site, in m
#' @param crsToSet if `feedingSites` is a data frame, what CRS to pass to sf::st_set_crs() (NOT transform!). If `feedingSites` is already an sf object, `crsToSet` will be overridden by whatever the object's CRS is, unless it is NA.
#' @param crsToReturn either "WGS84" (default) or "m" (returns in 32636, aka UTM36)
#' @param latCol if `feedingSites` is a data frame, the name of the column to use for latitude
#' @param longCol if `feedingSites` is a data frame, the name of the column to use for longitude
#' @return sf object containing feeding site polygons, buffered
#' @export
bufferFeedingSites <- function(feedingSites, feedingBuffer = 100,
                               crsToSet = "WGS84",
                               crsToReturn = "WGS84", latCol = "lat",
                               longCol = "long"){

  # Set up an sf object for use.
  if("sf" %in% class(feedingSites)){ # If feedingSites is an sf object...
    if(is.na(sf::st_crs(feedingSites))){ # only fill in crs if it is missing
      message(paste0("`feedingSites` is already an sf object but has no CRS. Setting CRS to ", crsToSet, "."))
      feedingSites <- sf::st_set_crs(feedingSites, crsToSet)
    }
  }else if(is.data.frame(feedingSites)){ # otherwise, if feedingSites is a data frame...
    # make sure it contains the lat and long cols
    checkmate::assertChoice(latCol, names(feedingSites))
    checkmate::assertChoice(longCol, names(feedingSites))

    # convert to an sf object
    feedingSites <- feedingSites %>%
      sf::st_as_sf(coords = c(longCol, latCol), remove = FALSE) %>%
      sf::st_set_crs(crsToSet) # assign the CRS

  }else{ # otherwise, throw an error.
    stop("`feedingSites` must be a data frame or an sf object.")
  }

  # Transform to meter-based CRS, using UTM region 36 (Israel), which is 32636.
  feedingSitesMeters <- feedingSites %>%
    sf::st_transform(32636)

  # Buffer
  checkmate::assertNumeric(feedingBuffer, len = 1)
  feedingSitesBuffered <- sf::st_buffer(feedingSitesMeters, feedingBuffer) %>%
    sf::st_union() %>%
    sf::st_cast("POLYGON") # make the buffered areas into polygons

  # Return polygons in the appropriate crs
  toReturn <- feedingSitesBuffered %>%
    sf::st_transform(crsToReturn)

  return(toReturn)
}

#' Group points by space and time
#'
#' This function takes a data frame of filtered, cleaned points and uses functions from `spatsoc` to group them by space and time. Then it uses vultureUtils::consecEdges to remove edges that don't occur in enough consecutive time groups.
#' @param dataset a data frame, filtered by speed etc, to use to create spatiotemporal groups.
#' @param distThreshold distance threshold at which to consider that two individuals are interacting (m).
#' @param consecThreshold Passed to vultureUtils::consecEdges. In how many consecutive time groups must the two individuals interact in order to be included? Default is 2.
#' @param crsToSet if `feedingSites` is a data frame, what CRS to pass to sf::st_set_crs() (NOT transform!). If `feedingSites` is already an sf object, `crsToSet` will be overridden by whatever the object's CRS is, unless it is NA.
#' @param timestampCol Passed to spatsoc::group_times. Name of date time column(s). either 1 POSIXct or 2 IDate and ITime. e.g.: 'datetime' or c('idate', 'itime')
#' @param timeThreshold Passed to spatsoc::group_times. Threshold for grouping times. e.g.: '2 hours', '10 minutes', etc. if not provided, times will be matched exactly. Note that provided threshold must be in the expected format: '## unit'.
#' @param idCol Name of the column containing individual ID's of the vultures.
#' @param latCol Name of the column containing latitude values
#' @param longCol Name of the column containing longitude values
#' @param returnDist Passed to spatsoc::edge_dist. Boolean indicating if the distance between individuals should be returned. If FALSE (default), only ID1, ID2 columns (and timegroup, splitBy columns if provided) are returned. If TRUE, another column "distance" is returned indicating the distance between ID1 and ID2. Default is TRUE.
#' @param fillNA Passed to spatsoc::edge_dist. Boolean indicating if NAs should be returned for individuals that were not within the threshold distance of any other. If TRUE, NAs are returned. If FALSE, only edges between individuals within the threshold distance are returned. Default is FALSE.
#' @return an edge list (data frame)
#' @export
# Convert to UTM
spaceTimeGroups <- function(dataset, distThreshold, consecThreshold = 2, crsToSet = "WGS84", timestampCol = "timestamp", timeThreshold = "10 minutes", idCol = "trackId", latCol = "location_lat", longCol = "location_long", returnDist = TRUE, fillNA = FALSE){
  # argument checks
  checkmate::assertDataFrame(dataset)
  checkmate::assertNumeric(distThreshold, len = 1, lower = 0, finite = TRUE)
  checkmate::assertNumeric(consecThreshold, len = 1, lower = 0)
  checkmate::assertCharacter(timestampCol, len = 1)
  checkmate::assertCharacter(timeThreshold, len = 1)
  checkmate::assertCharacter(idCol, len = 1)
  checkmate::assertCharacter(latCol, len = 1)
  checkmate::assertCharacter(longCol, len = 1)
  checkmate::assertLogical(returnDist, len = 1)
  checkmate::assertLogical(fillNA, len = 1)

  # Set up an sf object for use.
  if("sf" %in% class(dataset)){ # If dataset is an sf object...
    if(is.na(sf::st_crs(dataset))){ # only fill in crs if it is missing
      message(paste0("`dataset` is already an sf object but has no CRS. Setting CRS to ", crsToSet, "."))
      dataset <- sf::st_set_crs(dataset, crsToSet)
    }
  }else if(is.data.frame(dataset)){ # otherwise, if feedingSites is a data frame...
    # make sure it contains the lat and long cols
    checkmate::assertChoice(latCol, names(dataset))
    checkmate::assertChoice(longCol, names(dataset))

    # convert to an sf object
    dataset <- dataset %>%
      sf::st_as_sf(coords = c(.data[[longCol]], .data[[latCol]]), remove = FALSE) %>%
      sf::st_set_crs(crsToSet) # assign the CRS

  }else{ # otherwise, throw an error.
    stop("`dataset` must be a data frame or an sf object.")
  }

  # Save lat and long coords, in case we need them later. Then, convert to UTM.
  dataset <- dataset %>%
    dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                  lat = sf::st_coordinates(.)[,2]) %>%
    sf::st_transform(32636) %>% # convert to UTM: we'll need this for calculating distance later.
    dplyr::mutate(utmE = sf::st_coordinates(.)[,1],
                  utmN = sf::st_coordinates(.)[,2]) %>%
    sf::st_drop_geometry() # spatsoc won't work if this is still an sf object.

  # Convert the timestamp column to POSIXct.
  dataset <- dataset %>%
    dplyr::mutate({{timestampCol}} := as.POSIXct(.data[[timestampCol]], tz = "UTC"))

  # Convert to a data table for spatsoc.
  data.table::setDT(dataset)

  # Group the points into timegroups using spatsoc::group_times.
  spatsoc::group_times(dataset, datetime = timestampCol, threshold = timeThreshold)
  timegroupData <- dataset %>% # save information about when each timegroup starts and ends.
    dplyr::select(.data[[timestampCol]], .data$timegroup) %>%
    dplyr::group_by(.data$timegroup) %>%
    dplyr::summarize(minTimestamp = min(.data[[timestampCol]]),
                     maxTimestamp = max(.data[[timestampCol]]))

  # Group into point groups (spatial)
  spatsoc::group_pts(dataset, threshold = distThreshold, id = idCol,
                     coords = c("utmE", "utmN"), timegroup = "timegroup")

  # Generate edge lists by timegroup
  edges <- spatsoc::edge_dist(DT = dataset, threshold = distThreshold, id = idCol,
                              coords = c("utmE", "utmN"), timegroup = "timegroup",
                              returnDist = returnDist, fillNA = fillNA)

  # Remove self and duplicate edges
  edges <- edges %>%
    dplyr::filter(as.character(.data$ID1) < as.character(.data$ID2))

  # Now create a list where the edge only stays if it occurred in at least `consecThreshold` consecutive time steps.
  edgesFiltered <- consecEdges(edgeList = edges, consecThreshold = consecThreshold) %>%
    dplyr::ungroup()

  # Join to the timegroup data
  edgesFiltered <- edgesFiltered %>%
    dplyr::left_join(timegroupData, by = "timegroup")

  return(edgesFiltered)
}

#' Make a list of networks, given a time interval
#'
#' Inputs: a data frame containing time-grouped edges, and given an interval for making the networks. This function separates the data frame into a list of edge lists according to the provided time interval. Then, it generates a network (weighted or unweighted) for each of the edge lists. Note that depending on whether or not `interval` divides evenly into the range between `dateTimeStart` and `dateTimeEnd`, the last graph may be created from a smaller time period than the other graphs.
#' @param edges a data frame containing edges and their associated `timegroup`s.
#' @param interval A character string specifying an interval such as "3 days" or "2 hours" or "1 month" or "1.5 hours" or "3 days 2 hours". Interval will be coerced to a duration object using lubridate::as.duration()
#' @param dateTimeStart the dateTime object that defines the beginning of the time period to be divided. If not specified, defaults to the earliest `timestamp` in `fullData`. Must be in one of the following formats: "YYYY-MM-DD hh:mm:ss" or "YYYY-MM-DD hh:mm" or "YYYY-MM-DD". Hours must use 24 hour time--e.g. 5:00 pm would be 17:00.
#' @param dateTimeEnd the dateTime object that defines the end of the time period to be divided. If not specified, defaults to the latest `timestamp` in `fullData`. Must be in one of the following formats: "YYYY-MM-DD hh:mm:ss" or "YYYY-MM-DD hh:mm" or "YYYY-MM-DD". Hours must use 24 hour time--e.g. 5:00 pm would be 17:00.
#' @param id1Col name of the column in `edges` containing the ID of the first individual in a dyad
#' @param id2Col name of the column in `edges` containing the ID of the second individual in a dyad
#' @param weighted whether or not the resulting graphs should have weights attached
#' @param minTimestampCol the name of the column to use for assigning the minimum timestamp, if no `dateTimeStart` is provided. Default is "minTimestamp".
#' @param maxTimestampCol the name of the column to use for assigning the maximum timestamp, if no `dateTimeEnd` is provided. Default is "maxTimestamp".
#' @param allVertices whether to include all possible vertices in all graphs produced, even if they are not involved in any edges in the current time slice. Default is FALSE.
#' @return A list of igraph graph objects
#' @export
makeGraphs <- function(edges, interval, dateTimeStart = NULL,
                       dateTimeEnd = NULL, id1Col = "ID1", id2Col = "ID2",
                       weighted = FALSE, minTimestampCol = "minTimestamp",
                       maxTimestampCol = "maxTimestamp", allVertices = FALSE){
  # Some basic argument checks
  checkmate::assertLogical(weighted, len = 1)
  checkmate::assertCharacter(minTimestampCol, len = 1)
  checkmate::assertCharacter(maxTimestampCol, len = 1)
  checkmate::assertClass(edges[[minTimestampCol]], "POSIXct")
  checkmate::assertClass(edges[[maxTimestampCol]], "POSIXct")

  # Check that the user-defined time interval is coercible to a duration
  int <- lubridate::as.duration(interval)
  if(is.na(int)){
    stop("Argument `interval` could not be expressed as a duration: lubridate::as.duration() returned NA. Please make sure you are specifying a valid interval, such as '1 day', '3 hours', '2 weeks', etc.")
  }
  checkmate::assertClass(int, "Duration")
  # Note that even though we checked if this was coercible to a duration, we're not actually going to *use* the object `int`, because `cut()` just wants us to pass a character string.

  # Either assign dateTimeStart and dateTimeEnd, or coerce the user-provided inputs to lubridate datetimes.
  if(is.null(dateTimeStart)){
    dateTimeStart <- min(edges[[minTimestampCol]])
    warning(paste0("No start datetime provided. Using earliest timestamp, which is ", dateTimeStart, "."))
  }
  if(is.null(dateTimeEnd)){
    dateTimeEnd <- max(edges[[maxTimestampCol]])
    warning(paste0("No end datetime provided. Using latest timestamp, which is ", dateTimeEnd, "."))
  }

  start <- lubridate::parse_date_time(dateTimeStart, orders = c("%Y%m%d %H%M%S", "%Y%m%d %H%M", "%Y%m%d"))
  if(is.na(start)){
    stop("`dateTimeStart` could not be parsed. Please make sure you have used one of the following formats: YYYY-MM-DD hh:mm:ss, YYYY-MM-DD hh:mm, or YYYY-MM-DD.")
  }
  end <- lubridate::parse_date_time(dateTimeEnd, orders = c("%Y%m%d %H%M%S", "%Y%m%d %H%M", "%Y%m%d"))
  if(is.na(end)){
    stop("`dateTimeEnd` could not be parsed. Please make sure you have used one of the following formats: YYYY-MM-DD hh:mm:ss, YYYY-MM-DD hh:mm, or YYYY-MM-DD.")
  }

  # Get a vector of all the vertices that appear in the entire dataset.
  allVerticesVec <- sort(unique(c(edges[[id1Col]], edges[[id2Col]])))

  # Separate sequences by user-defined time interval
  # append the first and last dates to the data frame
  edges <- edges %>%
    tibble::add_row("minTimestamp" = start, .before = 1) %>%
    tibble::add_row("maxTimestamp" = end)

  # Now use `cut` and `seq` to group the data
  edges <- edges %>%
    dplyr::mutate(intervalGroup = lubridate::floor_date(.data$minTimestamp, unit = eval(interval)))

  # Split the data into a list
  dataList <- edges %>%
    dplyr::filter(!is.na(.data$ID1)) %>%
    dplyr::ungroup() %>%
    dplyr::select(.data[[id1Col]], .data[[id2Col]], .data$timegroup, .data$intervalGroup) %>%
    dplyr::group_by(.data$intervalGroup) %>%
    dplyr::group_split(.keep = T)

  nms <- unlist(lapply(dataList, function(x){
    as.character(utils::head(x$intervalGroup, 1))
  }))
  # XXX would make MUCH more sense to just call lapply on a function, instead of having the function makeGraphsList rely on having a list passed to it. that would also simplify the allVerticesVec argument because for each one you could pass in a complete list of vertices if allVertices == FALSE. Do this later.

  # Now make the networks, calling vultureUtils::makeGraphsList().
  networks <- makeGraphsList(dataList = dataList, weighted = weighted, id1Col = id1Col, id2Col = id2Col, allVertices = TRUE, allVerticesVec = allVerticesVec)

  # Name the networks by the breaks
  names(networks$graphs) <- nms

  # return the list of graphs and associated data
  return(networks)
}

#' Make a list of graphs
#'
#' Creates a list of igraph graphs (and the data to go along with them) from a list of edgelists.
#' @param dataList a list of edge lists (data frames).
#' @param weighted whether or not the resulting graphs should have weights attached
#' @param id1Col name of the column containing the ID of the first individual in a dyad
#' @param id2Col name of the column containing the ID of the second individual in a dyad
#' @param allVertices whether to include all possible vertices in all graphs produced, even if they are not involved in any edges in the current time slice. Default is FALSE
#' @param allVerticesVec vector to use to define all possible vertices.
#' @return A list of igraph graph objects
#' @export
#'
makeGraphsList <- function (dataList, weighted = FALSE, id1Col = "ID1", id2Col = "ID2", allVertices = FALSE, allVerticesVec = NULL)
{
  # If we specify all vertices, then we need a vector containing the names of all vertices.
  if(allVertices){
    checkmate::assertVector(allVerticesVec, null.ok = FALSE)
  }

  # Simplify the list down to just the columns needed
  simplified <- lapply(dataList, function(x) {
    x <- x %>% dplyr::select(.data[[id1Col]], .data[[id2Col]])
  })

  # Make graphs differently depending on whether weighted == FALSE or weighted == TRUE.
  if (weighted == FALSE) {
    simplified <- lapply(simplified, function(x) {
      x <- x %>% dplyr::distinct()
    })
    gs <- lapply(simplified, function(x) {
      if(allVertices){
        igraph::graph_from_data_frame(d = x, directed = FALSE,
                                      vertices = allVerticesVec)
      }else{
        igraph::graph_from_data_frame(d = x, directed = FALSE)
      }
    })
  }
  else {
    simplified <- lapply(simplified, function(x) {
      x <- x %>% dplyr::mutate(weight = 1) %>% dplyr::group_by(.data[[id1Col]],
                                                               .data[[id2Col]]) %>% dplyr::summarize(weight = sum(.data$weight)) %>%
        dplyr::ungroup()
    })
    gs <- lapply(simplified, function(x) {
      if(allVertices){
        igraph::graph_from_data_frame(d = x, directed = FALSE,
                                      vertices = allVerticesVec)
      }else{
        igraph::graph_from_data_frame(d = x, directed = FALSE)
      }
    })
  }
  return(list(graphs = gs, simplifiedData = simplified))
}

#' Create plots from a list of graphs
#'
#' Given a list of graphs, make plots. Optionally, use a consistent layout to visualize change over time.
#' @param graphList a list of graph objects.
#' @param coords either "fixed" (use same coordinates for all plots, to visualize change over time), or "free" (different coords for each plot)
#' @return A list of plot objects
#' @export
plotGraphs <- function(graphList, coords = "fixed"){
  checkmate::assertList(graphList)
  checkmate::assertSubset(coords, c("fixed", "free"))

  # For fixed coords:
  if(coords == "fixed"){
    # Get coordinates to use
    bigGraph <- do.call(igraph::union, graphList)
    xy <- as.data.frame(igraph::layout_nicely(bigGraph))
    row.names(xy) <- names(igraph::V(bigGraph))

    # Make a list to store the plots
    plotList <- lapply(graphList, function(x){
      verts <- names(igraph::V(x))
      coords <- xy[verts,]
      p <- ggraph::ggraph(x, layout = "manual", x = coords[,1], y = coords[,2])+
        ggraph::geom_edge_link()+
        ggraph::geom_node_point(shape = 19, size = 6)+
        ggraph::theme_graph()
      return(p)
    })
  }

  # For free coords:
  if(coords == "free"){
    # Make a list of plots
    plotList <- lapply(graphList, function(x){
      p <- ggraph::ggraph(x)+
        ggraph::geom_edge_link()+
        ggraph::geom_node_point(shape = 19, size = 6)+
        ggraph::theme_graph()
      return(p)
    })
  }

  return(plotList)
}

#' Make a gif from a graph list
#'
#' Given a list of plots, create a gif.
#' @param plotList a list of plots
#' @param fileName where to write the file. Must end with .gif.
#' @param interval a positive number to set the time interval of the animation (unit in seconds); default to 0.1.
#' @return A gif file
#' @export
makeGIF <- function(plotList, fileName, interval = 0.1){
  checkmate::assertList(plotList)
  checkmate::assertNumeric(interval, len = 1)
  pb <- utils::txtProgressBar(min = 0, max = length(plotList),
                              initial = 0, style = 3)
  # Save the gif
  animation::saveGIF({
    for(i in 1:length(plotList)){
      plot(plotList[[i]])
      utils::setTxtProgressBar(pb, i)
    }
  },
  movie.name = fileName,
  interval = interval)
}

#' Compute probabilities of gaining and losing edges.
#'
#' Given a list of graphs, compute the probability at each time step of gaining or losing edges, looking two timesteps back.
#' @param graphList a list of network graphs
#' @return a long-format data frame containing probabilities of losing/adding edges
#' @export
computeProbs <- function(graphList){
  # Get a complete list of all possible edges
  completeGraph <- do.call(igraph::union, graphList)
  complete_edgelist <- expand.grid(names(igraph::V(completeGraph)),
                                   names(igraph::V(completeGraph))) %>%
    dplyr::filter(as.character(.data$Var1) < as.character(.data$Var2)) %>%
    as.matrix()

  # get edge list for each element of the graph list
  els <- lapply(graphList, igraph::get.edgelist)

  # for each graph, check whether each edge is present or not
  tf <- lapply(els, function(x){
    complete_edgelist %in% x
  })

  # bind into a data frame showing presence/absence of edges over time.
  overTime <- do.call(cbind, tf) %>% as.data.frame()

  # add two blank steps before (all FALSE), and add the data for the individuals that make up each edge.
  beforeSteps <- data.frame(stepPrevPrev = rep(FALSE, nrow(overTime)),
                            stepPrev = rep(FALSE, nrow(overTime)))
  overTime <- cbind(stats::setNames(as.data.frame(complete_edgelist),
                                    c("ID1", "ID2")), beforeSteps, overTime) # this is our final history data frame

  # Create a data frame of probabilities based on overTime
  histdf <- data.frame("add00" = NA, "add10" = NA, "lose01" = NA, "lose11" = NA)
  for(i in 5:ncol(overTime)){
    vec <- vector(mode = "character", nrow(overTime))
    vec[which(!overTime[,i-2] & !overTime[,i-1])] <- "hist00"
    vec[which(!overTime[,i-2] & overTime[,i-1])] <- "hist01"
    vec[which(overTime[,i-2] & !overTime[,i-1])] <- "hist10"
    vec[which(overTime[,i-2] & overTime[,i-1])] <- "hist11"

    # compute the probabilities
    add00 <- sum(overTime[i] & vec == "hist00")/sum(vec == "hist00")
    add10 <- sum(overTime[i] & vec == "hist10")/sum(vec == "hist10")
    lose01 <- sum(!overTime[i] & vec == "hist01")/sum(vec == "hist01")
    lose11 <- sum(!overTime[i] & vec == "hist11")/sum(vec == "hist11")

    histdf[i-4,] <- c("add00" = add00, "add10" = add10, "lose01" = lose01, "lose11" = lose11)
  }

  histdfLong <- histdf %>%
    dplyr::mutate(earlyDate = names(overTime)[-1:-4]) %>%
    tidyr::pivot_longer(cols = -.data$earlyDate, names_to = "type", values_to = "prob")

  return(histdfLong)
}

#' Buffer an sf object
#'
#' Given an sf object in WGS84, convert it to a CRS with meters as the units, buffer by a given distance, and then convert it back.
#' @param obj an sf object to be buffered
#' @param dist buffer distance, in meters (m)
#' @param crsMeters crs with units of meters to be used. Default is 32636 (Israel, UTM zone 36)
#' @return A buffered sf object
#' @export
convertAndBuffer <- function(obj, dist = 50, crsMeters = 32636){
  checkmate::assertClass(obj, "sf")
  originalCRS <- sf::st_crs(obj)
  if(is.null(originalCRS)){
    stop("Object does not have a valid CRS.")
  }

  converted <- obj %>%
    sf::st_transform(crsMeters)

  buffered <- converted %>%
    sf::st_buffer(dist = dist)

  convertedBack <- buffered %>%
    sf::st_transform(originalCRS)

  return(convertedBack)
}
