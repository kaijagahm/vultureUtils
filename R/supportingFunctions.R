# Supporting functions (called by main functions)
if(getRversion() >= "2.15.1")  utils::globalVariables(".") # this supposedly helps deal with some of the data masking issues with dplyr/tidyverse syntax.

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
#' @param sri T/F (default is T). Whether or not to include SRI calculation.
#' @return an edge list (data frame)
#' @export
# Convert to UTM
spaceTimeGroups <- function(dataset, distThreshold, consecThreshold = 2, crsToSet = "WGS84", timestampCol = "timestamp", timeThreshold = "10 minutes", idCol = "trackId", latCol = "location_lat", longCol = "location_long", returnDist = TRUE, fillNA = FALSE, sri = T){
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

    if(nrow(dataset) == 0){
      stop("Dataset passed to vultureUtils::spaceTimeGroups has 0 rows. Cannot proceed with grouping.")
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
  dataset <- spatsoc::group_times(dataset, datetime = timestampCol, threshold = timeThreshold)
  timegroupData <- dataset %>% # save information about when each timegroup starts and ends.
    dplyr::select(.data[[timestampCol]], timegroup) %>% # XXX this is deprecated, fix.
    dplyr::group_by(.data$timegroup) %>%
    dplyr::summarize(minTimestamp = min(.data[[timestampCol]]),
                     maxTimestamp = max(.data[[timestampCol]]))

  # Retain timestamps for each point, with timegroup information appending. This will be joined back at the end, to fix #43 and make individual points traceable.
  timestamps <- dataset %>%
    dplyr::select(.data[[timestampCol]], .data[[idCol]], timegroup)

  # Group into point groups (spatial)
  dataset <- spatsoc::group_pts(dataset, threshold = distThreshold, id = idCol,
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

  if(sri){
    # Calculate SRI
    cat("Computing SRI... this may take a while if your dataset is large.\n")
    start <- Sys.time()
    dfSRI <- calcSRI(dataset = dataset, edges = edgesFiltered)
    end <- Sys.time()
    duration <- end-start
    cat(paste0("SRI computation completed in ", duration, " seconds."))
    outList <- list("edges" = edgesFiltered, "sri" = dfSRI)
  }else{
    outList <- list("edges" = edgesFiltered)
  }
  # XXX need a step here where I join `timestamps` to `edgesFiltered`, in order to address #43. But for this to work, I have to decide what to do about the problem with some individuals showing up twice within the same 10-minute window.
  # Should I average their position during the window? Or should I pick just the first fix? Or should I compute the distance twice and if either of them is close enough to another individual, we consider it an edge? Very important to figure this out.
  return(outList)
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

#' Calculate SRI
#'
#' Calculates SRI based on timegroup and individual occurrence information
#' @param dataset the cleaned dataset
#' @param edges edgelist created by spatsoc, with self edges and duplicate edges removed.
#' @return A data frame containing ID1, ID2, and SRI value.
#' @export
calcSRI <- function(dataset, edges){
  checkmate::assertSubset("timegroup", names(dataset))
  checkmate::assertSubset("trackId", names(dataset))
  checkmate::assertDataFrame(dataset)
  checkmate::assertDataFrame(edges)

  edges <- as_tibble(edges)

  ## get individuals per timegroup as a list
  # Info about timegroups and individuals, for SRI calculation
  timegroupsList <- dataset[,c("timegroup", "trackId")] %>%
    mutate(trackId = as.character(trackId)) %>%
    distinct() %>%
    group_by(timegroup) %>%
    group_split() %>%
    map(~.x$trackId)

  ## get unique set of timegroups
  timegroups <- unique(dataset$timegroup)

  ## get all unique pairs of individuals
  inds <- as.character(unique(dataset$trackId))
  allPairs <- expand.grid(inds, inds, stringsAsFactors = F) %>%
    setNames(., c("ID1", "ID2")) %>%
    filter(ID1 < ID2)
  allPairsList <- allPairs %>%
    group_by(ID1, ID2) %>%
    group_split() %>%
    map(., as.matrix)

  # wide data
  datasetWide <- dataset %>%
    dplyr::select(timegroup, trackId) %>%
    distinct() %>%
    mutate(val = TRUE) %>%
    tidyr::pivot_wider(id_cols = timegroup, names_from = "trackId",
                values_from = "val", values_fill = FALSE)

  ## get SRI information
  dfSRI <- map_dfr(allPairsList, ~{
    # define the two individuals
    a <- .x[1]
    b <- .x[2]
    colA <- datasetWide[,a]
    colB <- datasetWide[,b]

    ya <- sum(colA & !colB)
    yb <- sum(colB & !colA)
    nBoth <- sum(colA & colB)
    x <- edges %>%
      filter(ID1 %in% c(a, b) & ID2 %in% c(a, b)) %>%
      pull(timegroup) %>%
      unique() %>%
      length()
    yab <- nBoth - x
    sri <- x/(x+ya+yb+yab)
    dfRow <- data.frame(ID1 = a, ID2 = b, sri = sri)
    return(dfRow)
  })

  return(dfSRI)
}
