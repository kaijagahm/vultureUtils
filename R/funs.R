#' Download vulture data
#'
#' Download vulture project data from the Israel vulture study Movebank repository, with some minor specifications. Note that you must specify your movebank credentials. This is a wrapper function for move::getMovebankData() that includes the study name hard-coded in, so you don't have to type it each time. If you need to get data for a different study, just use move::getMovebankData() directly.
#' @param loginObject A Movebank login object, created by passing a username and password to move::movebankLogin. Passed to `login` in move::getMovebankData().
#' @param extraSensors Whether to include extra sensors. Defaults to FALSE. Passed to `includeExtraSensors` in move::getMovebankData().
#' @param removeDup Whether to remove duplicated timestamps. Defaults to TRUE. Passed to `removeDuplicatedTimestamps` in move::getMovebankData().
#' @param dateTimeStartUTC a POSIXct object, in UTC. Will be converted to character assuming UTC. Passed to `timestamp_start` in move::getMovebankData().
#' @param dateTimeEndUTC a POSIXct object, in UTC. Will be converted to character assuming UTC. Passed to `timestamp_end` in move::getMovebankData().
#' @return A movestack.
#' @examples
#' # Get data from all of 2021
#' downloadVultures(MB.LoginObject, dateTimeStartUTC = as.POSIXct("2021-01-01 00:00"),
#' dateTimeEndUTC = as.POSIXct("2021-12-31 23:59"))
#'
#' # Get data for just two individuals, with no date restrictions
#' downloadVultures(MB.LoginObject, animalName = c("A09w", "A10w"))
#' @export
downloadVultures <- function(loginObject, extraSensors = F, removeDup = T,
                             dateTimeStartUTC = NULL, dateTimeEndUTC = NULL, ...){
  # argument checks
  checkmate::assertClass(loginObject, "MovebankLogin")
  checkmate::assertLogical(extraSensors, len = 1)
  checkmate::assertLogical(removeDup, len = 1)
  checkmate::assertPOSIXct(dateTimeStartUTC, null.ok = TRUE)
  checkmate::assertPOSIXct(dateTimeEndUTC, null.ok = TRUE)

  move::getMovebankData(study = "Ornitela_Vultures_Gyps_fulvus_TAU_UCLA_Israel",
                               login = loginObject,
                               includeExtraSensors = FALSE,
                               deploymentAsIndividuals = FALSE,
                               removeDuplicatedTimestamps = TRUE,
                               timestamp_start = dateTimeStartUTC,
                               timestamp_end = dateTimeEndUTC,
                               ...)
}

#' Remove unnecessary vars
#'
#' Remove variables we don't need, so the data is smaller
#' @param dataset a dataset to remove variables from. Must be a data frame.
#' @return A dataset, with variables removed
#' @examples
#' removeUnnecessaryVars(datDF)
#' @export
removeUnnecessaryVars <- function(dataset){
  checkmate::assertDataFrame(dataset) # must be a data frame
  newDataset <- dataset %>%
    dplyr::select(-any_of(c("sensor_type_id","taxon_canonical_name","nick_name","earliest_date_born","sensor","optional",
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
    ))
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
#' @examples
#' maskData(dataset = filteredData, mask = israelMask, longCol = "location_long.1", latCol = "location_lat.1", crs = "WGS84")
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
  masked <- dataset_sf[mask, , op = sf::st_intersects]

  # return the masked dataset
  return(masked)
}

#' Which mostly in Israel?
#'
#' Only include individuals that spent a certain proportion of their total time tracked in Israel
#' @param dataset the full dataset, before masking to Israel only
#' @param israelDataset the dataset after being masked to Israel (output of maskIsrael function)
#' @param thresh proportion (between 0 and 1) of a vulture's total tracked days that it spent in Israel
#' @param dateCol the name of the column containing dates (must be the same in `dataset` and `israelDataset`). Defaults to "dateOnly".
#' @return A vector of trackIds for vultures
#' @export
mostlyInIsrael <- function(dataset, israelDataset, thresh = 0.333, dateCol = "dateOnly"){
  # check that the datasets contain "trackId" and `dateCol` columns
  checkmate::assertSubset("trackId", names(dataset))
  checkmate::assertSubset("trackId", names(israelDataset))
  checkmate::assertSubset(dateCol, names(dataset))
  checkmate::assertSubset(dateCol, names(israelDataset))
  # check that the `dateCol` columns actually are dates.
  checkmate::assertClass(datDF %>% dplyr::pull({{dateCol}}), "Date")
  checkmate::assertClass(datDFIsrael %>% dplyr::pull({{dateCol}}), "Date")

  # Look at date durations in the full dataset
  dates <- dataset %>%
    dplyr::group_by(trackId) %>%
    dplyr::summarize(duration =
                       as.numeric(max(.data[[dateCol]], na.rm = T) -
                                    min(.data[[dateCol]], na.rm = T)))

  # Look at date durations in the masked Israel dataset
  datesInIsrael <- datDFIsrael %>%
    as.data.frame() %>%
    dplyr::group_by(trackId) %>%
    dplyr::summarize(duration =
                       as.numeric(max(.data[[dateCol]], na.rm = T) -
                                    min(.data[[dateCol]], na.rm = T)))

  # Compare the two dates and calculate proportion
  datesCompare <- dplyr::left_join(dates, datesInIsrael %>%
                                     dplyr::select(trackId,
                                                   "durationIsrael" = duration)) %>%
    dplyr::mutate(propIsrael = durationIsrael/duration) # compute proportion of days spent in Israel

  whichInIsraelLongEnough <- datesCompare %>%
    dplyr::filter(propIsrael > thresh) %>%
    dplyr::pull(trackId) %>%
    unique()

  return(whichInIsraelLongEnough)
}

#' Filter edge list to exclude too few consecutive occurrences
#'
#' This function takes an edge list (a data frame) containing *ONE-WAY* edges (i.e. with self edges already removed, and with duplicates not included--already reduced to A-B only, not A-B and B-A). If the edge list contains duplicate edges (A-B and B-A), they will be treated separately. Data must already include timegroups.
#' @param edgeList edge list to work with
#' @param consecThreshold in how many consecutive time groups must the two individuals interact in order to be included?
#' @param id1Col column containing the ID of the first individual
#' @param id2Col column containing the ID of the second individual
#' @param timegroupCol column containing time groups (integer values), returned by spatsoc functions
#' @param returnGroups whether to return the indices used to group runs of consecutive time slices for each dyad. Default is FALSE.
#' @return An edge list (data frame) containing only edges that occurred in at least `consecThreshold` consecutive time groups.
#' @export
consecEdges <- function(edgeList, consecThreshold = 2, id1Col = "ID1", id2Col = "ID2", timegroupCol = "timegroup", returnGroups = FALSE){
  checkmate::assertDataFrame(edgeList)
  checkmate::assertInteger(edgeList[[timegroupCol]])

  # do the filtering
  consec <- edges %>%
    # for each edge, arrange by timegroup
    dplyr::group_by(.data[[id1Col]], .data[[id2Col]]) %>%
    dplyr::arrange(.data[[timegroupCol]], .by_group = TRUE) %>%

    # create a new index grp that groups rows into consecutive runs
    dplyr::mutate(grp = cumsum(c(1, diff(.data[[timegroupCol]]) != 1))) %>%
    dplyr::ungroup() %>%

    # group by the new `grp` column and remove any `grp`s that have less than `consecThreshold` rows (i.e. less than `consecThreshold` consecutive time groups for that edge)
    dplyr::group_by(.data[[id1Col]], .data[[id2Col]], grp) %>%
    dplyr::filter(dplyr::n() >= consecThreshold)

  if(returnGroups == FALSE){
    consec <- consec %>%
      dplyr::select(-grp)
    return(consec)
  }else{
    return(consec)
  }
}


#' Clean data and extract metadata
#'
#' Extract metadata by tag, and filter/clean the dataset for speed, gps satellites, and heading
#' @param df a data frame to filter
#' @param speedLower a single numeric value, the lower limit for ground speed to be included (m/s)
#' @param speedUpper a single numeric value, the upper limit for ground speed to be included (m/s)
#' @return A list: "filteredData" = filtered dataset; "tagsMetadata" = tag-by-tag metadata
#' @export
cleanAndMetadata <- function(df, speedLower = NULL, speedUpper = NULL){
  checkmate::assertDataFrame(df)
  checkmate::assertNumeric(speedLower, null.ok = TRUE, len = 1)
  checkmate::assertNumeric(speedUpper, null.ok = TRUE, len = 1)
  checkmate::assertChoice("trackId", names(df))

  # Split the dataset by individual
  indivsList <- df %>%
    as.data.frame() %>%
    dplyr::mutate(trackId = as.character(trackId)) %>%
    dplyr::group_by(trackId) %>%
    split(f = as.factor(.$trackId))

  # how many points for each individual, initially?
  initialPoints <- unlist(lapply(indivsList, nrow))

  # Apply filtering: speed, gps satellites, and heading
  indivsListFiltered <- lapply(indivsList, function(x){
    vultureUtils::filterLocs(x, speedThreshUpper = speedUpper,
                             speedThreshLower = speedLower)
  })

  # how many points per individual after filtering?
  finalPoints <- unlist(lapply(indivsListFiltered, nrow))

  # save some metadata by tag
  tagsMetadata1 <- data.frame(trackId = names(indivsList),
                              initialPoints = initialPoints,
                              finalPoints = finalPoints,
                              propRemoved = (initialPoints-finalPoints)/initialPoints,
                              row.names = NULL)

  # save the rest of the metadata
  indivsDFFiltered <- data.table::rbindlist(indivsListFiltered)
  tagsMetadata2 <- indivsDFFiltered %>%
    dplyr::group_by(trackId) %>%
    dplyr::summarize(firstDay = min(dateOnly, na.rm = T),
                     lastDay = max(dateOnly, na.rm = T),
                     trackDurationDays = length(unique(dateOnly)), # number of unique days tracked
                     daysBetweenStartEnd = lastDay - firstDay) # total date range over which the individual was tracked

  # join by trackId
  if(nrow(tagsMetadata1) == nrow(tagsMetadata2)){
    tagsMetadata <- dplyr::left_join(tagsMetadata1, tagsMetadata2, by = "trackId")
  }else{
    stop("Row counts don't match!")
  }

  # return
  return(list("filteredData" = indivsDFFiltered,
              "tagsMetadata" = tagsMetadata))
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
  # check that df is a data frame
  checkmate::assertDataFrame(df)

  # filter out bad gps data
  checkmate::assertChoice("gps_time_to_fix", names(df))
  df <- df %>%
    dplyr::filter(gps_time_to_fix <= 89)

  # filter out bad heading data
  checkmate::assertChoice("heading", names(df))
  df <- df %>%
    dplyr::filter(heading < 360 & heading > 0) # only reasonable headings, between 0 and 360.

  # only take locs that have at least 3 satellites
  checkmate::assertChoice("gps_satellite_count", names(df))
  df <- df %>%
    dplyr::filter(gps_satellite_count >= 3) # must have at least 3 satellites in order to triangulate.

  # Check threshold values for speed, and apply speed filters
  checkmate::assertChoice("ground_speed", names(df))
  checkmate::assertNumeric(speedThreshLower, null.ok = TRUE, len = 1)
  checkmate::assertNumeric(speedThreshUpper, null.ok = TRUE, len = 1)

  # if no speed thresholds are set, warn that we're not applying filtering.
  if(is.null(speedThreshLower) & is.null(speedThreshUpper)){
    warning("No speed thresholds set, so data will not be filtered for speed.")
  }

  # if at least one threshold is set, apply filtering
  if(!is.null(speedThreshLower)){
    df <- df %>%
      dplyr::filter(ground_speed > speedThreshLower)
  }
  if(!is.null(speedThreshUpper)){
    df <- df %>%
      dplyr::filter(ground_speed < speedThreshUpper)
  }
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

#' Make a list of networks, given a time interval
#'
#' Inputs: a data frame containing time-grouped edges, and given an interval for making the networks. This function separates the data frame into a list of edge lists according to the provided time interval. Then, it generates a network (weighted or unweighted) for each of the edge lists. Note that depending on whether or not `interval` divides evenly into the range between `dateTimeStart` and `dateTimeEnd`, the last graph may be created from a smaller time period than the other graphs.
#' @param edges a data frame containing edges and their associated `timegroup`s.
#' @param fullData a data frame containing the `timegroup` column and `timestamp`s of vulture relocations. This will be used to classify timegroups by interval.
#' @param interval A character string specifying an interval such as "3 days" or "2 hours" or "1 month" or "1.5 hours" or "3 days 2 hours". Interval will be coerced to a duration object using lubridate::as.duration()
#' @param dateTimeStart the dateTime object that defines the beginning of the time period to be divided. If not specified, defaults to the earliest `timestamp` in `fullData`. Must be in one of the following formats: "YYYY-MM-DD hh:mm:ss" or "YYYY-MM-DD hh:mm" or "YYYY-MM-DD". Hours must use 24 hour time--e.g. 5:00 pm would be 17:00.
#' @param dateTimeEnd the dateTime object that defines the end of the time period to be divided. If not specified, defaults to the latest `timestamp` in `fullData`. Must be in one of the following formats: "YYYY-MM-DD hh:mm:ss" or "YYYY-MM-DD hh:mm" or "YYYY-MM-DD". Hours must use 24 hour time--e.g. 5:00 pm would be 17:00.
#' @param id1Col name of the column in `edges` containing the ID of the first individual in a dyad
#' @param id2Col name of the column in `edges` containing the ID of the second individual in a dyad
#' @param weighted whether or not the resulting graphs should have weights attached
#' @return A list of igraph graph objects
#' @export
makeGraphs <- function(edges, fullData, interval, dateTimeStart = NULL,
                       dateTimeEnd = NULL, id1Col = "ID1", id2Col = "ID2",
                       weighted = FALSE){
  # Some basic argument checks
  checkmate::assertLogical(weighted, len = 1)

  # use `fulldata` to get min and max timestamps for each timegroup.
  checkmate::assertDataFrame(fullData)
  checkmate::assertChoice("timegroup", names(fullData))
  checkmate::assertChoice("timestamp", names(fullData))
  timegroupInfo <- fullData %>%
    dplyr::select(timegroup, timestamp) %>%
    dplyr::group_by(timegroup) %>%
    dplyr::summarize(minDatetime = min(timestamp),
                     maxDatetime = max(timestamp))

  # Check that the user-defined time interval is coercible to a duration
  int <- lubridate::as.duration(interval)
  if(is.na(int)){
    stop("Argument `interval` could not be expressed as a duration: lubridate::as.duration() returned NA. Please make sure you are specifying a valid interval, such as '1 day', '3 hours', '2 weeks', etc.")
  }
  checkmate::assertClass(int, "Duration")

  # Either assign dateTimeStart and dateTimeEnd, or coerce the user-provided inputs to lubridate datetimes.
  if(is.null(dateTimeStart)){
    dateTimeStart <- min(fullData$timestamp)
    warning(paste0("No start datetime provided. Using earliest `timestamp` from `fullData`, which is ", dateTimeStart, "."))
  }
  if(is.null(dateTimeEnd)){
    dateTimeEnd <- max(fullData$timestamp)
    warning(paste0("No end datetime provided. Using latest `timestamp` from `fullData`, which is ", dateTimeEnd, "."))
  }

  start <- lubridate::parse_date_time(dateTimeStart, orders = c("%Y%m%d %H%M%S", "%Y%m%d %H%M", "%Y%m%d"))
  if(is.na(start)){
    stop("`dateTimeStart` could not be parsed. Please make sure you have used one of the following formats: YYYY-MM-DD hh:mm:ss, YYYY-MM-DD hh:mm, or YYYY-MM-DD.")
  }
  end <- lubridate::parse_date_time(dateTimeEnd, orders = c("%Y%m%d %H%M%S", "%Y%m%d %H%M", "%Y%m%d"))
  if(is.na(end)){
    stop("`dateTimeEnd` could not be parsed. Please make sure you have used one of the following formats: YYYY-MM-DD hh:mm:ss, YYYY-MM-DD hh:mm, or YYYY-MM-DD.")
  }

  # Separate sequences by user-defined time interval
  # append the first and last dates to the data frame
  timegroupInfo <- timegroupInfo %>%
    tibble::add_row(minDatetime = start, .before = 1) %>%
    tibble::add_row(minDatetime = end)

  # Now use `cut` and `seq` to group the data
  breaks <- seq(from = start, to = end, by = int)
  groupedTimegroups <- timegroupInfo %>%
    dplyr::mutate(interval = cut(minDatetime, breaks)) %>%
    dplyr::select(timegroup, interval)

  # Join this information to the original data
  checkmate::assertDataFrame(edges)
  checkmate::assertChoice(id1Col, names(edges))
  checkmate::assertChoice(id2Col, names(edges))
  checkmate::assertChoice("timegroup", names(edges))
  dataList <- edges %>%
    ungroup() %>%
    dplyr::select(.data[[id1Col]], .data[[id2Col]], timegroup) %>%
    dplyr::left_join(groupedTimegroups, by = "timegroup") %>%
    dplyr::group_by(interval) %>%
    dplyr::group_split()

  # Now make the networks, calling vultureUtils::makeGraphsList().
  networks <- vultureUtils::makeGraphsList(dataList = dataList, weighted = weighted, id1Col = id1Col, id2Col = id2Col)

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
#' @return A list of igraph graph objects
#' @export
makeGraphsList <- function(dataList, weighted = FALSE, id1Col = "ID1", id2Col = "ID2"){
  # Simplify the list down to just the columns needed
  simplified <- lapply(dataList, function(x){
    x <- x %>%
      dplyr::select(.data[[id1Col]], .data[[id2Col]])
  })

  # Make graphs differently depending on whether weighted == FALSE or weighted == TRUE.
  if(weighted == FALSE){
    simplified <- lapply(simplified, function(x){
      x <- x %>%
        dplyr::distinct()
    })
    gs <- lapply(simplified, function(x){
      igraph::graph_from_data_frame(d = x, directed = FALSE)
    })
  }else{
    simplified <- lapply(simplified, function(x){
      x <- x %>%
        dplyr::mutate(weight = 1) %>%
        dplyr::group_by(.data[[id1Col]], .data[[id2Col]]) %>%
        dplyr::summarize(weight = sum(weight)) %>%
        dplyr::ungroup()
    })
    gs <- lapply(simplified, function(x){
      igraph::graph_from_data_frame(d = x, directed = FALSE)
    })
  }

  # return a list of graphs and the data to go along with them
  return(list("graphs" = gs, "simplifiedData" = simplified))
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
    row.names(xy) <- names(V(bigGraph))

    # Make a list to store the plots
    plotList <- lapply(graphList, function(x){
      verts <- names(V(x))
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

