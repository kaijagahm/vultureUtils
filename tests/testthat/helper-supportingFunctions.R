data_to_points_helper <- function(dataset, roostPolygons, roostBuffer = 50, speedThreshLower, speedThreshUpper, daytimeOnly = T){

  ## FILTER THE POINTS
  # Restrict interactions based on ground speed
  filteredData <- vultureUtils::filterLocs(df = dataset,
                                           speedThreshUpper = speedThreshUpper,
                                           speedThreshLower = speedThreshLower)

  # If roost polygons were provided, use them to filter out data
  if(!is.null(roostPolygons)){
    # Buffer the roost polygons
    roostPolygons <- convertAndBuffer(roostPolygons, dist = roostBuffer)

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
  }
  return(points)
}

points_to_edgelist_helper <- function(dataset, distThreshold, crsToSet = "WGS84", crsToTransform = 32636, timestampCol = "timestamp", timeThreshold = "10 minutes", idCol = "tag_id", latCol = "location_lat", longCol = "location_long", returnDist = TRUE, fillNA = FALSE){
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
      sf::st_as_sf(coords = c(.data[[longCol]], .data[[latCol]]), remove = FALSE) %>%
      sf::st_set_crs(crsToSet) # assign the CRS

  }else{ # otherwise, throw an error.
    stop("`dataset` must be a data frame or an sf object.")
  }

  # Save lat and long coords, in case we need them later. Then, convert to UTM.
  dataset <- dataset %>%
    sf::st_transform(crsToTransform)
  dataset$utmE <- unlist(purrr::map(dataset$geometry, 1))
  dataset$utmN <- unlist(purrr::map(dataset$geometry, 2))
  datset <- sf::st_drop_geometry(dataset) # spatsoc won't work if this is still an sf object.

  # Convert the timestamp column to POSIXct.
  dataset <- dataset %>%
    dplyr::mutate({{timestampCol}} := as.POSIXct(.data[[timestampCol]], format = "%Y-%m-%d %H:%M:%OS", tz = "UTC"))

  # Convert to a data table for spatsoc.
  data.table::setDT(dataset)

  # Group the points into timegroups using spatsoc::group_times.
  dataset <- spatsoc::group_times(dataset, datetime = timestampCol, threshold = timeThreshold)
  timegroupData <- dataset %>%
    dplyr::select(tidyselect::all_of(timestampCol), timegroup) %>% # save information about when each timegroup starts and ends.
    dplyr::group_by(timegroup) %>%
    dplyr::summarize(minTimestamp = min(.data[[timestampCol]], na.rm = T),
                     maxTimestamp = max(.data[[timestampCol]], na.rm = T))

  # Retain timestamps for each point, with timegroup information appending. This will be joined back at the end, to fix #43 and make individual points traceable.
  timestamps <- dataset[,c(timestampCol, idCol, "timegroup")]

  # Generate edge lists by timegroup
  edges <- spatsoc::edge_dist(DT = dataset, threshold = distThreshold, id = idCol,
                              coords = c("utmE", "utmN"), timegroup = "timegroup",
                              returnDist = returnDist, fillNA = T)

  # Remove self and duplicate edges
  edges <- edges %>%
    dplyr::filter(as.character(.data$ID1) < as.character(.data$ID2))

  return(list(edges, dataset, timegroupData))
}

parameter_calcSRI_helper <- function(dataset, edgesFiltered, timegroupData, idCol = "tag_id", latCol = "location_lat", longCol = "location_long"){
  # Join to the timegroup data
  edgesFiltered <- edgesFiltered %>%
    dplyr::left_join(timegroupData, by = "timegroup")

  # Compute interaction locations
  ## get locations of each individual at each time group
  locs <- dataset %>%
    tibble::as_tibble() %>%
    dplyr::select(tidyselect::all_of(c(idCol, "timegroup", latCol, longCol))) %>%
    dplyr::distinct() %>%
    dplyr::mutate(across(tidyselect::all_of(c(latCol, longCol)), as.numeric))

  # In case there is more than one point per individual per timegroup, get the mean.
  meanLocs <- locs %>%
    dplyr::group_by(across(all_of(c(idCol, "timegroup")))) %>%
    dplyr::summarize(mnLat = mean(.data[[latCol]], na.rm = T),
                     mnLong = mean(.data[[longCol]], na.rm = T))

  ef <- edgesFiltered %>%
    dplyr::left_join(meanLocs, by = c("ID1" = idCol, "timegroup")) %>%
    dplyr::rename("latID1" = mnLat, "longID1" = mnLong) %>%
    dplyr::left_join(meanLocs, by = c("ID2" = idCol, "timegroup")) %>%
    dplyr::rename("latID2" = mnLat, "longID2" = mnLong) %>%
    dplyr::mutate(interactionLat = (latID1 + latID2)/2,
                  interactionLong = (longID1 + longID2)/2)

  if(!(nrow(ef) == nrow(edgesFiltered))){
    stop("wrong number of rows") # XXX need a better way of preventing and handling this error.
  }
  edgesFiltered <- ef
  return(ef)
}
