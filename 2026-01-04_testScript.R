# 2026-01-04 test script
library(here)
library(tidyverse)
library(sf)
library(vultureUtils)

#source(here("feeding_and_flight_functions_May_06_2025.R"))

test <- readRDS(here("tests/testDataKaija/test.RDS"))
dates <- unique(test$dateOnly)
# smallertest <- test[test$dateOnly %in% dates[1:2],]
# dim(smallertest)

#--------
# Some code from Elvira's script to satisfy the getEdges_EDB function, which isn't entirely working/self-contained.
rp <- sf::st_read("tests/testthat/testdata/roosts50_kde95_cutOffRegion.kml") # needs to have roosts passed to it, because the default is not NULL

all_Nili_ids <- as.character(unique(test$Nili_id))
allPairs_entire_season <- expand.grid(ID1 = all_Nili_ids, ID2 = all_Nili_ids, stringsAsFactors = FALSE)
ap <- allPairs_entire_season

getEdges_EDB <- function(dataset,
                     roostPolygons = NULL, # 20260105 KG change--this needs to be NULL so we don't get an error.
                     roostBuffer,
                     consecThreshold,
                     distThreshold,
                     speedThreshUpper,
                     speedThreshLower,
                     timeThreshold = "10 minutes",
                     idCol = "Nili_id",
                     quiet = T,
                     includeAllVertices = F,
                     daytimeOnly = T,
                     return = "sri",
                     getLocs = FALSE,
                     speedCol = "ground_speed",
                     timestampCol = "timestamp",
                     allPairs_entire_season = allPairs_entire_season # 20260104: I added this argument because it was otherwise just drawing from the global environment.
                     ){

  #SAVE RAW DATASET COPY for later use
  dataset_rawdata <- dataset  #backup raw dataset

  #CONVERT timestampCol to POSIXct datetime
  dataset <- dataset %>%
    dplyr::mutate({{ timestampCol }} := as.POSIXct(.data[[timestampCol]], format = "%Y-%m-%d %H:%M:%OS", tz = "UTC"))

  #Convert dataset to data.table (needed for spatsoc functions)
  data.table::setDT(dataset)

  #GROUP POINTS INTO TIMEGROUPS (using spatsoc)
  dataset <- spatsoc::group_times(dataset, datetime = timestampCol, threshold = timeThreshold)
  dataset_denominator <- dataset  #save a copy of the timegrouped dataset for the SRI denominator, since we need to know how many timegroups individuals appear in vs. interact in

  #RECORD START/END TIME PER TIMEGROUP
  timegroupData <- dataset %>%
    dplyr::select(tidyselect::all_of(timestampCol), timegroup) %>%
    dplyr::group_by(timegroup) %>%
    dplyr::summarize(minTimestamp = min(.data[[timestampCol]], na.rm = TRUE),
                     maxTimestamp = max(.data[[timestampCol]], na.rm = TRUE))

  #CONVERT (back) TO sf OBJECT WITH LAT/LONG GEOMETRY
  #(needed for spatial filtering later)
  dataset_sf <- sf::st_as_sf(dataset, coords = c("location_long", "location_lat"), crs = "WGS84", remove = FALSE)

  #VALIDATE CRS / CONVERT TO sf IF NEEDED
  if ("sf" %in% class(dataset_sf)) {
    if (is.na(sf::st_crs(dataset_sf))) {
      message(paste0("`dataset_sf` has no CRS. Setting CRS to WGS84."))
      dataset_sf <- sf::st_set_crs(dataset_sf, "WGS84")
    }
  } else if (is.data.frame(dataset_sf)) {
    checkmate::assertChoice("location_lat", names(dataset_sf))
    checkmate::assertChoice("location_long", names(dataset_sf))
    if (nrow(dataset_sf) == 0) stop("dataset_sf has 0 rows.")
    dataset_sf <- dataset_sf %>%
      sf::st_as_sf(coords = c(.data[["location_long"]], .data[["location_lat"]]), remove = FALSE) %>%
      sf::st_set_crs("WGS84")
  } else {
    stop("`dataset_sf` must be a data frame or sf object.")
  }

  #WARN IF GETLOCS + return="sri" (invalid combination)
  if (getLocs & return == "sri") {
    warning("Cannot return interaction locations when return = 'sri'. Use return = 'edges' or 'both'.")
  }

  #------------------------------------------------------------
  #SAVE UNIQUE INDIVIDUALS BEFORE FILTERING (optional)
  #------------------------------------------------------------
  if (includeAllVertices) {
    uniqueIndivs <- unique(dataset_sf[[idCol]])
  }


  #------------------------------------------------------------
  #FILTER OUT POINTS INSIDE ROOST POLYGONS
  #------------------------------------------------------------
  #Check if the dataset is already an sf object
  is_sf <- checkmate::testClass(dataset_sf, "sf")

  #if not, convert it to an sf object
  if(is_sf == FALSE){
    #Convert to sf object
    dataset_sf <- sf::st_as_sf(dataset_sf, coords = c("location_long", "location_lat"), crs ="WGS84", remove = F) #XXX this is fragile for now but whatever
  }else{
    dataset_sf <- dataset_sf
  }

  if (!is.null(roostPolygons)) {
    if (!is.null(roostBuffer)) {
      roostPolygons <- convertAndBuffer(roostPolygons, dist = roostBuffer)  #buffer polygons
    }
    removedRoosts_dataset <- dataset_sf[lengths(sf::st_intersects(dataset_sf, roostPolygons)) == 0, ]
  } else {
    message("No roost polygons provided; skipping spatial exclusion.")
    removedRoosts_dataset <- dataset_sf
  }

  #FILTER FOR DAYTIME ONLY (if requested)
  if (daytimeOnly) {
    times <- suncalc::getSunlightTimes(date = unique(lubridate::date(removedRoosts_dataset$timestamp)),
                                       lat = 31.434306, lon = 34.991889,
                                       keep = c("sunrise", "sunset")) %>%
      dplyr::select(date, sunrise, sunset)

    removedRoosts_dataset <- removedRoosts_dataset %>%
      #remove leftover sunrise/sunset cols just in case
      {if("sunrise" %in% names(.)) dplyr::select(., -sunrise) else .}%>%
      {if("sunset" %in% names(.)) dplyr::select(., -sunset) else .}%>%
      dplyr::left_join(times, by = c("dateOnly" = "date")) %>%
      dplyr::mutate(daytime = dplyr::case_when(timestamp > .data$sunrise &
                                                 timestamp < .data$sunset ~ TRUE,
                                               TRUE ~ FALSE))

    # Remove nighttime points
    nNightPoints <- nrow(removedRoosts_dataset[removedRoosts_dataset$daytime == F,])
    dataset_dayonly <- removedRoosts_dataset %>%
      dplyr::filter(daytime == TRUE)
    nDayPoints <- nrow(dataset_dayonly)
    if(quiet == FALSE){
      cat(paste0("Removed ", nNightPoints, " nighttime points, leaving ",
                 nDayPoints, " points.\n"))
    }
  }

  #FILTER BASED ON SPEED (if thresholds set)
  filteredData <- filterLocs(df = dataset_dayonly,
                             speedThreshUpper = speedThreshUpper,
                             speedThreshLower = speedThreshLower,
                             speedCol = speedCol)


  #BUILD ALL POSSIBLE PAIRS (DYADS) FOR THE DAY
  all_ids_day <- as.character(unique(dataset[[idCol]]))
  allPairs_day <- expand.grid(ID1 = all_ids_day, ID2 = all_ids_day, stringsAsFactors = FALSE) %>%
    dplyr::mutate(pair = paste(ID1, ID2, sep = "_")) %>%
    dplyr::filter(ID1 != ID2)

  #PREPARE SEASONAL PAIR LIST
  allPairs_entire_season_output <- allPairs_entire_season %>%
    dplyr::mutate(pair = paste(ID1, ID2, sep = "_")) %>%
    dplyr::filter(ID1 != ID2) %>%
    dplyr::mutate(sri = ifelse(pair %in% allPairs_day$pair, NA, NA))

  #HANDLE EMPTY DATA AFTER FILTERING
  if (nrow(filteredData) == 0) {
    warning("After filtering, no data remains.")
    all_ids_day_filteredData <- unique(dataset_rawdata[[idCol]])
    allPairs_day_filteredData <- expand.grid(ID1 = all_ids_day_filteredData, ID2 = all_ids_day_filteredData, stringsAsFactors = FALSE) %>%
      dplyr::mutate(pair_filteredData = paste(ID1, ID2, sep = "_"), sri = NA) %>%
      dplyr::filter(ID1 != ID2)

    if (nrow(allPairs_day_filteredData) == 0) {
      allPairs_day_filteredData <- allPairs_entire_season_output %>% dplyr::select(ID1, ID2, sri)
    } else {
      matching_indices <- allPairs_day_filteredData$pair_filteredData %in% allPairs_entire_season_output$pair
      allPairs_day_filteredData$sri[matching_indices] <- 0
    }

    return(data.frame(ID1 = allPairs_day_filteredData$ID1,
                      ID2 = allPairs_day_filteredData$ID2,
                      sri = allPairs_day_filteredData$sri))
  }


  #------------------------------------------------------------
  #CALL spaceTimeGroups() FUNCTION # 20260104 KG this now calls spaceTimeGroups_EDB instead
  #- return either edges or SRI
  #------------------------------------------------------------
  if(nrow(filteredData) != 0){

    ##Do we need to compute SRI?
    if(return == "edges"){ #if SRI is not needed, we can save time by not computing it.
      if(quiet){
        ###EDGES ONLY, QUIET
        # 20260104: KG renamed function call here from spaceTimeGroups to spaceTimeGroups_EDB
        out <- suppressMessages(suppressWarnings(spaceTimeGroups_EDB(dataset = filteredData,
                                                                 sriDenominatorDataset = dataset_denominator, #XXX added this
                                                                 distThreshold = distThreshold,
                                                                 allPairs_entire_season_output= allPairs_entire_season_output,
                                                                 allPairs_day=allPairs_day,
                                                                 consecThreshold = consecThreshold,
                                                                 timeThreshold = timeThreshold,
                                                                 sri = FALSE,
                                                                 idCol = idCol,
                                                                 timegroupData = timegroupData)))
      }else{
        ###EDGES ONLY, WARNINGS
        # 20260104: KG renamed function call here from spaceTimeGroups to spaceTimeGroups_EDB
        #compute edges without suppressing warnings
        out <- spaceTimeGroups_EDB(dataset = filteredData,
                               sriDenominatorDataset = dataset_denominator, #XXX added this
                               distThreshold = distThreshold,
                               allPairs_entire_season_output= allPairs_entire_season_output,
                               allPairs_day=allPairs_day,
                               consecThreshold = consecThreshold,
                               timeThreshold = timeThreshold,
                               sri = FALSE,
                               idCol = idCol,
                               timegroupData = timegroupData)
      }

    }else if(return %in% c("sri", "both")){ #otherwise we need to compute SRI.
      if(quiet){
        ###EDGES AND SRI, QUIET
        # 20260104: KG renamed function call here from spaceTimeGroups to spaceTimeGroups_EDB
        #suppress warnings while computing edges and SRI, returning a list of edges+sri
        out <- suppressMessages(suppressWarnings(spaceTimeGroups_EDB(dataset = filteredData,
                                                                 sriDenominatorDataset = dataset_denominator, #XXX added this
                                                                 distThreshold = distThreshold,
                                                                 allPairs_entire_season_output= allPairs_entire_season_output,
                                                                 allPairs_day=allPairs_day,
                                                                 consecThreshold = consecThreshold,
                                                                 timeThreshold = timeThreshold,
                                                                 sri = TRUE,
                                                                 idCol = idCol,
                                                                 timegroupData = timegroupData)))
        if(return == "sri"){
          out <- out["sri"]
        }
      }else{
        ###EDGES AND SRI, WARNINGS
        # 20260104: KG renamed function call here from spaceTimeGroups to spaceTimeGroups_EDB
        #compute edges and SRI without suppressing warnings, returning a list of edges+sri
        out <- spaceTimeGroups_EDB(dataset = filteredData,
                               sriDenominatorDataset = dataset_denominator, #XXX added this
                               distThreshold = distThreshold,
                               allPairs_entire_season_output= allPairs_entire_season_output,
                               allPairs_day=allPairs_day,
                               consecThreshold = consecThreshold,
                               timeThreshold = timeThreshold,
                               sri = TRUE,
                               idCol = idCol,
                               timegroupData = timegroupData)
        if(return == "sri"){
          out <- out["sri"]
        }
      }
    }
  } #close the if(nrow(filteredData) != 0)

  #------------------------------------------------------------
  #REMOVE LOCATION COLUMNS IF getLocs = FALSE
  #------------------------------------------------------------
  locsColNames <- c("latID1", "longID1", "latID2", "longID2", "interactionLat", "interactionLong")
  if (!getLocs & return %in% c("edges", "both")) {
    if (!is.list(out)) {
      out <- dplyr::select(out, -any_of(locsColNames))
    } else if ("edges" %in% names(out)) {
      out$edges <- dplyr::select(out$edges, -any_of(locsColNames))
    }
  }


  #------------------------------------------------------------
  #OPTIONALLY APPEND VERTEX LIST
  #------------------------------------------------------------
  if (includeAllVertices) {
    toReturn <- append(out, list(as.character(uniqueIndivs)))
  } else {
    toReturn <- out
  }

  #------------------------------------------------------------
  #RETURN FINAL OUTPUT
  #------------------------------------------------------------
  if (length(toReturn) == 1) toReturn <- toReturn[[1]]
  return(toReturn)
}

# 20260104 parameter def block for testing-------
dataset <- test
roostPolygons = rp
roostBuffer = 1000
consecThreshold = 1
distThreshold = 1000
speedThreshUpper = NULL
speedThreshLower = 5
timeThreshold = "10 minutes"
idCol = "Nili_id"
quiet = T
includeAllVertices = F
daytimeOnly = T
return = "sri"
getLocs = FALSE
speedCol = "ground_speed"
# 20260104 \parameter def block for testing-------

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

spaceTimeGroups_EDB <- function(dataset,
                            sriDenominatorDataset,
                            distThreshold,
                            #add it here, too
                            allPairs_entire_season_output,
                            allPairs_day,
                            consecThreshold = 2,
                            crsToSet = "WGS84",
                            crsToTransform = 32636,
                            timestampCol = "timestamp",
                            timeThreshold = "10 minutes",
                            idCol = "Nili_id",
                            latCol = "location_lat",
                            longCol = "location_long",
                            returnDist = TRUE,
                            fillNA = FALSE,
                            sri = T,
                            timegroupData){
  #-----------------------------
  #CHECK AND PREPARE sf OBJECT
  #-----------------------------

  if ("sf" %in% class(dataset)) {            #If input is already an sf object
    if (is.na(sf::st_crs(dataset))) {        #But CRS is missing
      message(paste0("`dataset` is already an sf object but has no CRS. Setting CRS to ", crsToSet, "."))
      dataset <- sf::st_set_crs(dataset, crsToSet)  #Assign CRS
    }
  } else if (is.data.frame(dataset)) {       #If input is a regular data frame
    checkmate::assertChoice(latCol, names(dataset))  #Ensure lat column exists
    checkmate::assertChoice(longCol, names(dataset)) #Ensure long column exists

    if (nrow(dataset) == 0) {
      stop("Dataset passed to spaceTimeGroups has 0 rows. Cannot proceed with grouping.")
    }

    #Convert to sf object using coordinates
    dataset <- dataset %>%
      sf::st_as_sf(coords = c(.data[[longCol]], .data[[latCol]]), remove = FALSE) %>%
      sf::st_set_crs(crsToSet)   #Assign CRS
  } else {
    stop("`dataset` must be a data frame or an sf object.")
  }

  #-----------------------------
  #TRANSFORM TO UTM & EXTRACT COORDS
  #-----------------------------
  dataset <- dataset %>% sf::st_transform(crsToTransform)  #Convert CRS to meters
  dataset$utmE <- purrr::map_dbl(dataset$geometry, 1)      #Extract UTM Easting
  dataset$utmN <- purrr::map_dbl(dataset$geometry, 2)      #Extract UTM Northing
  dataset <- sf::st_drop_geometry(dataset)                 #Remove geometry for spatsoc compatibility

  #-----------------------------
  #BUILD INITIAL EDGE LIST (per timegroup)
  #-----------------------------
  edges <- spatsoc::edge_dist(DT = dataset,
                              threshold = distThreshold,
                              id = idCol,
                              coords = c("utmE", "utmN"),
                              timegroup = "timegroup",
                              returnDist = returnDist,
                              fillNA = TRUE)
  # 20260104 KG: After this step, there will be some NAs in ID2. This is because the argument `fillNA = TRUE` fills in rows for individuals that were not within the threshold distance of any other individuals within the given timegroup.
  # The problem seems to be that these are carrying forward into the sri calculation in a way that doesn't make sense...

  #-----------------------------
  #REMOVE SELF-LOOPS AND DUPLICATES
  #-----------------------------
  edges_without_duplicate <- edges %>%
    dplyr::filter(is.na(ID1) | is.na(ID2) | as.character(ID1) < as.character(ID2))

  #-----------------------------
  #FILTER EDGES BY CONSECUTIVE APPEARANCES
  #-----------------------------
  edgesFiltered <- consecEdges(edgeList = edges_without_duplicate,
                               consecThreshold = consecThreshold) %>%
    dplyr::ungroup()

  #-----------------------------
  #HANDLE EMPTY RESULT (no valid interactions)
  #-----------------------------
  if (nrow(edgesFiltered) == 0) {
    warning("After edgesFiltered, the dataset had 0 rows.")

    #Prepare output dataframes with NA or 0 SRI
    allPairs_day_no_interaction <- allPairs_day
    colnames(allPairs_day_no_interaction)[3] <- "pair_no_interaction"   #Rename 3rd column

    allPairs_day_season <- allPairs_entire_season_output                #Copy full season pairs

    if (!"edges" %in% names(allPairs_day_season)) {                     #Add edges column if missing
      allPairs_day_season$edges <- NA
    }

    #Set SRI = 0 for pairs with no interaction
    matching_indices <- allPairs_day_season$pair %in% allPairs_day_no_interaction$pair_no_interaction
    allPairs_day_season$sri[matching_indices] <- 0

    #Return empty but valid output
    return(list(
      "edges" = data.frame(
        ID1   = allPairs_day_season$ID1,
        ID2   = allPairs_day_season$ID2,
        edges = allPairs_day_season$edges
      ),
      "sri" = data.frame(
        ID1 = allPairs_day_season$ID1,
        ID2 = allPairs_day_season$ID2,
        sri = allPairs_day_season$sri
      )
    ))
  }

  #-----------------------------
  #ALCULATE INTERACTION LOCATIONS
  #-----------------------------
  locs <- dataset %>%
    tibble::as_tibble() %>%
    dplyr::select(tidyselect::all_of(c(idCol, "timegroup", latCol, longCol))) %>%
    dplyr::distinct() %>%
    dplyr::mutate(across(tidyselect::all_of(c(latCol, longCol)), as.numeric))

  #Take mean lat/long if multiple records per individual/timegroup
  meanLocs <- locs %>%
    dplyr::group_by(across(all_of(c(idCol, "timegroup")))) %>%
    dplyr::summarize(mnLat = mean(.data[[latCol]], na.rm = TRUE),
                     mnLong = mean(.data[[longCol]], na.rm = TRUE))

  #Join mean locations for ID1 and ID2 in edges
  ef <- edgesFiltered %>%
    dplyr::left_join(meanLocs, by = c("ID1" = idCol, "timegroup")) %>%
    dplyr::rename("latID1" = mnLat, "longID1" = mnLong) %>%
    dplyr::left_join(meanLocs, by = c("ID2" = idCol, "timegroup")) %>%
    dplyr::rename("latID2" = mnLat, "longID2" = mnLong) %>%
    dplyr::mutate(interactionLat = (latID1 + latID2)/2,
                  interactionLong = (longID1 + longID2)/2)

  #Ensure row counts match
  if (nrow(ef) != nrow(edgesFiltered)) {
    stop("wrong number of rows")
  }

  edgesFiltered <- ef

  #-----------------------------
  #OPTIONAL: CALCULATE SRI
  #-----------------------------
  if (sri) {
    if (nrow(edgesFiltered) > 0) {
      # 20260104 KG: changed function call to "calcSRI_EDB" from "calcSRI"
      dfSRI <- calcSRI_EDB(dataset = sriDenominatorDataset,  #Uses day-filtered dataset for denominator
                       edges = edgesFiltered,
                       allPairs_entire_season_output = allPairs_entire_season_output,
                       idCol = idCol)
    } else {
      warning("No edges to calculate SRI on.")
      dfSRI <- data.frame(ID1 = character(), ID2 = character(), sri = numeric())
    }

    outList <- list("edges" = edgesFiltered, "sri" = dfSRI)

  } else {
    outList <- list("edges" = edgesFiltered)
  }

  #-----------------------------
  #RETURN OUTPUT
  #-----------------------------
  return(outList)
}

# 20260104 KG: pasted this in from feeding_and_flight_functions, and renamed calcSRI_EDB from calcSRI.
calcSRI_EDB <- function(dataset,
                    edges,
                    allPairs_entire_season_output,
                    idCol = "Nili_id",
                    timegroupCol = "timegroup"){

  cat("\nComputing SRI... this may take a while if your dataset is large.\n")
  start <- Sys.time()  #track start time

  # Validation
  checkmate::assertSubset(timegroupCol, names(dataset))  #ensure timegroupCol exists
  checkmate::assertSubset(idCol, names(dataset))         #ensure idCol exists
  checkmate::assertDataFrame(dataset)                    #check dataset is dataframe
  checkmate::assertDataFrame(edges)                      #check edges is dataframe
  edges <- dplyr::as_tibble(edges)  #ensure edges is tibble for dplyr

  #Extract relevant columns from allPairs (existing list of ID1, ID2, optional pre-SRI)
  allPairs_day_sri <- allPairs_entire_season_output %>%
    dplyr::select(ID1, ID2, sri)
  # 20260104: I do not understand what this "optional pre-SRI" thing is doing. Is there ever a situation where allPairs_entire_season_output would have SRI values? Need to go back to when that was created and check.

  # Denominator -------------------------------------------------------------
  # Here we're talking about timegroups when an individual was *present.*
  #Create wide format matrix:
  #  rows = timegroups, cols = individuals
  #  TRUE if present, FALSE otherwise
  datasetWide <- dataset %>%
    sf::st_drop_geometry() %>%
    dplyr::select(tidyselect::all_of(c(timegroupCol, idCol))) %>%  #keep ID and timegroup cols
    dplyr::distinct() %>%
    dplyr::mutate(val = TRUE) %>%  #add flag val = TRUE
    tidyr::pivot_wider(id_cols = tidyselect::all_of(timegroupCol),
                       names_from = tidyselect::all_of(idCol),
                       values_from = "val", values_fill = FALSE)  #pivot wide

  # Get full dyad list, removing any where one individual of the dyad is NA because it doesn't make sense to calculate SRI for one individual.
  allPairs_edges <- as.data.frame(edges)[,c("ID1", "ID2")]
  dfSRI <- bind_rows(allPairs_day_sri, allPairs_edges) %>%
    dplyr::distinct(ID1, ID2, .keep_all = TRUE) # initialize output data frame
  dfSRI <- dfSRI[!is.na(dfSRI$ID1) & !is.na(dfSRI$ID2),]

  #---------------------------------------------
  #LOOP over dyads to calculate SRI
  #---------------------------------------------
  #Formula:
  # sri <- x / (x + yab + ya + yb)
  # where...
  #   a and b are the individuals
  #   x = in how many timegroups did these two individuals *interact*?
  #   nboth = in how many timegroups were both individuals *present*?
  #   yab = nBoth - x
  #   ya = n timegroups when individual a was present
  #   yb = n timegroups when individual b was present
  #Loop through each row of dfSRI to calculate/update the sri column
  for(i in seq_len(nrow(dfSRI))) {
    a <- dfSRI$ID1[i]  #ID1 (individual a of dyad k)
    b <- dfSRI$ID2[i]  #ID2 (individual b of dyad k)

    #Check if either ID is missing
    if(is.na(a) || is.na(b)) {
      dfSRI$sri[i] <- NA  #Set sri to NA if either ID is missing
      next  #Skip to the next iteration
    } # 20260105 KG this is where I think we should have removed the NAs ahead of time.

    #Check if either ID is not found in the list of valid IDs
    if(!(a %in% unique(dataset[[idCol]])) || !(b %in% unique(dataset[[idCol]]))) {
      dfSRI$sri[i] <- NA  #Set to NA if IDs not found
      next  #Skip to the next iteration
    }

    #Extract columns corresponding to a and b from datasetWide
    colA <- datasetWide[, a, drop = FALSE]  #Get column a
    colB <- datasetWide[, b, drop = FALSE]  #Get column b

    # nBoth = in how many timegroups were both individuals *present*?
    nBoth <- sum(colA & colB, na.rm = TRUE)

    # x = in how many timegroups did these two individuals *interact*?
    x <- nrow(unique(edges[edges$ID1 %in% c(a, b) & edges$ID2 %in% c(a, b), timegroupCol])) # in how many unique timegroups did individuals a and b interact?

    #yab = number of joint occurrences in datasetWide minus number of co-occurrences recorded in edges (i.e. simultaneous/joint/same-timegroup occurrences without co-occurrence/interaction)
    yab <- nBoth - x

    #ya and yb = in how many timegroups was *each* individual present?
    ya <- sum(colA, na.rm = TRUE)
    yb <- sum(colB, na.rm = TRUE)

    #--- SRI calculation ---
    #Calculate the Simple Ratio Index (SRI) using the formula:
    sri <- x / (x + yab + ya + yb)

    #If SRI is infinite (e.g., division by zero), set it to 0
    if (is.infinite(sri)) {
      sri <- 0
    }

    #--- Save result ---
    #Store the calculated SRI value back into the dfSRI data frame
    dfSRI$sri[i] <- sri
  }

  # complete the time message
  end <- Sys.time()
  cat(paste0("SRI computation completed in ", difftime(end, start, units = "secs"), " seconds.\n"))

  if (nrow(dfSRI) == 0) {
    message("Warning: `calcSRI()` returned an empty dataframe. Check dataset and edge list.")
  }
  return(dfSRI)
}

orig_test <- getEdges(test, roostBuffer = 1000, consecThreshold = 1, distThreshold = 1000, speedThreshUpper = NULL, speedThreshLower = 5, roostPolygons = rp, return = "sri")
edb_test <- getEdges_EDB(test, roostBuffer = 1000, consecThreshold = 1, distThreshold = 1000, speedThreshUpper = NULL, speedThreshLower = 5, roostPolygons = rp, allPairs_entire_season = ap, return = "sri")

dim(orig_test)
dim(edb_test)

orig_smallertest <- getEdges(smallertest, roostBuffer = 1000, consecThreshold = 1, distThreshold = 1000, speedThreshUpper = NULL, speedThreshLower = 5)
edb_smallertest <- getEdges_EDB(smallertest, roostBuffer = 1000, consecThreshold = 1, distThreshold = 1000, speedThreshUpper = NULL, speedThreshLower = 5)

dim(orig_smallertest)
dim(edb_smallertest)
