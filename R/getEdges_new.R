# 2026-01-04 test script
library(here)
library(tidyverse)
library(sf)
library(vultureUtils)

# New version of the getEdges function, following Elvira's logic (i.e., updated version of the function)
#' @param dataset a cleaned GPS dataset. Should be an sf object. Must have columns "location_long" and "location_lat" for conversion back to sf
#' @param roostPolygons an sf object containing roost polygons, to be used for excluding GPS points, or NULL
#' @param roostBuffer distance in meters by which to buffer roost polygons before masking, or NULL
#' @param consecThreshold
#' @param distThreshold threshold distance to define an interaction
#' @param speedThreshUpper upper speed threshold, or NULL. Not inclusive: e.g., will keep speeds < speedThreshUpper, but not =.
#' @param speedThreshLower lower speed threshold, or NULL. Not inclusive: e.g., will keep speeds > speedThreshLower, but not =.
#' @param timeThreshold for defining timegroups. Standard character string, e.g. "10 minutes"
#' @param idCol name of the column containing individual IDs
#' @param quiet whether to print messages about the filtering or not
#' @param includeAllVertices logical; whether to append a vector of all individuals in the original dataset to the output list
#' @param daytimeOnly logical; whether to filter points to daytime only before defining interactions.
#' @param sunLat numeric latitude to use for sunrise/sunset calculations, if daytimeOnly = T. Default is Jerusalem.
#' @param sunLong longitude to use for sunrise/sunset calculations, if daytimeOnly = T. Default is Jerusalem.
#' @param return what to return. Either "edges" (an edgelist), "sri" (SRI values), or "both" (a list containing both edges and sri)
#' @param getLocs Whether to return interaction locations (midpoint between points involved in each interaction). Valid only for "edges" and "both".
#' @param speedCol name of the column containing ground speed, in m/s. To be used for speed filtering
#' @param timestampCol name of the column containing the timestamps to be used. Must be in POSIXct format with an attached time zone; otherwise it will be coerced to POSIXct with UTC by default.
getEdges_new <- function(dataset,
                         roostPolygons = NULL,
                         roostBuffer = NULL,
                         consecThreshold = 1,
                         distThreshold,
                         speedThreshUpper = NULL,
                         speedThreshLower = NULL,
                         timeThreshold = "10 minutes",
                         idCol = "Nili_id",
                         quiet = T,
                         includeAllVertices = F,
                         daytimeOnly = T,
                         sunLat = 31.434306,
                         sunLong = 34.991889,
                         return = "sri",
                         getLocs = FALSE,
                         speedCol = "ground_speed",
                         timestampCol = "timestamp"){
  # 1. Preliminaries
  #SAVE RAW DATASET COPY for later use
  dataset_rawdata <- dataset

  # If timestampCol isn't already POSIXct, convert it, assuming UTC.
  if(!is.POSIXct(dataset[[timestampCol]])){
    dataset <- dataset %>%
      dplyr::mutate({{ timestampCol }} := as.POSIXct(.data[[timestampCol]], format = "%Y-%m-%d %H:%M:%OS", tz = "UTC"))
  }

  #Convert dataset to data.table (needed for spatsoc functions)
  data.table::setDT(dataset)

  #GROUP POINTS INTO TIMEGROUPS (using spatsoc)
  dataset <- spatsoc::group_times(dataset, datetime = timestampCol, threshold = timeThreshold)
  dataset_denominator <- dataset  #save a copy of the timegrouped dataset for the SRI denominator, since we need to know how many timegroups individuals appear in vs. interact in

  #CONVERT (back) TO sf OBJECT WITH LAT/LONG GEOMETRY
  #(needed for spatial filtering later)
  dataset_sf <- sf::st_as_sf(dataset, crs = "WGS84", remove = FALSE)

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

  #FILTER OUT POINTS INSIDE ROOST POLYGONS
  #Check if the dataset is already an sf object. If it isn't, convert it to an sf object.
  is_sf <- checkmate::testClass(dataset_sf, "sf")
  if(is_sf == FALSE){
    dataset_sf <- sf::st_as_sf(dataset_sf, coords = c("location_long", "location_lat"), crs ="WGS84", remove = F)
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
                                       lat = sunLat, lon = sunLong,
                                       keep = c("sunrise", "sunset")) %>%
      dplyr::select(date, sunrise, sunset)

    removedRoosts_dataset <- removedRoosts_dataset %>%
      #remove leftover sunrise/sunset cols just in case
      {if("sunrise" %in% names(.)) dplyr::select(., -sunrise) else .}%>%
      {if("sunset" %in% names(.)) dplyr::select(., -sunset) else .}%>%
      dplyr::left_join(times, by = c("dateOnly" = "date")) %>%
      dplyr::mutate(daytime = dplyr::case_when(.data[[timestampCol]] > .data$sunrise &
                                                 .data[[timestampCol]] < .data$sunset ~ TRUE,
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
  # NOTE: < and > are used.
  filteredData <- filterLocs(df = dataset_dayonly,
                             speedThreshUpper = speedThreshUpper,
                             speedThreshLower = speedThreshLower,
                             speedCol = speedCol)


  #BUILD ALL POSSIBLE PAIRS (DYADS) FOR THE focal period
  all_ids_focalPeriod <- as.character(unique(dataset[[idCol]]))
  allPairs_focalPeriod <- expand.grid(ID1 = all_ids_focalPeriod, ID2 = all_ids_focalPeriod, stringsAsFactors = FALSE) %>%
    dplyr::mutate(pair = paste(ID1, ID2, sep = "_")) %>%
    dplyr::filter(ID1 != ID2)

  #HANDLE EMPTY DATA AFTER FILTERING
  if (nrow(filteredData) == 0) {
    warning("After filtering, no data remains.")
    all_ids_focalPeriod_filteredData <- unique(dataset_rawdata[[idCol]])
    allPairs_focalPeriod_filteredData <- expand.grid(ID1 = all_ids_focalPeriod_filteredData, ID2 = all_ids_focalPeriod_filteredData, stringsAsFactors = FALSE) %>%
      dplyr::mutate(pair_filteredData = paste(ID1, ID2, sep = "_"), sri = NA) %>%
      dplyr::filter(ID1 != ID2)

    return(data.frame(ID1 = allPairs_focalPeriod_filteredData$ID1,
                      ID2 = allPairs_focalPeriod_filteredData$ID2,
                      sri = allPairs_focalPeriod_filteredData$sri))
  }

  #CALL spaceTimeGroups() FUNCTION
  #- return either edges or SRI
  if(nrow(filteredData) != 0){

    ##Do we need to compute SRI?
    if(return == "edges"){ #if SRI not needed, save time by not computing it
      if(quiet){
        ###EDGES ONLY, QUIET
        out <- suppressMessages(suppressWarnings(spaceTimeGroups_new(dataset = filteredData,
                                                                     denom = dataset_denominator,
                                                                     distThreshold = distThreshold,
                                                                     allPairs = allPairs_focalPeriod,
                                                                     consecThreshold = consecThreshold,
                                                                     timeThreshold = timeThreshold,
                                                                     sri = FALSE,
                                                                     idCol = idCol)))
      }else{
        ###EDGES ONLY, WARNINGS
        #compute edges without suppressing warnings
        out <- spaceTimeGroups_new(dataset = filteredData,
                                   denom = dataset_denominator,
                                   distThreshold = distThreshold,
                                   allPairs = allPairs_focalPeriod,
                                   consecThreshold = consecThreshold,
                                   timeThreshold = timeThreshold,
                                   sri = FALSE,
                                   idCol = idCol)
      }

    }else if(return %in% c("sri", "both")){ #otherwise we need to compute SRI.
      if(quiet){
        ###EDGES AND SRI, QUIET
        #suppress warnings while computing edges and SRI, returning a list of edges+sri
        out <- suppressMessages(suppressWarnings(spaceTimeGroups_new(dataset = filteredData,
                                                                     denom = dataset_denominator,
                                                                     distThreshold = distThreshold,
                                                                     allPairs = allPairs_focalPeriod,
                                                                     consecThreshold = consecThreshold,
                                                                     timeThreshold = timeThreshold,
                                                                     sri = TRUE,
                                                                     idCol = idCol)))
        if(return == "sri"){
          out <- out["sri"]
        }
      }else{
        ###EDGES AND SRI, WARNINGS
        #compute edges and SRI without suppressing warnings, returning a list of edges+sri
        out <- spaceTimeGroups_new(dataset = filteredData,
                                   denom = dataset_denominator,
                                   distThreshold = distThreshold,
                                   allPairs = allPairs_focalPeriod,
                                   consecThreshold = consecThreshold,
                                   timeThreshold = timeThreshold,
                                   sri = TRUE,
                                   idCol = idCol)
        if(return == "sri"){
          out <- out["sri"]
        }
      }
    }
  } #close the if(nrow(filteredData) != 0)

  #REMOVE LOCATION COLUMNS IF getLocs = FALSE
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
    toReturn <- append(out, list(as.character(unique(dataset_rawdata[[idCol]]))))
  } else {
    toReturn <- out
  }

  #------------------------------------------------------------
  #RETURN FINAL OUTPUT
  #------------------------------------------------------------
  if (length(toReturn) == 1) toReturn <- toReturn[[1]]
  return(toReturn)


}

# New version of the spaceTimeGroups function, following Elvira's logic (i.e., updated version of the function)
#' @param dataset filtered data, to be space-grouped (timegroups were already done earlier)
#' @param denom unfiltered, timegrouped data, for the SRI denominator.
#' @param distThreshold distance in meters used to define an interaction
#' @param allPairs data frame containing all pairs of individuals, bidirectional but with self edges removed.
#' @param consecThreshold
#' @param crsToSet CRS to set for the dataset without transforming
#' @param crsToTransform CRS to transform to
#' @param timestampCol name of the column containing the timestamps to be used. Must be in POSIXct format with an attached time zone
#' @param timeThreshold for defining timegroups. Standard character string, e.g. "10 minutes"
#' @param idCol
#' @param latCol
#' @param longCol
#' @param returnDist logical; whether to return the distance between individuals in an interaction or not
#' @param fillNA whether to fill the edgelist with NAs for individuals that did not interact with any others in that timegroup. See documentation for spatsoc::edge_dist().
#' @param sri logical; whether to calculate SRI
spaceTimeGroups_new <- function(dataset,
                                denom,
                                distThreshold,
                                allPairs,
                                consecThreshold = 1,
                                crsToSet = "WGS84",
                                crsToTransform = 32636,
                                timestampCol = "timestamp",
                                timeThreshold = "10 minutes",
                                idCol = "Nili_id",
                                latCol = "location_lat",
                                longCol = "location_long",
                                returnDist = TRUE,
                                fillNA = TRUE,
                                sri = T){

  #CHECK AND PREPARE sf OBJECT
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

  #TRANSFORM TO UTM & EXTRACT COORDS
  dataset <- dataset %>% sf::st_transform(crsToTransform)  #Convert CRS to meters
  dataset$utmE <- purrr::map_dbl(dataset$geometry, 1)      #Extract UTM Easting
  dataset$utmN <- purrr::map_dbl(dataset$geometry, 2)      #Extract UTM Northing
  dataset <- sf::st_drop_geometry(dataset)                 #Remove geometry for spatsoc compatibility

  #BUILD INITIAL EDGE LIST (per timegroup) and remove self edges and duplicates
  edges <- spatsoc::edge_dist(DT = dataset,
                              threshold = distThreshold,
                              id = idCol,
                              coords = c("utmE", "utmN"),
                              timegroup = "timegroup",
                              returnDist = returnDist,
                              fillNA = fillNA)
  edges_without_duplicate <- edges %>%
    dplyr::filter(is.na(ID1) | is.na(ID2) | as.character(ID1) < as.character(ID2))

  #FILTER EDGES BY CONSECUTIVE APPEARANCES
  edgesFiltered <- consecEdges(edgeList = edges_without_duplicate,
                               consecThreshold = consecThreshold) %>%
    dplyr::ungroup()

  #HANDLE EMPTY RESULT (no valid interactions)
  if (nrow(edgesFiltered) == 0) {
    warning("After edgesFiltered, the dataset had 0 rows.")

    #Prepare output dataframes with NA or 0 SRI
    allPairs_no_interaction <- allPairs
    colnames(allPairs_no_interaction)[3] <- "pair_no_interaction"   #Rename 3rd column

    #Set SRI = 0 for pairs with no interaction
    matching_indices <- allPairs$pair %in% allPairs_no_interaction$pair_no_interaction
    allPairs$sri[matching_indices] <- 0

    #Return empty but valid output
    return(list(
      "edges" = data.frame(
        ID1   = allPairs$ID1,
        ID2   = allPairs$ID2,
        edges = allPairs$edges
      ),
      "sri" = data.frame(
        ID1 = allPairs$ID1,
        ID2 = allPairs$ID2,
        sri = allPairs$sri
      )
    ))
  }

  #CALCULATE INTERACTION LOCATIONS
  locs <- dataset %>%
    tibble::as_tibble() %>%
    dplyr::select(tidyselect::all_of(c(idCol, "timegroup", latCol, longCol))) %>%
    dplyr::distinct() %>%
    dplyr::mutate(across(tidyselect::all_of(c(latCol, longCol)), as.numeric))

  #If multiple records per individual/timegroup, take mean lat/long
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

  #OPTIONAL: CALCULATE SRI
  if (sri) {
    if (nrow(edgesFiltered) > 0) {
      dfSRI <- calcSRI_new(fulldataset = denom,
                           edges = edgesFiltered,
                           allPairs = allPairs,
                           idCol = idCol,
                           timegroupCol = "timegroup")
    } else {
      warning("No edges to calculate SRI on.")
      dfSRI <- data.frame(ID1 = character(), ID2 = character(), sri = numeric())
    }

    outList <- list("edges" = edgesFiltered, "sri" = dfSRI)

  } else {
    outList <- list("edges" = edgesFiltered)
  }

  return(outList)
}

# New version of the calcSRI function, following Elvira's logic (i.e., updated version of the function)
#' @param fulldataset dataset to determine occurrences
#' @param edges edgelist, already filtered for consecutiveness if applicable
#' @param allPairs
#' @param idCol
#' @param timegroupCol
calcSRI_new <- function(fulldataset,
                        edges,
                        allPairs,
                        idCol = "Nili_id",
                        timegroupCol){

  cat("\nComputing SRI... this may take a while if your dataset is large.\n")
  start <- Sys.time()  #track start time

  # Validation
  checkmate::assertSubset(timegroupCol, names(dataset))  #ensure timegroupCol exists
  checkmate::assertSubset(idCol, names(dataset))         #ensure idCol exists
  checkmate::assertDataFrame(dataset)                    #check dataset is dataframe
  checkmate::assertDataFrame(edges)                      #check edges is dataframe
  edges <- dplyr::as_tibble(edges)  #ensure edges is tibble for dplyr

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
  dyads_from_edgelist <- as.data.frame(edges)[,c("ID1", "ID2")]
  dfSRI <- bind_rows(allPairs, dyads_from_edgelist) %>%
    dplyr::distinct(ID1, ID2, .keep_all = TRUE) # initialize output data frame
  dfSRI <- dfSRI[!is.na(dfSRI$ID1) & !is.na(dfSRI$ID2),] # remove any NA IDs that were introduced by the fillNA parameter in edge_dist (made sense for the edgelist, but not helpful for the SRI calculation)

  #LOOP over dyads to calculate SRI
  #Formula for SRI:
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

test <- readRDS(here("tests/testDataKaija/test.RDS"))
dates <- unique(test$dateOnly)
# smallertest <- test[test$dateOnly %in% dates[1:2],]
# dim(smallertest)

#--------
# Some code from Elvira's script to satisfy the getEdges_EDB function, which isn't entirely working/self-contained.
rp <- sf::st_read("tests/testthat/testdata/roosts50_kde95_cutOffRegion.kml") # needs to have roosts passed to it, because the default is not NULL

orig_test <- getEdges(test, roostBuffer = 1000, consecThreshold = 1, distThreshold = 1000, speedThreshUpper = NULL, speedThreshLower = 5, roostPolygons = rp, return = "sri")
new_test <- getEdges_new(test, roostBuffer = 1000, consecThreshold = 1, distThreshold = 1000, speedThreshUpper = NULL, speedThreshLower = 5, roostPolygons = rp, return = "sri") # this is waaaaay slow and I'm not sure why. Let's see why it's hanging.

# XXX start here: getEdges_new is hanging and I think it's an issue with "denom" in calcSRI_new. Need to debug.
