#*****************************************************************************************
#Manuscript: "What are the drivers of social interactions?"
#Author: Elvira D'Bastiani
#*Corresponding author: Elvira D'Bastiani
#Email: elviradbastiani@gmail.com
#*****************************************************************************************

#---------------------------------------------
#Setup: Clean environment and free memory
#---------------------------------------------
rm(list = ls())            #Remove all objects from workspace
gc()                       #Perform garbage collection

#---------------------------------------------
#Load Required Libraries
#---------------------------------------------
library(tidyverse)         #Data manipulation and wrangling
library(sf)                #Spatial data manipulation
library(igraph)            #Network analysis and visualization
#library(tidygraph)       #(commented out) Tidygraph for network integration with tidyverse
library(future)            #Parallel processing
library(furrr)             #Parallel functional programming
library(here)              #Manage file paths reproducibly
library(purrr)             #Functional programming tools
library(targets)           #Workflow/pipeline management
library(spatsoc)           #Spatiotemporal grouping for movement ecology
library(checkmate)         #Argument validation
library(suncalc)           #Calculate sunrise, sunset, etc.
library(geosphere)         #Spherical trigonometry (e.g., geographic distances)


#==============================================================================
#Helper Functions
#==============================================================================

#-------------------------------------------------------------------------------
#Function: convertAndBuffer
#-------------------------------------------------------------------------------
#Purpose: 
#- Convert an 'sf' spatial object from its original CRS to a CRS using meters
#- Apply a buffer (in meters)
#- Convert the buffered object back to the original CRS
#
#Inputs:
#obj       = an 'sf' object (must have a valid CRS)
#dist      = buffer distance (meters). Default = 50
#crsMeters = CRS code with units in meters (default = EPSG:32636, UTM zone 36N)
#
#Output:
#Returns a buffered 'sf' object in original CRS

#' Given an sf object in WGS84, convert it to a CRS with meters as the units, buffer by a given distance, and then convert it back.
#' @param obj an sf object to be buffered
#' @param dist buffer distance, in meters (m)
#' @param crsMeters crs with units of meters to be used. Default is 32636 (Israel, UTM zone 36)
#' @return A buffered sf object
#' @export
convertAndBuffer <- function(obj, dist = 50, crsMeters = 32636){
  #Validate that 'obj' is an 'sf' (simple features) object
  checkmate::assertClass(obj, "sf")
  
  #Validate that 'dist' is a single numeric value >= 0
  checkmate::assertNumeric(dist, len = 1, lower = 0)
  
  originalCRS <- sf::st_crs(obj)
  if(is.null(originalCRS)|is.na(originalCRS)){
    stop("Object does not have a valid CRS.")
  }
  
  #Extract the current CRS of the object
  originalCRS <- sf::st_crs(obj)
  
  #Check if the CRS is missing or invalid; stop execution if so
  if (is.null(originalCRS) | is.na(originalCRS)) {
    stop("Object does not have a valid CRS.")
  }
  
  #Convert geometry from original CRS to meter-based CRS (e.g., UTM)
  converted <- obj %>%
    sf::st_transform(crsMeters)
  
  #Apply buffer operation in meters
  buffered <- converted %>%
    sf::st_buffer(dist = dist)
  
  #Convert buffered object back to its original CRS
  convertedBack <- buffered %>%
    sf::st_transform(originalCRS)
  
  #Return the buffered object (in original CRS)
  return(convertedBack)
  
}

#-------------------------------------------------------------------------------
#Function: filterLocs
#-------------------------------------------------------------------------------
#Purpose:
#- Filters a dataframe to keep rows where ground speed is within specified thresholds
#
#Inputs:
#df              = input data frame containing ground speed column
#speedThreshLower= lower threshold for speed (numeric, optional; default NULL)
#speedThreshUpper= upper threshold for speed (numeric, optional; default NULL)
#speedCol        = name of the column containing speed values (default: "ground_speed")
#
#Output:
#Returns a filtered dataframe (rows outside thresholds are removed)
#-----------------------------------------------------------------------------

#' Filter dataset for reasonableness
#' @param df a data frame to filter
#' @param speedThreshLower a single numeric value, the lower limit for ground speed to be included (m/s)
#' @param speedThreshUpper a single numeric value, the upper limit for ground speed to be included (m/s)
#' @param speedCol Name of the column containing ground speed values. Default is "ground_speed".
#' @return A filtered data frame
#' @export
filterLocs <- function(df, speedThreshLower = NULL, speedThreshUpper = NULL, speedCol = "ground_speed"){
  #-----------------------------
  #Validate function arguments
  #-----------------------------
  
  #Check that speedThreshLower is a single numeric value or NULL
  checkmate::assertNumeric(speedThreshLower, null.ok = TRUE, len = 1)
  
  #Check that speedThreshUpper is a single numeric value or NULL
  checkmate::assertNumeric(speedThreshUpper, null.ok = TRUE, len = 1)
  
  #Check that speedCol exists as a column name in df
  checkmate::assertChoice(speedCol, choices = names(df))
  
  #-----------------------------
  #Apply speed filtering
  #-----------------------------
  
  #If both thresholds are NULL, warn the user: no filtering will occur
  if (is.null(speedThreshLower) & is.null(speedThreshUpper)) {
    warning("No speed thresholds set, so data will not be filtered for speed.")
  }
  
  #If lower threshold is specified: filter rows with speed > speedThreshLower
  if (!is.null(speedThreshLower)) {
    df <- df %>%
      dplyr::filter(.data[[speedCol]] > speedThreshLower)
  }
  
  #If upper threshold is specified: filter rows with speed < speedThreshUpper
  if (!is.null(speedThreshUpper)) {
    df <- df %>%
      dplyr::filter(.data[[speedCol]] < speedThreshUpper)
  }
  
  #Return the filtered dataframe
  return(df)
}

#-------------------------------------------------------------------------------
#Function: consecEdges
#Description: Keeps edges present in >= consecThreshold consecutive timegroups
#-------------------------------------------------------------------------------
#Purpose:
#- Filters an edge list to keep only edges that occur in at least 
#  `consecThreshold` consecutive time groups.
#
#Inputs:
#edgeList      = data frame with edges and timegroups
#consecThreshold = minimum number of consecutive time groups for an edge to be kept
#ID1Col        = name of column for first individual ID
#ID2Col        = name of column for second individual ID
#timegroupCol  = name of column for time group (integer)
#
#Output:
#Returns a filtered edge list with only edges meeting the threshold
#----------------------------------------------------------------------------

#' Filter edge list to exclude too few consecutive occurrences
#'
#' This function takes an edge list and removes edges that don't occur in at least `consecThreshold` consecutive time groups. It expects an edge list containing *ONE-WAY* edges (i.e. with self edges already removed, and with duplicates not included--already reduced to A-B only, not A-B and B-A). If the edge list contains duplicate edges (A-B and B-A), they will be treated as if they were separate edges. Data must already include `timegroup`s.
#' @param edgeList edge list to work with, containing only A-B edges. No self edges, no B-A edges.
#' @param consecThreshold in how many consecutive time groups must the two individuals interact in order to be included? Default is 2.
#' @param ID1Col Character. Name of the column containing the ID1 of the first individual
#' @param ID2Col Character. Name of the column containing the ID2 of the second individual
#' @param timegroupCol Character. Name of the column containing time groups (integer values), returned by spatsoc functions
#' @return An edge list (data frame) containing only edges that occurred in at least `consecThreshold` consecutive time groups.
#' @export
consecEdges <- function(edgeList, 
                        consecThreshold = 2, 
                        ID1Col = "ID1", 
                        ID2Col = "ID2", 
                        timegroupCol = "timegroup"){

  #-----------------------------
  #Validate function arguments
  #-----------------------------
  
  checkmate::assertDataFrame(edgeList)               #Ensure edgeList is a data frame
  checkmate::assertNumeric(consecThreshold, len = 1) #Ensure threshold is numeric scalar
  checkmate::assertCharacter(ID1Col, len = 1)        #Ensure ID1Col is a string
  checkmate::assertCharacter(ID2Col, len = 1)        #Ensure ID2Col is a string
  checkmate::assertChoice(ID1Col, names(edgeList))   #Ensure ID1Col exists in edgeList
  checkmate::assertChoice(ID2Col, names(edgeList))   #Ensure ID2Col exists in edgeList
  checkmate::assertCharacter(timegroupCol, len = 1)  #Ensure timegroupCol is a string
  checkmate::assertChoice(timegroupCol, names(edgeList)) #Ensure timegroupCol exists
  checkmate::assertInteger(edgeList[[timegroupCol]]) #Ensure timegroupCol values are integers
  
  #-----------------------------
  #Create unique edge-time combinations
  #-----------------------------
  uniquePairs <- edgeList %>%
    dplyr::select(tidyselect::all_of(c(ID1Col, ID2Col, timegroupCol))) %>%  #Keep only ID1, ID2, timegroup columns
    dplyr::distinct(.data[[timegroupCol]], .data[[ID1Col]], .data[[ID2Col]]) %>%  #Keep unique timegroup-ID1-ID2 rows
    dplyr::group_by(.data[[ID1Col]], .data[[ID2Col]]) %>%  #Group by edge (ID1-ID2)
    dplyr::arrange(.data[[timegroupCol]], .by_group = TRUE) %>%  #Sort timegroups chronologically within each pair
    
    #Create a grouping variable (`grp`) to identify consecutive sequences
    #When consecutive → same group; when time gap → new group
    dplyr::mutate("grp" = cumsum(c(1, diff(.data[[timegroupCol]]) != 1))) %>%
    
    dplyr::ungroup()  #Ungroup for next steps
  
  #-----------------------------
  #Filter edges that meet consecutive threshold
  #-----------------------------
  
  uniquePairs_toKeep <- uniquePairs %>%
    dplyr::group_by(.data[[ID1Col]], .data[[ID2Col]], grp) %>%  #Group by ID1, ID2, and consecutive group
    dplyr::filter(dplyr::n() >= consecThreshold) %>%             #Keep groups with >= consecThreshold rows
    dplyr::ungroup() %>%
    dplyr::select(-grp)  #Drop the helper column
  
  #-----------------------------
  #Join filtered edges back with original edgeList to restore all columns
  #-----------------------------
  
  consec <- uniquePairs_toKeep %>%
    dplyr::left_join(edgeList, by = c(ID1Col, ID2Col, timegroupCol))
  
  #-----------------------------
  #Return filtered edge list
  #-----------------------------
  
  return(consec)
}


#-------------------------------------------------------------------------------
#Function: calcSRI
#-------------------------------------------------------------------------------
#Purpose:
#- Calculate the Simple Ratio Index (SRI) for pairs of individuals
#  based on interaction data (edges) and timegroups
#
#Inputs:
#dataset                    = original cleaned dataset (with individual IDs and timegroups)
#edges                      = data frame of observed interactions (edge list)
#allPairs_entire_season_output = data frame of all possible pairs across the season
#idCol                      = column name for individual IDs (default: "Nili_id")
#timegroupCol               = column name for timegroup IDs (default: "timegroup")
#
#Output:
#Returns a data frame of ID1, ID2, and calculated SRI values
#-----------------------------------------------------------------------------

#' Calculates SRI based on timegroup and individual occurrence information
#'
#' @param dataset the cleaned dataset.
#' @param edges edgelist created by spatsoc, with self edges and duplicate edges removed.
#' @param idCol character. Name of the column containing ID values.
#' @param timegroupCol character. Name of the column containing timegroup values.
#' @return A data frame containing ID1, ID2, and SRI value.
#' @export

calcSRI <- function(dataset, 
                    edges, 
                    allPairs_entire_season_output,
                    idCol = "Nili_id", 
                    timegroupCol = "timegroup"){
  #---------------------------------------------
  #Notify user that computation is starting
  #---------------------------------------------
  cat("\nComputing SRI... this may take a while if your dataset is large.\n")
  start <- Sys.time()  #track start time
  
  #---------------------------------------------
  #INPUT VALIDATION
  #---------------------------------------------
  checkmate::assertSubset(timegroupCol, names(dataset))  #ensure timegroupCol exists
  checkmate::assertSubset(idCol, names(dataset))         #ensure idCol exists
  checkmate::assertDataFrame(dataset)                    #check dataset is dataframe
  checkmate::assertDataFrame(edges)                      #check edges is dataframe
  
  edges <- dplyr::as_tibble(edges)  #ensure edges is tibble for dplyr
  
  #---------------------------------------------
  #Create list of individuals per timegroup
  #(for info; not directly used in loop later)
  #---------------------------------------------
  timegroupsList <- dataset %>%
    dplyr::select(tidyselect::all_of(c(timegroupCol, idCol))) %>%  #keep ID and timegroup cols
    dplyr::mutate({{idCol}} := as.character(.data[[idCol]])) %>%   #convert ID col to character
    dplyr::distinct() %>%                                          #remove duplicate rows
    dplyr::group_by(.data[[timegroupCol]]) %>%                     #group by timegroup
    dplyr::group_split() %>%                                       #split into list of dfs per timegroup
    purrr::map(~.x[[idCol]])                                       #map to vector of IDs per timegroup
  
  #---------------------------------------------
  #Get unique set of timegroups
  #---------------------------------------------
  timegroups <- unique(dataset[[timegroupCol]])
  
  #---------------------------------------------
  #Extract relevant columns from allPairs
  # (existing list of ID1, ID2, optional pre-SRI)
  #---------------------------------------------
  allPairs_day_sri <- allPairs_entire_season_output %>% 
    dplyr::select(ID1, ID2, sri)
  
  #---------------------------------------------
  #Create wide format matrix:
  #  rows = timegroups, cols = individuals
  #  TRUE if present, FALSE otherwise
  #---------------------------------------------
  datasetWide <- dataset %>%
    sf::st_drop_geometry() %>%  #drop spatial geometry if exists
    dplyr::select(tidyselect::all_of(c(timegroupCol, idCol))) %>%  #keep ID and timegroup cols
    dplyr::distinct() %>%
    dplyr::mutate(val = TRUE) %>%  #add flag val = TRUE
    tidyr::pivot_wider(id_cols = tidyselect::all_of(timegroupCol), 
                       names_from = tidyselect::all_of(idCol),
                       values_from = "val", values_fill = FALSE)  #pivot wide
  
  #---------------------------------------------
  #Prepare edge list: ensure ID1, ID2 columns
  #---------------------------------------------
  allPairs_edges <- as.data.frame(edges)  #convert edges to dataframe
  allPairs_edges <- as.data.frame(cbind(edges$ID1, edges$ID2))  #keep ID1, ID2
  colnames(allPairs_edges) <- c("ID1", "ID2")  #set column names
  
  #---------------------------------------------
  #Merge edges and allPairs to get full dyad list
  #  keep unique rows only
  #---------------------------------------------
  merged_df_allPairs <- bind_rows(allPairs_day_sri, allPairs_edges) %>%
    dplyr::distinct(ID1, ID2, .keep_all = TRUE)
  
  
  #---------------------------------------------
  #Get list of valid IDs (colnames from datasetWide except timegroup)
  #---------------------------------------------
  ids_datasetWide <- colnames(datasetWide)[-1]  #remove timegroup col
  
  #---------------------------------------------
  #Initialize output dataframe (copy merged list)
  #---------------------------------------------
  dfSRI <- merged_df_allPairs
  
  #---------------------------------------------
  #LOOP over dyads to calculate SRI
  #---------------------------------------------
  #Loop through each row of dfSRI to calculate/update the sri column
  for(k in seq_len(nrow(dfSRI))) {
    a <- dfSRI$ID1[k]  #Extract ID1 for the k-th row
    b <- dfSRI$ID2[k]  #Extract ID2 for the k-th row
    
    #Check if either ID is missing
    if(is.na(a) || is.na(b)) {
      dfSRI$sri[k] <- NA  #Set sri to NA if either ID is missing
      next  #Skip to the next iteration
    }
    
    #Check if either ID is not found in the list of valid IDs
    if(!(a %in% ids_datasetWide) || !(b %in% ids_datasetWide)) {
      dfSRI$sri[k] <- NA  #Optional: set to NA if IDs not found
      next  #Skip to the next iteration
    }
    
    #Extract columns corresponding to a and b from datasetWide
    colA <- datasetWide[, a, drop = FALSE]  #Get column a
    colB <- datasetWide[, b, drop = FALSE]  #Get column b
    
    #Count the number of rows where both columns are TRUE (logical AND)
    nBoth <- sum(colA & colB, na.rm = TRUE)
    
    #--- Count number of unique co-occurrences in edges ---
    #Subset edges to rows where IDs match either a or b in ID1/ID2 columns
    #Then count unique occurrences across timegroupCol
    x <- nrow(unique(edges[edges$ID1 %in% c(a, b) & edges$ID2 %in% c(a, b), timegroupCol]))
    
    #--- Compute yab ---
    #yab = number of joint occurrences in datasetWide minus number of co-occurrences recorded in edges
    yab <- nBoth - x
    
    #--- Individual occurrence counts ---
    #Total number of TRUE (or 1) values for each individual ID, ignoring NAs
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
    dfSRI$sri[k] <- sri
  }

  #---------------------------------------------
  #(Optional: preview first rows)
  #---------------------------------------------
  head(dfSRI)
  
  # complete the time message
  end <- Sys.time()
  duration <- difftime(end, start, units = "secs")
  cat(paste0("SRI computation completed in ", duration, " seconds.\n"))
  
  if (nrow(dfSRI) == 0) {
    message("Warning: `calcSRI()` returned an empty dataframe. Check dataset and edge list.")
  }
  return(dfSRI)
  
  }
   
#-------------------------------------------------------------------------------
#Function: spaceTimeGroups - group points by space and time
#-------------------------------------------------------------------------------
#Purpose:
#- Groups animal tracking data by space and time
#- Generates an edge list of interactions (within distance threshold per timegroup)
#- Optionally calculates SRI (Simple Ratio Index)
#
#Inputs:
#dataset = sf object or data frame with lat/long coordinates
#sriDenominatorDataset = dataset for SRI denominator (e.g., filtered by day)
#distThreshold = distance threshold for interaction (meters)
#allPairs_entire_season_output = precomputed full pair list for season
#allPairs_day = pair list for specific day
#consecThreshold = min consecutive timegroups for valid interaction (default = 2)
#crsToSet = CRS string if CRS missing (default "WGS84")
#crsToTransform = CRS for metric units (default UTM zone 36: EPSG 32636)
#timestampCol, timeThreshold, idCol, latCol, longCol = column names
#returnDist, fillNA, sri = flags for distance column, NA handling, SRI calculation
#timegroupData = timegroup metadata (external input)
#
#Output:
#List with:
#  $edges: data frame of filtered edges
#  $sri:   data frame of SRI (optional)
#-----------------------------------------------------------------------------

#' This function takes a data frame of filtered, cleaned points and uses functions from `spatsoc` to group them by space and time. Then it uses vultureUtils::consecEdges to remove edges that don't occur in enough consecutive time groups.
#' @param dataset a data frame, filtered by speed etc, to use to create spatiotemporal groups.
#' @param distThreshold distance threshold at which to consider that two individuals are interacting (m).
#' @param consecThreshold Passed to vultureUtils::consecEdges. In how many consecutive time groups must the two individuals interact in order to be included? Default is 2.
#' @param crsToSet if `feedingSites` is a data frame, what CRS to pass to sf::st_set_crs() (NOT transform!). If `feedingSites` is already an sf object, `crsToSet` will be overridden by whatever the object's CRS is, unless it is NA.
#' @param crsToTransform CRS to transform the `dataset` to. Default is "32636" for ITM.
#' @param timestampCol Passed to spatsoc::group_times. Name of date time column(s). either 1 POSIXct or 2 IDate and ITime. e.g.: 'datetime' or c('idate', 'itime')
#' @param timeThreshold Passed to spatsoc::group_times. Threshold for grouping times. e.g.: '2 hours', '10 minutes', etc. if not provided, times will be matched exactly. Note that provided threshold must be in the expected format: '##unit'.
#' @param idCol Name of the column containing individual ID's of the vultures.
#' @param latCol Name of the column containing latitude values
#' @param longCol Name of the column containing longitude values
#' @param returnDist Passed to spatsoc::edge_dist. Boolean indicating if the distance between individuals should be returned. If FALSE (default), only ID1, ID2 columns (and timegroup, splitBy columns if provided) are returned. If TRUE, another column "distance" is returned indicating the distance between ID1 and ID2. Default is TRUE.
#' @param fillNA Passed to spatsoc::edge_dist. Boolean indicating if NAs should be returned for individuals that were not within the threshold distance of any other. If TRUE, NAs are returned. If FALSE, only edges between individuals within the threshold distance are returned. Default is FALSE.
#' @param sri T/F (default is T). Whether or not to include SRI calculation.
#' @return an edge list (data frame)
#' @export
#Convert to UTM

spaceTimeGroups <- function(dataset,
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
      dfSRI <- calcSRI(dataset = sriDenominatorDataset,  #Uses day-filtered dataset for denominator
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

#-------------------------------------------------------------------------------
#Function: getEdges
#-------------------------------------------------------------------------------
#Purpose:
#- Creates an edge list (or SRI table) of interactions from GPS data
#- Optionally filters for roost exclusion, daytime-only, speed thresholds
#- Can return edge list, SRI, or both
#
#Inputs:
#dataset              = raw tracking data
#roostPolygons        = spatial polygons to exclude points inside (optional)
#roostBuffer          = buffer distance around roost polygons
#consecThreshold      = minimum consecutive timegroups for valid edge
#distThreshold        = distance threshold for interaction
#speedThreshUpper     = max speed to include
#speedThreshLower     = min speed to include
#timeThreshold        = time window for grouping points
#idCol, timestampCol, speedCol = column names for ID, time, speed
#quiet                = suppress messages?
#includeAllVertices   = include vector of unique IDs?
#daytimeOnly          = restrict to daytime only?
#return               = "edges", "sri", or "both"
#getLocs              = include lat/long for interactions?
#
#Output:
#Edge list or SRI data frame, optionally with locations
#-------------------------------------------------------------------------------


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
#' @param timeThreshold timeThreshold Passed to spatsoc::group_times. Threshold for grouping times. e.g.: '2 hours', '10 minutes', etc. if not provided, times will be matched exactly. Note that provided threshold must be in the expected format: '##unit'. Default is "10 minutes"
#' @param idCol Passed to spaceTimeGroups: the name of the column containing individual vulture ID's. Default is Nili_id.
#' @param quiet Whether to silence the warning messages about grouping individuals with themselves inside the time threshold. Default is T. This occurs because if we set our time threshold to e.g. 10 minutes (the default), there are some GPS points that occur closer together than 10 minutes apart (e.g. if we experimentally set the tags to take points every 5 minutes). As a result, we will "group" together the same individual with itself, resulting in some self edges. I currently have a step in the code that simply removes these self edges, so there should be no problem here. But if you set `quiet = F`, you will at least be warned with the message `"Warning: found duplicate id in a timegroup and/or splitBy - does your group_times threshold match the fix rate?"` when this is occurring.
#' @param includeAllVertices logical. Whether to include another list item in the output that's a vector of all individuals in the dataset. For use in creating sparse graphs. Default is F.
#' @param daytimeOnly T/F, whether to restrict interactions to daytime only. Default is T.
#' @param return One of "edges" (default, returns an edgelist, would need to be used in conjunction with includeAllVertices = T in order to include all individuals, since otherwise they wouldn't be included in the edgelist. Also includes timegroup information, which SRI cannot do. One row in this data frame represents a single edge in a single timegroup.); "sri" (returns a data frame with three columns, ID1, ID2, and sri. Includes pairs whose SRI values are 0, which means it includes all individuals and renders includeAllVertices obsolete.); and "both" (returns a list with two components: "edges" and "sri" as described above.)
#' @param getLocs Whether or not to return locations where the interactions happened (for edge list only, doesn't make sense for SRI). Default is FALSE. If getLocs is set to TRUE when return = "sri", a message will tell the user that no locations can be returned for SRI.
#' @param speedCol Name of the column containing ground speed values. Default is "ground_speed".
#' @return An edge list containing the following columns: `timegroup` gives the numeric index of the timegroup during which the interaction takes place. `minTimestamp` and `maxTimestamp` give the beginning and end times of that timegroup. `ID1` is the id of the first individual in this edge, and `ID2` is the id of the second individual in this edge.
#' @export
#' 

getEdges <- function(dataset,
                     roostPolygons = roostPolygons,
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
                     timestampCol = "timestamp"){
  
  #------------------------------------------------------------
  #SAVE RAW DATASET COPY
  #------------------------------------------------------------
  dataset_rawdata <- dataset  #backup raw dataset
  
  #------------------------------------------------------------
  #CONVERT timestampCol to POSIXct datetime
  #------------------------------------------------------------
  dataset <- dataset %>%
    dplyr::mutate({{ timestampCol }} := as.POSIXct(.data[[timestampCol]], format = "%Y-%m-%d %H:%M:%OS", tz = "UTC"))
  
  #------------------------------------------------------------
  #Convert dataset to data.table (needed for spatsoc functions)
  #------------------------------------------------------------
  data.table::setDT(dataset)
  
  #------------------------------------------------------------
  #GROUP POINTS INTO TIMEGROUPS (using spatsoc)
  #------------------------------------------------------------
  dataset <- spatsoc::group_times(dataset, datetime = timestampCol, threshold = timeThreshold)
  
  dataset_denominator <- dataset  #save a copy for SRI denominator
  
  #------------------------------------------------------------
  #RECORD START/END TIME PER TIMEGROUP
  #------------------------------------------------------------
  timegroupData <- dataset %>%
    dplyr::select(tidyselect::all_of(timestampCol), timegroup) %>%
    dplyr::group_by(timegroup) %>%
    dplyr::summarize(minTimestamp = min(.data[[timestampCol]], na.rm = TRUE),
                     maxTimestamp = max(.data[[timestampCol]], na.rm = TRUE))
  
  #------------------------------------------------------------
  #CONVERT TO sf OBJECT WITH LAT/LONG GEOMETRY
  #(needed for spatial filtering later)
  #------------------------------------------------------------
  dataset_sf <- sf::st_as_sf(dataset, coords = c("location_long", "location_lat"), crs = "WGS84", remove = FALSE)
  
  #------------------------------------------------------------
  #VALIDATE CRS / CONVERT TO sf IF NEEDED
  #------------------------------------------------------------
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
  
  
  #------------------------------------------------------------
  #WARN IF GETLOCS + return="sri" (invalid combination)
  #------------------------------------------------------------
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

  #------------------------------------------------------------
  #FILTER FOR DAYTIME ONLY (if requested)
  #------------------------------------------------------------
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
    
    #Filter out nighttimes
    nNightPoints <- nrow(removedRoosts_dataset[removedRoosts_dataset$daytime == F,])
    dataset_dayonly <- removedRoosts_dataset %>%
      dplyr::filter(daytime == TRUE)
    nDayPoints <- nrow(dataset_dayonly)
    if(quiet == FALSE){
      cat(paste0("Removed ", nNightPoints, " nighttime points, leaving ",
                 nDayPoints, " points.\n"))
    }
  }
  
  #------------------------------------------------------------
  #FILTER BASED ON SPEED (if thresholds set)
  #------------------------------------------------------------
  filteredData <- filterLocs(df = dataset_dayonly,
                             speedThreshUpper = speedThreshUpper,
                             speedThreshLower = speedThreshLower,
                             speedCol = speedCol)
  
  
  #------------------------------------------------------------
  #BUILD ALL POSSIBLE PAIRS (DYADS) FOR THE DAY
  #------------------------------------------------------------
  all_ids_day <- as.character(unique(dataset[[idCol]]))
  allPairs_day <- expand.grid(ID1 = all_ids_day, ID2 = all_ids_day, stringsAsFactors = FALSE) %>%
    dplyr::mutate(pair = paste(ID1, ID2, sep = "_")) %>%
    dplyr::filter(ID1 != ID2)
  
  #------------------------------------------------------------
  #PREPARE SEASONAL PAIR LIST
  #------------------------------------------------------------
  allPairs_entire_season_output <- allPairs_entire_season %>%
    dplyr::mutate(pair = paste(ID1, ID2, sep = "_")) %>%
    dplyr::filter(ID1 != ID2) %>%
    dplyr::mutate(sri = ifelse(pair %in% allPairs_day$pair, NA, NA))
  

  #------------------------------------------------------------
  #HANDLE EMPTY DATA AFTER FILTERING
  #------------------------------------------------------------
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
  #CALL spaceTimeGroups() FUNCTION
  #- return either edges or SRI
  #------------------------------------------------------------
  if(nrow(filteredData) != 0){
    
    ##Do we need to compute SRI?
    if(return == "edges"){ #if SRI is not needed, we can save time by not computing it.
      if(quiet){
        ###EDGES ONLY, QUIET
        out <- suppressMessages(suppressWarnings(spaceTimeGroups(dataset = filteredData,
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
        #compute edges without suppressing warnings
        out <- spaceTimeGroups(dataset = filteredData,
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
        #suppress warnings while computing edges and SRI, returning a list of edges+sri
        out <- suppressMessages(suppressWarnings(spaceTimeGroups(dataset = filteredData,
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
        #compute edges and SRI without suppressing warnings, returning a list of edges+sri
        out <- spaceTimeGroups(dataset = filteredData,
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


# Prepare a subset of data
# Get roosts Polygons---------------------------------------------------
roostPolygons <- sf::st_read(here::here("Z:/Elvira/Ergm_manuscript/raw_data/roosts50_kde95_cutOffRegion.kml"))

# See the data of the seasons in one day (time windows size) -------------------
#Load each movebank dataset (seasons) to an individual .Rda file
load(here::here("Z:/Elvira/Ergm_manuscript/raw_data/data_cut_preBreeding_2020.Rda"))

# What are the different time windows we want to test?
timewindows <- c(1) 

total_seasons <- 1

seasons_names <- c("data_cut_preBreeding_2020")

# Initialize lists to hold the outputs of get*Edges.
sris_flight <- vector(mode = "list", length = length(total_seasons))
sris_feeding <- vector(mode = "list", length = length(total_seasons))

# Function to Remove Prefix
remove_prefix <- function(strings) {
  # Use sub() to replace the prefix with an empty string
  cleaned_strings <- sub("^data_cut_", "", strings)
  return(cleaned_strings)
}

# Run a for loop across all the seasons
for(j in 1:total_seasons) {
  season <- seasons_names[j]
  
  print(season)
  
  season_name_file <- remove_prefix(season)
  print(season_name_file)
  
  datalist <- base::get(season)  # Assuming season names correspond to variable names in the environment
  
  for(i in 1:length(timewindows)) {
    cat("Working on data split into", timewindows[i], "day intervals\n")
    
    data_season_list <- datalist[[i]]
    
    # Extract unique Nili_id values from all lists in data_season_list
    all_Nili_ids_season <- unique(unlist(lapply(data_season_list, function(x) x$Nili_id)))
    # Convert to character (if necessary)
    all_Nili_ids_season <- as.character(all_Nili_ids_season)
    # Print or view
    print(all_Nili_ids_season)
    # Create all possible pairs (dyads)
    allPairs_entire_season <- expand.grid(ID1 = all_Nili_ids_season, ID2 = all_Nili_ids_season, stringsAsFactors = FALSE) 
    dim(allPairs_entire_season) 
    
    cat("Working on flight\n")
    
    fl <- suppressWarnings(furrr::future_map(data_season_list, ~{
      library(sf) # have to have this here; it's a quirk of future::purrr::map().
      getEdges(.x, roostPolygons = roostPolygons,
               roostBuffer = 50, 
               consecThreshold = 2,
               distThreshold = 1000, 
               speedThreshUpper = NULL, 
               speedThreshLower = 5, 
               timeThreshold = "10 minutes", 
               idCol = "Nili_id", 
               quiet = T,
               includeAllVertices = F, 
               daytimeOnly = T, 
               return = "sri", 
               getLocs = FALSE)
      
      
    }, .progress = T))
    
    cat("Working on feeding\n")
    fe <- suppressWarnings(furrr::future_map(data_season_list, ~{
      library(sf) # have to have this here; it's a quirk of future::purrr::map().
      getEdges(.x,
               roostPolygons = roostPolygons, 
               roostBuffer = 50, 
               consecThreshold = 2, 
               distThreshold = 50, 
               speedThreshUpper = 5, 
               speedThreshLower = NULL, 
               timeThreshold = "10 minutes", 
               idCol = "Nili_id", 
               quiet = T, 
               includeAllVertices = F, 
               daytimeOnly = T, 
               return = "sri", 
               getLocs = F, 
               speedCol = "ground_speed",
               timestampCol = "timestamp")
      
    }, .progress = T))
    
    # Save the results to their lists
    sris_flight[[i]] <- fl
    sris_feeding[[i]] <- fe
    
    # Clean up memory 
    rm(list = c("datalist", "fl", "fe"))
    gc()
  }
  
  ##save the season files sris_flight, sris_feeding
  
  #flight
  object_name_sris_flight <- paste0("sri_flight_", season_name_file)
  assign(object_name_sris_flight, sris_flight)
  save(list = object_name_sris_flight, file = here::here(paste0("Z:/Elvira/Ergm_manuscript/raw_data/social_behaviors/", object_name_sris_flight, ".Rda")))
  
  #feeding
  object_name_sris_feeding <- paste0("sri_feeding_", season_name_file)
  assign(object_name_sris_feeding, sris_feeding)
  save(list = object_name_sris_feeding, file = here::here(paste0("Z:/Elvira/Ergm_manuscript/raw_data/social_behaviors/", object_name_sris_feeding, ".Rda")))
  
}







