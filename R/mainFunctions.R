getEdges <- function(dataset, roostPolygons, roostBuffer, consecThreshold, distThreshold, speedThreshUpper, speedThreshLower, timeThreshold = "10 minutes", idCol = "Nili_id", quiet = T, includeAllVertices = F, daytimeOnly = T, return = "edges"){
  # Argument checks
  checkmate::assertDataFrame(dataset)
  checkmate::assertClass(roostPolygons, "sf")
  checkmate::assertNumeric(roostBuffer, len = 1)
  checkmate::assertNumeric(consecThreshold, len = 1)
  checkmate::assertNumeric(distThreshold, len = 1)
  checkmate::assertNumeric(speedThreshUpper, len = 1, null.ok = TRUE)
  checkmate::assertNumeric(speedThreshLower, len = 1, null.ok = TRUE)
  checkmate::assertCharacter(timeThreshold, len = 1)
  checkmate::assertLogical(daytimeOnly, len = 1)
  checkmate::assertSubset(return, choices = c("edges", "sri", "both"),
                          empty.ok = FALSE)
  checkmate::assertSubset("ground_speed", names(dataset)) # necessary for filterLocs.
  checkmate::assertSubset("timestamp", names(dataset)) # for sunrise/sunset calculations.
  checkmate::assertSubset("dateOnly", names(dataset)) # for sunrise/sunset calculations
  checkmate::assertSubset("location_lat", names(dataset)) # passed to spaceTimeGroups. XXX fix with GH#58
  checkmate::assertSubset("location_long", names(dataset)) # passed to spaceTimeGroups. XXX fix with GH#58
  checkmate::assertSubset(idCol, names(dataset)) # passed to spaceTimeGroups.

  # Get all unique individuals before applying any filtering
  if(includeAllVertices){
    uniqueIndivs <- unique(dataset[[idCol]])
  }

  ## FILTER THE POINTS
  # Restrict interactions based on ground speed
  filteredData <- vultureUtils::filterLocs(df = dataset,
                                           speedThreshUpper = speedThreshUpper,
                                           speedThreshLower = speedThreshLower)

  # Buffer the roost polygons
  roostPolygons <- convertAndBuffer(roostPolygons, dist = roostBuffer)

  # Exclude any points that fall within a (buffered) roost polygon
  points <- filteredData[lengths(sf::st_intersects(filteredData, roostPolygons)) == 0,]

  # Restrict based on daylight
  if(daytimeOnly){
    times <- suncalc::getSunlightTimes(date = unique(lubridate::date(points$timestamp)), lat = 31.434306, lon = 34.991889,
                                       keep = c("sunrise", "sunset")) %>%
      dplyr::select(date, sunrise, sunset) # XXX the coordinates I'm using here are from the centroid of Israel calculated here: https://rona.sh/centroid. This is just a placeholder until we decide on a more accurate way of doing this.
    points <- points %>%
      dplyr::select(-c("sunrise", "sunset")) %>% # remove leftover sunrise/sunset columns just in case
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
                                                                               sri = FALSE)))
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
