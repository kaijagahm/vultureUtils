
#' Data download
#'
#' Download vulture project data from the Israel vulture study Movebank repository, with some minor specifications. Note that you must specify your movebank credentials.
#' @param loginObject A Movebank login object, created by passing a username and password to move::movebankLogin
#' @param extraSensors Whether to include extra sensors. Defaults to FALSE.
#' @param removeDup Whether to remove duplicated timestamps. Defaults to TRUE.
#' @param dateTimeStartUTC a POSIXct object, in UTC. Will be converted to character assuming UTC.
#' @param dateTimeEndUTC a POSIXct object, in UTC. Will be converted to character assuming UTC.
#' @return A movestack
#' @export
downloadVultures <- function(loginObject, extraSensors = F, removeDup = T,
                             dateTimeStartUTC = NULL, dateTimeEndUTC = NULL){
  move::getMovebankData(study = "Ornitela_Vultures_Gyps_fulvus_TAU_UCLA_Israel",
                        login = loginObject,
                        includeExtraSensors = FALSE,
                        deploymentAsIndividuals = FALSE,
                        removeDuplicatedTimestamps = TRUE,
                        timestamp_start = dateTimeStartUTC,
                        timestamp_end = dateTimeEndUTC)
}

#' Mask Israel
#'
#' Crop data to only include locations within Israel
#' @param mask a polygon
#' @param longCol the name of the column in the dataset containing longitude values
#' @param latCol the name of the column in the dataset containing latitude values
#' @param crs (To be passed to `st_set_crs()`). One of (i) character: a string accepted by GDAL, (ii) integer, a valid EPSG value (numeric), or (iii) an object of class crs.
#' @return A movestack
#' @export
maskIsrael <- function(dataset, longCol = "location_long", latCol = "location_lat", crs){
  # read in the Israel mask
  mask <- sf::st_read("data/maskIsrael.kml", quiet = TRUE)

  # check if the dataset is already an sf object
  issf <- checkmate::testClass(dataset, "sf")

  # if not, convert it to an sf object
  if(issf == FALSE){
    checkmate::assertSubset(x = c(longCol, latCol), choices = names(dataset))
    dataset_sf <- sf::st_as_sf(dataset, coords = c(longCol, latCol))
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
                       as.numeric(max({{dateCol}}, na.rm = T) -
                                    min({{dateCol}}, na.rm = T)))

  # Look at date durations in the masked Israel dataset
  datesInIsrael <- datDFIsrael %>%
    as.data.frame() %>%
    dplyr::group_by(trackId) %>%
    dplyr::summarize(duration =
                       as.numeric(max({{dateCol}}, na.rm = T) -
                                    min({{dateCol}}, na.rm = T)))

  # Compare the two dates and calculate proportion
  datesCompare <- left_join(dates, datesInIsrael %>%
                              dplyr::select(trackId,
                                            "durationIsrael" = duration)) %>%
    mutate(propIsrael = durationIsrael/duration) # compute proportion of days spent in Israel

  whichInIsraelLongEnough <- datesCompare %>%
    dplyr::filter(propIsrael > thresh) %>%
    pull(trackId) %>%
    unique()

  return(whichInIsraelLongEnough)
}


#' Create directed matrices
#'
#' Create directed matrices from vulture data
#' @param dataset the vulture dataset
#' @param distThreshold distance threshold for what is considered an "interaction"
#' @param sim # number of simulations?? not sure about this one.
#' @param co #  number of co-occurrences? not sure about this one either.
#' @return A list: "SimlDataPntCnt" = sim, "CoocurCountr" = co
#' @export
createDirectedMatrices <- function(dataset, distThreshold, sim = SimlDataPntCnt, co = CoocurCountr){
  checkmate::assertDataFrame(dataset)

  columnsToSelect <- c("ID", "coords.x2", "coords.x1", "Easting",
                       "Northing", "timegroup", "group")

  checkmate::assertSubset(columnsToSelect, names(dataset))

  # Start a progress bar
  pb <- txtProgressBar(min = 0, max = max(dataset$timegroup),
                       initial = 0, style = 3)

  # For each time group: -----------------------------------------------------
  for(i in 1:max(dataset$timegroup)){ # loop on all time groups

    # extract current time group
    timegroupDF <- dataset %>%
      dplyr::filter(timegroup == i) %>%
      dplyr::select(all_of(columnsToSelect))

    # Compute dyads and distances ---------------------------------------------
    # working within this time group: dyads and distances. Now we don't need `timegroup` but instead `group` (spatiotemporal).
    # further subset columns of interest
    timegroupDF <- timegroupDF %>%
      dplyr::filter(timegroup == i) %>%
      dplyr::select(ID, coords.x2, coords.x1, group)
    # these vultures were observed around the same time on the same day.

    # Put each vulture with each other vulture so that their locations can be compared to see who was close to each other:
    DT <- reshape::expand.grid.df(timegroupDF, timegroupDF)

    # Rename columns
    names(DT)[5:7] <- c("ID2", "lat_secondInd", "long_secondInd")
    # XXX KG note: should re-do this with a method other than numerical indexing.

    data.table::setDT(DT)[, dist_km := geosphere::distGeo(matrix(c(coords.x1, coords.x2), ncol = 2),
                                                          matrix(c(long_secondInd, lat_secondInd),
                                                                 ncol = 2))/1000]
    # using `setDT` here instead of `as.data.table` because big lists/dataframes make copies first and then perform a function and can thus take a long time and a lot of memory.
    # `dist_km` finds the shortest distance between two points, i.e. latlong of ID and ID2. So those with self would obviously give dist_km == 0.
    # `distGeo` is a highly accurate estimate of the shortest distance between two points on an ellipsoid (default is WGS84 ellipsoid). The shortest path between two points on an ellipsoid is called the geodesic.


    # create all possible dyads of vultures in a long format:

    # XXX KG what's going on here? could probably make this code more efficient.
    # Identify all self-association dyads with zero inter-location distance and same ID's.
    presentVultures <- DT %>%
      dplyr::filter(dist_km == 0 & as.character(ID) == as.character(ID2)) %>%
      dplyr::select(ID, ID2)

    # These are the vultures present in this time group - selecting only ID and ID2 i.e. columns 1 & 5
    presentVultures <- as.data.frame(unique(presentVultures$ID)) # all self-associating dyads

    presentVultures <- reshape::expand.grid.df(presentVultures, presentVultures, unique = F)
    # transform back to rows of ID1 = ID2.
    # these are the dyads that have concurrent time point
    names(presentVultures) <- c("ID", "ID2") # these are all the vultures whose location was recorded around the same time including self.


    # Loop on current dyads (including self) to update co-occurrences ---------
    for(dyadcnt in 1:nrow(presentVultures)){ # length just half since each dyad is counted twice AB and BA
      Dyadind <- which(sim$ID == presentVultures$ID[dyadcnt] &
                         sim$ID2 == presentVultures$ID2[dyadcnt])
      # In the above line: just identifying which row in the empty sim ID1 and ID2 dyads are the same as PresentVulture for one timegroup at a time.
      #identified all rows of self AND non-self association in same timegroup and 0 distance in sim (SimlDataPntCnt)
      ##gives which row number in sim (SimlDataPntCnt) has the same dyad
      #Nitika - Identified all dyads from presentVultures by subsetting all rows that had dist_km=0 and self-association

      sim$counter[Dyadind] <- sim$counter[Dyadind] + 1 # Dyadind is the row number with that dyad
      # addint 1 to the frequency such that this dyad could've hung out close to each other because they were around at the same time.

      # add another (1) tallymark to counter in front of the dyad every time the pair co-occurs in time
    } # for loop on current dyads

    # Since self dyads appear only once, they should also be counted twice like AB and BA
    SelfDyad <- which(presentVultures$ID == presentVultures$ID2) # since self dyads appear only once, another loop on them, so the diag will be counted twice like the rest.

    for(dyadcnt in SelfDyad){
      Dyadind <- which(sim$ID == presentVultures$ID[dyadcnt] &
                         sim$ID2 == presentVultures$ID2[dyadcnt])
      #sim$counter[Dyadind] <- sim$counter[Dyadind] + 1
      # Nitika: avoiding counting self dyads twice because we aren't keeping directed edges for non-self
    }


    # Set interacting dyads ---------------------------------------------------
    InteractingSelf <- data.table::subset(DT, dist_km == 0 &
                                            as.character(ID) == as.character(ID2))
    # just including self interactions once, not multiple times.

    InteractingSelf <- InteractingSelf[!duplicated(InteractingSelf$ID),]
    # just including self interactions once, not multiple times.

    InteractingDyads <- data.table::subset(DT, (dist_km <= distThreshold/1000) &
                                             as.character(ID) != as.character(ID2))
    # not including self interactions
    # only here, in the interacting dyads, do we check if a dyad was spatially proximate.

    # subset data table such that non self-overlapping IDs as well as within a certain distance from each other
    InteractingDyads <- InteractingDyads[!duplicated(InteractingDyads[, c("ID", "ID2")])]

    # for debugging:
    if(dim(presentVultures)[1] < dim(InteractingDyads)[1]){
      break
    }

    # bind non-self overlapping rows with self-overlapping rows
    InteractingDyads <- rbind(InteractingDyads, InteractingSelf)

    # Update co-occurrence counter --------------------------------------------
    for(dyadcnt in 1:dim(InteractingDyads)[1]){
      Dyadind <- which(co$ID == InteractingDyads$ID[dyadcnt] &
                         co$ID2 == InteractingDyads$ID2[dyadcnt])

      # Identifying in the empty data frame where SPATIO-temporal proximity is recorded, which dyads are in which rows
      # Spatial proximity between dyads was calculated after SimlDataPntCnt in the step where self and non-self <2km dist threshold was calculated as InteractingDyads
      co$counter[Dyadind] <- co$counter[Dyadind] + 1
    }


    # Free up some memory -----------------------------------------------------
    rm(list = c("DT", "InteractingDyads", "InteractingSelf", "SelfDyad",
                "Dyadind", "dyadcnt", "timegroupDF"))

    # Update the progressbar
    setTxtProgressBar(pb, i)

  } # close loop over time groups

  return(list("SimlDataPntCnt" = sim, "CoocurCountr" = co))
}
#'
