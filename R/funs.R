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

#' Create directed matrices
#'
#' Create directed matrices from vulture data
#' @param Dataset the vulture dataset
#' @param DistThreshold distance threshold for what is considered an "interaction"
#' @param sim # number of simulations?? not sure about this one.
#' @param co #  number of co-occurrences? not sure about this one either.
#' @return A list: "SimlDataPntCnt" = sim, "CoocurCountr" = co
#' @export
createDirectedMatrices <- function(Dataset, DistThreshold, sim = SimlDataPntCnt, co = CoocurCountr){

  columnsToSelect <- c("ID", "coords.x2", "coords.x1", "Easting",
                       "Northing", "timegroup", "group")

  # Start a progress bar
  pb <- txtProgressBar(min = 0, max = max(Dataset$timegroup), initial = 0, style = 3)

  # For each time group: -----------------------------------------------------
  for(timgrpind in 1:max(Dataset$timegroup)){ # loop on all time groups
    ## `timegroup` var in `Dataset` comes from analysis.

    # extract current time group (#18458 has a good example)
    timegroupDF <- subset(Dataset, timegroup == timgrpind,
                          select = columnsToSelect)


    # Compute dyads and distances ---------------------------------------------
    # working within this time group: dyads and distances. Now we don't need `timegroup` but instead `group` (spatiotemporal).
    # further subset columns of interest
    timegroupDF <- subset(timegroupDF, timegroup = timgrpind,
                          select = c("ID", "coords.x2", "coords.x1", "group"))
    # these vultures were observed around the same time on the same day.

    # Put each vulture with each other vulture so that their locations can be compared to see who was close to each other:
    DT <- reshape::expand.grid.df(timegroupDF, timegroupDF) # entire column written twice laterally (next to each other)

    names(DT)[5:7] <- c("ID2", "lat_secondInd", "long_secondInd") # rename cols.
    # XXX KG note: should re-do this with a method other than numerical indexing.

    data.table::setDT(DT)[, dist_km := geosphere::distGeo(matrix(c(coords.x1, coords.x2), ncol = 2),
                                                          matrix(c(long_secondInd, lat_secondInd),
                                                                 ncol = 2))/1000]
    # using `setDT` here instead of `as.data.table` because big lists/dataframes make copies first and then perform a function and can thus take a long time and a lot of memory.
    # `dist_km` finds the shortest distance between two points, i.e. latlong of ID and ID2. So those with self would obviously give dist_km == 0.
    # `distGeo` is a highly accurate estimate of the shortest distance between two points on an ellipsoid (default is WGS84 ellipsoid). The shortest path between two points on an ellipsoid is called the geodesic.


    # create all possible dyads of vultures in a long format:

    # Identify all self-association dyads with zero inter-location distance and same ID's.
    PresentVultures <- subset(DT, dist_km == 0 &
                                as.character(ID) == as.character(ID2),
                              select = c("ID", "ID2"))
    # These are the vultures present in this time group - selecting only ID and ID2 i.e. columns 1 & 5
    PresentVultures <- as.data.frame(unique(PresentVultures$ID)) # all self-associating dyads

    PresentVultures <- reshape::expand.grid.df(PresentVultures, PresentVultures, unique = F)
    # transform back to rows of ID1 = ID2.
    # these are the dyads that have concurrent time point
    names(PresentVultures) <- c("ID", "ID2") # these are all the vultures whose location was recorded around the same time including self.


    # Loop on current dyads (including self) to update co-occurrences ---------
    for(dyadcnt in 1:dim(PresentVultures)[1]){ # length just half since each dyad is counted twice AB and BA
      Dyadind <- which(sim$ID == PresentVultures$ID[dyadcnt] &
                         sim$ID2 == PresentVultures$ID2[dyadcnt])
      # In the above line: just identifying which row in the empty sim ID1 and ID2 dyads are the same as PresentVulture for one timegroup at a time.
      #identified all rows of self AND non-self association in same timegroup and 0 distance in sim (SimlDataPntCnt)
      ##gives which row number in sim (SimlDataPntCnt) has the same dyad
      #Nitika - Identified all dyads from PresentVultures by subsetting all rows that had dist_km=0 and self-association

      sim$counter[Dyadind] <- sim$counter[Dyadind] + 1 # Dyadind is the row number with that dyad
      # addint 1 to the frequency such that this dyad could've hung out close to each other because they were around at the same time.

      # add another (1) tallymark to counter in front of the dyad every time the pair co-occurs in time
    } # for loop on current dyads

    # Since self dyads appear only once, they should also be counted twice like AB and BA
    SelfDyad <- which(PresentVultures$ID == PresentVultures$ID2) # since self dyads appear only once, another loop on them, so the diag will be counted twice like the rest.

    for(dyadcnt in SelfDyad){
      Dyadind <- which(sim$ID == PresentVultures$ID[dyadcnt] &
                         sim$ID2 == PresentVultures$ID2[dyadcnt])
      #sim$counter[Dyadind] <- sim$counter[Dyadind] + 1
      # Nitika: avoiding counting self dyads twice because we aren't keeping directed edges for non-self
    }


    # Set interacting dyads ---------------------------------------------------
    InteractingSelf <- subset(DT, dist_km == 0 &
                                as.character(ID) == as.character(ID2))
    # just including self interactions once, not multiple times.

    InteractingSelf <- InteractingSelf[!duplicated(InteractingSelf$ID),]
    # just including self interactions once, not multiple times.

    InteractingDyads <- subset(DT, (dist_km <= DistThreshold/1000) &
                                 as.character(ID) != as.character(ID2))
    # not including self interactions
    # only here, in the interacting dyads, do we check if a dyad was spatially proximate.

    # subset data table such that non self-overlapping IDs as well as within a certain distance from each other
    InteractingDyads <- InteractingDyads[!duplicated(InteractingDyads[, c("ID", "ID2")])]

    # for debugging:
    if(dim(PresentVultures)[1] < dim(InteractingDyads)[1]){
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
    setTxtProgressBar(pb, timgrpind)

  } # close loop over time groups

  return(list("SimlDataPntCnt" = sim, "CoocurCountr" = co))
}
#'
