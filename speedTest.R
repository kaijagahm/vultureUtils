# Testing to see if I can speed up the SRI calculation
library(tidyverse)
library(sf)
library(profvis)

load("data/testDay.Rda")
roostPolygons <- sf::st_read("data/roosts25_cutOffRegion.kml")
testDay <- testDay %>%
  sf::st_as_sf(coords = c("location_long", "location_lat"), remove = F) %>%
  sf::st_set_crs("WGS84")

edges <- getFlightEdges(testDay, roostPolygons = roostPolygons, return = "edges")

data.table::setDT(testDay)
testDayTimegrouped <- spatsoc::group_times(DT = testDay, datetime = "timestamp", threshold = "10 minutes")

calcSRI <- function(dataset, edges, idCol = "trackId", timegroupCol = "timegroup"){
  # setup for time warning
  cat("\nComputing SRI... this may take a while if your dataset is large.\n")
  start <- Sys.time()

  # arg checks
  checkmate::assertSubset(timegroupCol, names(dataset))
  checkmate::assertSubset(idCol, names(dataset))
  checkmate::assertDataFrame(dataset)
  checkmate::assertDataFrame(edges)

  edges <- dplyr::as_tibble(edges)

  ## get individuals per timegroup as a list
  # Info about timegroups and individuals, for SRI calculation
  timegroupsList <- dataset %>%
    dplyr::select(all_of(timegroupCol), all_of(idCol)) %>%
    dplyr::mutate({{idCol}} := as.character(.data[[idCol]])) %>%
    dplyr::distinct() %>%
    dplyr::group_by(.data[[timegroupCol]]) %>%
    dplyr::group_split() %>%
    purrr::map(~.x[[idCol]])

  ## get unique set of timegroups
  timegroups <- unique(dataset[[timegroupCol]])

  ## get all unique pairs of individuals
  inds <- as.character(unique(dataset[[idCol]]))
  allPairs <- expand.grid(ID1 = inds, ID2 = inds, stringsAsFactors = F) %>%
    filter(ID1 < ID2)

  # wide data
  datasetWide <- dataset %>%
    sf::st_drop_geometry() %>%
    dplyr::select(tidyselect::all_of(c(timegroupCol, idCol))) %>%
    dplyr::distinct() %>%
    dplyr::mutate(val = TRUE) %>%
    tidyr::pivot_wider(id_cols = timegroupCol, names_from = idCol,
                       values_from = "val", values_fill = FALSE)

  ## get SRI information
  dfSRI <- purrr::pmap_dfr(allPairs, ~{
    a <- .x
    b <- .y
    colA <- datasetWide[,a]
    colB <- datasetWide[,b]
    nBoth <- sum(colA & colB)
    x <- length(unique(edges[edges$ID1 %in% c(a, b) & edges$ID2 %in% c(a, b), timegroupCol]))
    yab <- nBoth - x
    sri <- x/(x+yab)
    dfRow <- data.frame("ID1" = a, "ID2" = b, "sri" = sri)
    return(dfRow)
  })

  # complete the time message
  end <- Sys.time()
  duration <- difftime(end, start, units = "secs")
  cat(paste0("SRI computation completed in ", duration, " seconds."))
  return(dfSRI)
}

t <- calcSRI(dataset = testDay, edges = edges)

hist(t$sri)

