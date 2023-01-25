# Test script for building a co-roost network
# We want to have two data formats possible: either individual/date/polygonName, or individual/date/lat/long.

library(tidyverse)
library(sf)

roosts <- read.csv("data/Roosts_df_mod.csv")

## TYPE 1 DATA: ROOST LOCATIONS
# Let's take just a subset: 2022 only
simpleRoosts <- roosts %>%
  filter(year == 2022) %>%
  dplyr::select(Nili_id, roost_date, location_long, location_lat) %>%
  sf::st_as_sf(coords = c("location_long", "location_lat")) %>%
  sf::st_set_crs("WGS84")

roostPolygons <- sf::st_read("data/AllRoostPolygons.kml")

## TYPE 2 DATA: POLYGON
polygonData <- sf::st_join(x = simpleRoosts, y = roostPolygons) %>%
  sf::st_drop_geometry() %>%
  select(Nili_id, roost_date, "roostID" = Name)

idCol <- "Nili_id"
polygonEdges <- polygonData %>%
  dplyr::group_by(roost_date, roostID) %>%
  dplyr::group_split(.keep = T) %>%
  purrr::map_dfr(~{tidyr::expand_grid("ID1" = .x[[idCol]], .x)}) %>%
  dplyr::rename("ID2" = all_of(idCol)) %>%
  dplyr::filter(ID1 < ID2)


colName <- "mpg"
dataset <- mtcars
myFun <- function(dataset, colName){
  out <- dataset %>%
    dplyr::select(all_of(colName))
}

myFun(dataset, colName)






