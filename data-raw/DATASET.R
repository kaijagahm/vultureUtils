## code to prepare `DATASET` dataset goes here
jamPolygons <- sf::st_read("data-raw/GPS_jamming_3.kml")
usethis::use_data(jamPolygons, internal = TRUE, overwrite = TRUE)
