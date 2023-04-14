test_that("getFeedingEdges works", {
  base::load(test_path("testdata", "a.Rda"))
  aSF <- sf::st_as_sf(a, coords = c("location_long", "location_lat"), remove = F, crs = "WGS84") %>%
    dplyr::mutate(timestamp = lubridate::ymd_hms(timestamp),
                  dateOnly = lubridate::ymd(dateOnly))
  r <- sf::st_read(test_path("testdata", "roosts25_cutOffRegion.kml"))

  # XXX need to use some data that actually does produce interactions

  # errors and warnings
  w <- capture_warnings(getFeedingEdges(dataset = aSF, roostPolygons = r, idCol = "id", return = "edges"))
  expect_match(w, "Item 1 has 0 rows but longest item has 1; filled with NA", all = FALSE)
  expect_match(w, "After filtering, the dataset had 0 rows.", all = FALSE)
  expect_error(getFeedingEdges(dataset = a, roostPolygons = r, idCol = "id", return = "edges"))
})

