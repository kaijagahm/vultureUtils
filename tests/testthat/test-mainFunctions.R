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

test_that("get_roosts_df works", {
  base::load(test_path("testdata", "a.Rda"))
  aSF <- sf::st_as_sf(a, coords = c("location_long", "location_lat"), remove = F, crs = "WGS84") %>%
    dplyr::mutate(timestamp = lubridate::ymd_hms(timestamp),
                  dateOnly = lubridate::ymd(dateOnly))

  # A normal run
  ## messages
  m <- capture_output(get_roosts_df(aSF, id = "id"))
  expect_match(m, "Finding roosts... this may take a while if your dataset is large.", all = FALSE)
  expect_match(m, "Roost computation completed in", all = FALSE)

  ## create the output
  roostsOut <- get_roosts_df(aSF, id = "id", quiet = T)

  ## test the output
  expect_equal(class(roostsOut), c("tbl_df", "tbl", "data.frame")) # output is a tibble/data frame
  expect_equal(names(roostsOut), c("id", "date", "roost_date", "sunrise", "sunset", "sunrise_twilight", "sunset_twilight", "daylight", "is_roost", "location_lat", "location_long"
  ))
  expect_equal(unique(roostsOut$id), unique(aSF$id))
  expect_equal(all(roostsOut$date %in% aSF$dateOnly), TRUE)
  expect_equal(nrow(roostsOut %>% dplyr::distinct(id, date)) < nrow(aSF %>% dplyr::distinct(id, dateOnly)), TRUE)
})
