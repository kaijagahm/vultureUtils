# getFeedingEdges, which also tests getEdges
test_that("getFeedingEdges works", {
  # sample data we can use for testing
  base::load(test_path("testdata", "ed_0905_0908.Rda"))
  a <- ed_0905_0908 # name this something more usable
  rp <- sf::st_read(test_path("testdata", "roosts25_cutOffRegion.kml")) # roost polygons
  # sample data that will *not* produce interactions
  aSample <- a[1:50,]

  # produce some interactions
  edges <- getFeedingEdges(dataset = a, roostPolygons = rp, idCol = "id", return = "edges")
  ind <- getFeedingEdges(dataset = a, roostPolygons = rp, idCol = "id", return = "edges", includeAllVertices = T)
  sri <- getFeedingEdges(dataset = a, roostPolygons = rp, idCol = "id", return = "sri")
  both <- getFeedingEdges(dataset = a, roostPolygons = rp, idCol = "id", return = "both")
  o <- capture_output(getFeedingEdges(dataset = a, roostPolygons = rp, idCol = "id", return = "sri"))
  expect_match(o, "Computing SRI... this may take a while if your dataset is large.", all = FALSE)
  expect_match(o, "SRI computation completed in", all = FALSE)
  expect_equal(class(edges), c("tbl_df", "tbl", "data.frame"))
  expect_equal(class(sri), "data.frame")
  expect_equal(class(both), "list")
  expect_equal(class(both[[1]]), c("tbl_df", "tbl", "data.frame"))
  expect_equal(class(both[[2]]), "data.frame")
  expect_equal(class(ind), "list")
  expect_equal(class(ind[[2]]), "character")

  # quiet = F
  output_qf <- capture_output(getFeedingEdges(dataset = a, roostPolygons = rp, idCol = "id", return = "edges", quiet = F))
  expect_match(output_qf, "Removed 0 nighttime points, leaving 91 points.", all = FALSE)

  # errors and warnings
  w <- capture_warnings(getFeedingEdges(dataset = aSample, roostPolygons = rp, idCol = "id", return = "edges"))
  expect_match(w, "Item 1 has 0 rows but longest item has 1; filled with NA", all = FALSE)
  expect_match(w, "After filtering, the dataset had 0 rows.", all = FALSE)
})

# get_roosts_df
test_that("get_roosts_df works", {
  base::load(test_path("testdata", "elviraData_2021.09.03_2021.09.13.Rda"))
  a <- elviraData_2021.09.03_2021.09.13

    # A normal run
  ## messages
  m <- capture_output(get_roosts_df(a, id = "id"))
  expect_match(m, "Finding roosts... this may take a while if your dataset is large.", all = FALSE)
  expect_match(m, "Roost computation completed in", all = FALSE)

  ## create the output
  roostsOut <- get_roosts_df(a, id = "id", quiet = T)
  a_kmh <- a
  a_kmh$ground_speed <- a_kmh$ground_speed*3.6
  roostsOut_diffSpeed <- get_roosts_df(a_kmh, id = "id", quiet = T, speed_units = "km/h")

  ## test the output
  expect_equal(class(roostsOut), c("tbl_df", "tbl", "data.frame")) # output is a tibble/data frame
  expect_equal(names(roostsOut), c("id", "date", "roost_date", "sunrise", "sunset", "sunrise_twilight", "sunset_twilight", "daylight", "is_roost", "location_lat", "location_long"
  ))
  expect_equal(all(unique(roostsOut$id) %in% unique(a$id)), TRUE)
  expect_equal(all(roostsOut$date %in% a$dateOnly), TRUE)
  expect_equal(nrow(roostsOut %>% dplyr::distinct(id, date)) < nrow(a %>% dplyr::distinct(id, dateOnly)), TRUE)
})

# getRoostEdges
test_that("getRoostEdges works", {
  base::load(test_path("testdata", "elviraData_2021.09.03_2021.09.13.Rda"))
  a <- elviraData_2021.09.03_2021.09.13
  rp <- sf::st_read(test_path("testdata", "roosts25_cutOffRegion.kml")) # roost polygons
  r <- get_roosts_df(df = a, id = "id")
  dist_e <- getRoostEdges(r, mode = "distance", idCol = "id", return = "edges")
  poly_e <- getRoostEdges(r, mode = "polygon", roostPolygons = rp, idCol = "id", return = "edges")
  dist_s <- getRoostEdges(r, mode = "distance", idCol = "id", return = "sri")
  poly_s <- getRoostEdges(r, mode = "polygon", roostPolygons = rp, idCol = "id", return = "sri")

  # test the outputs
  expect_equal(class(dist_e), c("data.table", "data.frame"))
  expect_equal(class(poly_e), c("tbl_df", "tbl", "data.frame"))
  expect_equal(class(dist_s), "data.frame")
  expect_equal(class(poly_s), "data.frame")
})

# cleanData
test_that("cleanData works", {
  base::load(test_path("testdata", "rawDataSample.Rda"))
  a <- rawDataSample
  mask <- sf::st_read(test_path("testdata", "CutOffRegion.kml"))
  c <- cleanData(a, mask = mask, idCol = "trackId", removeVars = T) # normal use case, re-masked
  b <- cleanData(a, mask = mask, idCol = "trackId", removeVars = T, reMask = F) # not re-masked
  c_notRemoved <- cleanData(a, mask = mask, idCol = "trackId", removeVars = F)

  # not downsampled
  nd <- cleanData(a, mask = mask, idCol = "trackId", removeVars = T, downsample = F)

  # not quiet
  nq <- cleanData(a, mask = mask, idCol = "trackId", removeVars = T, quiet = F)

  # expectations
  expect_equal(ncol(c_notRemoved) > ncol(c), TRUE) # more vars if not removed (duh)
  expect_equal(class(c), c("sf", "tbl_df", "tbl", "data.frame")) # expected classes
  expect_equal(class(nd), class(c)) # class shouldn't change if you don't downsample the data
  expect_equal(class(b), c("tbl_df", "tbl", "data.frame")) # if we don't re-mask, the resulting object doesn't have class sf. #XXX should look into fixing this to make it consistent.
  expect_equal(nrow(nd) > nrow(c), TRUE) # there should be more data if we don't downsample
  expect_equal(nq, c) # setting quiet = F should not change the output
  o <- capture_messages(cleanData(a, mask = mask, idCol = "trackId", removeVars = T, quiet = F))
  expect_match(o, "dropping Z and/or M coordinate", all = FALSE)
  expect_match(o, "Joining, by =", all = FALSE)
})

