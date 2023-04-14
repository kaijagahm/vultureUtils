test_that("convertAndBuffer works", {
  # Set up sample data
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"))
  ncBuff <- convertAndBuffer(nc)
  ncNACRS <- nc
  sf::st_crs(ncNACRS) <- NA
  basicPoint <- sf::st_point(c(1,2))

  # some vulture data
  base::load(test_path("testdata", "a.Rda"))
  aSF <- sf::st_as_sf(a, coords = c("location_long", "location_lat"), remove = F, crs = "WGS84")
  aSFBuff <- convertAndBuffer(aSF)

  # Equal
  expect_equal(class(nc), class(ncBuff)) # output should be the same type of object as the input
  expect_equal("sf" %in% class(ncBuff), TRUE) # output should be an sf object
  expect_equal(all(sf::st_area(ncBuff) > sf::st_area(nc)), TRUE) # buffering should make the area larger
  expect_equal(sf::st_crs(nc, parameters = TRUE)$units_gdal, sf::st_crs(ncBuff, parameters = TRUE)$units_gdal) # check that the units are the same before and after
  expect_equal(all(sf::st_geometry_type(aSFBuff) == "POLYGON"), TRUE) # points get buffered to polygons

  # Error
  expect_error(convertAndBuffer(nc, dist = -10)) # negative distances shouldn't work
  expect_error(convertAndBuffer(basicPoint)) # can't buffer something that's not an sf object
  expect_error(convertAndBuffer(ncNACRS)) # can't buffer if CRS is NA (or NULL, but I don't have an explicit test yet for NULL bc I can't figure out how to set the CRS to NULL)
})

test_that("filterLocs works", {
  # set up data
  base::load(test_path("testdata", "a.Rda"))
  h <- 14
  l <- 2
  filtHigh <- filterLocs(df = a, speedThreshLower = NULL, speedThreshUpper = h)
  filtLow <- filterLocs(df = a, speedThreshLower = l, speedThreshUpper = NULL)
  filtBoth <- filterLocs(df = a, speedThreshLower = l, speedThreshUpper = h)

  # rows are getting removed
  expect_equal(nrow(filtHigh) < nrow(a), TRUE)
  expect_equal(nrow(filtLow) < nrow(a), TRUE)
  expect_equal(nrow(filtBoth) < nrow(a), TRUE)
  expect_equal(nrow(filtBoth) < nrow(filtHigh) & nrow(filtBoth) < nrow(filtLow), TRUE)

  # data structure doesn't change
  expect_equal(class(filtBoth), class(a))
  expect_equal(ncol(filtBoth), ncol(a))

  # errors and warnings
  expect_warning(filterLocs(df = a, speedThreshLower = NULL, speedThreshUpper = NULL))
  expect_error(filterLocs(df = a, speedCol = "fakeCol"))
})




