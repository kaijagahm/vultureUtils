test_that("convertAndBuffer works", {
  # Set up sample data
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"))
  ncBuff <- convertAndBuffer(nc)
  ncNoCRS <- nc
  sf::st_crs(ncNoCRS) <- NA
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
  expect_error(convertAndBuffer(ncNoCRS)) # can't buffer if there's no CRS
})

test_that("filterLocs works", {
  base::load(test_path("testdata", "a.Rda"))

})




