# convertAndBuffer
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

# filterLocs
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

# maskData
test_that("maskData works", {
  base::load(test_path("testdata", "ed_0905_0908.Rda"))
  a <- ed_0905_0908
  mask <- sf::st_read(test_path("testdata", "CutOffRegion.kml"))
  smallMask <- sf::st_buffer(mask %>% sf::st_transform(32636), -100000)
  m <- maskData(dataset = a, mask = smallMask)
  # should get the same result if the input is not an sf object.
  a_notSF <- sf::st_drop_geometry(a)
  mm <- maskData(dataset = a_notSF, mask = smallMask)

  # expectations about the masked dataset
  expect_equal(nrow(m) < nrow(a), TRUE)
  expect_equal(class(a), class(m))
  expect_equal(m, mm)
})

test_that("consecEdges works", {
  base::load(test_path("testdata", "edges_2021.08.27_2021.09.10_25m_20min.Rda"))
  a <- edges_2021.08.27_2021.09.10_25m_20min # rename this to make it easier to read
  cs1 <- consecEdges(edgeList = a, consecThreshold = 1)
  cs2 <- consecEdges(edgeList = a, consecThreshold = 2)
  cs5 <- consecEdges(edgeList = a, consecThreshold = 5)
  cs100 <- consecEdges(edgeList = a, consecThreshold = 100)
  expect_equal(nrow(cs1), 21550)
  expect_equal(nrow(cs2), 16914)
  expect_equal(nrow(cs5), 6075)
  expect_equal(nrow(cs100), 0)
})



