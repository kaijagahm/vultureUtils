# test_that("downloadVultures works", {
#   base::load("~/Desktop/otherDir/movebankCredentials/pw.Rda")
#   MB.LoginObject <- move::movebankLogin(username = 'kaijagahm', password = pw)
#   rm(pw)
#
#   expected_30min <- suppressMessages(suppressWarnings(downloadVultures(loginObject = MB.LoginObject, extraSensors = F, removeDup = T, dateTimeStartUTC = "2021-03-01 06:00", dateTimeEndUTC = "2021-03-01 06:30", addDateOnly = T, dfConvert = T, quiet = F)))
#
#   expected_30min_noDateOnly <- suppressMessages(suppressWarnings(downloadVultures(loginObject = MB.LoginObject, extraSensors = F, removeDup = T, dateTimeStartUTC = "2021-03-01 06:00", dateTimeEndUTC = "2021-03-01 06:30", addDateOnly = F, dfConvert = T, quiet = F)))
#
#   # without dateOnly
#   expect_s3_class(expected_30min, class = "data.frame")
#   expect_equal(all(c("location_lat", "location_long", "timestamp", "trackId", "gps_time_to_fix", "heading", "gps_satellite_count", "ground_speed", "external_temperature", "barometric_height", "dateOnly") %in% names(expected_30min)), TRUE)
#
#   # with dateOnly
#   expect_s3_class(expected_30min_noDateOnly, class = "data.frame")
#   expect_equal(all(c("location_lat", "location_long", "timestamp", "trackId", "gps_time_to_fix", "heading", "gps_satellite_count", "ground_speed", "external_temperature", "barometric_height") %in% names(expected_30min_noDateOnly)), TRUE)
#
# })

base::load("~/Desktop/otherDir/jan01_03_2021_raw.Rda")
mask <- sf::st_read("~/Desktop/otherDir/CutOffRegion.kml")
roosts <- sf::st_read("~/Desktop/otherDir/roosts25_cutOffRegion.kml")
crs <- "WGS84"
crsITM <- 32636

# test_that("data can be cleaned", {
#   cleaned <- cleanData(dataset = jan01_03_2021_raw, mask = mask, crs = crs)
#
#   expect_s3_class(cleaned, class = "data.frame") # should be a data frame
#   expect_s3_class(cleaned, class = "sf") # should be an sf object
#   expect_equal(all(sf::st_is(cleaned, "POINT")), TRUE) # should be an sf points object
#   expect_equal(all(c("location_lat", "location_long", "timestamp", "trackId") %in% names(cleaned)), TRUE) # should include necessary columns
#   expect_equal(sf::st_crs(cleaned), sf::st_crs(crs)) # make sure the crs got set properly
# })
#
# test_that("getEdges works", {
#   base::load("~/Desktop/otherDir/cleaned_jan.Rda")
#   edges <- suppressWarnings(getEdges(dataset = cleaned_jan, roostPolygons = roosts, roostBuffer = 50, consecThreshold = 2, distThreshold = 100, speedThreshUpper = NULL, speedThreshLower = NULL, timeThreshold = "10 minutes", return = "edges"))
#   expect_s3_class(edges, "data.frame")
#   expect_named(edges, c("timegroup", "ID1", "ID2", "distance", "minTimestamp", "maxTimestamp"))
#
#   sri <- suppressWarnings(getEdges(dataset = cleaned_jan, roostPolygons = roosts, roostBuffer = 50, consecThreshold = 2, distThreshold = 100, speedThreshUpper = NULL, speedThreshLower = NULL, timeThreshold = "10 minutes", return = "sri"))
#   expect_s3_class(sri, "data.frame")
#   expect_named(sri, c("ID1", "ID2", "sri"))
#
#   both <- suppressWarnings(getEdges(dataset = cleaned_jan, roostPolygons = roosts, roostBuffer = 50, consecThreshold = 2, distThreshold = 100, speedThreshUpper = NULL, speedThreshLower = NULL, timeThreshold = "10 minutes", return = "both"))
#   expect_equal(is.list(both), TRUE)
#   expect_named(both, c("edges", "sri"))
#   expect_s3_class(both[["edges"]], "data.frame")
#   expect_named(both[["edges"]], c("timegroup", "ID1", "ID2", "distance", "minTimestamp", "maxTimestamp"))
#   expect_s3_class(both[["sri"]], "data.frame")
#   expect_named(both[["sri"]], c("ID1", "ID2", "sri"))
# })
#
# test_that("getFeedingEdges works", {
#   base::load("~/Desktop/otherDir/cleaned_jan.Rda")
#   edges <- getFeedingEdges(dataset = cleaned_jan, roostPolygons = roosts, roostBuffer = 50, consecThreshold = 2, distThreshold = 50, speedThreshUpper = 5, speedThreshLower = NULL, timeThreshold = "10 minutes", return = "edges")
#   expect_s3_class(edges, "data.frame")
#   expect_named(edges, c("timegroup", "ID1", "ID2", "distance", "minTimestamp", "maxTimestamp"))
#
#   sri <- getFeedingEdges(dataset = cleaned_jan, roostPolygons = roosts, roostBuffer = 50, consecThreshold = 2, distThreshold = 50, speedThreshUpper = 5, speedThreshLower = NULL, timeThreshold = "10 minutes", return = "sri")
#   expect_s3_class(sri, "data.frame")
#   expect_named(sri, c("ID1", "ID2", "sri"))
#
#   both <- getFeedingEdges(dataset = cleaned_jan, roostPolygons = roosts, roostBuffer = 50, consecThreshold = 2, distThreshold = 50, speedThreshUpper = 5, speedThreshLower = NULL, timeThreshold = "10 minutes", return = "both")
#   expect_equal(is.list(both), TRUE)
#   expect_named(both, c("edges", "sri"))
#   expect_s3_class(both[["edges"]], "data.frame")
#   expect_named(both[["edges"]], c("timegroup", "ID1", "ID2", "distance", "minTimestamp", "maxTimestamp"))
#   expect_s3_class(both[["sri"]], "data.frame")
#   expect_named(both[["sri"]], c("ID1", "ID2", "sri"))
# })
#
# test_that("getFlightEdges works", {
#   base::load("~/Desktop/otherDir/cleaned_jan.Rda")
#   edges <- getFlightEdges(dataset = cleaned_jan, roostPolygons = roosts, roostBuffer = 50, consecThreshold = 2, distThreshold = 1000, speedThreshUpper = NULL, speedThreshLower = 5, timeThreshold = "10 minutes", return = "edges")
#   expect_s3_class(edges, "data.frame")
#   expect_named(edges, c("timegroup", "ID1", "ID2", "distance", "minTimestamp", "maxTimestamp"))
#
#   sri <- getFlightEdges(dataset = cleaned_jan, roostPolygons = roosts, roostBuffer = 50, consecThreshold = 2, distThreshold = 1000, speedThreshUpper = NULL, speedThreshLower = 5, timeThreshold = "10 minutes", return = "sri")
#   expect_s3_class(sri, "data.frame")
#   expect_named(sri, c("ID1", "ID2", "sri"))
#
#   both <- getFlightEdges(dataset = cleaned_jan, roostPolygons = roosts, roostBuffer = 50, consecThreshold = 2, distThreshold = 1000, speedThreshUpper = NULL, speedThreshLower = 5, timeThreshold = "10 minutes", return = "both")
#   expect_equal(is.list(both), TRUE)
#   expect_named(both, c("edges", "sri"))
#   expect_s3_class(both[["edges"]], "data.frame")
#   expect_named(both[["edges"]], c("timegroup", "ID1", "ID2", "distance", "minTimestamp", "maxTimestamp"))
#   expect_s3_class(both[["sri"]], "data.frame")
#   expect_named(both[["sri"]], c("ID1", "ID2", "sri"))
# })

test_that("roosts can be calculated", {
  base::load("~/Desktop/otherDir/cleaned_jan.Rda")
  roosts <- get_roosts_df(cleaned_jan, id = "trackId")

  expect_s3_class(roosts, "data.frame")
  expect_named(roosts, c("trackId", "date", "roost_date", "sunrise", "sunset", "sunrise_twilight", "sunset_twilight", "daylight", "is_roost", "location_lat", "location_long"))
})

base::load("~/Desktop/otherDir/janRoosts.Rda")

test_that("getRoostEdges works (distance mode)", {
  edges <- getRoostEdges(dataset = janRoosts, mode = "distance", distThreshold = 500, return = "edges")
  both <- getRoostEdges(dataset = janRoosts, mode = "distance", distThreshold = 500, return = "both")
  sri <- getRoostEdges(dataset = janRoosts, mode = "distance", distThreshold = 500, return = "sri")

  # XXX start here
})
