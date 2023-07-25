# TODO: testing data organization, turn testing files into temp files, have target files be the only permanent files (testthat keeps newly generated testing files when diff)
test_that("cleanData snapshot test", {
  base::load(test_path("testdata", "month_data.Rda"))
  data <- month_data
  mask <- sf::st_read(test_path("testdata", "CutOffRegion.kml"))
  cleanData_testing_data <- vultureUtils::cleanData(data, mask, idCol = "tag_id")
  save(cleanData_testing_data,file=test_path("testdata", "cleanData_testing_data.Rda"))
  announce_snapshot_file("cleanData_target_data.Rda")
  expect_snapshot_file(test_path("testdata", "cleanData_testing_data.Rda"), "cleanData_target_data.Rda")
})

test_that("getFlightEdges no polygon sri snapshot test", {
  base::load(test_path("_snaps", "mainFunctions", "cleanData_target_data.Rda")) # can't reassign name
  cleaned_data <- cleanData_testing_data
  getFlightEdges_noPolygonSRI_testing_data <- vultureUtils::getFlightEdges(cleaned_data, roostPolygons = NULL, distThreshold = 1000, idCol = "tag_id", return ="sri")
  save(getFlightEdges_noPolygonSRI_testing_data,file=test_path("testdata", "getFlightEdges_noPolygonSRI_testing_data.Rda"))
  announce_snapshot_file("getFlightEdges_noPolygonSRI_target_data.Rda")
  expect_snapshot_file(test_path("testdata", "getFlightEdges_noPolygonSRI_testing_data.Rda"),
                       "getFlightEdges_noPolygonSRI_target_data.Rda")
})

test_that("getFlightEdges polygon sri snapshot test", {
  base::load(test_path("_snaps", "mainFunctions", "cleanData_target_data.Rda")) # can't reassign name
  cleaned_data <- cleanData_testing_data
  roostPolygons <- sf::st_read(test_path("testdata", "roosts50_kde95_cutOffRegion.kml"))
  getFlightEdges_PolygonSRI_testing_data <- vultureUtils::getFlightEdges(cleaned_data, roostPolygons = roostPolygons, distThreshold = 1000, idCol = "tag_id", return ="sri")
  save(getFlightEdges_PolygonSRI_testing_data,file=test_path("testdata", "getFlightEdges_PolygonSRI_testing_data.Rda"))
  announce_snapshot_file("getFlightEdges_PolygonSRI_target_data.Rda")
  expect_snapshot_file(test_path("testdata", "getFlightEdges_PolygonSRI_testing_data.Rda"),
                       "getFlightEdges_PolygonSRI_target_data.Rda")
})

test_that("getFlightEdges no polygon edges snapshot test", {
  base::load(test_path("_snaps", "mainFunctions", "cleanData_target_data.Rda")) # can't reassign name
  cleaned_data <- cleanData_testing_data
  getFlightEdges_noPolygonEdges_testing_data <- vultureUtils::getFlightEdges(cleaned_data, roostPolygons = NULL, distThreshold = 1000, idCol = "tag_id", return ="edges")
  save(getFlightEdges_noPolygonEdges_testing_data,file=test_path("testdata", "getFlightEdges_noPolygonEdges_testing_data.Rda"))
  announce_snapshot_file("getFlightEdges_noPolygonEdges_target_data.Rda")
  expect_snapshot_file(test_path("testdata", "getFlightEdges_noPolygonEdges_testing_data.Rda"),
                       "getFlightEdges_noPolygonEdges_target_data.Rda")
})

test_that("getFlightEdges polygon edges snapshot test", {
  base::load(test_path("_snaps", "mainFunctions", "cleanData_target_data.Rda")) # can't reassign name
  cleaned_data <- cleanData_testing_data
  roostPolygons <- sf::st_read(test_path("testdata", "roosts50_kde95_cutOffRegion.kml"))
  getFlightEdges_PolygonEdges_testing_data <- vultureUtils::getFlightEdges(cleaned_data, roostPolygons = roostPolygons, distThreshold = 1000, idCol = "tag_id", return ="edges")
  save(getFlightEdges_PolygonEdges_testing_data,file=test_path("testdata", "getFlightEdges_PolygonEdges_testing_data.Rda"))
  announce_snapshot_file("getFlightEdges_PolygonEdges_target_data.Rda")
  expect_snapshot_file(test_path("testdata", "getFlightEdges_PolygonEdges_testing_data.Rda"),
                       "getFlightEdges_PolygonEdges_target_data.Rda")
})

test_that("getFeedingEdges no polygon sri snapshot test", {
  base::load(test_path("_snaps", "mainFunctions", "cleanData_target_data.Rda")) # can't reassign name
  cleaned_data <- cleanData_testing_data
  getFeedingEdges_noPolygonSRI_testing_data <- vultureUtils::getFeedingEdges(cleaned_data, roostPolygons = NULL, distThreshold = 50, idCol = "tag_id", return ="sri")
  save(getFeedingEdges_noPolygonSRI_testing_data,file=test_path("testdata", "getFeedingEdges_noPolygonSRI_testing_data.Rda"))
  announce_snapshot_file("getFeedingEdges_noPolygonSRI_target_data.Rda")
  expect_snapshot_file(test_path("testdata", "getFeedingEdges_noPolygonSRI_testing_data.Rda"),
                       "getFeedingEdges_noPolygonSRI_target_data.Rda")
})

test_that("getFeedingEdges polygon sri snapshot test", {
  base::load(test_path("_snaps", "mainFunctions", "cleanData_target_data.Rda")) # can't reassign name
  cleaned_data <- cleanData_testing_data
  roostPolygons <- sf::st_read(test_path("testdata", "roosts50_kde95_cutOffRegion.kml"))
  getFeedingEdges_PolygonSRI_testing_data <- vultureUtils::getFeedingEdges(cleaned_data, roostPolygons = roostPolygons, distThreshold = 50, idCol = "tag_id", return ="sri")
  save(getFeedingEdges_PolygonSRI_testing_data,file=test_path("testdata", "getFeedingEdges_PolygonSRI_testing_data.Rda"))
  announce_snapshot_file("getFeedingEdges_PolygonSRI_target_data.Rda")
  expect_snapshot_file(test_path("testdata", "getFeedingEdges_PolygonSRI_testing_data.Rda"),
                       "getFeedingEdges_PolygonSRI_target_data.Rda")
})

test_that("getFeedingEdges no polygon edges snapshot test", {
  base::load(test_path("_snaps", "mainFunctions", "cleanData_target_data.Rda")) # can't reassign name
  cleaned_data <- cleanData_testing_data
  getFeedingEdges_noPolygonEdges_testing_data <- vultureUtils::getFeedingEdges(cleaned_data, roostPolygons = NULL, distThreshold = 50, idCol = "tag_id", return ="edges")
  save(getFeedingEdges_noPolygonEdges_testing_data,file=test_path("testdata", "getFeedingEdges_noPolygonEdges_testing_data.Rda"))
  announce_snapshot_file("getFeedingEdges_noPolygonEdges_target_data.Rda")
  expect_snapshot_file(test_path("testdata", "getFeedingEdges_noPolygonEdges_testing_data.Rda"),
                       "getFeedingEdges_noPolygonEdges_target_data.Rda")
})

test_that("getFeedingEdges polygon edges snapshot test", {
  base::load(test_path("_snaps", "mainFunctions", "cleanData_target_data.Rda")) # can't reassign name
  cleaned_data <- cleanData_testing_data
  roostPolygons <- sf::st_read(test_path("testdata", "roosts50_kde95_cutOffRegion.kml"))
  getFeedingEdges_PolygonEdges_testing_data <- vultureUtils::getFeedingEdges(cleaned_data, roostPolygons = roostPolygons, distThreshold = 50, idCol = "tag_id", return ="edges")
  save(getFeedingEdges_PolygonEdges_testing_data,file=test_path("testdata", "getFeedingEdges_PolygonEdges_testing_data.Rda"))
  announce_snapshot_file("getFeedingEdges_PolygonEdges_target_data.Rda")
  expect_snapshot_file(test_path("testdata", "getFeedingEdges_PolygonEdges_testing_data.Rda"),
                       "getFeedingEdges_PolygonEdges_target_data.Rda")
})

test_that("getRoostEdges no polygon sri snapshot test", {
  base::load(test_path("_snaps", "mainFunctions", "get_roosts_df_target_data.Rda")) # can't reassign name
  roosts_data <- get_roosts_df_testing_data
  getRoostEdges_noPolygonSRI_testing_data <- vultureUtils::getRoostEdges(roosts_data, roostPolygons = NULL, idCol = "tag_id", return ="sri", latCol = "location_lat", longCol = "location_long", dateCol = "roost_date")
  save(getRoostEdges_noPolygonSRI_testing_data,file=test_path("testdata", "getRoostEdges_noPolygonSRI_testing_data.Rda"))
  announce_snapshot_file("getRoostEdges_noPolygonSRI_target_data.Rda")
  expect_snapshot_file(test_path("testdata", "getRoostEdges_noPolygonSRI_testing_data.Rda"),
                       "getRoostEdges_noPolygonSRI_target_data.Rda")
})

test_that("getRoostEdges polygon sri snapshot test", {
  base::load(test_path("_snaps", "mainFunctions", "get_roosts_df_target_data.Rda")) # can't reassign name
  roosts_data <- get_roosts_df_testing_data
  roostPolygons <- sf::st_read(test_path("testdata", "roosts50_kde95_cutOffRegion.kml"))
  getRoostEdges_PolygonSRI_testing_data <- vultureUtils::getRoostEdges(roosts_data, roostPolygons = roostPolygons, idCol = "tag_id", return ="sri", latCol = "location_lat", longCol = "location_long", dateCol = "roost_date")
  save(getRoostEdges_PolygonSRI_testing_data,file=test_path("testdata", "getRoostEdges_PolygonSRI_testing_data.Rda"))
  announce_snapshot_file("getRoostEdges_PolygonSRI_target_data.Rda")
  expect_snapshot_file(test_path("testdata", "getRoostEdges_PolygonSRI_testing_data.Rda"),
                       "getRoostEdges_PolygonSRI_target_data.Rda")
})

test_that("getRoostEdges no polygon edges snapshot test", {
  base::load(test_path("_snaps", "mainFunctions", "get_roosts_df_target_data.Rda")) # can't reassign name
  roosts_data <- get_roosts_df_testing_data
  getRoostEdges_noPolygonEdges_testing_data <- vultureUtils::getRoostEdges(roosts_data, roostPolygons = NULL, idCol = "tag_id", return ="edges", latCol = "location_lat", longCol = "location_long", dateCol = "roost_date")
  save(getRoostEdges_noPolygonEdges_testing_data,file=test_path("testdata", "getRoostEdges_noPolygonEdges_testing_data.Rda"))
  announce_snapshot_file("getRoostEdges_noPolygonEdges_target_data.Rda")
  expect_snapshot_file(test_path("testdata", "getRoostEdges_noPolygonEdges_testing_data.Rda"),
                       "getRoostEdges_noPolygonEdges_target_data.Rda")
})

test_that("getRoostEdges polygon edges snapshot test", {
  base::load(test_path("_snaps", "mainFunctions", "get_roosts_df_target_data.Rda")) # can't reassign name
  roosts_data <- get_roosts_df_testing_data
  roostPolygons <- sf::st_read(test_path("testdata", "roosts50_kde95_cutOffRegion.kml"))
  getRoostEdges_PolygonEdges_testing_data <- vultureUtils::getRoostEdges(roosts_data, roostPolygons = roostPolygons, idCol = "tag_id", return ="edges", latCol = "location_lat", longCol = "location_long", dateCol = "roost_date")
  save(getRoostEdges_PolygonEdges_testing_data,file=test_path("testdata", "getRoostEdges_PolygonEdges_testing_data.Rda"))
  announce_snapshot_file("getRoostEdges_PolygonEdges_target_data.Rda")
  expect_snapshot_file(test_path("testdata", "getRoostEdges_PolygonEdges_testing_data.Rda"),
                       "getRoostEdges_PolygonEdges_target_data.Rda")
})

test_that("get_roosts_df snapshot test", {
  base::load(test_path("_snaps", "mainFunctions", "cleanData_target_data.Rda"))
  cleaned_data <- cleanData_testing_data
  get_roosts_df_testing_data <- vultureUtils::get_roosts_df(cleaned_data, id = "tag_id", timestamp = "timestamp", x = "location_long", y = "location_lat", ground_speed = "ground_speed", speed_units = "m/s", quiet = F)
  save(get_roosts_df_testing_data,file=test_path("testdata", "get_roosts_df_testing_data.Rda"))
  announce_snapshot_file("get_roosts_df_target_data.Rda")
  expect_snapshot_file(test_path("testdata", "get_roosts_df_testing_data.Rda"), "get_roosts_df_target_data.Rda")
})

test_that("makeGraph unweighted flight no polygon sri snapshot test", {
  base::load(test_path("_snaps", "mainFunctions", "getFlightEdges_noPolygonSRI_target_data.Rda"))
  sri_data <- getFlightEdges_noPolygonSRI_testing_data
  flight_no_poly_sri_unweighted_testing_data <- vultureUtils::makeGraph(mode = "sri", data = sri_data, weighted = F)
  igraph::write_graph(flight_no_poly_sri_unweighted_testing_data, file=test_path("testdata", "flight_no_poly_sri_unweighted_testing_data.txt"), "edgelist")
  announce_snapshot_file("flight_no_poly_sri_unweighted_target_data.txt")
  expect_snapshot_file(test_path("testdata", "flight_no_poly_sri_unweighted_testing_data.txt"), "flight_no_poly_sri_unweighted_target_data.txt")
})

test_that("makeGraph unweighted flight polygon sri snapshot test", {
  base::load(test_path("_snaps", "mainFunctions", "getFlightEdges_PolygonSRI_target_data.Rda"))
  sri_data <- getFlightEdges_PolygonSRI_testing_data
  flight_poly_sri_unweighted_testing_data <- vultureUtils::makeGraph(mode = "sri", data = sri_data, weighted = F)
  igraph::write_graph(flight_poly_sri_unweighted_testing_data, file=test_path("testdata", "flight_poly_sri_unweighted_testing_data.txt"), "edgelist")
  announce_snapshot_file("flight_poly_sri_unweighted_target_data.txt")
  expect_snapshot_file(test_path("testdata", "flight_poly_sri_unweighted_testing_data.txt"), "flight_poly_sri_unweighted_target_data.txt")
})

test_that("makeGraph unweighted flight no polygon edgelist snapshot test", {
  base::load(test_path("_snaps", "mainFunctions", "getFlightEdges_noPolygonEdges_target_data.Rda"))
  edges_data <- getFlightEdges_noPolygonEdges_testing_data
  flight_no_poly_edgelist_unweighted_testing_data <- vultureUtils::makeGraph(mode = "edgelist", data = edges_data, weighted = F)
  igraph::write_graph(flight_no_poly_edgelist_unweighted_testing_data, file=test_path("testdata", "flight_no_poly_edgelist_unweighted_testing_data.txt"), "edgelist")
  announce_snapshot_file("flight_no_poly_edgelist_unweighted_target_data.txt")
  expect_snapshot_file(test_path("testdata", "flight_no_poly_edgelist_unweighted_testing_data.txt"), "flight_no_poly_edgelist_unweighted_target_data.txt")
})

test_that("makeGraph unweighted flight polygon edgelist snapshot test", {
  base::load(test_path("_snaps", "mainFunctions", "getFlightEdges_PolygonEdges_target_data.Rda"))
  edges_data <- getFlightEdges_PolygonEdges_testing_data
  flight_poly_edgelist_unweighted_testing_data <- vultureUtils::makeGraph(mode = "edgelist", data = edges_data, weighted = F)
  igraph::write_graph(flight_poly_edgelist_unweighted_testing_data, file=test_path("testdata", "flight_poly_edgelist_unweighted_testing_data.txt"), "edgelist")
  announce_snapshot_file("flight_poly_edgelist_unweighted_target_data.txt")
  expect_snapshot_file(test_path("testdata", "flight_poly_edgelist_unweighted_testing_data.txt"), "flight_poly_edgelist_unweighted_target_data.txt")
})

test_that("makeGraph unweighted feeding no polygon sri snapshot test", {
  base::load(test_path("_snaps", "mainFunctions", "getFeedingEdges_noPolygonSRI_target_data.Rda"))
  sri_data <- getFeedingEdges_noPolygonSRI_testing_data
  feeding_no_poly_sri_unweighted_testing_data <- vultureUtils::makeGraph(mode = "sri", data = sri_data, weighted = F)
  igraph::write_graph(feeding_no_poly_sri_unweighted_testing_data, file=test_path("testdata", "feeding_no_poly_sri_unweighted_testing_data.txt"), "edgelist")
  announce_snapshot_file("feeding_no_poly_sri_unweighted_target_data.txt")
  expect_snapshot_file(test_path("testdata", "feeding_no_poly_sri_unweighted_testing_data.txt"), "feeding_no_poly_sri_unweighted_target_data.txt")
})

test_that("makeGraph unweighted feeding polygon sri snapshot test", {
  base::load(test_path("_snaps", "mainFunctions", "getFeedingEdges_PolygonSRI_target_data.Rda"))
  sri_data <- getFeedingEdges_PolygonSRI_testing_data
  feeding_poly_sri_unweighted_testing_data <- vultureUtils::makeGraph(mode = "sri", data = sri_data, weighted = F)
  igraph::write_graph(feeding_poly_sri_unweighted_testing_data, file=test_path("testdata", "feeding_poly_sri_unweighted_testing_data.txt"), "edgelist")
  announce_snapshot_file("feeding_poly_sri_unweighted_target_data.txt")
  expect_snapshot_file(test_path("testdata", "feeding_poly_sri_unweighted_testing_data.txt"), "feeding_poly_sri_unweighted_target_data.txt")
})

test_that("makeGraph unweighted feeding no polygon edgelist snapshot test", {
  base::load(test_path("_snaps", "mainFunctions", "getFeedingEdges_noPolygonEdges_target_data.Rda"))
  edges_data <- getFeedingEdges_noPolygonEdges_testing_data
  feeding_no_poly_edgelist_unweighted_testing_data <- vultureUtils::makeGraph(mode = "edgelist", data = edges_data, weighted = F)
  igraph::write_graph(feeding_no_poly_edgelist_unweighted_testing_data, file=test_path("testdata", "feeding_no_poly_edgelist_unweighted_testing_data.txt"), "edgelist")
  announce_snapshot_file("feeding_no_poly_edgelist_unweighted_target_data.txt")
  expect_snapshot_file(test_path("testdata", "feeding_no_poly_edgelist_unweighted_testing_data.txt"), "feeding_no_poly_edgelist_unweighted_target_data.txt")
})

test_that("makeGraph unweighted feeding polygon edgelist snapshot test", {
  base::load(test_path("_snaps", "mainFunctions", "getFeedingEdges_PolygonEdges_target_data.Rda"))
  edges_data <- getFeedingEdges_PolygonEdges_testing_data
  feeding_poly_edgelist_unweighted_testing_data <- vultureUtils::makeGraph(mode = "edgelist", data = edges_data, weighted = F)
  igraph::write_graph(feeding_poly_edgelist_unweighted_testing_data, file=test_path("testdata", "feeding_poly_edgelist_unweighted_testing_data.txt"), "edgelist")
  announce_snapshot_file("feeding_poly_edgelist_unweighted_target_data.txt")
  expect_snapshot_file(test_path("testdata", "feeding_poly_edgelist_unweighted_testing_data.txt"), "feeding_poly_edgelist_unweighted_target_data.txt")
})

test_that("makeGraph unweighted roost no polygon sri snapshot test", {
  base::load(test_path("_snaps", "mainFunctions", "getRoostEdges_noPolygonSRI_target_data.Rda"))
  sri_data <- getRoostEdges_noPolygonSRI_testing_data
  roost_no_poly_sri_unweighted_testing_data <- vultureUtils::makeGraph(mode = "sri", data = sri_data, weighted = F)
  igraph::write_graph(roost_no_poly_sri_unweighted_testing_data, file=test_path("testdata", "roost_no_poly_sri_unweighted_testing_data.txt"), "edgelist")
  announce_snapshot_file("roost_no_poly_sri_unweighted_target_data.txt")
  expect_snapshot_file(test_path("testdata", "roost_no_poly_sri_unweighted_testing_data.txt"), "roost_no_poly_sri_unweighted_target_data.txt")
})

test_that("makeGraph unweighted roost polygon sri snapshot test", {
  base::load(test_path("_snaps", "mainFunctions", "getRoostEdges_PolygonSRI_target_data.Rda"))
  sri_data <- getRoostEdges_PolygonSRI_testing_data
  roost_poly_sri_unweighted_testing_data <- vultureUtils::makeGraph(mode = "sri", data = sri_data, weighted = F)
  igraph::write_graph(roost_poly_sri_unweighted_testing_data, file=test_path("testdata", "roost_poly_sri_unweighted_testing_data.txt"), "edgelist")
  announce_snapshot_file("roost_poly_sri_unweighted_target_data.txt")
  expect_snapshot_file(test_path("testdata", "roost_poly_sri_unweighted_testing_data.txt"), "roost_poly_sri_unweighted_target_data.txt")
})

test_that("makeGraph unweighted roost no polygon edgelist snapshot test", {
  base::load(test_path("_snaps", "mainFunctions", "getRoostEdges_noPolygonEdges_target_data.Rda"))
  edges_data <- getRoostEdges_noPolygonEdges_testing_data
  roost_no_poly_edgelist_unweighted_testing_data <- vultureUtils::makeGraph(mode = "edgelist", data = edges_data, weighted = F)
  igraph::write_graph(roost_no_poly_edgelist_unweighted_testing_data, file=test_path("testdata", "roost_no_poly_edgelist_unweighted_testing_data.txt"), "edgelist")
  announce_snapshot_file("roost_no_poly_edgelist_unweighted_target_data.txt")
  expect_snapshot_file(test_path("testdata", "roost_no_poly_edgelist_unweighted_testing_data.txt"), "roost_no_poly_edgelist_unweighted_target_data.txt")
})

test_that("makeGraph unweighted roost polygon edgelist snapshot test", {
  base::load(test_path("_snaps", "mainFunctions", "getRoostEdges_PolygonEdges_target_data.Rda"))
  edges_data <- getRoostEdges_PolygonEdges_testing_data
  roost_poly_edgelist_unweighted_testing_data <- vultureUtils::makeGraph(mode = "edgelist", data = edges_data, weighted = F)
  igraph::write_graph(roost_poly_edgelist_unweighted_testing_data, file=test_path("testdata", "roost_poly_edgelist_unweighted_testing_data.txt"), "edgelist")
  announce_snapshot_file("roost_poly_edgelist_unweighted_target_data.txt")
  expect_snapshot_file(test_path("testdata", "roost_poly_edgelist_unweighted_testing_data.txt"), "roost_poly_edgelist_unweighted_target_data.txt")
})
test_that("makeGraph weighted flight no polygon sri snapshot test", {
  base::load(test_path("_snaps", "mainFunctions", "getFlightEdges_noPolygonSRI_target_data.Rda"))
  sri_data <- getFlightEdges_noPolygonSRI_testing_data
  flight_no_poly_sri_weighted_testing_data <- vultureUtils::makeGraph(mode = "sri", data = sri_data, weighted = T)
  igraph::write_graph(flight_no_poly_sri_weighted_testing_data, file=test_path("testdata", "flight_no_poly_sri_weighted_testing_data.txt"), "edgelist")
  announce_snapshot_file("flight_no_poly_sri_weighted_target_data.txt")
  expect_snapshot_file(test_path("testdata", "flight_no_poly_sri_weighted_testing_data.txt"), "flight_no_poly_sri_weighted_target_data.txt")
})

test_that("makeGraph weighted flight polygon sri snapshot test", {
  base::load(test_path("_snaps", "mainFunctions", "getFlightEdges_PolygonSRI_target_data.Rda"))
  sri_data <- getFlightEdges_PolygonSRI_testing_data
  flight_poly_sri_weighted_testing_data <- vultureUtils::makeGraph(mode = "sri", data = sri_data, weighted = T)
  igraph::write_graph(flight_poly_sri_weighted_testing_data, file=test_path("testdata", "flight_poly_sri_weighted_testing_data.txt"), "edgelist")
  announce_snapshot_file("flight_poly_sri_weighted_target_data.txt")
  expect_snapshot_file(test_path("testdata", "flight_poly_sri_weighted_testing_data.txt"), "flight_poly_sri_weighted_target_data.txt")
})

test_that("makeGraph weighted flight no polygon edgelist snapshot test", {
  base::load(test_path("_snaps", "mainFunctions", "getFlightEdges_noPolygonEdges_target_data.Rda"))
  edges_data <- getFlightEdges_noPolygonEdges_testing_data
  flight_no_poly_edgelist_weighted_testing_data <- vultureUtils::makeGraph(mode = "edgelist", data = edges_data, weighted = T)
  igraph::write_graph(flight_no_poly_edgelist_weighted_testing_data, file=test_path("testdata", "flight_no_poly_edgelist_weighted_testing_data.txt"), "edgelist")
  announce_snapshot_file("flight_no_poly_edgelist_weighted_target_data.txt")
  expect_snapshot_file(test_path("testdata", "flight_no_poly_edgelist_weighted_testing_data.txt"), "flight_no_poly_edgelist_weighted_target_data.txt")
})

test_that("makeGraph weighted flight polygon edgelist snapshot test", {
  base::load(test_path("_snaps", "mainFunctions", "getFlightEdges_PolygonEdges_target_data.Rda"))
  edges_data <- getFlightEdges_PolygonEdges_testing_data
  flight_poly_edgelist_weighted_testing_data <- vultureUtils::makeGraph(mode = "edgelist", data = edges_data, weighted = T)
  igraph::write_graph(flight_poly_edgelist_weighted_testing_data, file=test_path("testdata", "flight_poly_edgelist_weighted_testing_data.txt"), "edgelist")
  announce_snapshot_file("flight_poly_edgelist_weighted_target_data.txt")
  expect_snapshot_file(test_path("testdata", "flight_poly_edgelist_weighted_testing_data.txt"), "flight_poly_edgelist_weighted_target_data.txt")
})

test_that("makeGraph weighted feeding no polygon sri snapshot test", {
  base::load(test_path("_snaps", "mainFunctions", "getFeedingEdges_noPolygonSRI_target_data.Rda"))
  sri_data <- getFeedingEdges_noPolygonSRI_testing_data
  feeding_no_poly_sri_weighted_testing_data <- vultureUtils::makeGraph(mode = "sri", data = sri_data, weighted = T)
  igraph::write_graph(feeding_no_poly_sri_weighted_testing_data, file=test_path("testdata", "feeding_no_poly_sri_weighted_testing_data.txt"), "edgelist")
  announce_snapshot_file("feeding_no_poly_sri_weighted_target_data.txt")
  expect_snapshot_file(test_path("testdata", "feeding_no_poly_sri_weighted_testing_data.txt"), "feeding_no_poly_sri_weighted_target_data.txt")
})

test_that("makeGraph weighted feeding polygon sri snapshot test", {
  base::load(test_path("_snaps", "mainFunctions", "getFeedingEdges_PolygonSRI_target_data.Rda"))
  sri_data <- getFeedingEdges_PolygonSRI_testing_data
  feeding_poly_sri_weighted_testing_data <- vultureUtils::makeGraph(mode = "sri", data = sri_data, weighted = T)
  igraph::write_graph(feeding_poly_sri_weighted_testing_data, file=test_path("testdata", "feeding_poly_sri_weighted_testing_data.txt"), "edgelist")
  announce_snapshot_file("feeding_poly_sri_weighted_target_data.txt")
  expect_snapshot_file(test_path("testdata", "feeding_poly_sri_weighted_testing_data.txt"), "feeding_poly_sri_weighted_target_data.txt")
})

test_that("makeGraph weighted feeding no polygon edgelist snapshot test", {
  base::load(test_path("_snaps", "mainFunctions", "getFeedingEdges_noPolygonEdges_target_data.Rda"))
  edges_data <- getFeedingEdges_noPolygonEdges_testing_data
  feeding_no_poly_edgelist_weighted_testing_data <- vultureUtils::makeGraph(mode = "edgelist", data = edges_data, weighted = T)
  igraph::write_graph(feeding_no_poly_edgelist_weighted_testing_data, file=test_path("testdata", "feeding_no_poly_edgelist_weighted_testing_data.txt"), "edgelist")
  announce_snapshot_file("feeding_no_poly_edgelist_weighted_target_data.txt")
  expect_snapshot_file(test_path("testdata", "feeding_no_poly_edgelist_weighted_testing_data.txt"), "feeding_no_poly_edgelist_weighted_target_data.txt")
})

test_that("makeGraph weighted feeding polygon edgelist snapshot test", {
  base::load(test_path("_snaps", "mainFunctions", "getFeedingEdges_PolygonEdges_target_data.Rda"))
  edges_data <- getFeedingEdges_PolygonEdges_testing_data
  feeding_poly_edgelist_weighted_testing_data <- vultureUtils::makeGraph(mode = "edgelist", data = edges_data, weighted = T)
  igraph::write_graph(feeding_poly_edgelist_weighted_testing_data, file=test_path("testdata", "feeding_poly_edgelist_weighted_testing_data.txt"), "edgelist")
  announce_snapshot_file("feeding_poly_edgelist_weighted_target_data.txt")
  expect_snapshot_file(test_path("testdata", "feeding_poly_edgelist_weighted_testing_data.txt"), "feeding_poly_edgelist_weighted_target_data.txt")
})

test_that("makeGraph weighted roost no polygon sri snapshot test", {
  base::load(test_path("_snaps", "mainFunctions", "getRoostEdges_noPolygonSRI_target_data.Rda"))
  sri_data <- getRoostEdges_noPolygonSRI_testing_data
  roost_no_poly_sri_weighted_testing_data <- vultureUtils::makeGraph(mode = "sri", data = sri_data, weighted = T)
  igraph::write_graph(roost_no_poly_sri_weighted_testing_data, file=test_path("testdata", "roost_no_poly_sri_weighted_testing_data.txt"), "edgelist")
  announce_snapshot_file("roost_no_poly_sri_weighted_target_data.txt")
  expect_snapshot_file(test_path("testdata", "roost_no_poly_sri_weighted_testing_data.txt"), "roost_no_poly_sri_weighted_target_data.txt")
})

test_that("makeGraph weighted roost polygon sri snapshot test", {
  base::load(test_path("_snaps", "mainFunctions", "getRoostEdges_PolygonSRI_target_data.Rda"))
  sri_data <- getRoostEdges_PolygonSRI_testing_data
  roost_poly_sri_weighted_testing_data <- vultureUtils::makeGraph(mode = "sri", data = sri_data, weighted = T)
  igraph::write_graph(roost_poly_sri_weighted_testing_data, file=test_path("testdata", "roost_poly_sri_weighted_testing_data.txt"), "edgelist")
  announce_snapshot_file("roost_poly_sri_weighted_target_data.txt")
  expect_snapshot_file(test_path("testdata", "roost_poly_sri_weighted_testing_data.txt"), "roost_poly_sri_weighted_target_data.txt")
})

test_that("makeGraph weighted roost no polygon edgelist snapshot test", {
  base::load(test_path("_snaps", "mainFunctions", "getRoostEdges_noPolygonEdges_target_data.Rda"))
  edges_data <- getRoostEdges_noPolygonEdges_testing_data
  roost_no_poly_edgelist_weighted_testing_data <- vultureUtils::makeGraph(mode = "edgelist", data = edges_data, weighted = T)
  igraph::write_graph(roost_no_poly_edgelist_weighted_testing_data, file=test_path("testdata", "roost_no_poly_edgelist_weighted_testing_data.txt"), "edgelist")
  announce_snapshot_file("roost_no_poly_edgelist_weighted_target_data.txt")
  expect_snapshot_file(test_path("testdata", "roost_no_poly_edgelist_weighted_testing_data.txt"), "roost_no_poly_edgelist_weighted_target_data.txt")
})

test_that("makeGraph weighted roost polygon edgelist snapshot test", {
  base::load(test_path("_snaps", "mainFunctions", "getRoostEdges_PolygonEdges_target_data.Rda"))
  edges_data <- getRoostEdges_PolygonEdges_testing_data
  roost_poly_edgelist_weighted_testing_data <- vultureUtils::makeGraph(mode = "edgelist", data = edges_data, weighted = T)
  igraph::write_graph(roost_poly_edgelist_weighted_testing_data, file=test_path("testdata", "roost_poly_edgelist_weighted_testing_data.txt"), "edgelist")
  announce_snapshot_file("roost_poly_edgelist_weighted_target_data.txt")
  expect_snapshot_file(test_path("testdata", "roost_poly_edgelist_weighted_testing_data.txt"), "roost_poly_edgelist_weighted_target_data.txt")
})
