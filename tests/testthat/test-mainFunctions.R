# TODO: testing data organization, turn testing files into temp files, have target files be the only permanent files (testthat keeps newly generated testing files when diff)
test_that("cleanData snapshot test", {
  base::load(test_path("testdata", "month_data.Rda"))
  data <- month_data
  mask <- sf::st_read(test_path("testdata", "CutOffRegion.kml"))
  cleanData_data <- vultureUtils::cleanData(data, mask, idCol = "tag_id")
  withr::local_file("cleanData_data.Rda")
  save(cleanData_data,file="cleanData_data.Rda")
  announce_snapshot_file("cleanData_data.Rda")
  expect_snapshot_file("cleanData_data.Rda")
})

test_that("getFlightEdges no polygon sri snapshot test", {
  base::load(test_path("_snaps", "mainFunctions", "cleanData_data.Rda"))
  cleaned_data <- cleanData_data
  getFlightEdges_noPolygonSRI_data <- vultureUtils::getFlightEdges(cleaned_data, roostPolygons = NULL, distThreshold = 1000, idCol = "tag_id", return ="sri")
  withr::local_file("getFlightEdges_noPolygonSRI_data.Rda")
  save(getFlightEdges_noPolygonSRI_data,file="getFlightEdges_noPolygonSRI_data.Rda")
  announce_snapshot_file("getFlightEdges_noPolygonSRI_data.Rda")
  expect_snapshot_file("getFlightEdges_noPolygonSRI_data.Rda")
})

test_that("getFlightEdges polygon sri snapshot test", {
  base::load(test_path("_snaps", "mainFunctions", "cleanData_data.Rda"))
  cleaned_data <- cleanData_data
  roostPolygons <- sf::st_read(test_path("testdata", "roosts50_kde95_cutOffRegion.kml"))
  getFlightEdges_PolygonSRI_data <- vultureUtils::getFlightEdges(cleaned_data, roostPolygons = roostPolygons, distThreshold = 1000, idCol = "tag_id", return ="sri")
  withr::local_file("getFlightEdges_PolygonSRI_data.Rda")
  save(getFlightEdges_PolygonSRI_data,file="getFlightEdges_PolygonSRI_data.Rda")
  announce_snapshot_file("getFlightEdges_PolygonSRI_data.Rda")
  expect_snapshot_file("getFlightEdges_PolygonSRI_data.Rda")
})

test_that("getFlightEdges no polygon edges snapshot test", {
  base::load(test_path("_snaps", "mainFunctions", "cleanData_data.Rda"))
  cleaned_data <- cleanData_data
  getFlightEdges_noPolygonEdges_data <- vultureUtils::getFlightEdges(cleaned_data, roostPolygons = NULL, distThreshold = 1000, idCol = "tag_id", return ="edges")
  withr::local_file("getFlightEdges_noPolygonEdges_data.Rda")
  save(getFlightEdges_noPolygonEdges_data,file="getFlightEdges_noPolygonEdges_data.Rda")
  announce_snapshot_file("getFlightEdges_noPolygonEdges_data.Rda")
  expect_snapshot_file("getFlightEdges_noPolygonEdges_data.Rda")
})

test_that("getFlightEdges polygon edges snapshot test", {
  base::load(test_path("_snaps", "mainFunctions", "cleanData_data.Rda"))
  cleaned_data <- cleanData_data
  roostPolygons <- sf::st_read(test_path("testdata", "roosts50_kde95_cutOffRegion.kml"))
  getFlightEdges_PolygonEdges_data <- vultureUtils::getFlightEdges(cleaned_data, roostPolygons = roostPolygons, distThreshold = 1000, idCol = "tag_id", return ="edges")
  withr::local_file("getFlightEdges_PolygonEdges_data.Rda")
  save(getFlightEdges_PolygonEdges_data,file="getFlightEdges_PolygonEdges_data.Rda")
  announce_snapshot_file("getFlightEdges_PolygonEdges_data.Rda")
  expect_snapshot_file("getFlightEdges_PolygonEdges_data.Rda")
})

test_that("getFeedingEdges no polygon sri snapshot test", {
  base::load(test_path("_snaps", "mainFunctions", "cleanData_data.Rda"))
  cleaned_data <- cleanData_data
  getFeedingEdges_noPolygonSRI_data <- vultureUtils::getFeedingEdges(cleaned_data, roostPolygons = NULL, distThreshold = 50, idCol = "tag_id", return ="sri")
  withr::local_file("getFeedingEdges_noPolygonSRI_data.Rda")
  save(getFeedingEdges_noPolygonSRI_data,file="getFeedingEdges_noPolygonSRI_data.Rda")
  announce_snapshot_file("getFeedingEdges_noPolygonSRI_data.Rda")
  expect_snapshot_file("getFeedingEdges_noPolygonSRI_data.Rda")
})

test_that("getFeedingEdges polygon sri snapshot test", {
  base::load(test_path("_snaps", "mainFunctions", "cleanData_data.Rda"))
  cleaned_data <- cleanData_data
  roostPolygons <- sf::st_read(test_path("testdata", "roosts50_kde95_cutOffRegion.kml"))
  getFeedingEdges_PolygonSRI_data <- vultureUtils::getFeedingEdges(cleaned_data, roostPolygons = roostPolygons, distThreshold = 50, idCol = "tag_id", return ="sri")
  withr::local_file("getFeedingEdges_PolygonSRI_data.Rda")
  save(getFeedingEdges_PolygonSRI_data,file="getFeedingEdges_PolygonSRI_data.Rda")
  announce_snapshot_file("getFeedingEdges_PolygonSRI_data.Rda")
  expect_snapshot_file("getFeedingEdges_PolygonSRI_data.Rda")
})

test_that("getFeedingEdges no polygon edges snapshot test", {
  base::load(test_path("_snaps", "mainFunctions", "cleanData_data.Rda")) # can't reassign name
  cleaned_data <- cleanData_data
  getFeedingEdges_noPolygonEdges_data <- vultureUtils::getFeedingEdges(cleaned_data, roostPolygons = NULL, distThreshold = 50, idCol = "tag_id", return ="edges")
  withr::local_file("getFeedingEdges_noPolygonEdges_data.Rda")
  save(getFeedingEdges_noPolygonEdges_data,file="getFeedingEdges_noPolygonEdges_data.Rda")
  announce_snapshot_file("getFeedingEdges_noPolygonEdges_data.Rda")
  expect_snapshot_file("getFeedingEdges_noPolygonEdges_data.Rda")
})

test_that("getFeedingEdges polygon edges snapshot test", {
  base::load(test_path("_snaps", "mainFunctions", "cleanData_data.Rda")) # can't reassign name
  cleaned_data <- cleanData_data
  roostPolygons <- sf::st_read(test_path("testdata", "roosts50_kde95_cutOffRegion.kml"))
  getFeedingEdges_PolygonEdges_data <- vultureUtils::getFeedingEdges(cleaned_data, roostPolygons = roostPolygons, distThreshold = 50, idCol = "tag_id", return ="edges")
  withr::local_file("getFeedingEdges_PolygonEdges_data.Rda")
  save(getFeedingEdges_PolygonEdges_data,file="getFeedingEdges_PolygonEdges_data.Rda")
  announce_snapshot_file("getFeedingEdges_PolygonEdges_data.Rda")
  expect_snapshot_file("getFeedingEdges_PolygonEdges_data.Rda")
})

test_that("get_roosts_df snapshot test", {
  base::load(test_path("_snaps", "mainFunctions", "cleanData_data.Rda"))
  cleaned_data <- cleanData_data
  get_roosts_df_data <- vultureUtils::get_roosts_df(cleaned_data, id = "tag_id", timestamp = "timestamp", x = "location_long", y = "location_lat", ground_speed = "ground_speed", speed_units = "m/s", quiet = F)
  withr::local_file("get_roosts_df_data.Rda")
  save(get_roosts_df_data,file="get_roosts_df_data.Rda")
  announce_snapshot_file("get_roosts_df_data.Rda")
  expect_snapshot_file("get_roosts_df_data.Rda")
})

test_that("getRoostEdges no polygon sri snapshot test", {
  base::load(test_path("_snaps", "mainFunctions", "get_roosts_df_data.Rda")) # can't reassign name
  roosts_data <- get_roosts_df_data
  getRoostEdges_noPolygonSRI_data <- vultureUtils::getRoostEdges(roosts_data, roostPolygons = NULL, idCol = "tag_id", return ="sri", latCol = "location_lat", longCol = "location_long", dateCol = "roost_date")
  withr::local_file("getRoostEdges_noPolygonSRI_data.Rda")
  save(getRoostEdges_noPolygonSRI_data,file="getRoostEdges_noPolygonSRI_data.Rda")
  announce_snapshot_file("getRoostEdges_noPolygonSRI_data.Rda")
  expect_snapshot_file("getRoostEdges_noPolygonSRI_data.Rda")
})

test_that("getRoostEdges polygon sri snapshot test", {
  base::load(test_path("_snaps", "mainFunctions", "get_roosts_df_data.Rda")) # can't reassign name
  roosts_data <- get_roosts_df_data
  roostPolygons <- sf::st_read(test_path("testdata", "roosts50_kde95_cutOffRegion.kml"))
  getRoostEdges_PolygonSRI_data <- vultureUtils::getRoostEdges(roosts_data, roostPolygons = roostPolygons, idCol = "tag_id", return ="sri", latCol = "location_lat", longCol = "location_long", dateCol = "roost_date")
  withr::local_file("getRoostEdges_PolygonSRI_data.Rda")
  save(getRoostEdges_PolygonSRI_data,file="getRoostEdges_PolygonSRI_data.Rda")
  announce_snapshot_file("getRoostEdges_PolygonSRI_data.Rda")
  expect_snapshot_file("getRoostEdges_PolygonSRI_data.Rda")
})

test_that("getRoostEdges no polygon edges snapshot test", {
  base::load(test_path("_snaps", "mainFunctions", "get_roosts_df_data.Rda")) # can't reassign name
  roosts_data <- get_roosts_df_data
  getRoostEdges_noPolygonEdges_data <- vultureUtils::getRoostEdges(roosts_data, roostPolygons = NULL, idCol = "tag_id", return ="edges", latCol = "location_lat", longCol = "location_long", dateCol = "roost_date")
  withr::local_file("getRoostEdges_noPolygonEdges_data.Rda")
  save(getRoostEdges_noPolygonEdges_data,file="getRoostEdges_noPolygonEdges_data.Rda")
  announce_snapshot_file("getRoostEdges_noPolygonEdges_data.Rda")
  expect_snapshot_file("getRoostEdges_noPolygonEdges_data.Rda")
})

test_that("getRoostEdges polygon edges snapshot test", {
  base::load(test_path("_snaps", "mainFunctions", "get_roosts_df_data.Rda")) # can't reassign name
  roosts_data <- get_roosts_df_data
  roostPolygons <- sf::st_read(test_path("testdata", "roosts50_kde95_cutOffRegion.kml"))
  getRoostEdges_PolygonEdges_data <- vultureUtils::getRoostEdges(roosts_data, roostPolygons = roostPolygons, idCol = "tag_id", return ="edges", latCol = "location_lat", longCol = "location_long", dateCol = "roost_date")
  withr::local_file("getRoostEdges_PolygonEdges_data.Rda")
  save(getRoostEdges_PolygonEdges_data,file="getRoostEdges_PolygonEdges_data.Rda")
  announce_snapshot_file("getRoostEdges_PolygonEdges_data.Rda")
  expect_snapshot_file("getRoostEdges_PolygonEdges_data.Rda")
})

test_that("makeGraph unweighted flight no polygon sri snapshot test", {
  base::load(test_path("_snaps", "mainFunctions", "getFlightEdges_noPolygonSRI_data.Rda"))
  sri_data <- getFlightEdges_noPolygonSRI_data
  makeGraph_flightNoPolySRIUnweighted_data <- vultureUtils::makeGraph(mode = "sri", data = sri_data, weighted = F)
  withr::local_file("makeGraph_flightNoPolySRIUnweighted_data.txt")
  igraph::write_graph(makeGraph_flightNoPolySRIUnweighted_data, file="makeGraph_flightNoPolySRIUnweighted_data.txt")
  announce_snapshot_file("makeGraph_flightNoPolySRIUnweighted_data.txt")
  expect_snapshot_file("makeGraph_flightNoPolySRIUnweighted_data.txt")
})

test_that("makeGraph unweighted flight polygon sri snapshot test", {
  base::load(test_path("_snaps", "mainFunctions", "getFlightEdges_PolygonSRI_data.Rda"))
  sri_data <- getFlightEdges_PolygonSRI_data
  makeGraph_flightPolySRIUnweighted_data <- vultureUtils::makeGraph(mode = "sri", data = sri_data, weighted = F)
  withr::local_file("makeGraph_flightPolySRIUnweighted_data.txt")
  # save(makeGraph_flightPolySRIUnweighted_data, file="makeGraph_flightPolySRIUnweighted_data.Rda")
  igraph::write_graph(makeGraph_flightPolySRIUnweighted_data, file="makeGraph_flightPolySRIUnweighted_data.txt")
  announce_snapshot_file("makeGraph_flightPolySRIUnweighted_data.txt")
  expect_snapshot_file("makeGraph_flightPolySRIUnweighted_data.txt")
})

test_that("makeGraph unweighted flight no polygon edgelist snapshot test", {
  base::load(test_path("_snaps", "mainFunctions", "getFlightEdges_noPolygonEdges_data.Rda"))
  edges_data <- getFlightEdges_noPolygonEdges_data
  makeGraph_flightNoPolyEdgelistUnweighted_data <- vultureUtils::makeGraph(mode = "edgelist", data = edges_data, weighted = F)
  withr::local_file("makeGraph_flightNoPolyEdgelistUnweighted_data.txt")
  # save(makeGraph_flightNoPolyEdgelistUnweighted_data, file="makeGraph_flightNoPolyEdgelistUnweighted_data.Rda")
  igraph::write_graph(makeGraph_flightNoPolyEdgelistUnweighted_data, file="makeGraph_flightNoPolyEdgelistUnweighted_data.txt")
  announce_snapshot_file("makeGraph_flightNoPolyEdgelistUnweighted_data.txt")
  expect_snapshot_file("makeGraph_flightNoPolyEdgelistUnweighted_data.txt")
})

test_that("makeGraph unweighted flight polygon edgelist snapshot test", {
  base::load(test_path("_snaps", "mainFunctions", "getFlightEdges_PolygonEdges_data.Rda"))
  edges_data <- getFlightEdges_PolygonEdges_data
  makeGraph_flightPolyEdgelistUnweighted_data <- vultureUtils::makeGraph(mode = "edgelist", data = edges_data, weighted = F)
  withr::local_file("makeGraph_flightPolyEdgelistUnweighted_data.txt")
  # save(makeGraph_flightPolyEdgelistUnweighted_data, file="makeGraph_flightPolyEdgelistUnweighted_data.Rda")
  igraph::write_graph(makeGraph_flightPolyEdgelistUnweighted_data, file="makeGraph_flightPolyEdgelistUnweighted_data.txt")
  announce_snapshot_file("makeGraph_flightPolyEdgelistUnweighted_data.txt")
  expect_snapshot_file("makeGraph_flightPolyEdgelistUnweighted_data.txt")
})

test_that("makeGraph unweighted feeding no polygon sri snapshot test", {
  base::load(test_path("_snaps", "mainFunctions", "getFeedingEdges_noPolygonSRI_data.Rda"))
  sri_data <- getFeedingEdges_noPolygonSRI_data
  makeGraph_feedingNoPolySRIUnweighted_data <- vultureUtils::makeGraph(mode = "sri", data = sri_data, weighted = F)
  withr::local_file("makeGraph_feedingNoPolySRIUnweighted_data.txt")
  # save(makeGraph_feedingNoPolySRIUnweighted_data, file="makeGraph_feedingNoPolySRIUnweighted_data.Rda")
  igraph::write_graph(makeGraph_feedingNoPolySRIUnweighted_data, file="makeGraph_feedingNoPolySRIUnweighted_data.txt")
  announce_snapshot_file("makeGraph_feedingNoPolySRIUnweighted_data.txt")
  expect_snapshot_file("makeGraph_feedingNoPolySRIUnweighted_data.txt")
})

test_that("makeGraph unweighted feeding polygon sri snapshot test", {
  base::load(test_path("_snaps", "mainFunctions", "getFeedingEdges_PolygonSRI_data.Rda"))
  sri_data <- getFeedingEdges_PolygonSRI_data
  makeGraph_feedingPolySRIUnweighted_data <- vultureUtils::makeGraph(mode = "sri", data = sri_data, weighted = F)
  withr::local_file("makeGraph_feedingPolySRIUnweighted_data.txt")
  # save(makeGraph_feedingPolySRIUnweighted_data, file="makeGraph_feedingPolySRIUnweighted_data.Rda")
  igraph::write_graph(makeGraph_feedingPolySRIUnweighted_data, file="makeGraph_feedingPolySRIUnweighted_data.txt")
  announce_snapshot_file("makeGraph_feedingPolySRIUnweighted_data.txt")
  expect_snapshot_file("makeGraph_feedingPolySRIUnweighted_data.txt")
})

test_that("makeGraph unweighted feeding no polygon edgelist snapshot test", {
  base::load(test_path("_snaps", "mainFunctions", "getFeedingEdges_noPolygonEdges_data.Rda"))
  edges_data <- getFeedingEdges_noPolygonEdges_data
  makeGraph_feedingNoPolyEdgelistUnweighted_data <- vultureUtils::makeGraph(mode = "edgelist", data = edges_data, weighted = F)
  withr::local_file("makeGraph_feedingNoPolyEdgelistUnweighted_data.txt")
  # save(makeGraph_feedingNoPolyEdgelistUnweighted_data, file="makeGraph_feedingNoPolyEdgelistUnweighted_data.Rda")
  igraph::write_graph(makeGraph_feedingNoPolyEdgelistUnweighted_data, file="makeGraph_feedingNoPolyEdgelistUnweighted_data.txt")
  announce_snapshot_file("makeGraph_feedingNoPolyEdgelistUnweighted_data.txt")
  expect_snapshot_file("makeGraph_feedingNoPolyEdgelistUnweighted_data.txt")
})

test_that("makeGraph unweighted feeding polygon edgelist snapshot test", {
  base::load(test_path("_snaps", "mainFunctions", "getFeedingEdges_PolygonEdges_data.Rda"))
  edges_data <- getFeedingEdges_PolygonEdges_data
  makeGraph_feedingPolyEdgelistUnweighted_data <- vultureUtils::makeGraph(mode = "edgelist", data = edges_data, weighted = F)
  withr::local_file("makeGraph_feedingPolyEdgelistUnweighted_data.txt")
  # save(makeGraph_feedingPolyEdgelistUnweighted_data, file="makeGraph_feedingPolyEdgelistUnweighted_data.Rda")
  igraph::write_graph(makeGraph_feedingPolyEdgelistUnweighted_data, file="makeGraph_feedingPolyEdgelistUnweighted_data.txt")
  announce_snapshot_file("makeGraph_feedingPolyEdgelistUnweighted_data.txt")
  expect_snapshot_file("makeGraph_feedingPolyEdgelistUnweighted_data.txt")
})

test_that("makeGraph unweighted roost no polygon sri snapshot test", {
  base::load(test_path("_snaps", "mainFunctions", "getRoostEdges_noPolygonSRI_data.Rda"))
  sri_data <- getRoostEdges_noPolygonSRI_data
  makeGraph_roostNoPolySRIUnweighted_data <- vultureUtils::makeGraph(mode = "sri", data = sri_data, weighted = F)
  withr::local_file("makeGraph_roostNoPolySRIUnweighted_data.txt")
  # save(makeGraph_roostNoPolySRIUnweighted_data, file="makeGraph_roostNoPolySRIUnweighted_data.Rda")
  igraph::write_graph(makeGraph_roostNoPolySRIUnweighted_data, file="makeGraph_roostNoPolySRIUnweighted_data.txt")
  announce_snapshot_file("makeGraph_roostNoPolySRIUnweighted_data.txt")
  expect_snapshot_file("makeGraph_roostNoPolySRIUnweighted_data.txt")
})

test_that("makeGraph unweighted roost polygon sri snapshot test", {
  base::load(test_path("_snaps", "mainFunctions", "getRoostEdges_PolygonSRI_data.Rda"))
  sri_data <- getRoostEdges_PolygonSRI_data
  makeGraph_roostPolySRIUnweighted_data <- vultureUtils::makeGraph(mode = "sri", data = sri_data, weighted = F)
  withr::local_file("makeGraph_roostPolySRIUnweighted_data.txt")
  # save(makeGraph_roostPolySRIUnweighted_data, file="makeGraph_roostPolySRIUnweighted_data.Rda")
  igraph::write_graph(makeGraph_roostPolySRIUnweighted_data, file="makeGraph_roostPolySRIUnweighted_data.txt")
  announce_snapshot_file("makeGraph_roostPolySRIUnweighted_data.txt")
  expect_snapshot_file("makeGraph_roostPolySRIUnweighted_data.txt")
})

test_that("makeGraph unweighted roost no polygon edgelist snapshot test", {
  base::load(test_path("_snaps", "mainFunctions", "getRoostEdges_noPolygonEdges_data.Rda"))
  edges_data <- getRoostEdges_noPolygonEdges_data
  makeGraph_roostNoPolyEdgelistUnweighted_data <- vultureUtils::makeGraph(mode = "edgelist", data = edges_data, weighted = F)
  withr::local_file("makeGraph_roostNoPolyEdgelistUnweighted_data.txt")
  # save(makeGraph_roostNoPolyEdgelistUnweighted_data, file="makeGraph_roostNoPolyEdgelistUnweighted_data.Rda")
  igraph::write_graph(makeGraph_roostNoPolyEdgelistUnweighted_data, file="makeGraph_roostNoPolyEdgelistUnweighted_data.txt")
  announce_snapshot_file("makeGraph_roostNoPolyEdgelistUnweighted_data.txt")
  expect_snapshot_file("makeGraph_roostNoPolyEdgelistUnweighted_data.txt")
})

test_that("makeGraph unweighted roost polygon edgelist snapshot test", {
  base::load(test_path("_snaps", "mainFunctions", "getRoostEdges_PolygonEdges_data.Rda"))
  edges_data <- getRoostEdges_PolygonEdges_data
  makeGraph_roostPolyEdgelistUnweighted_data <- vultureUtils::makeGraph(mode = "edgelist", data = edges_data, weighted = F)
  withr::local_file("makeGraph_roostPolyEdgelistUnweighted_data.txt")
  # save(makeGraph_roostPolyEdgelistUnweighted_data, file="makeGraph_roostPolyEdgelistUnweighted_data.Rda")
  igraph::write_graph(makeGraph_roostPolyEdgelistUnweighted_data, file="makeGraph_roostPolyEdgelistUnweighted_data.txt")
  announce_snapshot_file("makeGraph_roostPolyEdgelistUnweighted_data.txt")
  expect_snapshot_file("makeGraph_roostPolyEdgelistUnweighted_data.txt")
})
test_that("makeGraph weighted flight no polygon sri snapshot test", {
  base::load(test_path("_snaps", "mainFunctions", "getFlightEdges_noPolygonSRI_data.Rda"))
  sri_data <- getFlightEdges_noPolygonSRI_data
  makeGraph_flightNoPolySRIWeighted_data <- vultureUtils::makeGraph(mode = "sri", data = sri_data, weighted = T)
  withr::local_file("makeGraph_flightNoPolySRIWeighted_data.txt")
  # save(makeGraph_flightNoPolySRIWeighted_data, file="makeGraph_flightNoPolySRIWeighted_data.Rda")
  igraph::write_graph(makeGraph_flightNoPolySRIWeighted_data, file="makeGraph_flightNoPolySRIWeighted_data.txt")
  announce_snapshot_file("makeGraph_flightNoPolySRIWeighted_data.txt")
  expect_snapshot_file("makeGraph_flightNoPolySRIWeighted_data.txt")
})

test_that("makeGraph weighted flight polygon sri snapshot test", {
  base::load(test_path("_snaps", "mainFunctions", "getFlightEdges_PolygonSRI_data.Rda"))
  sri_data <- getFlightEdges_PolygonSRI_data
  makeGraph_flightPolySRIWeighted_data <- vultureUtils::makeGraph(mode = "sri", data = sri_data, weighted = T)
  withr::local_file("makeGraph_flightPolySRIWeighted_data.txt")
  # save(makeGraph_flightPolySRIWeighted_data, file="makeGraph_flightPolySRIWeighted_data.Rda")
  igraph::write_graph(makeGraph_flightPolySRIWeighted_data, file="makeGraph_flightPolySRIWeighted_data.txt")
  announce_snapshot_file("makeGraph_flightPolySRIWeighted_data.txt")
  expect_snapshot_file("makeGraph_flightPolySRIWeighted_data.txt")
})

test_that("makeGraph weighted flight no polygon edgelist snapshot test", {
  base::load(test_path("_snaps", "mainFunctions", "getFlightEdges_noPolygonEdges_data.Rda"))
  edges_data <- getFlightEdges_noPolygonEdges_data
  makeGraph_flightNoPolyEdgelistWeighted_data <- vultureUtils::makeGraph(mode = "edgelist", data = edges_data, weighted = T)
  withr::local_file("makeGraph_flightNoPolyEdgelistWeighted_data.txt")
  # save(makeGraph_flightNoPolyEdgelistWeighted_data, file="makeGraph_flightNoPolyEdgelistWeighted_data.Rda")
  igraph::write_graph(makeGraph_flightNoPolyEdgelistWeighted_data, file="makeGraph_flightNoPolyEdgelistWeighted_data.txt")
  announce_snapshot_file("makeGraph_flightNoPolyEdgelistWeighted_data.txt")
  expect_snapshot_file("makeGraph_flightNoPolyEdgelistWeighted_data.txt")
})

test_that("makeGraph weighted flight polygon edgelist snapshot test", {
  base::load(test_path("_snaps", "mainFunctions", "getFlightEdges_PolygonEdges_data.Rda"))
  edges_data <- getFlightEdges_PolygonEdges_data
  makeGraph_flightPolyEdgelistWeighted_data <- vultureUtils::makeGraph(mode = "edgelist", data = edges_data, weighted = T)
  withr::local_file("makeGraph_flightPolyEdgelistWeighted_data.txt")
  # save(makeGraph_flightPolyEdgelistWeighted_data, file="makeGraph_flightPolyEdgelistWeighted_data.Rda")
  igraph::write_graph(makeGraph_flightPolyEdgelistWeighted_data, file="makeGraph_flightPolyEdgelistWeighted_data.txt")
  announce_snapshot_file("makeGraph_flightPolyEdgelistWeighted_data.txt")
  expect_snapshot_file("makeGraph_flightPolyEdgelistWeighted_data.txt")
})

test_that("makeGraph weighted feeding no polygon sri snapshot test", {
  base::load(test_path("_snaps", "mainFunctions", "getFeedingEdges_noPolygonSRI_data.Rda"))
  sri_data <- getFeedingEdges_noPolygonSRI_data
  makeGraph_feedingNoPolySRIWeighted_data <- vultureUtils::makeGraph(mode = "sri", data = sri_data, weighted = T)
  withr::local_file("makeGraph_feedingNoPolySRIWeighted_data.txt")
  # save(makeGraph_feedingNoPolySRIWeighted_data, file="makeGraph_feedingNoPolySRIWeighted_data.Rda")
  igraph::write_graph(makeGraph_feedingNoPolySRIWeighted_data, file="makeGraph_feedingNoPolySRIWeighted_data.txt")
  announce_snapshot_file("makeGraph_feedingNoPolySRIWeighted_data.txt")
  expect_snapshot_file("makeGraph_feedingNoPolySRIWeighted_data.txt")
})

test_that("makeGraph weighted feeding polygon sri snapshot test", {
  base::load(test_path("_snaps", "mainFunctions", "getFeedingEdges_PolygonSRI_data.Rda"))
  sri_data <- getFeedingEdges_PolygonSRI_data
  makeGraph_feedingPolySRIWeighted_data <- vultureUtils::makeGraph(mode = "sri", data = sri_data, weighted = T)
  withr::local_file("makeGraph_feedingPolySRIWeighted_data.txt")
  # save(makeGraph_feedingPolySRIWeighted_data, file="makeGraph_feedingPolySRIWeighted_data.Rda")
  igraph::write_graph(makeGraph_feedingPolySRIWeighted_data, file="makeGraph_feedingPolySRIWeighted_data.txt")
  announce_snapshot_file("makeGraph_feedingPolySRIWeighted_data.txt")
  expect_snapshot_file("makeGraph_feedingPolySRIWeighted_data.txt")
})

test_that("makeGraph weighted feeding no polygon edgelist snapshot test", {
  base::load(test_path("_snaps", "mainFunctions", "getFeedingEdges_noPolygonEdges_data.Rda"))
  edges_data <- getFeedingEdges_noPolygonEdges_data
  makeGraph_feedingNoPolyEdgelistWeighted_data <- vultureUtils::makeGraph(mode = "edgelist", data = edges_data, weighted = T)
  withr::local_file("makeGraph_feedingNoPolyEdgelistWeighted_data.txt")
  # save(makeGraph_feedingNoPolyEdgelistWeighted_data, file="makeGraph_feedingNoPolyEdgelistWeighted_data.Rda")
  igraph::write_graph(makeGraph_feedingNoPolyEdgelistWeighted_data, file="makeGraph_feedingNoPolyEdgelistWeighted_data.txt")
  announce_snapshot_file("makeGraph_feedingNoPolyEdgelistWeighted_data.txt")
  expect_snapshot_file("makeGraph_feedingNoPolyEdgelistWeighted_data.txt")
})

test_that("makeGraph weighted feeding polygon edgelist snapshot test", {
  base::load(test_path("_snaps", "mainFunctions", "getFeedingEdges_PolygonEdges_data.Rda"))
  edges_data <- getFeedingEdges_PolygonEdges_data
  makeGraph_feedingPolyEdgelistWeighted_data <- vultureUtils::makeGraph(mode = "edgelist", data = edges_data, weighted = T)
  withr::local_file("makeGraph_feedingPolyEdgelistWeighted_data.txt")
  # save(makeGraph_feedingPolyEdgelistWeighted_data, file="makeGraph_feedingPolyEdgelistWeighted_data.Rda")
  igraph::write_graph(makeGraph_feedingPolyEdgelistWeighted_data, file="makeGraph_feedingPolyEdgelistWeighted_data.txt")
  announce_snapshot_file("makeGraph_feedingPolyEdgelistWeighted_data.txt")
  expect_snapshot_file("makeGraph_feedingPolyEdgelistWeighted_data.txt")
})

test_that("makeGraph weighted roost no polygon sri snapshot test", {
  base::load(test_path("_snaps", "mainFunctions", "getRoostEdges_noPolygonSRI_data.Rda"))
  sri_data <- getRoostEdges_noPolygonSRI_data
  makeGraph_roostNoPolySRIWeighted_data <- vultureUtils::makeGraph(mode = "sri", data = sri_data, weighted = T)
  withr::local_file("makeGraph_roostNoPolySRIWeighted_data.txt")
  # save(makeGraph_roostNoPolySRIWeighted_data, file="makeGraph_roostNoPolySRIWeighted_data.Rda")
  igraph::write_graph(makeGraph_roostNoPolySRIWeighted_data, file="makeGraph_roostNoPolySRIWeighted_data.txt")
  announce_snapshot_file("makeGraph_roostNoPolySRIWeighted_data.txt")
  expect_snapshot_file("makeGraph_roostNoPolySRIWeighted_data.txt")
})

test_that("makeGraph weighted roost polygon sri snapshot test", {
  base::load(test_path("_snaps", "mainFunctions", "getRoostEdges_PolygonSRI_data.Rda"))
  sri_data <- getRoostEdges_PolygonSRI_data
  makeGraph_roostPolySRIWeighted_data <- vultureUtils::makeGraph(mode = "sri", data = sri_data, weighted = T)
  withr::local_file("makeGraph_roostPolySRIWeighted_data.txt")
  # save(makeGraph_roostPolySRIWeighted_data, file="makeGraph_roostPolySRIWeighted_data.Rda")
  igraph::write_graph(makeGraph_roostPolySRIWeighted_data, file="makeGraph_roostPolySRIWeighted_data.txt")
  announce_snapshot_file("makeGraph_roostPolySRIWeighted_data.txt")
  expect_snapshot_file("makeGraph_roostPolySRIWeighted_data.txt")
})

test_that("makeGraph weighted roost no polygon edgelist snapshot test", {
  base::load(test_path("_snaps", "mainFunctions", "getRoostEdges_noPolygonEdges_data.Rda"))
  edges_data <- getRoostEdges_noPolygonEdges_data
  makeGraph_roostNoPolyEdgelistWeighted_data <- vultureUtils::makeGraph(mode = "edgelist", data = edges_data, weighted = T)
  withr::local_file("makeGraph_roostNoPolyEdgelistWeighted_data.txt")
  # save(makeGraph_roostNoPolyEdgelistWeighted_data, file="makeGraph_roostNoPolyEdgelistWeighted_data.Rda")
  igraph::write_graph(makeGraph_roostNoPolyEdgelistWeighted_data, file="makeGraph_roostNoPolyEdgelistWeighted_data.txt")
  announce_snapshot_file("makeGraph_roostNoPolyEdgelistWeighted_data.txt")
  expect_snapshot_file("makeGraph_roostNoPolyEdgelistWeighted_data.txt")
})

test_that("makeGraph weighted roost polygon edgelist snapshot test", {
  base::load(test_path("_snaps", "mainFunctions", "getRoostEdges_PolygonEdges_data.Rda"))
  edges_data <- getRoostEdges_PolygonEdges_data
  makeGraph_roostPolyEdgelistWeighted_data <- vultureUtils::makeGraph(mode = "edgelist", data = edges_data, weighted = T)
  withr::local_file("makeGraph_roostPolyEdgelistWeighted_data.txt")
  # save(makeGraph_roostPolyEdgelistWeighted_data, file="makeGraph_roostPolyEdgelistWeighted_data.Rda")
  igraph::write_graph(makeGraph_roostPolyEdgelistWeighted_data, file="makeGraph_roostPolyEdgelistWeighted_data.txt")
  announce_snapshot_file("makeGraph_roostPolyEdgelistWeighted_data.txt")
  expect_snapshot_file("makeGraph_roostPolyEdgelistWeighted_data.txt")
})
