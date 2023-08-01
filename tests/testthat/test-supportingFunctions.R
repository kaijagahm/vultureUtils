test_that("calcSpeeds check", {
  base::load(test_path("testdata", "month_data.Rda"))
  a <- month_data
  test_data <- a[1:3,]
  test_data[,"tag_id"] <- 0 # group only 1 id
  default_timestamp = strptime("2023-6-24 01:23:45", "%Y-%m-%d %H:%M:%S")
  test_data$timestamp <- default_timestamp

  test_data[1, 'timestamp'] <- test_data[1, 'timestamp'] - 60 * 60
  test_data[3, 'timestamp'] <- test_data[3, 'timestamp'] + 60 * 60

  test_data$location_long.1 <- 30
  test_data$location_lat.1 <- 30
  test_data[3, 'location_long.1'] <- 29.98
  test_data[3, 'location_lat.1'] <- 29.98
  test_data[1, 'location_long.1'] <- 30.02
  test_data[1, 'location_lat.1'] <- 30.02
  b <- calcSpeeds(df = test_data, grpCol = "tag_id", longCol = 'location_long.1', latCol = 'location_lat.1')
  b <- b[, c('tag_id', 'timestamp', 'location_long.1', 'location_lat.1', 'lead_hour_diff_sec'
             ,'lag_hour_diff_sec', 'lead_dist_m', 'lag_dist_m', 'lead_speed_m_s', 'lag_speed_m_s')]
  expect_equal(typeof(b), typeof(a)) # dataframe type shouldn't change
  expect_equal(b$tag_id, c(0, 0, 0)) # id shouldn't change
  expect_equal(b$timestamp, test_data$timestamp)     # columns shouldn't change
  expect_equal(b$location_long.1, test_data$location_long.1)
  expect_equal(b$location_lat.1, test_data$location_lat.1)

  expect_true(all(b$lead_hour_diff_sec > 0 | is.na(b$lead_hour_diff_sec))) # time signs should match
  expect_true(all(b$lag_hour_diff_sec < 0 | is.na(b$lag_hour_diff_sec)))

  expect_equal(b$lead_hour_diff_sec, c(3600, 3600, NA)) # time lengths should match (3600s difference)
  expect_equal(b$lag_hour_diff_sec, c(NA, -3600, -3600))

  expected_distance <- 2939 # lead and lag distances should be the same
  expect_true(all(round(b$lead_dist_m[!is.na(b$lead_dist_m)], digits=0) == expected_distance))
  expect_true(all(round(b$lag_dist_m[!is.na(b$lag_dist_m)],digits=0) == expected_distance))

  expected_lead_speed <- 0.82 # speed check
  expected_lag_speed <- -0.82

  expect_true(all(b$lead_speed_m_s[!is.na(b$lead_speed_m_s)] == expected_lead_speed))

  expect_true(all(b$lag_speed_m_s[!is.na(b$lag_speed_m_s)] == expected_lag_speed))

  ## test same timestamp diff_sec adjustment
  test_data$timestamp <- default_timestamp

  c <- calcSpeeds(df = test_data, grpCol = "tag_id", longCol = 'location_long.1', latCol = 'location_lat.1')
  c <- c[, c('tag_id', 'timestamp', 'location_long.1', 'location_lat.1', 'lead_hour_diff_sec'
             ,'lag_hour_diff_sec', 'lead_dist_m', 'lag_dist_m', 'lead_speed_m_s', 'lag_speed_m_s')]

  expect_equal(c$lead_hour_diff_sec, c(0.01, 0.01, NA))
  expect_equal(c$lag_hour_diff_sec, c(NA, 0.01, 0.01))

  ## first outlier check
  test_data$timestamp <- default_timestamp
  test_data[1, 'timestamp'] <- default_timestamp - 50
  test_data[3, 'timestamp'] <- default_timestamp + 50

  d <- calcSpeeds(df = test_data, grpCol = "tag_id", longCol = 'location_long.1', latCol = 'location_lat.1')
  d <- d[, c('tag_id', 'timestamp', 'location_long.1', 'location_lat.1', 'lead_hour_diff_sec'
             ,'lag_hour_diff_sec', 'lead_dist_m', 'lag_dist_m', 'lead_speed_m_s', 'lag_speed_m_s')]
  expected_lead_speed <- 59 # speed check
  expected_lag_speed <- -59

  expect_true(all(round(d$lead_speed_m_s[!is.na(d$lead_speed_m_s)], digits = 0) == expected_lead_speed))
  expect_true(all(round(d$lag_speed_m_s[!is.na(d$lag_speed_m_s)], digits = 0) == expected_lag_speed))

  ## second outlier check

  test_data$timestamp <- default_timestamp
  test_data[1, 'timestamp'] <- default_timestamp - 60 * 60
  test_data[3, 'timestamp'] <- default_timestamp + 50

  e <- calcSpeeds(df = test_data, grpCol = "tag_id", longCol = 'location_long.1', latCol = 'location_lat.1')
  e <- e[, c('tag_id', 'timestamp', 'location_long.1', 'location_lat.1', 'lead_hour_diff_sec'
             ,'lag_hour_diff_sec', 'lead_dist_m', 'lag_dist_m', 'lead_speed_m_s', 'lag_speed_m_s')]
<<<<<<< HEAD
  e %>% print(width=Inf)
=======
>>>>>>> 5433d06d1e367813397afa1fa0bd76edcda87b36
  expected_lead_speed <- 59 # speed check
  expected_lag_speed <- -1

  expect_true(round(e[2, 'lead_speed_m_s'], digits = 0) == expected_lead_speed)
  expect_true(round(e[2, 'lag_speed_m_s'], digits = 0) == expected_lag_speed)

  ## TODO: third outlier check with day/night
})

test_that("calcSpeedsVert check", {

  ## normal usage

  base::load(test_path("testdata", "month_data.Rda"))
  a <- month_data
  test_data <- a[1:3,]
  test_data[,"tag_id"] <- 0 # group only 1 id
  default_timestamp = strptime("2023-6-24 01:23:45", "%Y-%m-%d %H:%M:%S")
  test_data$timestamp <- default_timestamp

  test_data[1, 'timestamp'] <- test_data[1, 'timestamp'] - 60 * 60
  test_data[3, 'timestamp'] <- test_data[3, 'timestamp'] + 60 * 60

  test_data$height_above_msl <- 750
  test_data[1, 'height_above_msl'] <- 250
  test_data[3, 'height_above_msl'] <- 1250

  b <- calcSpeedsVert(df = test_data, grpCol = "tag_id", altCol = "height_above_msl")
  b <- b[, c('tag_id', 'timestamp', 'height_above_msl', 'lead_hour_diff_sec'
             ,'lag_hour_diff_sec', 'lead_dist_mV', 'lag_dist_mV', 'lead_speed_m_s', 'lag_speed_m_s')]
  expect_equal(typeof(b), typeof(a)) # dataframe type shouldn't change
  expect_equal(b$tag_id, c(0, 0, 0)) # id shouldn't change
  expect_equal(b$timestamp, test_data$timestamp)     # columns shouldn't change
  expect_equal(b$height_above_msl, test_data$height_above_msl)

  expect_true(all(b$lead_hour_diff_sec > 0 | is.na(b$lead_hour_diff_sec))) # time signs should match
  expect_true(all(b$lag_hour_diff_sec < 0 | is.na(b$lag_hour_diff_sec)))

  expect_equal(b$lead_hour_diff_sec, c(3600, 3600, NA)) # time lengths should match (3600s difference)
  expect_equal(b$lag_hour_diff_sec, c(NA, -3600, -3600))

  expected_lead_distance <- 500
  expected_lag_distance <- -500
  expect_true(all(round(b$lead_dist_mV[!is.na(b$lead_dist_mV)], digits=0) == expected_lead_distance))
  expect_true(all(round(b$lag_dist_mV[!is.na(b$lag_dist_mV)],digits=0) == expected_lag_distance))

  expected_lead_speed <- 0.14 # speed check
  expected_lag_speed <- 0.14

  expect_true(all(b$lead_speed_m_s[!is.na(b$lead_speed_m_s)] == expected_lead_speed))

  expect_true(all(b$lag_speed_m_s[!is.na(b$lag_speed_m_s)] == expected_lag_speed))

  ## test same timestamp diff_sec adjustment

  test_data[1, 'timestamp'] <- test_data[2, 'timestamp']
  test_data[3, 'timestamp'] <- test_data[2, 'timestamp']

  c <- calcSpeedsVert(df = test_data, grpCol = "tag_id", altCol = "height_above_msl")
  c <- c[, c('tag_id', 'timestamp', 'height_above_msl', 'lead_hour_diff_sec'
             ,'lag_hour_diff_sec', 'lead_dist_mV', 'lag_dist_mV', 'lead_speed_m_s', 'lag_speed_m_s')]

  expect_equal(c$lead_hour_diff_sec, c(0.01, 0.01, NA))
  expect_equal(c$lag_hour_diff_sec, c(NA, 0.01, 0.01))

  ## outlier check

  test_data[1, 'timestamp'] <- test_data[2, 'timestamp'] - 60
  test_data[3, 'timestamp'] <- test_data[2, 'timestamp'] + 60

  test_data$height_above_msl <- 750
  test_data[1, 'height_above_msl'] <- 450
  test_data[3, 'height_above_msl'] <- 1050

  d <- calcSpeedsVert(df = test_data, grpCol = "tag_id", altCol = "height_above_msl")
  d <- d[, c('tag_id', 'timestamp', 'height_above_msl', 'lead_hour_diff_sec'
             ,'lag_hour_diff_sec', 'lead_dist_mV', 'lag_dist_mV', 'lead_speed_m_s', 'lag_speed_m_s')]
  expected_lead_speed <- 5 # speed check
  expected_lag_speed <- 5

  expect_true(all(d$lead_speed_m_s[!is.na(d$lead_speed_m_s)] == expected_lead_speed))
  expect_true(all(d$lag_speed_m_s[!is.na(d$lag_speed_m_s)] == expected_lag_speed))
})

test_that("removeUnnecessaryVariables check", {
  base::load(test_path("testdata", "month_data.Rda"))
  data <- month_data
  default_removed <- c("sensor_type_id","taxon_canonical_name","nick_name","earliest_date_born","sensor","optional",
                      "sensor_type","mw_activity_count","eobs_accelerations_raw","eobs_acceleration_sampling_frequency_per_axis",
                      "eobs_acceleration_axes","argos_valid_location_algorithm","argos_sensor_4","argos_sensor_3","argos_sensor_2",
                      "argos_sensor_1","argos_semi_minor","argos_semi_major","argos_pass_duration","argos_orientation","argos_nopc",
                      "argos_lat1","argos_lat2","1084088","argos_lon1","argos_lon2","argos_nb_mes","argos_nb_mes_120",
                      "eobs_key_bin_checksum","eobs_fix_battery_voltage","eobs_battery_voltage","eobs_status",
                      "eobs_start_timestamp","eobs_type_of_fix","eobs_used_time_to_get_fix","eobs_temperature",
                      "gps_dop","magnetic_field_raw_x","magnetic_field_raw_y","magnetic_field_raw_z","ornitela_transmission_protocol",
                      "tag_voltage","algorithm_marked_outlier","argos_altitude","argos_best_level","argos_lc","argos_iq",
                      "argos_gdop","argos_error_radius","argos_calcul_freq","timestamps","height_raw",
                      "barometric_pressure","barometric_height","battery_charging_current","eobs_activity","manually_marked_outlier",
                      "eobs_activity_samples", "acceleration_raw_y", "battery_charge_percent", "data_decoding_software","gps_vdop","height_above_ellipsoid",
                      'acceleration_raw_x','acceleration_raw_z',"acceleration_raw_z","eobs_horizontal_accuracy_estimate","eobs_speed_accuracy_estimate")
  vars_removed_data <- vultureUtils::removeUnnecessaryVars(data)

  # test that all default removed columns are removed

  expect_true(all(!(default_removed %in% names(vars_removed_data))))

  # test keeping and removing other columns

  cols_to_drop <- c("gps_time_to_fix", "heading")
  cols_to_keep <- c("barometric_height", "timestamps")

  vars_removed_data <- vultureUtils::removeUnnecessaryVars(data, cols_to_drop, cols_to_keep)

  expect_true(all(!(cols_to_drop %in% names(vars_removed_data))))
  expect_true(all(cols_to_keep %in% names(vars_removed_data)))
  expect_equal(names(vars_removed_data), setdiff(setdiff(names(data), setdiff(default_removed, cols_to_keep)), cols_to_drop))
})

## NOTE: MASKDATA HAS A ARGUMENT PASSING ERROR (passes crs instead of crsToSet)
test_that("maskData check", {

  base::load(test_path("testdata", "month_data.Rda"))
  data <- month_data
  mask <- sf::st_read(test_path("testdata", "CutOffRegion.kml"))

  maskData_data <- vultureUtils::maskData(dataset = data, mask = mask, longCol = "location_long.1", latCol = "location_lat.1", crsToSet = "WGS84")

  expect_true("sf" %in% class(maskData_data)) # data should be converted to sf object

  withr::local_file("maskData_data.Rda")
  save(maskData_data,file="maskData_data.Rda")
  announce_snapshot_file("maskData_data.Rda")
  expect_snapshot_file("maskData_data.Rda")
})
## NOTE: numerical thresholding is > exclusive
test_that("mostlyInMask check", {
  base::load(test_path("testdata", "month_data.Rda"))
  data <- month_data
  base::load(test_path("_snaps", "supportingFunctions", "maskData_data.Rda"))
  inMask <- maskData_data

  num_thresh <- 20 # numerical days threshold

  below_thresh <- data[1,]
  below_thresh$tag_id <- 0
  above_thresh <- data[1,]
  above_thresh$tag_id <- 1
  num_data <- above_thresh
  num_inMask <- above_thresh
  for (i in 1:num_thresh){
    below_thresh$dateOnly <- as.Date(below_thresh$dateOnly) + 1  # add days for below threshold and above
    above_thresh$dateOnly <- as.Date(above_thresh$dateOnly) + 1
    num_data <- rbind(num_data, below_thresh, above_thresh)
    num_inMask <- rbind(num_data, below_thresh, above_thresh)
  }

  mostlyInMask_days_data <- vultureUtils::mostlyInMask(dataset = num_data,
                                                          maskedDataset = num_inMask,
                                                          thresh = num_thresh,
                                                          dateCol = "dateOnly",
                                                          idCol = "tag_id")

  expect_equal(mostlyInMask_days_data, 1) # return value should only be id of vulture that is stays above the threshold in mask
  expect_output(vultureUtils::mostlyInMask(dataset = num_data,
                                                          maskedDataset = num_inMask,
                                                          thresh = num_thresh,
                                                          dateCol = "dateOnly",
                                                          idCol = "tag_id"), "thresholding by number of days")
  ## default proportion usage with snapshot testing

  mostlyInMask_data <- vultureUtils::mostlyInMask(dataset = data,
                             maskedDataset = inMask,
                             thresh = 0.33,
                             dateCol = "dateOnly",
                             idCol = "tag_id")

  expect_output(vultureUtils::mostlyInMask(dataset = data,
                                                          maskedDataset = inMask,
                                                          thresh = 0.33,
                                                          dateCol = "dateOnly",
                                                          idCol = "tag_id"), "thresholding by proportion of duration")
  withr::local_file("mostlyInMask_data.Rda")
  save(mostlyInMask_data,file="mostlyInMask_data.Rda")
  announce_snapshot_file("mostlyInMask_data.Rda")
  expect_snapshot_file("mostlyInMask_data.Rda")
})

test_that("filterLocs check", {
  base::load(test_path("_snaps", "mainFunctions", "cleanData_data.Rda"))
  cleaned_data <- cleanData_data

  expect_equal(vultureUtils::filterLocs(data.frame(ground_speed=c(1, 2, 3)), speedThreshLower=1.5), data.frame(ground_speed=c(2, 3))) # lower filter within bounds
  expect_equal(vultureUtils::filterLocs(data.frame(ground_speed=c(1, 2, 3)), speedThreshLower=2), data.frame(ground_speed=c(3))) # lower filter edge case
  expect_equal(vultureUtils::filterLocs(data.frame(ground_speed=c(1, 2, 3)), speedThreshUpper=2.5), data.frame(ground_speed=c(1, 2))) # upper filter within bounds
  expect_equal(vultureUtils::filterLocs(data.frame(ground_speed=c(1, 2, 3)), speedThreshUpper=2), data.frame(ground_speed=c(1))) # upper filter edge case

  expect_equal(filterLocs(data.frame(ground_speed=c(1, 2, 3)), speedThreshLower=1.5, speedThreshUpper=2.5), data.frame(ground_speed=c(2))) # double filter in bounds
  expect_warning(filterLocs(data.frame(ground_speed=c(1, 2, 3)), speedThreshLower=NULL, speedThreshUpper=NULL),
                 "No speed thresholds set, so data will not be filtered for speed.") # check for no filter warning given explicit null
  expect_warning(filterLocs(data.frame(ground_speed=c(1, 2, 3))),
                 "No speed thresholds set, so data will not be filtered for speed.") # check for no filter warning

  # typical getFeedingEdges usage

  filterLocs_upper_data <- vultureUtils::filterLocs(df = cleaned_data,
                                           speedThreshUpper = 5,
                                           speedThreshLower = NULL)
  withr::local_file("filterLocs_upper_data.Rda")
  save(filterLocs_upper_data,file="filterLocs_upper_data.Rda")
  announce_snapshot_file("filterLocs_upper_data.Rda")
  expect_snapshot_file("filterLocs_upper_data.Rda")

  # typical getFlightEdges usage

  filterLocs_lower_data <- vultureUtils::filterLocs(df = cleaned_data,
                                           speedThreshUpper = NULL,
                                           speedThreshLower = 5)
  withr::local_file("filterLocs_lower_data.Rda")
  save(filterLocs_upper_data,file="filterLocs_lower_data.Rda")
  announce_snapshot_file("filterLocs_lower_data.Rda")
  expect_snapshot_file("filterLocs_lower_data.Rda")
})

test_that("convertAndBuffer check", {
  roostPolygons <- sf::st_read(test_path("testdata", "roosts50_kde95_cutOffRegion.kml"))
  default_roost_buffer <- 50
  convertAndBuffer_data <- vultureUtils::convertAndBuffer(roostPolygons, dist = default_roost_buffer)
  withr::local_file("convertAndBuffer_data.Rda")
  save(convertAndBuffer_data,file="convertAndBuffer_data.Rda")
  announce_snapshot_file("convertAndBuffer_data.Rda")
  expect_snapshot_file("convertAndBuffer_data.Rda")
})
# NOTE: error with spatsoc::edge_dist "found duplicate id in a timegroup and/or splitBy - does your group_times threshold match the fix rate?"
# NOTE: should be fine
test_that("spaceTimeGroups check", {
  base::load(test_path("_snaps", "mainFunctions", "cleanData_data.Rda"))
  cleaned_data <- cleanData_data
  roostPolygons <- sf::st_read(test_path("testdata", "roosts50_kde95_cutOffRegion.kml"))
  roostBuffer <- 50
  daytimeOnly <- T

  idCol <- "tag_id"           # default parameters
  consecThreshold <- 2
  timeThreshold <- "10 minutes"

  dist_feeding <- 50
  dist_flight <- 1000

  flight_points <- data_to_points_helper(dataset = cleaned_data, roostPolygons = roostPolygons, roostBuffer = roostBuffer, speedThreshLower = 5, speedThreshUpper = NULL, daytimeOnly = daytimeOnly)    # convert cleaned data to points (steps taken in getEdges)
  feeding_points <- data_to_points_helper(dataset = cleaned_data, roostPolygons = roostPolygons, roostBuffer = roostBuffer, speedThreshLower = NULL, speedThreshUpper = 5, daytimeOnly = daytimeOnly)

  # typical getEdges wrapper calls from getFlightEdges and getFeedingEdges

  spaceTimeGroups_flightSRIPolygon_data <- vultureUtils::spaceTimeGroups(dataset = flight_points,
                                distThreshold = dist_flight,
                                consecThreshold = consecThreshold,
                                timeThreshold = timeThreshold,
                                sri = TRUE,
                                idCol = idCol)
  withr::local_file("spaceTimeGroups_flightSRIPolygon_data.Rda")
  save(spaceTimeGroups_flightSRIPolygon_data,file="spaceTimeGroups_flightSRIPolygon_data.Rda")
  announce_snapshot_file("spaceTimeGroups_flightSRIPolygon_data.Rda")
  expect_snapshot_file("spaceTimeGroups_flightSRIPolygon_data.Rda")

  spaceTimeGroups_flightEdgesPolygon_data <- vultureUtils::spaceTimeGroups(dataset = flight_points,
                                                                          distThreshold = dist_flight,
                                                                          consecThreshold = consecThreshold,
                                                                          timeThreshold = timeThreshold,
                                                                          sri = FALSE,
                                                                          idCol = idCol)
  withr::local_file("spaceTimeGroups_flightEdgesPolygon_data.Rda")
  save(spaceTimeGroups_flightEdgesPolygon_data,file="spaceTimeGroups_flightEdgesPolygon_data.Rda")
  announce_snapshot_file("spaceTimeGroups_flightEdgesPolygon_data.Rda")
  expect_snapshot_file("spaceTimeGroups_flightEdgesPolygon_data.Rda")

  spaceTimeGroups_feedingSRIPolygon_data <- vultureUtils::spaceTimeGroups(dataset = feeding_points,
                                                                            distThreshold = dist_feeding,
                                                                            consecThreshold = consecThreshold,
                                                                            timeThreshold = timeThreshold,
                                                                            sri = TRUE,
                                                                            idCol = idCol)
  withr::local_file("spaceTimeGroups_feedingSRIPolygon_data.Rda")
  save(spaceTimeGroups_feedingSRIPolygon_data,file="spaceTimeGroups_feedingSRIPolygon_data.Rda")
  announce_snapshot_file("spaceTimeGroups_feedingSRIPolygon_data.Rda")
  expect_snapshot_file("spaceTimeGroups_feedingSRIPolygon_data.Rda")

  spaceTimeGroups_feedingEdgesPolygon_data <- vultureUtils::spaceTimeGroups(dataset = feeding_points,
                                                                            distThreshold = dist_feeding,
                                                                            consecThreshold = consecThreshold,
                                                                            timeThreshold = timeThreshold,
                                                                            sri = FALSE,
                                                                            idCol = idCol)
  withr::local_file("spaceTimeGroups_feedingEdgesPolygon_data.Rda")
  save(spaceTimeGroups_feedingEdgesPolygon_data,file="spaceTimeGroups_feedingEdgesPolygon_data.Rda")
  announce_snapshot_file("spaceTimeGroups_feedingEdgesPolygon_data.Rda")
  expect_snapshot_file("spaceTimeGroups_feedingEdgesPolygon_data.Rda")

  # # test no roostPolygon filtering ## NOTE: looks like it's taking too long
  #
  # flight_points <- data_to_points_helper(dataset = cleaned_data, roostPolygons = roostPolygons, roostBuffer = roostBuffer, speedThreshLower = 5, speedThreshUpper = NULL, daytimeOnly = daytimeOnly)    # convert cleaned data to points (steps taken in getEdges)
  # feeding_points <- data_to_points_helper(dataset = cleaned_data, roostPolygons = roostPolygons, roostBuffer = roostBuffer, speedThreshLower = NULL, speedThreshUpper = 5, daytimeOnly = daytimeOnly)
  #
  # spaceTimeGroups_flightSRInoPolygon_data <- vultureUtils::spaceTimeGroups(dataset = flight_points,
  #                                                                                distThreshold = dist_flight,
  #                                                                                consecThreshold = consecThreshold,
  #                                                                                timeThreshold = timeThreshold,
  #                                                                                sri = TRUE,
  #                                                                                idCol = idCol)
  # withr::local_file("spaceTimeGroups_flightSRInoPolygon_data.Rda")
  # save(spaceTimeGroups_flightSRInoPolygon_data,file="spaceTimeGroups_flightSRInoPolygon_data.Rda")
  # announce_snapshot_file("spaceTimeGroups_flightSRInoPolygon_data.Rda")
  # expect_snapshot_file("spaceTimeGroups_flightSRInoPolygon_data.Rda")
  #
  # spaceTimeGroups_flightEdgesnoPolygon_data <- vultureUtils::spaceTimeGroups(dataset = flight_points,
  #                                                                                  distThreshold = dist_flight,
  #                                                                                  consecThreshold = consecThreshold,
  #                                                                                  timeThreshold = timeThreshold,
  #                                                                                  sri = FALSE,
  #                                                                                  idCol = idCol)
  # withr::local_file("spaceTimeGroups_flightEdgesnoPolygon_data.Rda")
  # save(spaceTimeGroups_flightEdgesnoPolygon_data,file="spaceTimeGroups_flightEdgesnoPolygon_data.Rda")
  # announce_snapshot_file("spaceTimeGroups_flightEdgesnoPolygon_data.Rda")
  # expect_snapshot_file("spaceTimeGroups_flightEdgesnoPolygon_data.Rda")
  #
  # spaceTimeGroups_feedingSRInoPolygon_data <- vultureUtils::spaceTimeGroups(dataset = feeding_points,
  #                                                                                 distThreshold = dist_feeding,
  #                                                                                 consecThreshold = consecThreshold,
  #                                                                                 timeThreshold = timeThreshold,
  #                                                                                 sri = TRUE,
  #                                                                                 idCol = idCol)
  # withr::local_file("spaceTimeGroups_feedingSRInoPolygon_data.Rda")
  # save(spaceTimeGroups_feedingSRInoPolygon_data,file="spaceTimeGroups_feedingSRInoPolygon_data.Rda")
  # announce_snapshot_file("spaceTimeGroups_feedingSRInoPolygon_data.Rda")
  # expect_snapshot_file("spaceTimeGroups_feedingSRInoPolygon_data.Rda")
  #
  # spaceTimeGroups_feedingEdgesnoPolygon_data <- vultureUtils::spaceTimeGroups(dataset = feeding_points,
  #                                                                                   distThreshold = dist_feeding,
  #                                                                                   consecThreshold = consecThreshold,
  #                                                                                   timeThreshold = timeThreshold,
  #                                                                                   sri = FALSE,
  #                                                                                   idCol = idCol)
  # withr::local_file("spaceTimeGroups_feedingEdgesnoPolygon_data.Rda")
  # save(spaceTimeGroups_feedingEdgesnoPolygon_data,file="spaceTimeGroups_feedingEdgesnoPolygon_data.Rda")
  # announce_snapshot_file("spaceTimeGroups_feedingEdgesnoPolygon_data.Rda")
  # expect_snapshot_file("spaceTimeGroups_feedingEdgesnoPolygon_data.Rda")
})
# NOTE: fillNA defaults to FALSE but is never used. It is reassigned as TRUE during spatsoc usage, except in getRoostEdges
# NOTE: error with data.table::setDT "All formats failed to parse. No formats found."
test_that("consecEdges check", {
  base::load(test_path("_snaps", "mainFunctions", "cleanData_data.Rda"))
  cleaned_data <- cleanData_data
  roostPolygons <- sf::st_read(test_path("testdata", "roosts50_kde95_cutOffRegion.kml"))
  roostBuffer <- 50
  daytimeOnly <- T

  idCol <- "tag_id"           # default parameters
  consecThreshold <- 2
  timeThreshold <- "10 minutes"
  timestampCol <- "timestamp"
  crsToSet <- "WGS84"
  crsToTransform <- 32636
  latCol <- "location_lat"
  longCol <- "location_long"
  returnDist <- TRUE
  fillNA <- FALSE

  dist_feeding <- 50
  dist_flight <- 1000

  flight_points <- data_to_points_helper(dataset = cleaned_data, roostPolygons = roostPolygons, roostBuffer = roostBuffer, speedThreshLower = 5, speedThreshUpper = NULL, daytimeOnly = daytimeOnly)    # convert cleaned data to points (steps taken in getEdges)
  feeding_points <- data_to_points_helper(dataset = cleaned_data, roostPolygons = roostPolygons, roostBuffer = roostBuffer, speedThreshLower = NULL, speedThreshUpper = 5, daytimeOnly = daytimeOnly)

  flight_edgelist <- points_to_edgelist_helper(flight_points, dist_flight, crsToSet = crsToSet,            # convert points to edgelist (steps taken in spaceTimeGroups)
                                               crsToTransform = crsToTransform, timestampCol = timestampCol, timeThreshold = timeThreshold,
                                               idCol = idCol, latCol = latCol, longCol = longCol, returnDist = returnDist, fillNA = fillNA)[[1]]

  feeding_edgelist <- points_to_edgelist_helper(feeding_points, dist_feeding, crsToSet = crsToSet,
                                               crsToTransform = crsToTransform, timestampCol = timestampCol, timeThreshold = timeThreshold,
                                               idCol = idCol, latCol = latCol, longCol = longCol, returnDist = returnDist, fillNA = fillNA)[[1]]

  consecEdges_flightPolygon_data <- vultureUtils::consecEdges(edgeList = flight_edgelist, consecThreshold = consecThreshold)
  withr::local_file("consecEdges_flightPolygon_data.Rda")
  save(consecEdges_flightPolygon_data,file="consecEdges_flightPolygon_data.Rda")
  announce_snapshot_file("consecEdges_flightPolygon_data.Rda")
  expect_snapshot_file("consecEdges_flightPolygon_data.Rda")

  consecEdges_feedingPolygon_data <- vultureUtils::consecEdges(edgeList = feeding_edgelist, consecThreshold = consecThreshold)
  withr::local_file("consecEdges_feedingPolygon_data.Rda")
  save(consecEdges_feedingPolygon_data,file="consecEdges_feedingPolygon_data.Rda")
  announce_snapshot_file("consecEdges_feedingPolygon_data.Rda")
  expect_snapshot_file("consecEdges_feedingPolygon_data.Rda")
})

test_that("calcSRI check", {
  base::load(test_path("_snaps", "mainFunctions", "cleanData_data.Rda"))
  cleaned_data <- cleanData_data
  roostPolygons <- sf::st_read(test_path("testdata", "roosts50_kde95_cutOffRegion.kml"))
  roostBuffer <- 50
  daytimeOnly <- T

  idCol <- "tag_id"           # default parameters
  consecThreshold <- 2
  timeThreshold <- "10 minutes"
  timestampCol <- "timestamp"
  crsToSet <- "WGS84"
  crsToTransform <- 32636
  latCol <- "location_lat"
  longCol <- "location_long"
  returnDist <- TRUE
  fillNA <- FALSE

  dist_feeding <- 50
  dist_flight <- 1000

  flight_points <- data_to_points_helper(dataset = cleaned_data, roostPolygons = roostPolygons, roostBuffer = roostBuffer, speedThreshLower = 5, speedThreshUpper = NULL, daytimeOnly = daytimeOnly)    # convert cleaned data to points (steps taken in getEdges)
  feeding_points <- data_to_points_helper(dataset = cleaned_data, roostPolygons = roostPolygons, roostBuffer = roostBuffer, speedThreshLower = NULL, speedThreshUpper = 5, daytimeOnly = daytimeOnly)
  flight_data <- points_to_edgelist_helper(dataset = flight_points, distThreshold = dist_flight, crsToSet = crsToSet, # retrieve modified dataset from converting to points
                                               crsToTransform = crsToTransform, timestampCol = timestampCol, timeThreshold = timeThreshold,
                                               idCol = idCol, latCol = latCol, longCol = longCol, returnDist = returnDist, fillNA = fillNA)

  feeding_data <- points_to_edgelist_helper(dataset = feeding_points, distThreshold = dist_feeding, crsToSet = crsToSet,
                                               crsToTransform = crsToTransform, timestampCol = timestampCol, timeThreshold = timeThreshold,
                                               idCol = idCol, latCol = latCol, longCol = longCol, returnDist = returnDist, fillNA = fillNA)
  flight_dataset <- flight_data[[2]]
  flight_timegroup_data <- flight_data[[3]]

  feeding_dataset <- feeding_data[[2]]
  feeding_timegroup_data <- feeding_data[[3]]

  base::load(test_path("_snaps", "supportingFunctions", "consecEdges_flightPolygon_data.Rda"))    # retrieve tested edgelist from consecEdges
  flight_edges <- consecEdges_flightPolygon_data
  base::load(test_path("_snaps", "supportingFunctions", "consecEdges_feedingPolygon_data.Rda"))
  feeding_edges <- consecEdges_feedingPolygon_data

  calcSRI_flight_edges <- parameter_calcSRI_helper(dataset = flight_dataset, edgesFiltered = flight_edges, timegroupData = flight_timegroup_data,  # finish data modification from spaceTimeGroups before call to calcSRI
                                                     idCol = idCol, latCol = latCol, longCol = longCol)

  calcSRI_feeding_edges <- parameter_calcSRI_helper(dataset = feeding_dataset, edgesFiltered = feeding_edges, timegroupData = feeding_timegroup_data,
                                                     idCol = idCol, latCol = latCol, longCol = longCol)

  calcSRI_flight_data <- vultureUtils::calcSRI(dataset = flight_dataset, edges = calcSRI_flight_edges, idCol = idCol)
  withr::local_file("calcSRI_flight_data.Rda")
  save(calcSRI_flight_data,file="calcSRI_flight_data.Rda")
  announce_snapshot_file("calcSRI_flight_data.Rda")
  expect_snapshot_file("calcSRI_flight_data.Rda")

  calcSRI_feeding_data <- vultureUtils::calcSRI(dataset = feeding_dataset, edges = calcSRI_feeding_edges, idCol = idCol)
  withr::local_file("calcSRI_feeding_data.Rda")
  save(calcSRI_feeding_data,file="calcSRI_feeding_data.Rda")
  announce_snapshot_file("calcSRI_feeding_data.Rda")
  expect_snapshot_file("calcSRI_feeding_data.Rda")
})
