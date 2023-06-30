test_that("calcSpeed check", {
  base::load(test_path("testdata", "raw2022.Rda"))
  a <- raw2022
  test_data <- a[1:3,]
  test_data[,"tag_id"] <- 0 # group only 1 id
  default_timestamp = strptime("2023-6-24 01:23:45", "%Y-%m-%d %H:%M:%S")
  test_data$timestamp <- default_timestamp

  test_data[1, 'timestamp'] <- test_data[1, 'timestamp'] - 60 * 60 ## try ordering later
  test_data[3, 'timestamp'] <- test_data[3, 'timestamp'] + 60 * 60

  test_data$location_long.1 <- 30
  test_data$location_lat.1 <- 30
  test_data[1, 'location_long.1'] <- 29.98
  test_data[1, 'location_lat.1'] <- 29.98
  test_data[3, 'location_long.1'] <- 30.02
  test_data[3, 'location_lat.1'] <- 30.02
  b <- calcSpeeds(df = test_data, grpCol = "tag_id", longCol = 'location_long.1', latCol = 'location_lat.1')
  b <- b[, c('tag_id', 'timestamp', 'location_long.1', 'location_lat.1', 'lead_hour_diff_sec'
             ,'lag_hour_diff_sec', 'lead_dist_m', 'lag_dist_m', 'lead_speed_m_s', 'lag_speed_m_s')]
  b %>% print(width=Inf)
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
  
  #TODO test timestamp ordering, groupby, same timestamp spiking, outlier distances/times/speeds
  
})




