test_that("cleanData outlier check", {
  base::load(test_path("testdata", "raw2022.Rda"))
  a <- raw2022
  mask <- sf::st_read(test_path("testdata", "CutOffRegion.kml"))
  default_row <- a[1]
  default_row['external_temperature'] <- 1 # make sure control row is not outlier
  outlier <- default_row
  outlier['external_temperature'] <- 0     # make outlier
  outlier['barometric_height'] <- 0
  outlier['ground_speed'] <- 0
  test_data <- data.frame(c(default_row, outlier), nrows=2, byrow=T)
  
  cleaned_outlier <- cleanData(test_data, idCol = "trackId", removeVars = T)

  expect_equal(nrow(cleaned_outlier), 1) # cleaned data should remove 1 outlier
  
  # attempt all permutations of outlier stats
  
  test_data <- data.frame()
  
  perms <- expand.grid(rep(list(0:1), 3))
  for (i in seq_len(nrow(perms))){
    perm <- perms[i]
    outlier['external_temperature'] <- perm[1]
    outlier['barometric_height'] <- perm[2]
    outlier['ground_speed'] <- perm[3]
    
    test_data <- rbind(test_data, outlier)
  }
  
  cleaned_outlier_perms <- cleanData(test_data, mask = mask, idCol = "trackId", removeVars = T)
  
  expect_equal(nrow(cleaned_outlier_perms), 7) # all permutations should only have 1 outlier
  expect_equal("outlier" %in% colnames(cleaned_outlier_perms), F) # outlier column name should be removed
})

test_that("cleanData bad gps check", {
  base::load(test_path("testdata", "raw2022.Rda"))
  a <- raw2022
  mask <- sf::st_read(test_path("testdata", "CutOffRegion.kml"))
  default_row <- a[1]
  default_row['gps_time_to_fix'] <- 20 # control row
  badgps_greater <- default_row
  badgps_greater['gps_time_to_fix'] <- 120
  badgps_equal <- default_row
  badgps_equal['gps_time_to_fix'] <- 89
  badgps_less <- default_row
  badgps_less['gps_time_to_fix'] <- -20
  test_data <- data.frame(c(default_row, badgps_less, badgps_equal, badgps_greater), nrows=4, byrow=T)
  
  cleaned_gps <- cleanData(test_data, idCol = "trackId", removeVars = T)
  
  expect_equal(nrow(cleaned_gps), 1) # gps_time_to_fix <= 89 should return 2 rows, but -20 is illogical
})

test_that("cleanData bad heading check", {
  base::load(test_path("testdata", "raw2022.Rda"))
  a <- raw2022
  mask <- sf::st_read(test_path("testdata", "CutOffRegion.kml"))
  default_row <- a[1]
  default_row['heading'] <- 180 # control row
  heading_greater <- default_row
  heading_greater['gps_time_to_fix'] <- 400
  heading_less <- default_row
  heading_less['gps_time_to_fix'] <- -20
  test_data <- data.frame(c(default_row, heading_less, heading_greater), nrows=3, byrow=T)
  
  cleaned_heading <- cleanData(test_data, idCol = "trackId", removeVars = T)
  
  expect_equal(nrow(cleaned_heading), 1) # heading shouldn't accept values not between 0-360
})

test_that("cleanData sattelite check", {
  base::load(test_path("testdata", "raw2022.Rda"))
  a <- raw2022
  mask <- sf::st_read(test_path("testdata", "CutOffRegion.kml"))
  default_row <- a[1]
  default_row['gps_satellite_count'] <- 5 # control row
  sattelite_equal <- default_row
  sattelite_equal['gps_satellite_count'] <- 3
  sattelite_less <- default_row
  sattelite_less['gps_satellite_count'] <- 2
  sattelite_neg <- default_row
  sattelite_neg['gps_satellite_count'] <- -1
  test_data <- data.frame(c(default_row, sattelite_equal, sattelite_less, sattelite_neg), nrows=4, byrow=T)
  
  cleaned_sattelite <- cleanData(test_data, idCol = "trackId", removeVars = T)
  
  expect_equal(nrow(cleaned_sattelite), 2) # 3 sattelites inclusive
})