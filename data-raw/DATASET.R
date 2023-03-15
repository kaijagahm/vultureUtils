## This code prepares datasets from Movebank for testing.

## 3-day datasets
base::load("~/Desktop/otherDir/movebankCredentials/pw.Rda")
MB.LoginObject <- move::movebankLogin(username = 'kaijagahm', password = pw)
rm(pw)
mask <- sf::st_read("~/Desktop/otherDir/CutOffRegion.kml")
roosts <- sf::st_read("~/Desktop/otherDir/roosts25_cutOffRegion.kml")
crs <- "WGS84"
crsITM <- 32636

# Download two 3-day datasets from movebank (note: will have to separately ensure that downloadVultures is functioning as expected.)
jan01_03_2021_raw <- downloadVultures(loginObject = MB.LoginObject, dateTimeStartUTC = "2021-01-01 00:00", dateTimeEndUTC = "2021-03-01 11:59")
jun01_03_2022_raw <- downloadVultures(loginObject = MB.LoginObject, dateTimeStartUTC = "2022-06-01 00:00", dateTimeEndUTC = "2022-06-03 11:59")

# Save these as internal datasets
save(jan01_03_2021_raw, file = "~/Desktop/otherDir/jan01_03_2021_raw.Rda")
save(jun01_03_2022_raw, file = "~/Desktop/otherDir/jun01_03_2022_raw.Rda")

# Clean the data
cleaned_jan <- cleanData(dataset = jan01_03_2021_raw, mask = mask, crs = crs)
cleaned_jun <- cleanData(dataset = jun01_03_2022_raw, mask = mask, crs = crs)

save(cleaned_jan, file = "~/Desktop/otherDir/cleaned_jan.Rda")
save(cleaned_jun, file = "~/Desktop/otherDir/cleaned_jun.Rda")

# Get edgelists
## feeding
jan_feeding_both <- getFeedingEdges(dataset = cleaned_jan, roostPolygons = roosts, return = "both")
jun_feeding_both <- getFeedingEdges(dataset = cleaned_jun, roostPolygons = roosts, return = "both")
save(jan_feeding_both, file = "~/Desktop/otherDir/jan_feeding_both.Rda")
save(jun_feeding_both, file = "~/Desktop/otherDir/jun_feeding_both.Rda")

## flight
jan_flight_both <- getFlightEdges(dataset = cleaned_jan, roostPolygons = roosts, return = "both")
jun_flight_both <- getFlightEdges(dataset = cleaned_jun, roostPolygons = roosts, return = "both")
save(jan_flight_both, file = "~/Desktop/otherDir/jan_flight_both.Rda")
save(jun_flight_both, file = "~/Desktop/otherDir/jun_flight_both.Rda")

## roosting
### get roosts
janRoosts <- get_roosts_df(df = cleaned_jan, id = "trackId")
junRoosts <- get_roosts_df(df = cleaned_jun, id = "trackId")
save(janRoosts, file = "~/Desktop/otherDir/janRoosts.Rda")
save(junRoosts, file = "~/Desktop/otherDir/junRoosts.Rda")

### distance
jan_roost_dist_both <- getRoostEdges(dataset = janRoosts, mode = "distance", return = "both")
jun_roost_dist_both <- getRoostEdges(dataset = junRoosts, mode = "distance", return = "both")
save(jan_roost_dist_both, file = "~/Desktop/otherDir/jan_roost_dist_both.Rda")
save(jun_roost_dist_both, file = "~/Desktop/otherDir/jun_roost_dist_both.Rda")

### polygon
jan_roost_poly_both <- getRoostEdges(dataset = janRoosts, mode = "polygon", return = "both", roostPolygons = roosts)
jun_roost_poly_both <- getRoostEdges(dataset = junRoosts, mode = "polygon", return = "both", roostPolygons = roosts)
save(jan_roost_poly_both, file = "~/Desktop/otherDir/jan_roost_poly_both.Rda")
save(jun_roost_poly_both, file = "~/Desktop/otherDir/jun_roost_poly_both.Rda")
