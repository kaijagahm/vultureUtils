# Misc functions (deprecated, not fully developed, etc.)
if(getRversion() >= "2.15.1")  utils::globalVariables(".") # this supposedly helps deal with some of the data masking issues with dplyr/tidyverse syntax.

#' Convert coordinates of an sf object
#'
#' Given an sf object in WGS84, convert it to a CRS with meters as the units.
#' @param obj an sf object to be buffered
#' @param crsMeters crs with units of meters to be used. Default is 32636 (Israel, UTM zone 36)
#' @return A buffered sf object
#' @export
convert <- function(obj, crsMeters = 32636){
  checkmate::assertClass(obj, "sf")
  originalCRS <- sf::st_crs(obj)
  if(is.null(originalCRS)){
    stop("Object does not have a valid CRS.")
  }

  converted <- obj %>%
    sf::st_transform(crsMeters)

  return(converted)
}

#' Calculate speeds based on lead/lag
#'
#' Supporting function for removeSpeedOutliers
#' @param dataset the dataset to remove outliers from
#' @param idCol name of the column containing the animal identifier
#' @param timestampCol name of the column containing timestamps
#' @param x name of the column containing longitude values
#' @param y name of the column containing latitude values
#' @return a dataset with speeds calculated
#' @export
calculateSpeeds <- function(dataset, idCol, timestampCol, x, y){
  # argument checks
  checkmate::assertDataFrame(dataset)
  checkmate::assertCharacter(idCol, len = 1)
  checkmate::assertCharacter(timestampCol, len = 1)
  checkmate::assertCharacter(x, len = 1)
  checkmate::assertCharacter(y, len = 1)
  checkmate::assertNumeric(dataset[[x]])
  checkmate::assertNumeric(dataset[[y]])

  # speed calculations
  out <- dataset %>%
    dplyr::group_by(.data[[idCol]]) %>%
    dplyr::arrange({{timestampCol}}) %>%
    dplyr::mutate(lead_hour_diff_sec = round(as.numeric(difftime(dplyr::lead(.data[[timestampCol]]), .data[[timestampCol]], units = "secs")), 3),
                  lead_hour_diff_sec = ifelse(.data$lead_hour_diff_sec == 0, 0.01, .data$lead_hour_diff_sec),
                  lag_hour_diff_sec = round(as.numeric(difftime(dplyr::lag(.data[[timestampCol]]), .data[[timestampCol]], units = "secs")), 3),
                  lag_hour_diff_sec = ifelse(.data$lag_hour_diff_sec == 0, 0.01, .data$lag_hour_diff_sec),
                  lead_dist_m = round(geosphere::distGeo(p1 = cbind(dplyr::lead(.data[[x]]), dplyr::lead(.data[[y]])),
                                                         p2 = cbind(.data[[x]], .data[[y]])), 3),
                  lag_dist_m = round(geosphere::distGeo(p1 = cbind(dplyr::lag(.data[[x]]), dplyr::lag(.data[[y]])),
                                                        p2 = cbind(.data[[x]], .data[[y]])), 3)) %>%
    dplyr::mutate(lead_speed_m_s = round(.data$lead_dist_m / .data$lead_hour_diff_sec, 2),
                  lag_speed_m_s = round(.data$lag_dist_m / .data$lag_hour_diff_sec, 2)) %>%
    dplyr::ungroup()
  return(out)
}

#' Remove speed outliers
#'
#' This function identifies points that are errors/outliers based on the inferred lead/lag speed. Written by Marta Acácio as part of the data cleaning workflow; adapted for this package by Kaija Gahm.
#' @param dataset the dataset to remove outliers from
#' @param idCol name of the column containing the animal identifier
#' @param timestampCol name of the column containing timestamps
#' @param x name of the column containing longitude values
#' @param y name of the column containing latitude values
#' @param includeSpeedCols logical, whether to return a data frame including the calculated speeds (T) or remove the calculated speed columns (F, default)
#' @return a data frame with speed-based outliers removed.
#' @export
removeSpeedOutliers <- function(dataset, idCol = "trackId", timestampCol = "timestamp", x = "location_long", y = "location_lat", includeSpeedCols = FALSE){
  # argument checks
  checkmate::assertDataFrame(dataset)
  checkmate::assertCharacter(idCol, len = 1)
  checkmate::assertCharacter(timestampCol, len = 1)
  checkmate::assertCharacter(x, len = 1)
  checkmate::assertCharacter(y, len = 1)
  checkmate::assertNumeric(dataset[[x]])
  checkmate::assertNumeric(dataset[[y]])
  checkmate::assertLogical(includeSpeedCols, len = 1)

  # Compute speeds and filter
  dataset <- vultureUtils::calculateSpeeds(dataset = dataset, idCol = idCol, timestampCol = timestampCol, x = x, y = y)

  # First remove those that are for sure outliers: lead + lag > 180km/h
  dataset2 <- dataset %>%
    dplyr::filter(.data$lead_speed_m_s <= 50 & abs(.data$lag_speed_m_s) <= 50) %>%
    dplyr::select(-c("lead_hour_diff_sec", "lead_dist_m", "lead_speed_m_s",
                     "lag_hour_diff_sec", "lag_dist_m", "lag_speed_m_s"))

  # Re-calculate the speeds (because we removed some observations before)
  dataset3 <- vultureUtils::calculateSpeeds(dataset = dataset2, idCol = idCol, timestampCol = timestampCol, x = x, y = y)

  # However, this does not get rid of all the outliers, unfortunately. So I will use only the lead to remove some more outliers
  dataset3 <- dataset3 %>%
    dplyr::filter(.data$lead_speed_m_s <= 50)

  # This also does not get rid of all the outliers... But most of them are at night, which because of the reduced schedule, does not seem like such a large speed (many hours divided by a few kms)

  # so first, we will calculate if the fix is during the day or during the night
  crds <- matrix(c(dataset3[[x]], dataset3[[y]]),
                 nrow = nrow(dataset3),
                 ncol = 2)

  dataset3$sunrise <- maptools::sunriset(crds, dataset3[[timestampCol]],
                                         proj4string = sp::CRS("+proj=longlat +datum=WGS84"),
                                         direction = "sunrise", POSIXct.out = TRUE)$time
  dataset3$sunset <- maptools::sunriset(crds, dataset3[[timestampCol]],
                                        proj4string = sp::CRS("+proj=longlat +datum=WGS84"),
                                        direction = "sunset", POSIXct.out = TRUE)$time

  dataset3 <- dataset3 %>%
    dplyr::mutate(daylight = ifelse(.data[[timestampCol]] >= .data$sunrise & .data[[timestampCol]] <= .data$sunset, "day", "night"))

  # Re-calculate the speeds
  dataset4 <- vultureUtils::calculateSpeeds(dataset = dataset3, idCol = idCol, timestampCol = timestampCol, x = x, y = y)

  # Exclude if during the night the distance between two locations are more than 10km apart
  dataset4 <- dataset4 %>%
    dplyr::mutate(
      day_diff = as.numeric(difftime(dplyr::lead(.data$date), .data$date, units = "days")),
      night_outlier = ifelse(.data$daylight == "night" &
                               .data$day_diff %in% c(0,1) &
                               dplyr::lead(.data$daylight) == "night" &
                               .data$lead_dist_m > 10000 , 1, 0)) %>%
    dplyr::filter(.data$night_outlier == 0)

  if(includeSpeedCols){
    return(dataset4)
  }else{
    dataset4 <- dataset4 %>%
      dplyr::select(-c("lead_hour_diff_sec", "lead_dist_m", "lead_speed_m_s",
                       "lag_hour_diff_sec", "lag_dist_m", "lag_speed_m_s"))
    return(dataset4)
  }
}

#' Get roosts
#'
#' With several methods, get the roost locations of a vulture on each night. This function was written by Marta Acácio. The function consecutively calculates the night roost based on the following five methods:
#' 1.  It is the last GPS location of the day, and it is at night (and during the "night hours"), and the speed is equal or less than 4m/s (i.e., the bird was considered to not be flying);
#' 2.  It is the last GPS location of the day, and it is at night (and during the "night hours"), and the speed is NA;
#' 3.  It is the first GPS location of the day, and it is at night (and during the "morning hours"), and the speed is equal or less than 4m/s;
#' 4.  It is the first GPS location of the day, and it is at night (and during the "morning hours"), and the speed is NA;
#' 5.  The last GPS location of the day (independently of the light or the hours) is within the defined buffer (pre-defined, 1km) of the first GPS location of the following day.
#' The roost day is assigned in the following way:
#' 1.  If it is the last location of the day, it is that day's night roost;
#' 2.  If it was the first location of the day, it was the previous day's night roost;
#' 3.  If for a particular date, the roost was calculated using more than 1 method, the selected roost is the earliest calculated roost.
#' @param id animal identifier
#' @param timestamp object of class `as.POSIXct`
#' @param x longitude, in decimal degrees
#' @param y latitude, in decimal degrees
#' @param ground_speed ground speed of the animal
#' @param speed_units units of speed (either "m/s" or "km/h"). If speed is provided in "km/h" it is transformed to "m/s".
#' @param buffer optional, numerical value indicating the number of kms to consider the buffer for the roost calculation. The pre-defined value is 1km
#' @param twilight optional, numerical value indicating number of minutes to consider the twilight for calculating the day and night positions. If set to 0, the night period starts at sunset and the day period starts at sunrise. The pre-defined value is 61, so the night period starts 61 minutes before sunset and the day period starts 61 minutes after sunrise
#' @param morning_hours optional, vector indicating the range of hours (in UTC) that are considered the morning period. The pre-defined vector is `c(0:12)`
#' @param night_hours optional, vector indicating the range of hours (in UTC) that are considered the night period. The pre-defined vector is `c(13:23)`
#' @return a data frame of the calculated roosts for every animal.
#' @export
get_roosts <- function(id, timestamp, x, y, ground_speed, speed_units = c("m/s", "km/h"), buffer = 1, twilight = 61, morning_hours = c(0:12), night_hours = c(13:23)){

  # Transform the twilight period into seconds
  twilight <- twilight * 60

  # If the speed is in km/h transform into m/s
  if(speed_units == "km/h"){
    ground_speed <- round(.data$ground_speed / 3.6, 3)
  }

  # Create the dataset
  df <- data.frame("id" = id,
                   "timestamp" = timestamp,
                   "location_long" = x,
                   "location_lat" =  y,
                   "ground_speed" = ground_speed)

  df$timestamp <- as.POSIXct(df$timestamp,
                             format = "%Y-%m-%d %H:%M:%S",
                             tz = "UTC")

  if(sum(is.na(df$timestamp)) > 0){
    stop("Timestamp needs to be defined as.POSIXct (%Y-%m-%d %H:%M:%S)")
  }

  df$date <- as.Date(df$timestamp)

  # Calculate the roosts
  roost.df <- df[FALSE,]

  for(i in 1:dplyr::n_distinct(df$id)){

    temp.id <- unique(df$id)[i]

    id.df <- df %>%
      dplyr::filter(.data$id == temp.id) %>%
      dplyr::group_by(.data$date) %>%
      dplyr::arrange(.data$timestamp) %>%
      dplyr::mutate(
        row_id = dplyr::case_when(
          dplyr::row_number() == 1 ~ "first",
          dplyr::row_number() == max(dplyr::row_number()) ~ "last"),
        hour = lubridate::hour(.data$timestamp)) %>%
      dplyr::filter(.data$row_id %in% c("first", "last")) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        day_diff = round(difftime(dplyr::lead(.data$date), .data$date, units="days")),
        dist_km = ifelse(.data$day_diff == 1,
                         round(geosphere::distGeo(p1 =
                                                    cbind(dplyr::lead(.data$location_long),
                                                          dplyr::lead(.data$location_lat)),
                                                  p2 =
                                                    cbind(.data$location_long,
                                                          .data$location_lat))*0.001, 2), NA))

    # Calculate the time of sunrise and sunset for the locations
    crds <- matrix(c(id.df$location_long,
                     id.df$location_lat),
                   nrow = nrow(id.df),
                   ncol = 2)

    id.df$sunrise <- maptools::sunriset(crds,
                                        id.df$timestamp,
                                        proj4string =
                                          sp::CRS("+proj=longlat +datum=WGS84"),
                                        direction = "sunrise",
                                        POSIXct.out = TRUE)$time

    id.df$sunset <- maptools::sunriset(crds,
                                       id.df$timestamp,
                                       proj4string =
                                         sp::CRS("+proj=longlat +datum=WGS84"),
                                       direction = "sunset",
                                       POSIXct.out = TRUE)$time

    # Set the twilight
    id.df$sunrise_twilight <- id.df$sunrise + twilight
    id.df$sunset_twilight <- id.df$sunset - twilight

    id.df <- id.df %>%
      dplyr::mutate(daylight = ifelse(.data$timestamp >= .data$sunrise_twilight &
                                        .data$timestamp <= .data$sunset_twilight,
                                      "day", "night"))

    rm(crds)

    # Identify the roosts
    id.df <- id.df %>%
      dplyr::mutate(
        is_roost = dplyr::case_when(
          .data$row_id == "last" & .data$daylight == "night" & .data$hour %in% night_hours &
            .data$ground_speed <= 4 ~ 1,
          .data$row_id == "last" & .data$daylight == "night" & .data$hour %in% night_hours &
            is.na(.data$ground_speed) ~ 1,
          .data$row_id == "first" & .data$daylight == "night" & .data$hour %in% morning_hours &
            .data$ground_speed <= 4 ~ 1,
          .data$row_id == "first" & .data$daylight == "night" & .data$hour %in% morning_hours &
            is.na(.data$ground_speed) ~ 1,
          dist_km <= buffer ~ 1),
        roost_date = dplyr::case_when(
          .data$is_roost == 1 & .data$row_id == "last" ~ paste(as.character(.data$date)),
          .data$is_roost == 1 & .data$row_id == "first" ~ paste(as.character(.data$date-1))),
        roost_date = as.Date(.data$roost_date))

    temp.id.roosts <- dplyr::filter(id.df, .data$is_roost == 1)

    # If there is more than 1 roost per day, keep the earliest roost (night roost)
    temp.id.roosts <- temp.id.roosts %>%
      dplyr::group_by(.data$roost_date) %>%
      dplyr::arrange(.data$timestamp) %>%
      dplyr::filter(dplyr::row_number() == 1) %>%
      dplyr::ungroup() %>%
      dplyr::select(-c("row_id", "hour"))

    roost.df <- rbind(roost.df, temp.id.roosts)

    rm(temp.id.roosts)
    rm(id.df)
    rm(temp.id)

  }

  return(roost.df)

}

#' Compute probabilities of gaining and losing edges.
#'
#' Given a list of graphs, compute the probability at each time step of gaining or losing edges, looking two timesteps back.
#' @param graphList a list of network graphs
#' @return a long-format data frame containing probabilities of losing/adding edges
#' @export
computeProbs <- function(graphList){
  # Get a complete list of all possible edges
  completeGraph <- do.call(igraph::union, graphList)
  complete_edgelist <- expand.grid(names(igraph::V(completeGraph)),
                                   names(igraph::V(completeGraph))) %>%
    dplyr::filter(as.character(.data$Var1) < as.character(.data$Var2)) %>%
    as.matrix()

  # get edge list for each element of the graph list
  els <- purrr::map(graphList, igraph::get.edgelist)

  # for each graph, check whether each edge is present or not
  tf <- purrr::map(els, ~complete_edgelist %in% .x)

  # bind into a data frame showing presence/absence of edges over time.
  overTime <- do.call(cbind, tf) %>% as.data.frame()

  # add two blank steps before (all FALSE), and add the data for the individuals that make up each edge.
  beforeSteps <- data.frame(stepPrevPrev = rep(FALSE, nrow(overTime)),
                            stepPrev = rep(FALSE, nrow(overTime)))
  overTime <- cbind(stats::setNames(as.data.frame(complete_edgelist),
                                    c("ID1", "ID2")), beforeSteps, overTime) # this is our final history data frame

  # Create a data frame of probabilities based on overTime
  histdf <- data.frame("add00" = NA, "add10" = NA, "lose01" = NA, "lose11" = NA)
  for(i in 5:ncol(overTime)){
    vec <- vector(mode = "character", nrow(overTime))
    vec[which(!overTime[,i-2] & !overTime[,i-1])] <- "hist00"
    vec[which(!overTime[,i-2] & overTime[,i-1])] <- "hist01"
    vec[which(overTime[,i-2] & !overTime[,i-1])] <- "hist10"
    vec[which(overTime[,i-2] & overTime[,i-1])] <- "hist11"

    # compute the probabilities
    add00 <- sum(overTime[i] & vec == "hist00")/sum(vec == "hist00")
    add10 <- sum(overTime[i] & vec == "hist10")/sum(vec == "hist10")
    lose01 <- sum(!overTime[i] & vec == "hist01")/sum(vec == "hist01")
    lose11 <- sum(!overTime[i] & vec == "hist11")/sum(vec == "hist11")

    histdf[i-4,] <- c("add00" = add00, "add10" = add10, "lose01" = lose01, "lose11" = lose11)
  }

  histdfLong <- histdf %>%
    dplyr::mutate(earlyDate = names(overTime)[-1:-4]) %>%
    tidyr::pivot_longer(cols = -.data$earlyDate, names_to = "type", values_to = "prob")

  return(histdfLong)
}

#' Make a gif from a graph list
#'
#' Given a list of plots, create a gif.
#' @param plotList a list of plots
#' @param fileName where to write the file. Must end with .gif.
#' @param interval a positive number to set the time interval of the animation (unit in seconds); default to 0.1.
#' @return A gif file
#' @export
makeGIF <- function(plotList, fileName, interval = 0.1){
  checkmate::assertList(plotList)
  checkmate::assertNumeric(interval, len = 1)
  pb <- utils::txtProgressBar(min = 0, max = length(plotList),
                              initial = 0, style = 3)
  # Save the gif
  animation::saveGIF({
    for(i in 1:length(plotList)){
      plot(plotList[[i]])
      utils::setTxtProgressBar(pb, i)
    }
  },
  movie.name = fileName,
  interval = interval)
}
