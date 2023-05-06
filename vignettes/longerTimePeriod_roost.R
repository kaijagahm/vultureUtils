# Data
load("data/roosts.Rda")
r <- roosts[[1]]
simple <- r %>%
  select(trackId, date, location_lat, location_long)

# Make a single column containing just one grouping variable for the entire time period. Here is an example grouping by weeks:
simple <- simple %>%
  mutate(week = cut(date, "7 days"))

# Now feed it into getRoostEdges, setting the `dateCol` parameter to use this new "week" column *instead* of the "date" column. NOTE: if you use it this way, you can't use return = "sri"--I don't know what will happen. I would have to change the code to make it work. But if you just use return = "edges" and do a little post-processing, it'll be fine.
edges <- getRoostEdges(dataset = simple, mode = "distance", distThreshold = 25, latCol = "location_lat", longCol = "location_long", idCol = "trackId",
                       dateCol = "week", # IMPORTANT! use the new column here, instead of the original date column.
                       crsToSet = "WGS84", crsToTransform = 32636,
                       return = "edges") # IMPORTANT! I think this will only work with return = "edges". Don't try to use return = "sri".
# you will get a warning message here about duplicate rows. Don't worry about it! Of course there will be duplicates, since each individual will occur more than once within the time window.

# Now we can use some post-processing to look at how many times each dyad interacts.
# Make sure we only have edges going one way, and no self edges:
edges <- edges %>%
  filter(ID1 < ID2) # removes duplicates and self edges

# Count number of interactions
forIgraph <- edges %>%
  group_by(week, ID1, ID2) %>%
  summarize(weight = n())

# And then I think you can pass that into igraph as an edge list, maybe using igraph::graph_from_data_frame, or something? Just use the `weight` column to define the edge weights.

# Here's another example where, instead of cutting the date sequence into weeks, you only have one single time period (such as your 14-day windows). You're still going to have to make a column containing the grouping variable, but you can call it whatever you want.
simple <- simple %>%
  mutate(entireTimePeriod = "anything you want to put here, it doesn't matter, it just has to be treated as a factor")

# Now feed it into getRoostEdges, setting the `dateCol` parameter to use this new "week" column *instead* of the "date" column. NOTE: if you use it this way, you can't use return = "sri"--I don't know what will happen. I would have to change the code to make it work. But if you just use return = "edges" and do a little post-processing, it'll be fine.
edges <- getRoostEdges(dataset = simple, mode = "distance", distThreshold = 25, latCol = "location_lat", longCol = "location_long", idCol = "trackId",
                       dateCol = "entireTimePeriod", # IMPORTANT! use the new column here, instead of the original date column.
                       crsToSet = "WGS84", crsToTransform = 32636,
                       return = "edges") # IMPORTANT! I think this will only work with return = "edges". Don't try to use return = "sri".
# you will get a warning message here about duplicate rows. Don't worry about it! Of course there will be duplicates, since each individual will occur more than once within the time window.

# Now we can use some post-processing to look at how many times each dyad interacts.
# Make sure we only have edges going one way, and no self edges:
edges <- edges %>%
  filter(ID1 < ID2) # removes duplicates and self edges

# Count number of interactions
forIgraph <- edges %>%
  group_by(entireTimePeriod, ID1, ID2) %>%
  summarize(weight = n())

