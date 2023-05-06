# Data
load("../../Downloads/mydata_1.rda")
# Make a single column containing just one grouping variable for the entire time period. Here is an example grouping by weeks:
# Now feed it into getFeedingEdges
edges <- getFeedingEdges(dataset = mydata_1, roostPolygons = NULL,
                         roostBuffer = 50, consecThreshold = 2, distThreshold = 25, speedThreshUpper = 5, speedThreshLower = NULL, timeThreshold = "20 days", idCol = "id", return = "edges")

# Now we can use some post-processing to look at how many times each dyad interacts.
# Make sure we only have edges going one way, and no self edges:
edges <- edges %>%
  mutate(ID1 = as.character(ID1),
         ID2 = as.character(ID2)) %>%
  filter(ID1 < ID2) # removes duplicates and self edges

# Count number of interactions
forIgraph <- edges %>%
  group_by(ID1, ID2) %>%
  summarize(weight = n())

