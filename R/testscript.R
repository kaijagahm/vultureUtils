library(tidyverse)
test <- data.frame(timegroup = c(1, 1, 2, 3, 3, 4, 4, 4, 5, 5), indiv = c("A", "B", "A", "B", "C", "B", "C", "D", "A", "D"))

edges <- data.frame(timegroup = c(1, 3, 5), ID1 = c("A", "B", "A"), ID2 = c("B", "C", "D"))

allPairs <- expand.grid("ID1" = unique(test$indiv), "ID2" = unique(test$indiv), stringsAsFactors = F) %>%
  filter(ID1 < ID2)
allPairsList <- allPairs %>% group_by(ID1, ID2) %>% group_split() %>% map(., as.matrix)

timegroupsList <- test %>%
  group_by(timegroup) %>%
  group_split() %>%
  map(~pull(.x, indiv))

timegroups <- unique(test$timegroup)

# Each of the below things has to happen once *for each dyad*, which means we will be looping through the rows of allPairs. To do that, I've turned allPairs into a list, called allPairsList, because I'm not sure if there is a way to directly map through the rows of a data frame.
# Can probably use pmap for this. Come back. # XXX

# Going to map through allPairsList. The output of this will be a data frame, which I can append to the allPairs data frame.
dfSRI <- map_dfr(allPairsList, ~{
  # define the two individuals
  a <- .x[1]
  b <- .x[2]

  # compute groups that have just a, just b, both, neither, or an edge.
  justA <- timegroups[map_lgl(timegroupsList, ~{(a %in% .x) & !(b %in% .x)})]
  justB <- timegroups[map_lgl(timegroupsList, ~{(b %in% .x) & !(a %in% .x)})]
  hasBoth <- timegroups[map_lgl(timegroupsList, ~{(a %in% .x) & (b %in% .x)})]
  hasEdge <- edges %>%
    filter(ID1 %in% c(a, b) & ID2 %in% c(a, b)) %>%
    pull(timegroup) %>%
    unique()
  hasNeither <- timegroups[map_lgl(timegroupsList, ~{!(a %in% .x) & !(b %in% .x)})]

  # use this information to compute ya, yb, x, yab, and ynull.
  ya <- length(justA) # n timegroups with A but not B
  yb <- length(justB) # n timegroups with B but not A
  x <- length(hasEdge) # n timegroups with an interaction between A and B
  yab <- length(hasBoth) - x
  ynull <- length(hasNeither) # n timegroups with neither A nor B

  # return all of these measures as a data frame row.
  out <- data.frame("ya" = ya, "yb" = yb, "x" = x, "yab" = yab, "ynull" = ynull)
})

# Attach the data frame to the allPairs data frame
allPairs <- allPairs %>%
  bind_cols(dfSRI)

# Calculate SRI. Note that SRI ignores ynull... this just is the way it is. But I wanted to calculate ynull anyway to make sure I knew what we were dealing with.
allPairs <- allPairs %>%
  mutate(sri = x/(x + ya + yb + yab))

# To incorporate this into the package, will need the equivalent of data, edges, timegroups...
