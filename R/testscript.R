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

# Each of the below things has to happen once *for each dyad*, which means we will be looping through the rows of allPairs.

# ya = n timegroups with just A, but not B
ya <- map_dbl(allPairsList, ~{
  a <- .x[1]
  containsA <- timegroups[map_lgl(timegroupsList, ~a %in% .x)]
  return(length(containsA))
})

# yb = n timegroups with just B, but not A
yb <- map_dbl(allPairsList, ~{
  b <- .x[2]
  containsB <- timegroups[map_lgl(timegroupsList, ~b %in% .x)]
  return(length(containsB))
})

# x = n timegroups with an edge between A and B
x <- map_dbl(allPairsList, ~{
  a <- .x[1]
  b <- .x[2]
  hasEdge <- edges %>% filter(ID1 %in% c(a, b) & ID2 %in% c(a, b)) %>% pull(timegroup) %>% unique()
  return(length(hasEdge))
})

# yab = timegroups with both A and B, but not together. So, [timegroups with both A and B] - x
bothAB <- map_dbl(allPairsList, ~{
  a <- .x[1]
  b <- .x[2]
  hasBoth <- timegroups[map_lgl(timegroupsList, ~{a %in% .x & b %in% .x})]
  return(length(hasBoth))
})
yab <- bothAB - x

# ynull = timegroups with neither A nor B
ynull <- map_dbl(allPairsList, ~{
  a <- .x[1]
  b <- .x[2]
  hasNeither <- timegroups[map_lgl(timegroupsList, ~{!(a %in% .x) & !(b %in% .x)})]
  return(length(hasNeither))
})
