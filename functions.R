library(rgeos)

# \
# > Functions
# /

# mutate column types from param
# @param data - a data.frame containing columns to mutate
# @return data - mutated data.frame
mutateType <- function(data) {
  if ("district.id" %in% names(data)) {
    data$district.id <- as.numeric(data$district.id)
  }
  if ("district.name" %in% names(data)) {
    data$district.name <- as.factor(as.character(data$district.name))
  }
  if ("votes" %in% names(data)) {
    data$votes <- as.numeric(as.character(data$votes))
  }
  if ("percentage" %in% names(data)) {
    data$percentage <- as.numeric(as.character(data$percentage))
  }
  if ("party" %in% names(data)) {
    data$party <- as.factor(as.character(data$party))
  }
  if ("candidate" %in% names(data)) {
    data$candidate <- as.character(data$candidate)
  }
  return(data)
}
# decouple sp object into a list containing sub sp objects
# @param sp - an sp object
# @return spl - a list containing decoupled sub sp objects
spDecouple <- function(sp) {
  spl <- list()
  for (i in 1:length(sp@polygons)) {
    spl[[i]] <- SpatialPolygons(list(Polygons(sp@polygons[[i]]@Polygons, ID = sp@polygons[[i]]@ID)))
  }
  return(spl)
}
# get outer or inner polygons from sp object
# @param sp - an sp object
# @param filter - 1 for outer, -1 for inner polygons
# @return sp - a sp object containing filtered polygons
spExtract <- function(sp, filter) {
  for (i in 1:length(sp@polygons)) {
    outer <- Filter(function(f){f@ringDir==filter}, sp@polygons[[i]]@Polygons)
    sp@polygons[[i]]@Polygons <- outer
  }
  return(sp)
}
# check if the polygons in an sp object contains each other
# @param sp - an sp object
# @return relation - a data.frame with index for 
# contained polygon (inner) and its container (outer)
spRelation <- function(sp) {
  spl <- spDecouple(sp)
  relation <- list()
  for (i in 1:length(spl)) {
    relation[[i]] <- sapply(spl, function(x) {gContainsProperly(x, spl[[i]], byid = TRUE)})
  }
  inner <- which((sapply(relation, sum) > 0) == T)
  outer <- sapply(relation[inner], function(x) {which(x == T)})
  relation <- data.frame(inner, outer)
  return(relation)
}
# substract a contained polygon from its container
# @param sp - an sp object
# @param inner - contained polygon index
# @param outer - container polygon index
# @return sp - the processed sp object
spDifference <- function(sp, inner, outer) {
  spl <- spDecouple(sp)
  for (i in 1:length(inner)) {
    substracted <- gDifference(spl[[outer[i]]], spl[[inner[i]]])
    sp@polygons[[outer[i]]] <- substracted@polygons[[1]]
  }
  return(sp)
}
# fix crack in polygon by expanding or shrinking boundaries
# @param sp - an sp object
# @param index - the index of the polygon in sp
# @param width - the width to expand (positive value) or shrink (negative value)
# @return sp - the processed sp object
spFix <- function(sp, index, width) {
  spl <- spDecouple(sp)
  for (i in 1:length(index)) {
    sp@polygons[[index[i]]] <- gBuffer(spl[[index[i]]], width=width[i])@polygons[[1]]
  }
  return(sp)
}
