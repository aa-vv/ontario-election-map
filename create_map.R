library(tools)
library(gdata)
library(dplyr)
library(rgdal)
library(maptools)
library(gpclib)
library(leaflet)

source("./functions.R")

# \
# > Download Data
# /

# data downloaded from www.elections.on.ca
url.result <- "https://www.elections.on.ca/content/dam/NGW/sitecontent/2018/results/Valid%20Votes%20Cast%20for%20Each%20Candidate%20-%202018%20General%20Election%20-%20Excel.zip"
url.shape <- "https://www.elections.on.ca/content/dam/NGW/sitecontent/2017/preo/shapefiles/Polling%20Division%20Shapefile%20-%202018%20General%20Election.zip"
# check file existence, create dir and download from urls
if (!file.exists("./result")) {
  dir.create("./result")
}
if (!file.exists("./shape")) {
  dir.create("./shape")
}
if (!file.exists("./result/result.zip")) {
  download.file(url.result, "./result/result.zip")
}
if (!file.exists("./shape/shape.zip")) {
  download.file(url.shape, "./shape/shape.zip")
}
# unzip the files
unzip("./result/result.zip", exdir = "./result", junkpaths = T)
unzip("./shape/shape.zip", exdir = "./shape", junkpaths = T)

# \
# > Tidy Data
# /

# loop reading files, select certain rows and columns and combine results
# tmp is retained to reduce rerunning time
result <- data.frame()
if (!exists("tmp")) {
  for (i in 1:124) {
    ed.id <- formatC(i, width = 3, format = "d", flag = "0")
    file.prefix <- "result/Summary of Valid Votes Cast for Each Candidate - ED"
    file.path <- paste0(file.prefix, ed.id, ".xls")
    tmp <- read.xls(file.path)
    ed.name <- tmp[4, 1]
    tmp <- tmp[-c(4, 5), ]
    tmp <- tmp[3:dim(tmp)[1], 5:8]
    tmp <- cbind(i, ed.name, tmp)
    result <- rbind(result, tmp)
  }
} else {
  result <- tmp
}
tmp <- result
# name columns
col.names <- c("district.id", "district.name", "votes", "percentage", "party", "candidate")
names(result) <- col.names
# mutate type and order by district.name ascending and vote descending
result <- mutateType(result)
result <- result[order(result$district.name, -result$votes), ]
# convert latin character and standardize name display
result$district.name <- iconv(gsub("—", " - ", result$district.name), "latin1", "UTF-8")
result$candidate <- sapply(iconv(result$candidate, "latin1", "UTF-8"), function(x) toTitleCase(tolower(x)))
# get winning party data in each district
result.win <- group_by(result, district.id) %>%
  summarize(
    first(district.name),
    first(votes),
    first(percentage),
    first(party),
    first(candidate)
  )
names(result.win) <- col.names
# order result.win by district.name ascending and vote descending
result.win <- result.win[order(result.win$district.name, -result.win$votes), ]

# \
# > Shapes and Polygons
# /

# read shapefile
ed.shape <- readOGR("shape/POLLING_DIVISION.shp", layer = "POLLING_DIVISION")
# get district.name from result.win according to ed.shape$ED_ID
ed.shape$district.name <- result.win$district.name[match(ed.shape$ED_ID, result.win$district.id)]
# default projection needs to be corrected in order to display polygons properly
proj <- sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
ed.shape <- sp::spTransform(ed.shape, proj)
# sometimes this permit needs to be enabled to perform unionSpatialPolygons(...)
gpclibPermit()
# merge polygons in the same district
# this function coerces IDs into characters by default, numeric num causes incorrect order
ed.shape.poly <- unionSpatialPolygons(ed.shape, ed.shape$district.name)
# extract outer polygons
ed.shape.poly <- spExtract(ed.shape.poly, 1)
# check if the polygons contain each other
relation <- spRelation(ed.shape.poly)
# substract contained polygon from its container
ed.shape.poly <- spDifference(ed.shape.poly, relation$inner, relation$outer)
# fix silvers by expanding the edge
ed.shape.poly <- spFix(ed.shape.poly, 88, 3e-4)
# get data out of shape file
ed.shape.data <- data.frame(ed.shape)
# acquire unique district id and assign row names
ed.shape.data <- unique(ed.shape.data[2])
row.names(ed.shape.data) <- as.character(1:dim(ed.shape.data)[1])
# join result.win into ed.shape.data according to district.id
names(ed.shape.data) <- c("district.id")
ed.shape.data <- right_join(result.win, ed.shape.data, by = "district.id")
# mutate data type to get rid of redundant factor levels and order only by district.name
ed.shape.data <- mutateType(ed.shape.data)
ed.shape.data <- ed.shape.data[order(ed.shape.data$district.name), ]
# combine merged polygons and data into shape object
ed.shape <- SpatialPolygonsDataFrame(ed.shape.poly, ed.shape.data, match.ID = F)
# mutate data type to get rid of redundant factor levels
ed.shape <- mutateType(ed.shape)

# \
# > Visualization
# /

# create color options
pal <- colorFactor(
  palette = c("green", "red", "orange", "blue"),
  domain = ed.shape$party
)
# prepare popups, arrange the table according to the district.id order in result
party.num <- table(result$district.id)[unique(result$district.id)]
# loop through each district, get all party info and put them in html table
popup <- vector()
for (i in 1:124) {
  popup[i] <- paste(
    "Riding", result.win$district.id[i], ":",
    result.win$district.name[i], "<br> <br>",
    "<table style=\"width:100%\">
    <tr>
    <th align=\"left\"> Party </th>
    <th align=\"left\"> Candidate </th>
    <th align=\"right\"> Votes </th>
    <th align=\"right\"> % </th>
    </tr>"
  )
  for (j in 1:party.num[i]) {
    index <- sum(party.num[0:(i - 1)]) + j
    if (j == 1) {
      td.sl <- "<td align=\"left\"> <font color=\"red\">"
      td.sr <- "<td align=\"right\"> <font color=\"red\">"
      td.c <- "</font> </td>"
    } else {
      td.sl <- "<td align=\"left\">"
      td.sr <- "<td align=\"right\">"
      td.c <- "</td>"
    }
    popup[i] <- paste(
      popup[i],
      "<tr>",
      td.sl, result[index, 5], td.c,
      td.sl, result[index, 6], td.c,
      td.sr, result[index, 3], td.c,
      td.sr, formatC(result[index, 4], digit = 1, format = "f"), td.c,
      "</tr>"
    )
    if (j == 1) {
      popup[i] <- paste(popup[i], "</i>")
    }
  }
  popup[i] <- paste(popup[i], "</table>")
}
# visulization using leaflet
leaflet(ed.shape) %>%
  addTiles() %>%
  addPolygons(
    color = "#404040", weight = 0.5, smoothFactor = 0.99, opacity = 1,
    fillOpacity = ~percentage / 100, fillColor = ~pal(party),
    popup = popup, label = ~district.name,
    highlightOptions = highlightOptions(
      color = "white", weight = 2, bringToFront = T
    )
  ) %>%
  addLegend(
    position = "topright",
    pal = pal, values = ~party,
    title = "Party",
    opacity = 0.5
  )
