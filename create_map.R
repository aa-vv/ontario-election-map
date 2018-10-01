library(rgdal)
library(leaflet)
library(maptools)
library(gpclib)
library(rgeos)
library(gdata)
library(dplyr)
# clean data


TidyStruct <- function(data) {
    data$election.district.name <- as.factor(as.character(data$election.district.name))
    data$votes <- as.numeric(as.character(data$votes))
    data$percentage <- as.numeric(as.character(data$percentage))
    data$political.interest.code <- as.factor(as.character(data$political.interest.code))
    data$candidate.name <- as.character(data$candidate.name)
    return(data)
}

result <- data.frame()

for (i in 1:124) {
    ed.id <- formatC(i, width = 3, format = "d", flag = "0")
    file.prefix <- "result/Summary of Valid Votes Cast for Each Candidate - ED"
    file.path <- paste0(file.prefix, ed.id, ".xls")
    data <- read.xls(file.path)
    ed.name <- data[4,1]
    data <- data[-c(4,5),]
    data <- data[3:dim(data)[1], 5:8]
    data <- cbind(i, ed.name, data)
    result <- rbind(result, data)
}

col.names <- c("election.district.id", "election.district.name", "votes", "percentage", "political.interest.code", "candidate.name")
names(result) <- col.names
result <- TidyStruct(result)

result.win <- group_by(result, election.district.id)
result.win <- summarize(result.win, first(election.district.name), first(votes), first(percentage), first(political.interest.code), first(candidate.name))



# ogrListLayers("shape/POLLING_DIVISION.shp")

# read shape
ed.shape <- readOGR("shape/POLLING_DIVISION.shp", layer = "POLLING_DIVISION")
ed.shape$ED_ID <- factor(ed.shape$ED_ID)
# correct projection
proj <- sp::CRS('+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0')
ed.shape <- sp::spTransform(ed.shape, proj)
# union polygons
gpclibPermit()
ed.shape.poly <- unionSpatialPolygons(ed.shape, ed.shape$ED_ID)

leaflet(ed.shape.poly[2]) %>%
    addPolygons()
    
# get data
ed.shape.data <- data.frame(ed.shape)
ed.shape.data <- unique(ed.shape.data[, c(2,5)])
row.names(ed.shape.data) <- as.character(1:dim(ed.shape.data)[1])
# combine data
names(ed.shape.data) <- c("election.district.id", "election.district.name")
ed.shape.data <- right_join(result.win, ed.shape.data, by = "election.district.id")[,-7]
names(ed.shape.data) <- col.names
ed.shape.data <- TidyStruct(ed.shape.data)
# combine data and shape
ed.shape <- SpatialPolygonsDataFrame(ed.shape.poly, ed.shape.data)

pal <- colorFactor(
    palette = c("green", "red", "orange", "blue"),
    domain = ed.shape$political.interest.code
)
leaflet(ed.shape) %>%
    addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = ~pal(political.interest.code), 
              popup = paste("ID", ed.shape$election.district.id, "<br>",
                            "Region:", ed.shape$election.district.name, "<br>",
                            "Party:", ed.shape$political.interest.code, "<br>",
                            "Candidate:", ed.shape$candidate.name, "<br>",
                            "Votes:", ed.shape$votes, "<br>",
                            "Percentage:", ed.shape$percentage),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE)) %>%
    addLegend(position = "topright",
              pal = pal, values = ~political.interest.code,
              title = "Party",
              opacity = 0.5
    )



