library(rgdal)
library(leaflet)
library(maptools)
library(gpclib)
library(rgeos)

# ogrListLayers("shape/POLLING_DIVISION.shp")

ontario <- readOGR("shape/POLLING_DIVISION.shp", layer = "POLLING_DIVISION")

proj <- sp::CRS('+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0')
ontario <- sp::spTransform(ontario,proj)

gpclibPermit()

ontario.df <- data.frame(ontario)
ontario.df <- unique(ontario.df[, c(2,5)])
row.names(ontario.df) <- as.character(1:dim(ontario.df)[1])

region <- unionSpatialPolygons(ontario, ontario$ED_ID)

# region <- ontario[order(ontario$SHAPE_Area, decreasing = T)[1:10],]
ontario.election <- SpatialPolygonsDataFrame(region, ontario.df)

leaflet(ontario.election) %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = ~colorQuantile("YlOrRd", ED_ID)(ED_ID),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE))


