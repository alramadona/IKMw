
# intro -------------------------------------------------------------------
## https://www.paulamoraga.com/tutorial-areal-data/

library(SpatialEpi)
data(pennLC)

?pennLC

head(pennLC$geo, 10)

head(pennLC$data, 10)
head(pennLC$smoking, 10)

pennLC$spatial.polygon

# data preparation
library(dplyr)

d <- group_by(pennLC$data, county) %>% summarize(Y = sum(cases))
head(d)

pennLC$data <- pennLC$data[order(pennLC$data$county, pennLC$data$race, pennLC$data$gender, pennLC$data$age), ]
E <- expected(population = pennLC$data$population, cases = pennLC$data$cases, n.strata = 16)

d$E <- E
head(d)

d <- merge(d, pennLC$smoking, by = "county")

d$SMR <- d$Y/d$E

head(d, 10)

# add data to map
map <- pennLC$spatial.polygon

library(sp)

rownames(d) <- d$county
map <- SpatialPolygonsDataFrame(map, d, match.ID = TRUE)
head(map@data)

library(leaflet)

l <- leaflet(map) %>% addTiles()

pal <- colorNumeric(palette = "YlOrRd", domain = map$SMR)

l %>% addPolygons(color = "grey", weight = 1, fillColor = ~pal(SMR), fillOpacity = 0.5) %>%
  addLegend(pal = pal, values = ~SMR, opacity = 0.5, title = "SMR", position = "bottomright")

## extra

labels <- sprintf("<strong>%s</strong><br/>Observed: %s <br/>Expected: %s <br/>Smokers proportion: %s <br/>SMR: %s",
                  map$county, map$Y,  round(map$E, 2), map$smoking, round(map$SMR, 2)) %>%
  lapply(htmltools::HTML)

l %>% addPolygons(color = "grey", weight = 1, fillColor = ~pal(SMR), fillOpacity = 0.5,
                  highlightOptions = highlightOptions(weight = 4),
                  label = labels,
                  labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                              textsize = "15px", direction = "auto")) %>%
  addLegend(pal = pal, values = ~SMR, opacity = 0.5, title = "SMR", position = "bottomright")


# studi kasus -------------------------------------------------------------

dat <- read.csv("dat/Yk_kasus.csv")
dat$pop <- as.integer((dat$Pop_2016_II + dat$Pop_2017_I)/2)

totPop <- sum(dat$pop)
totKasus <- sum(dat$kasus)

dat$E <- as.integer(dat$pop/totPop * totKasus)

dat$SMR <- dat$kasus/dat$E

#library(maptools)
#mapYk <- readShapePoly("dat/map/yogyakarta-village.shp")

library(rgdal)
mapYk <- readOGR("dat/map/yogyakarta-village.shp", verbose = FALSE)

rownames(dat) <- dat$Idx
mapYk <- SpatialPolygonsDataFrame(mapYk, dat, match.ID = TRUE)
head(mapYk@data,45)

library(leaflet)

l <- leaflet(mapYk) %>% addTiles()

pal <- colorNumeric(palette = "YlOrRd", domain = mapYk$SMR)

l %>% addPolygons(color = "grey", weight = 1, fillColor = ~pal(SMR), fillOpacity = 0.5) %>%
  addLegend(pal = pal, values = ~SMR, opacity = 0.5, title = "SMR", position = "bottomright")

## extra

labels <- sprintf("<strong>%s</strong><br/>Kasus: %s <br/>Expected: %s <br/>Jumlah Penduduk: %s <br/>SMR: %s",
                  mapYk$village, mapYk$kasus, mapYk$E, mapYk$pop, round(mapYk$SMR,2)) %>%
  lapply(htmltools::HTML)

l %>% addPolygons(color = "grey", weight = 1, fillColor = ~pal(SMR), fillOpacity = 0.5,
                  highlightOptions = highlightOptions(weight = 4),
                  label = labels,
                  labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                              textsize = "15px", direction = "auto")) %>%
  addLegend(pal = pal, values = ~SMR, opacity = 0.5, title = "SMR", position = "bottomright")


# points ------------------------------------------------------------------

dat <- read.csv("dat/Yk_points.csv")
dat$n <- c(1:nrow(dat))

# https://rstudio.github.io/leaflet/markers.html
leaflet(data = dat) %>% addTiles() %>%
  addMarkers(~lon, ~lat, popup = ~as.character(keterangan), label = ~as.character(keterangan)) %>% 
  addProviderTiles(providers$CartoDB.Positron)

leaflet(data = dat) %>% addTiles() %>%
  addCircleMarkers(~lon, ~lat, radius = ~n) %>% 
  addProviderTiles(providers$CartoDB.Positron)
