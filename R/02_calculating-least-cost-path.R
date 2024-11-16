## script that reads in connectivity layer and calculates least cost path between populations 
library(tidyverse)
library(terra)
library(tidyterra)

## get all connectivity layers
folders <- list.files("data-raw/connectivity-layers/final_output_LEpaper/")[2:13]

## read in each connectivity layer and average 
paths = paste("data-raw/connectivity-layers/final_output_LEpaper/", folders, "/normalized_cum_currmap.tif", sep  ="")

con <- rast(paths[12])
## reproject coordinate system
con <- project(con, "EPSG:4326")

for(i in 1:(length(paths)-1)) {
  ## read and average 
  rast_temp = rast(paths[i])
  rast_temp <- project(rast_temp, "EPSG:4326")
  rast_temp <- resample(rast_temp, con)
  con <- sum(con, rast_temp)
  print(i)
}

## save composite
#writeRaster(con, "data-processed/connectivity-composite-norm-sum-cum-currmap.tif", overwrite=TRUE)
con <- rast("data-processed/connectivity-composite-norm-sum-cum-currmap.tif")

## plot the normalized current flow 
ggplot() +
  geom_spatraster(data = con, aes(fill = normalized_cum_currmap)) +
  coord_sf() +
  scale_x_continuous(limits = c(-180, -50)) +
  scale_y_continuous(limits = c(10, 90)) +
  scale_fill_binned(type = 'viridis',
                    na.value = "transparent", 
                    breaks = c(0, seq(from = 1, to = 12, by = 1))) +
  labs(fill = "Normalized cumulative current flow")


## calculate the cost along the least cost path between each population 
library(gdistance)

## read in population coordinates
coords <- read.csv("data-processed/pop-distance-key.csv")

## create a transition object from the raster of flow 
## high flow = low cost, so take - of flow value
library(raster)

## convert to raster
r = raster(con)

## aggregate to make computation faster 
r <- aggregate(r, 4, fun = mean)
plot(r)

## make transition matrix
matrix <- transition(r, 
                 function(x) (1/x), 
                 directions = 8)

## correct for curvature of the earth 
matrix <- geoCorrection(matrix, type = "c", multpl = FALSE)

costdist <- c()
i = 1
while(i <= nrow(coords)) {
  dist = costDistance(matrix, 
                      fromCoords = c(coords$Longitude1[i], coords$Latitude1[i]), 
                      toCoords = c(coords$Longitude2[i], coords$Latitude2[i]))
  costdist <- append(costdist, dist)
  
  print(paste0("done ", i))
  i = i + 1
}

##save
write_rds(costdist, 'data-processed/costdist.rds')

## join to pop pair object 
coords$cost = costdist

## how many are Inf? (e.g. pops were on different land masses )
length(which(is.infinite(costdist))) # 123 / 7304

## filter them out 
coords <- coords[!is.infinite(coords$cost),]
  
nrow(coords) # 7181

## check: is cost higher for longer distances?
coords %>%
  ggplot(aes(x = dist, y = cost, colour = Binomial)) +
  geom_point() +
  theme(legend.position = "none")
## yes - but with variation
## good sign 

lpi <- read.csv("data-processed/lpi-subset.csv")
lpi <- filter(lpi, pop_id %in% c(coords$pop_1, coords$pop_2))
lpi_long <- read.csv("data-processed/pop-distance-key-with-cost.csv")
lpi_long <- filter(lpi_long, pop_1 %in% c(coords$pop_1) & pop_2 %in% c(coords$pop_2))

## plot on map - colour lines by cost 
ggplot(na, aes(x=long, y=lat, group = group)) + theme_minimal() + 
  geom_polygon(fill = "grey") + 
  coord_fixed() +
  labs(y = "Latitude", x = "Longitude") +
  geom_point(data = lpi, 
             aes(x = Longitude, y = Latitude),
             inherit.aes = FALSE, 
             size = 0.5) +
  theme(legend.position = "none") +
  scale_x_continuous(limits = c(-180, -50)) +
  geom_segment(data = coords,
               aes(x = Longitude1, y = Latitude1, xend = Longitude2, yend = Latitude2, colour = cost), 
               inherit.aes = FALSE, linewidth = .05)+
  scale_color_continuous()

## save new objects 
write.csv(lpi, "data-processed/lpi-subset-with-cost.csv", row.names = FALSE)
write.csv(coords, "data-processed/pop-distance-key-with-cost.csv", row.names = FALSE)
write.csv(lpi_long, "data-processed/lpi-subset-long-with-cost.csv", row.names = FALSE)

length(unique(lpi$Binomial))
length(unique(coords$Binomial))
length(unique(lpi_long$Binomial))
## next: 
## measure synchrony in population time series 


## test model with distance
## and then compete against model with distance + cost accumulated/distance 



  