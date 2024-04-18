#this script organises and processes the different data sources that are needed to input into the INLA model 
#this data is for each of the covariates that we are going to consider in our models 
#this script creates a covariate stack to use in Model 1 which considers general changes to surface water 

###Load the required packages###
library(raster)
library(tidyverse)
library(rgdal)
library(ggplot2)
library(rasterVis)
library(maptools)
library(rgeos)

###Read in the transition maps####
#load the transition map which compares the surface water changes between the two five year periods
#resolution of this is 500m
load('transition_map_500R.Rdata')

#upload the map that shows the initial state of the surface water at 500m resolution 
load('initial_sw_500R.Rdata')

###Considering the impact of elevation###
elevation <- raster("data-raw/elevation_map_500.tif")

elevation <- readOGR(dsn = getwd(), 
                    layer = "elevation_map_500")

#crop this layer to the same as the transition_map
ext <- extent(c(103.6, 107.7, 12.50122, 14.8))
elevation <- crop(elevation, ext)

#set the values that actually map the raster
ext <- extent(elevation)
ext

plot(elevation)
save(elevation, file = 'elevation.Rdata')

#reload to use for the INLA model 
load('elevation.Rdata')

###load the gibis estimated distribution shapefile###
gibis <- readOGR(dsn = getwd(), 
                   layer = "Thaumatibis_gigantea")
plot(gibis)

###load the protected areas###
#protected areas across Cambodia - converted to lat lon in QGIS
PAs <- readOGR(dsn = "data-raw", 
               layer = "PAs_latlon")

plot(PAs)

#save the file
save(PAs, file = 'PAs.Rdata')

#reload to use for the inla model 
load("PAs.Rdata")

#selecting the PAs that are within the g ibis range
PAs <- PAs[gibis,]

#check the projection - lat and long
proj4string(PAs)

#rasterising protected areas shapefile 
#making areas that are pa's 1's and those that are not 0's...? 
PAs_rast <- rasterize(PAs, transition_copy)
PAs_rast[!(is.na(PAs_rast))] <- 1
PAs_rast[(is.na(PAs_rast))] <- 0

#making the PAs layer numeric for the staack 
values(PAs_rast) <- as.numeric(values(PAs_rast))

###Considering presence of roads###
#shapefile of all the roads across Cambodia 
roads_2020 <- readOGR(dsn = getwd(), 
                      layer = "osm_road_2020")

#selecting the roads within the gibis range
roads_2020 <- roads_2020[gibis,]
plot(roads_2020)

#check the projection - lat and long
proj4string(roads)
proj4string(roads_2020)

#convert the projection of roads to latlon to match the transition maps layer
roads_latlon <- spTransform(roads_2020, proj4string(transition_map))

#load saved data
load('roads_2020_edit.Rdata')

#rasterise the road shapefile 
#NOTE this process takes a long time 
roads_rast <- rasterize(roads_2020, transition_copy)
roads_rast[!(is.na(roads_rast))] <- 1

#compute distance from transition map raster to the road raster
#distance to object in question
#(use rgeos packages and gDistance)
roads_dist <- distance(roads_rast)

###Considering influence of economic land concessions###
#mapped economic land concessions in Cambodia - converted to lat lon 

ELCs <- readOGR(dsn = getwd(), 
              layer = "ELC_latlon")

plot(ELCs)

#save
save(ELCs, file = 'ELCs.Rdata')

#then re-load and use for the inla model
load('ELCs.Rdata')

#cropping the ELCs shapefile to the gibis range
ELCs <- ELCs[gibis,]
plot(ELCs)

#check the projection - lat and lons
proj4string(ELCs)

#rasterising ELCs shapefile 
ELCs_rast <- rasterize(ELCs, transition_copy)
ELCs_rast[!(is.na(ELCs_rast))] <- 1
ELCs_rast[(is.na(ELCs_rast))] <- 0

#makes the ELCs layer numeric so it can be stacked
values(ELCs_rast) <- as.numeric(values(ELCs_rast))

###Creating a raster stack of the covariates###
#create a raster stack of all the components to then plug into the INLA
cov_stack <- stack(transition_copy, initial_sw1a, elevation, PAs_rast, ELCs_rast, roads_dist)

#test to see what is happening!!
print(cov_stack)

#saving the covariate stack as a tif file 
writeRaster(cov_stack, filename= 'covariates_stack_bino.tif')


