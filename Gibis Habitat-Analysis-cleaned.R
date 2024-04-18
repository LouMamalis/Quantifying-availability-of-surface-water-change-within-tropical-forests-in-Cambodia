#This script was run on the Viking computer at the University of York
#it looks at the number of pixels/area that are within the 75th percentile area distance from nests to surface water

###Loading the required packages### 
library(raster)
library(rgdal)

###Read in data files###
all_dists_old <- stack("data-raw/all_dists_im_1000.tif")

#these are raster files imported from GEE
#these rasters show the distances of all pixels to the nearest surface water for each quarter within the whole time period
#there are 36 tif files WHY!? 
all_dists1 <- stack("all_dists_im-0000000000-0000000000.tif")
all_dists2 <- stack("all_dists_im-0000000000-0000002560.tif")
all_dists3 <- stack("all_dists_im-0000000000-0000005120.tif")
all_dists4 <- stack("all_dists_im-0000000000-0000007680.tif")
all_dists5 <- stack("all_dists_im-0000000000-0000010240.tif")
all_dists6 <- stack("all_dists_im-0000000000-0000012800.tif")
all_dists7 <- stack("all_dists_im-0000002560-0000000000.tif")
all_dists8 <- stack("all_dists_im-0000002560-0000002560.tif")
all_dists9 <- stack("all_dists_im-0000002560-0000005120.tif")
all_dists10 <- stack("all_dists_im-0000002560-0000007680.tif")
all_dists11 <- stack("all_dists_im-0000002560-0000010240.tif")
all_dists12 <- stack("all_dists_im-0000002560-0000012800.tif")
all_dists13 <- stack("all_dists_im-0000005120-0000000000.tif")
all_dists14 <- stack("all_dists_im-0000005120-0000002560.tif")
all_dists15 <- stack("all_dists_im-0000005120-0000005120.tif")
all_dists16 <- stack("all_dists_im-0000005120-0000007680.tif")
all_dists17 <- stack("all_dists_im-0000005120-0000010240.tif")
all_dists18 <- stack("all_dists_im-0000005120-0000012800.tif")
all_dists19 <- stack("all_dists_im-0000007680-0000000000.tif")
all_dists20 <- stack("all_dists_im-0000007680-0000002560.tif")
all_dists21 <- stack("all_dists_im-0000007680-0000005120.tif")
all_dists22 <- stack("all_dists_im-0000007680-0000007680.tif")
all_dists23 <- stack("all_dists_im-0000007680-0000010240.tif")
all_dists24 <- stack("all_dists_im-0000007680-0000012800.tif")
all_dists25 <- stack("all_dists_im-0000010240-0000000000.tif")
all_dists26 <- stack("all_dists_im-0000010240-0000002560.tif")
all_dists27 <- stack("all_dists_im-0000010240-0000005120.tif")
all_dists28 <- stack("all_dists_im-0000010240-0000007680.tif")
all_dists29 <- stack("all_dists_im-0000010240-0000010240.tif")
all_dists30 <- stack("all_dists_im-0000010240-0000012800.tif")
all_dists31 <- stack("all_dists_im-0000012800-0000000000.tif")
all_dists32 <- stack("all_dists_im-0000012800-0000002560.tif")
all_dists33 <- stack("all_dists_im-0000012800-0000005120.tif")
all_dists34 <- stack("all_dists_im-0000012800-0000007680.tif")
all_dists35 <- stack("all_dists_im-0000012800-0000010240.tif")
all_dists36 <- stack("all_dists_im-0000012800-0000012800.tif")

#here we trim any that are outside the study area:
#to do this we read the three PA boundaries:
PA_bound <- readOGR(dsn = 'data-raw', layer = 'PAs_latlon')
PA_bound <- PA_bound[PA_bound$name %in% c("Kulen Promtep Wildlife Sanctuary", "Preah Rokar Wildlife Sanctuary", "Chheb Wildlife Sanctuary"),]
plot(PA_bound)

#try making a smaller list to run through 
#all_dists3 seems to be the problem layer as it just crashes on this layer even when i run it by itself 
all_dists_test <- list("all_dists3")#, "all_dists4", "all_dists5", "all_dists6")
 
alldists_objs <- ls(pattern = "all_dists")  #vector of names of all the distance stacks

#crop(all_dists_test, PA_bound)
#crop(all_dists1, PA_bound)

###Trim them###
#try runs through each raster to check it and try to crop it
#if it doesnt crop because the extents dont match then it will add it to the try-error list and assign it as an NA
#if it does work and it is in the extent then it will call it i
#this will be shown in the environment

for (i in alldists_objs) {
  print(i)
 tmp <- try(crop(get(i), extent(PA_bound), filename=paste0(i, "_c.tif")))
 if(class(tmp) == "try-error") assign(i, NA) else assign(i, tmp)
}

#save this
save.image(file = "AllDists.Rdata") #save it

#re-load it for the next step
#note cannot skip the lines above and just run from here if needed
load("AllDists.Rdata")

###Create a list### 
#of all the indiv tif files, selects those from the previous function that were not NAs and then use these and merge them together 
valid_tiffs <- alldists_objs[sapply(alldists_objs, function(x) object.size(get(x))) > 56]

#collate all the tif files together
all_tifs <- c(all_dists1, all_dists2, all_dists3, all_dists4, all_dists5, all_dists6,
             all_dists7, all_dists8, all_dists9, all_dists10, all_dists11, all_dists12,
             all_dists13, all_dists14, all_dists15, all_dists16, all_dists17, all_dists18,
             all_dists19, all_dists20, all_dists21, all_dists22, all_dists23, all_dists24,
             all_dists25, all_dists26, all_dists27, all_dists28, all_dists29, all_dists30,
             all_dists31, all_dists32, all_dists33, all_dists34, all_dists35, all_dists36)

#
tiff_list <- mget(valid_tiffs)

all_tiffs_m <- tiff_list[[1]]

for(i in 2 : NROW(tiff_list)) {
  print (i)
  all_tiffs_m <- merge(all_tiffs_m, tiff_list[[i]], filename = paste0(valid_tiffs[i], "_m"))
}

#save these merged distances
save.image(file = "AllDists_merged.Rdata")

#re-load it for the next step
#note cannot skip the lines above and just run from here if needed
load("AllDists_merged.Rdata")

###Area calcs###
#here we are selecting the year bands from the surface water 
#years are 2000 to 2020 and each one has 4 which is each quarter per year
year_bands <- rep(2000:2020, each = 4)
  
#we want to extract only the pixels within the gibis range 
#multiply by a raster of the whole range range with values of 1 within the range 
#and NA pixels outside of the range 
ibis_bound <- readOGR(dsn = 'data-raw', layer = 'Thaumatibis_gigantea') #use the giant ibis shapefile
ibis_rast <- rasterize(ibis_bound, raster(all_tiffs_m[[1]])) #check this field section is correct!!
all_tiffs_m <- all_tiffs_m * ibis_rast #all_tiffs_m is a rasterstack of all the distance to surface water calculations 
all_tiffs_m <- stack(all_tiffs_m) #need to convert this from a rasterbrick to a rasterstack again to do the next part

#compute annual min and max
#min is the wet season and the max is the dry season
#min distance to water will be during the wet season as there is more water around
ann_min <- function(x) aggregate(c(x), by = list(year = year_bands), FUN = min, na.rm = TRUE)$x
annual_min_dist <- calc(all_tiffs_m, fun = ann_min) * ibis_rast
ann_max <- function(x) aggregate(c(x), by = list(year = year_bands), FUN = max, na.rm = TRUE)$x
annual_max_dist <- calc(all_tiffs_m, fun = ann_max) * ibis_rast

###Computing the percentiles### 
#looking at the 75 percentile of the distance (m) to surface water for nest and non nest points 
#wet season
quant_nests <- 0
quant_not_nests <- 14.6 

#dry season
quant_ds_nests <- 93
quant_ds_not_nests <- 153

#this section is setting to calculate the number of pixels/area that is within the 75 percentile values for nests and non nests
#using the annual min and max values for each pixel 
within_dry_dist_nests <- annual_max_dist <= quant_ds_nests
within_dry_dist_not_nests <- annual_max_dist <= quant_ds_not_nests
within_wet_dist_nests <- annual_min_dist <= quant_nests
within_wet_dist_not_nests <- annual_min_dist <= quant_not_nests

#compute the sum in each RasterLayer to be then used for plotting
within_dry_annual_extent_not_nests <- cellStats(within_dry_dist_not_nests, 'sum')
within_dry_annual_extent_nests <- cellStats(within_dry_dist_nests, 'sum')
within_wet_annual_extent_not_nests <- cellStats(within_wet_dist_not_nests, 'sum')
within_wet_annual_extent_nests <- cellStats(within_wet_dist_nests, 'sum')

#save 
save.image("FinalResults.Rdata")

