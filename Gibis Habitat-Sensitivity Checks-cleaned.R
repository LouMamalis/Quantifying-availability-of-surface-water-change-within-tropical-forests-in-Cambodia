#This script is for the completion of the sensitivity testing for the gibis habitat range selection
#We chose the 75th percentile to define the area that we considered as suitable habitat for the giant ibis
#So we wanted to compare this with the 65th and 85th percentiles to show that they had similar results as the 75th so it was not a arbitrary choice 
#this data has come from the colab script gi-ibis_WaterDist
#the results calculated in this script are included in the supplementary information along with the paper
#it follows on from the Gibis Habitat-Analysis-cleaned.R which calculates the 65th percentile results seperately 

###Loading the required packages### 
library(raster)
library(rgdal)

###Read in data files###
#these are mostly raster files which look at the location of nest and non-nest points to areas of surface water 
load("R:/rsrch/cb751/phd/lcm566/Giant Ibis projeto/Changes to surface water/Surface water/surface_water/outputs/FinalResults_sensitivity.Rdata")

###Distances to nest and non-nest points###
#defining the distance to surface water for each percentile in each part of the year
#looking at distance to surface water of giant ibis nest points for the different percentiles 
#these calcs were done in the gi-ibis_WaterDist colab script:
#25% = 0m 50% = 0m 65% = 0m 75% = 0m 85% = 15m
#quant_nests_75 <- 0
quant_nests_65 <- 0
quant_nests_85 <- 15

#looking at distance to surface water of automatically generated non-nest points for the different percentiles 
#these calcs were done in the gi-ibis_WaterDist colab script:
#25% = 0m 50% = 0m 65% = 0m 75% = 14.6m 85% = 15m
#quant_not_nests_75 <- 14.6 
quant_not_nests_65 <- 0 
quant_not_nests_85 <- 15 

#dry season
#looking at distance to surface water of giant ibis nest points for the different percentiles in the dry season
#these calcs were done in the gi-ibis_WaterDist colab script:
#25% = 5m 50% = 50m 65% = 75m 75% = 93m 85% = 139m
#quant_ds_nests_75 <- 93
quant_ds_nests_65 <- 75
quant_ds_nests_85 <- 139

#looking at distance to surface water of automatically generated non-nest points for the different percentiles in the dry season
#these calcs were done in the gi-ibis_WaterDist colab script:
#25% = 21m 50% = 75m 65% = 111m 75% = 153m 85% = 217m
#quant_ds_not_nests_75 <- 153
quant_ds_not_nests_65 <- 111
quant_ds_not_nests_85 <- 217

###Area calcs###
#here we are selecting the year bands from the surface water 
#years are 2000 to 2020 and each one has 4 which is each quarter per year
year_bands <- rep(2000:2020, each = 4)

#we want to extract only the pixels within the gibis range 
#multiply by a raster of the whole range range with values of 1 within the range 
#and NA pixels outside of the range 
ibis_bound <- readOGR(dsn = 'data-raw', layer = 'Thaumatibis_gigantea') #read in the ibis range shapefile
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
#comparing the 65, 75 and 85th percentiles of the distance to water of the nest points 
#this section is setting to calculate the area that is within the percentile values for nests and non nest points
#using this <= for smaller or equal to, to capture all the pixels within this distance 
#using the annual min and max values for each pixel calculated above 
#65th percentile
within_dry_dist_nests_65 <- annual_max_dist <= quant_ds_nests_65
within_dry_dist_not_nests_65 <- annual_max_dist <= quant_ds_not_nests_65
within_wet_dist_nests_65 <- annual_min_dist <= quant_nests_65
within_wet_dist_not_nests_65 <- annual_min_dist <= quant_not_nests_65

#65th percentile 
within_dry_annual_extent_not_nests_65 <- cellStats(within_dry_dist_not_nests_65, 'sum')
within_dry_annual_extent_nests_65 <- cellStats(within_dry_dist_nests_65, 'sum')
within_wet_annual_extent_not_nests_65 <- cellStats(within_wet_dist_not_nests_65, 'sum')
within_wet_annual_extent_nests_65 <- cellStats(within_wet_dist_nests_65, 'sum')

#85th percentile
within_dry_dist_nests_85 <- annual_max_dist <= quant_ds_nests_85
within_dry_dist_not_nests_85 <- annual_max_dist <= quant_ds_not_nests_85
within_wet_dist_nests_85 <- annual_min_dist <= quant_nests_85
within_wet_dist_not_nests_85 <- annual_min_dist <= quant_not_nests_85

#85th percentile 
within_dry_annual_extent_not_nests_85 <- cellStats(within_dry_dist_not_nests_85, 'sum')
within_dry_annual_extent_nests_85 <- cellStats(within_dry_dist_nests_85, 'sum')
within_wet_annual_extent_not_nests_85 <- cellStats(within_wet_dist_not_nests_85, 'sum')
within_wet_annual_extent_nests_85 <- cellStats(within_wet_dist_nests_85, 'sum')

#save
save.image("FinalResults_sensitivity.Rdata")

#to get the actual values run these lines
#65th percentile
within_dry_annual_extent_nests_65
within_wet_annual_extent_nests_65

#85th percentile
within_dry_annual_extent_nests_85
within_wet_annual_extent_nests_85

#getting the median values from the plotting_prime_habitat_area.r script 
#re-jigging the data so we can plot and look at it and plot and then get the median 
#65th percentile 
results_long_65 <- data.frame(season = rep(c("wet", "dry"), each = 10),
                           time_period = rep(c("1st", "2nd"), each = 5),
                           year = c(2000:2004, 2016:2020),
                           area = c(within_wet_annual_extent_nests_65[c(1:5, 17:21)],
                                    within_dry_annual_extent_nests_65[c(1:5, 17:21)])/ ((1000/30)^2))

#85th percentile 
results_long_85 <- data.frame(season = rep(c("wet", "dry"), each = 10),
                           time_period = rep(c("1st", "2nd"), each = 5),
                           year = c(2000:2004, 2016:2020),
                           area = c(within_wet_annual_extent_nests_85[c(1:5, 17:21)],
                                    within_dry_annual_extent_nests_85[c(1:5, 17:21)])/ ((1000/30)^2))

#for ease have combined the two columns of data for season and time period 
results_long_65$group <- paste(results_long_65$season, results_long_65$time_period, sep = "_")
results_long_85$group <- paste(results_long_85$season, results_long_85$time_period, sep = "_")

#then plot it!
#here we have a basic plot to show the changes
#make this fancier and draw out the medians for comparison! 
boxplot(area ~  season + time_period, data = results_long_65)
boxplot(area ~  season + time_period, data = results_long_85)

#medians that have been plotted on the graph
#have subset the data based on the time periods using the []
#have then converted it to km2
#65th percentile 
dry_nests_first_65 <- within_dry_annual_extent_nests_65[c(1:5)]/ ((1000/30)^2)
wet_nests_first_65 <- within_wet_annual_extent_nests_65[c(1:5)]/ ((1000/30)^2)
dry_nests_second_65 <- within_dry_annual_extent_nests_65[c(17:21)]/ ((1000/30)^2)
wet_nests_second_65 <- within_wet_annual_extent_nests_65[c(17:21)]/ ((1000/30)^2)

#looking at the medians for comparison
median(dry_nests_first_65)
median(wet_nests_first_65)
median(dry_nests_second_65)
median(wet_nests_second_65)

#same again for 85th percentile 
dry_nests_first_85 <- within_dry_annual_extent_nests_85[c(1:5)]/ ((1000/30)^2)
wet_nests_first_85 <- within_wet_annual_extent_nests_85[c(1:5)]/ ((1000/30)^2)
dry_nests_second_85 <- within_dry_annual_extent_nests_85[c(17:21)]/ ((1000/30)^2)
wet_nests_second_85 <- within_wet_annual_extent_nests_85[c(17:21)]/ ((1000/30)^2)

#looking at the medians for comparison
median(dry_nests_first_85)
median(wet_nests_first_85)
median(dry_nests_second_85)
median(wet_nests_second_85)

