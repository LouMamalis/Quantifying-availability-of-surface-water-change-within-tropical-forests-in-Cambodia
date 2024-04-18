#This script plots the loss of the prime giant ibis habitat over time due to the loss of surface water
#it follows on from the colab script script gi-ibis_WaterDist 
#and from the prime_giibis_habitat_viking.R script

###Loading the required packages### 
library(tidyverse)

###Read in data files###
#these data files are from the colab script and from the final results 
load("R:/rsrch/cb751/phd/lcm566/Giant Ibis projeto/Changes to surface water/Surface water/surface_water/outputs/AllDists.Rdata")
load("R:/rsrch/cb751/phd/lcm566/Giant Ibis projeto/Changes to surface water/Surface water/surface_water/outputs/AllDists_merged.Rdata")

#this data is loaded from the Gibis Habitat-Analysis-cleaned.R which was run on viking 
load("R:/rsrch/cb751/phd/lcm566/Giant Ibis projeto/Changes to surface water/Surface water/surface_water/outputs/FinalResults.Rdata")

###Data organisation###
#created a season column for wet and dry and selected the years from the first and second 5 year time periods for the analysis
#converted it to km2
results.long <- data.frame(season = rep(c("wet", "dry"), each = 10),
                           time_period = rep(c("1st", "2nd"), each = 5),
                           year = c(2000:2004, 2016:2020),
                           area = c(within_wet_annual_extent_nests[c(1:5, 17:21)],
                           within_dry_annual_extent_nests[c(1:5, 17:21)])/ ((1000/30)^2))

#combined the two columns of data for season and time period 
results.long$group <- paste(results.long$season, results.long$time_period, sep = "_")

###Plotting###
#need these for the facet wrap function in the geom_bar
plot_names <- c('1st' = "2000-2004",
                '2nd' = "2016-2020")

#making plot
ggplot(results.long, aes(x=time_period, y=area, fill=season)) +
  geom_boxplot() +
  xlab("Time period") + #x axis label
  ylab(bquote("Area of suitable giant ibis nesting habitat  " (km^2))) +
  scale_x_discrete(labels=c("2000-2004", "2016-2020")) +
  theme(axis.title.x = element_text(size=12, face="bold"),
  axis.title.y = element_text(size=12, face="bold"))  + #change the x axis labels 
  scale_fill_manual(values=c("#ce7e00", "#00868B"), #dry and wet season colours 
                  labels=c('Dry', 'Wet'))+
  theme_classic()
  
#save 
ggsave("figs/gibis_habitat_plot.png", width = 16, height = 14, units = "cm")

