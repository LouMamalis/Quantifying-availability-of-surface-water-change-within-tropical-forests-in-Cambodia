#In this script we are reading in the exports from the google earth engine analysis 
#which looks at the changes in surface water across the study area
#We are creating plots to show the changes in surface water over time and the relationship with annual precipitation
#we also completed stats tests to look at the relationships between the surface water and preciptation

###Loading the required packages### 
library(ggplot2)
library(dplyr)
library(magrittr)
library(tidyverse)
library(tidyr)

###Read in data files###
#average surface water area per year from google earth engine data
all_sw <- read.csv("data-raw/exportsw_sum.csv")

#average surface water area per year for the dry season from google earth engine data
ds_sw <- read.csv("data-raw/exportdsw_sum.csv")

#data for the average precip per year from CHIRPS data set in GEE
annual_precip <- read.csv("data-raw/year_precip.csv")

###Prepping the data### 
#adding a new year column to surface water df
all_sw$year <- c(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012,
                 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020)

#check 
print(all_sw) 

##joining the surface water and precip data together 
#join on column 'year'
all_data <- annual_precip %>%
  dplyr::left_join(all_sw)

#then joining the surface water dry season dataframe 
#join on column 'year'
all_data <- all_data %>%
  dplyr::left_join(ds_sw)

###Prelim plotting###
#scatter plot of relationship - precip and annual surface water
#added a trendline - no strong pattern
ggplot(all_data, aes(x = mean_annual_precip, y = pixel_sum)) +
  geom_point() +
  geom_smooth(method=lm)

#scatter plot of surface water change over time
#added a trendline - decline 
ggplot(all_data, aes(x = year, y = pixel_sum)) +
  geom_point() +
  geom_smooth(method=lm)

#scatter plot of average precip over time 
#added a trendline - decline
ggplot(all_data, aes(x = year, y = mean_annual_precip)) +
  geom_point() +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Mean annual precipitation (mm)") +
  geom_smooth(method=lm)

ggsave("mean_annual_precip_trendline_plot.jpg")

#line plot of change in surface water (number of flooded pixels) over time
all_data  %>%
  ggplot(aes(x = year, y = pixel_sum)) +
  geom_line(colour = "blue") +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Total # pixels") +
  theme(panel.background = element_blank()) +
  theme_classic()

#changing the units from number of pixels to km2 (# of pixels is not a really helpful unit)
sw_km2 <- all_data$pixel_sum*0.0009

#create a vector of annual surface water in km2 (2000-2020)
surface_water_km2 <- c(30960, 31140, 36360, 34020, 37350, 37890, 
                       32580, 34380, 33390, 35370, 29970, 28260, 
                       28530, 31320, 30240, 29340, 30150, 28440, 
                       29520, 27630, 29700)

#convert pixels to km2 for dry season surface water values as well
ds_sw_km2 <- ds_sw$pixelsum_ds*0.0009

#create a vector of annual dry season surface water in km2 (2000-2020)
ds_surface_water_km2 <- c(2316.701, 2709.941, 4060.129, 3505.024, 
                          4881.497, 3341.849, 2419.121, 3289.057, 
                          1397.293, 3493.639, 1253.212, 1317.857, 
                          1535.843, 2334.303, 1896.858, 2378.697, 
                          2784.012, 2046.715, 2816.830, 2444.325, 
                          2927.953)

#use these vectors to create new columns in the dataframe
all_data$sw_km2 <- surface_water_km2 #total
all_data$ds_sw_km2 <- ds_surface_water_km2 #dry season

#plot the surface water area in km2 over time 
all_data  %>%
  ggplot(aes(x = year, y = sw_km2)) +
  geom_line(colour = "blue") +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Total surface water (km2)") +
  theme(panel.background = element_blank()) +
  theme_classic()

#plotting the average annual precipitation over time and surface water area change
precip_plot <- all_data  %>%
  ggplot(aes(x = year, y = mean_annual_precip)) +
  geom_line(colour = "black") +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Mean precipitaion (mm)") +
  theme(panel.background = element_blank()) +
  theme_classic()+
  geom_smooth(method=lm, se=FALSE, col='blue', linewidth=1)

#view
precip_plot

###Compilation plots###
#creating a graph with 2 y axes to plot the surface water AND precipitation over time 

#Value used to transform the data
coeff <- 10

#plot both surface water change and precipitation
#not an effective plot 
ggplot(all_data, aes(x=year)) +
  geom_line( aes(y=sw_km2), colour = "dark blue", size = 1) + 
  geom_line( aes(y=mean_annual_precip*coeff), colour = "red", linewidth = 1) + #times this by the coeff
  scale_y_continuous (name = "Total surface water (km2)", sec.axis = sec_axis(trans = ~./coeff, name = "Total precipitation (mm)"))

#plotting the annual surface water with the precip and the annual dry season surface water
#this creates a bar plot of the surface water and a line plot for precip with 2 y-axes 
fig1 <- ggplot(data = all_data, mapping = aes(x = year, y = sw_km2)) + 
  geom_bar(stat = "identity", width = 0.5, aes(fill="Surface Water (km^2)")) + 
  geom_line(mapping = aes(y = mean_annual_precip*coeff, color="Precipitation (mm)"), size=1) +
  scale_y_continuous("Surface Water (km^2)", sec.axis = sec_axis(~ ./coeff, name = "Precipitation (mm)")) +
  scale_colour_manual(values = c("blue")) +
  scale_fill_manual(values = c("light blue")) +
  theme_classic() +
  theme(legend.title = element_blank())

#view
fig1

###Final plot###
#data prep - first we create a new dataframe
all_data_new <- data.frame(year = all_data$year,
                           sw_km2 = all_data$sw_km2,
                           sw_km2_ds = all_data$ds_sw_km2,
                           precip = all_data$mean_annual_precip)

#pivot longer for correct format for plotting data 
all_data_new_long <- all_data_new %>% 
  tidyr::pivot_longer(
    !year, names_to = "precip_var", values_to = "mm"
    
  ) %>% filter(precip_var != "precip")

#create the coeff so we can have two y axes for the plot
coeff = 10

#plot - annual surface water area, annual surface water area (dry season) and annual precipitation 
ggplot() + 
  geom_bar(data = all_data_new_long, mapping = aes(x=year, y=mm, fill = precip_var), 
           position="dodge", stat="identity") + #this makes the bar plot with the long data 
  scale_fill_manual(values = c("sw_km2" = "#00868B", "sw_km2_ds"= "#ce7e00"), #this sets the colour of the bars according to the precip_var label 
                    labels=c('Annual surface water', 'Dry season surface water')) + #adds the labels to the legend 
  geom_smooth(data = all_data_new_long, mapping = aes(x=year, y=mm, color = precip_var), 
              method=lm, se=TRUE, size=0.5) +
  geom_line(data = all_data_new, mapping = aes(x=year, y = precip*coeff, colour = "Annual precipitation (mm)"), #add the precip line plot
            size = 0.5) + 
  scale_y_continuous(sec.axis = sec_axis(~ ./coeff, name = "Annual precipitation (mm)"),) + #adds second y axis and the label 
  scale_color_manual(values=c("#003044", "#00868B", "#ce7e00"), name ="", #this assigns the colours for the trendlines and suppresses the legend name
                     labels=c("Annual precipitation", "Wet season", "Dry season")) + #this gives the legend names
  geom_smooth(data = all_data_new, mapping = aes(x=year, y=precip*coeff, color = precip), #multiply by the coeff so the line is in the right place 
              colour = "#003044", method=lm, se=TRUE, size=0.5) + #sets the colour of the trendline
  labs(fill = ("")) + #removed the legend title for the sw legend 
  xlab("Year") + #x axis label
  ylab(bquote("Area of surface water " (km^2))) + #y axis label with km2 properly written
  theme_bw() + #remove the grey background
  theme_classic()  +#removes the grid lines on the plot
  guides(fill = "none") + #suppresses the legend for the scale_fill_manual section 
  theme(legend.position = "bottom", #move legend to bottom
        text=element_text(size=10)) #change font size of ALL text in plot 

#save
ggsave("precip_sw.png", width = 10, height = 8, units = "cm")

####Stats tests####
##Shapiro-wilks test
#testing if the data is normally distributed
#precip data 
shapiro.test(all_data$mean_annual_precip) #--> 0.05709

#annual sw
shapiro.test(all_data$sw_km2) #--> 0.08707

#dry season sw
shapiro.test(all_data$ds_sw_km2) #-->0.5614

##precipitation 
#linear regression to look at precip interactions with time 
summary(lm(data = all_data_new, precip ~ year))$coefficients

##annual surface water area
#linear regression to look at surface water area interactions with time 
summary(lm(data = all_data_new, sw_km2 ~ year))$coefficients

#linear regression to look at surface water area and precipitation
summary(lm(data = all_data_new, sw_km2 ~ precip))$coefficients

##dry season annual surface water area 
#linear regression to look at dry season surface water area interactions with time 
summary(lm(data = all_data_new, sw_km2_ds ~ year))$coefficients

#linear regression to look at dry season surface water area and precipitation
summary(lm(data = all_data_new, sw_km2_ds ~ precip))$coefficients

##Pearsons correlation tests
#precipitation and surface water area
cor.test(all_data_new$precip, all_data_new$sw_km2, method = "pearson")

#precipitation and dry season surface water area
cor.test(all_data_new$precip, all_data_new$sw_km2_ds, method = "pearson")

###Summary calculations###
#filtering the data to only include the dry season (period column) from all the data for 2000-2005
#then summing it
#then working out the mean for all years
early <- all_data2 %>% filter(period == 'ds', year < 2006)
sum(early$sw_km2) #20815.14
mean(early$sw_km2) #3469.19

#filtering the data to only include the dry season (period column) from all the data for 2015-2020
#then summing it
#then working out the mean for all years
later <- all_data2 %>% filter(period == 'ds', year > 2014)
sum(later$sw_km2) #15398.53
mean(later$sw_km2) #2566.422

#working out the differences
sum(early$sw_km2) - sum(later$sw_km2) #5416.609
mean(early$sw_km2) - mean(later$sw_km2) #902.7682

#% difference
(mean(early$sw_km2) - mean(later$sw_km2))/mean(early$sw_km2)*100 #26.02245%
(mean(early$sw_km2) - mean(later$sw_km2))/mean(later$sw_km2)*100 #35.17614%
