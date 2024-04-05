# title: "Jervis Bay Herpetofauna"
# author: Anokhi Saha
# date: "11 July 2023"

# 1. load the required packages

require(tidyverse)
require(dplyr)
require(ggplot2)
require(stringr)
require(readxl)
require(maps)

# 2. Load the data 

setwd("~/Documents/bio/Jervis Bay Herpetofauna")
dt<- read.csv("JervisBay_Herpetofauna_AnS.csv", header = TRUE)
View(dt)

# 3. Data set criteria check

# Abundance is numeric? (TRUE)
is.numeric(dt$Abundance) 

# Are there 2 or more unique years in the dataset? (TRUE)
n_distinct(dt$year) >= 2

# Year, month and day must be integers or factors
# To check this we can split up the date column into year, month, and day
separate(dt, "date", c("Year", "Month", "Day"), sep = "-")
dt$Year <- word(dt$`date`, sep = "-", start=1)
dt$Month <- word(dt$`date`, sep = "-", start=2)
dt$Day <- word(dt$`date`, sep = "-", start=3)
dt$`date` <- NULL

str(dt)
dt$Year <- as.factor(dt$Year)
dt$Month <- as.factor(dt$Month)
dt$Day <- as.factor(dt$Day)
is.factor(dt$Year) | is.integer(dt$Year)    # TRUE
is.factor(dt$Month) | is.integer(dt$Month)  # TRUE
is.factor(dt$Day) | is.integer(dt$Day)      # TRUE

# Taxonomic fields must be characters or factors? (TRUE)
dt$scientific_name <- as.factor(dt$scientific_name)
is.factor(dt$scientific_name) | is.integer(dt$scientific_name)

# 4. Remove any extraneous columns that BioTIME doesn't use
dt$id <- NULL
dt$site_code <- NULL
dt$year <- NULL
dt$season <- NULL
dt$survey_day <- NULL
dt$plot_distance <- NULL
dt$temperature <- NULL
dt$start_time <- NULL
dt$wind <- NULL
dt$soil_moisture <- NULL
dt$cloud <- NULL
dt$external_grass_cover <- NULL
dt$a_herp_visit_table_comments <- NULL
dt$visit_code <- NULL
dt$common_name <- NULL
dt$cover <- NULL
dt$gravid <- NULL
dt$a_herp_data_table_comments <- NULL

# 5. Renaming year column to match ours
colnames(dt)[1] <- 'Taxon'
colnames(dt)[2] <- 'Abundance'

# 6. Primary field check

# Check for blank rows in the table and remove them
dt <- dt[!apply(dt == "", 1, any), ]

# 7. Add the lats and longs and check location on map > Looks right!
dt$Latitude <- as.numeric(rep('-35.154597', nrow(dt)))
dt$Longitude <- as.numeric(rep('150.6799405', nrow(dt)))

world_map <- map_data('world')
world <- ggplot(world_map) + 
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  coord_fixed() + 
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())
points <- world + geom_point(data=dt %>% distinct(Latitude, Longitude, Year), 
                             aes(x=Longitude, y=Latitude, fill = Year), shape=21)
points + coord_fixed(xlim=c(100,175), ylim=c(-40,0))
detach('package:maps')

# 8. Secondary field check

# Make a copy
dt$Species <- dt$Taxon

# Check for family names (none)
str_which(dt$Species, 'idae$|eae$') 

# Visual check and checks of the taxonomy show no errors
sort(unique(dt$Species)) %>% word(., start=2, end=-1)
sort(unique(word(dt$Species, 1)))
sort(unique(word(dt$Species, 2)))

# Separate genus to its own column
dt$Genus <- word(dt$Taxon, 1) 

# Separate species to its own column.
dt$Species <- word(dt$Species, start=2) 

# Now we can remove the taxon column
dt$Taxon <- NULL

# 9. Prepare for export

# Add in the empty column
dt$Biomass <- rep('', nrow(dt))
dt$Plot <- rep('', nrow(dt))
dt$Family <- rep('', nrow(dt))
dt$DepthElevation <- rep('', nrow(dt))
dt$StudyID <- rep('', nrow(dt))

# Aggregate abundance records that are same species, plot, and survey day.
dt_merged <- dt %>% group_by(Biomass, Family, Genus, Species, Plot, Latitude, Longitude, DepthElevation, Day, Month, Year, StudyID) %>% 
  summarise(Abundance=sum(Abundance)) %>% ungroup() %>% arrange(Year, Family, Genus, Species)

dim(dt)[1]-dim(dt_merged)[1] #1200 rows have been agregated

# Save the dataset name as an object so we save some typing
dataset_name <- 'JervisBay_Herpetofauna_AnS'

# Put in as many non-blank fields unique to the sampling event (136 sampling events)
dt_merged$SampleDescription <- as.factor(with(dt_merged, paste(dataset_name, Latitude, Longitude, Year, Month, Day, sep='_')))
length(levels(dt_merged$SampleDescription)) 

# Now we'll reorder the columns to the proper format:
dt_merged <- dt_merged[c('Abundance',
                         'Biomass',
                         'Family',
                         'Genus',
                         'Species',
                         'SampleDescription',
                         'Plot',
                         'Latitude',
                         'Longitude',
                         'DepthElevation',
                         'Day',
                         'Month',
                         'Year',
                         'StudyID')] %>% arrange(Year, Family, Genus, Species)

# final check!
head(dt_merged) 

# 10. Export and spreadsheet prep

# choose a file in the directory you want this saved in
setwd(file.choose() %>% dirname())
write.csv(dt_merged, paste0(getwd(), dataset_name, '_rawdata_AnS.csv'), row.names=F, na='')

# 11. Meta data

# load libraries
library(sf)
library(clipr)
library(sp)
library(rgeos)

# 12. Convert data points into point spatial object
points <- SpatialPoints(cbind(dt$Longitude,dt$Latitude))

# 13. Calculate convex hull, area and centroid
# > Area =  Only one set of co-orindates present
# > lat =-35.1546 / lon = 150.6799

convhull<-gConvexHull(points)
gArea(convhull) 
centroid<-gCentroid(convhull)    
centroid@coords

# 14. We can plot this 
# > looks right!

plot(points,cex=0.5,axes=F,pch=19, xlab="Longitude", ylab="Latitude")
axis(side=2,las=1,cex.axis=0.8, tck=-.01,line=-0.2)
axis(side=1,tck=-.01,labels=NA) 
axis(side=1,lwd = 0, line = -0.6,cex.axis=.8, labels=T) 

# add the convex hull
plot(convhull,add=T)

# add the centroid
points(centroid,col="Blue",pch=19,cex=1.2)

################################# end of code ##################################







