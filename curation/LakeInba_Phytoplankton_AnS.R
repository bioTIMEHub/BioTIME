# Title:  Phytoplankton species abundance in Lake Inba (Japan) from 1986 to 2016
# Author: Anokhi Saha

# ----------------------------

# INDEX: 
#   I.    Loading the data        Line 15 - 26
#   II.   Criteria checks         Line 28 - 73
#   III.  Adding Additional Info  Line 75 - 124
#   IV.   Errors in data          Line 126 - 165
#   V.    Prepare for export      Line 169 - 212
#   VI.   Metadata                Line 215 - 254

# ----------------------------

# 1. Load the required packages

require(tidyverse)
require(readxl)
require(maps)

# 2. Read in data
setwd("~/Documents/bio/Lake Inba")
dt<- read.csv(file = 'ERDP-2017-04.2.1-Inba_Phyto.csv')
View(dt)

# ----------------------------

# We can use R to check the criteria: 
# - Dataset consists of at least 2 years of sampling (they do not have to be consecutive) = YES
# - Dataset consists of entire assemblages, not just populations = YES
# - Data should record abundance, biomass or both. = YES (abundance)
# - Sampling methods *and effort* are consistent through time. = YES
# - Individuals are identified to species level where possible. = YES

# 3. Check data 

# Are there more than two distinct years? (TRUE)
n_distinct(dt$Year) >= 2

# Make sure none of the rows for Abundance are zero > TRUE
min(dt$Abundance) > 0 

# Then, lets check that the variables are in the correct form
# **Abundance or biomass**: numeric
# **Coordinates**: numeric
# **Dates**: POSIXct or 
# **Year, month, day* columns,**: integers or factors
# **Plot**: factors or integers
# **DepthElevation**: numeric or factors (if they're a treatment category)
# **Taxonomic**: characters or factors

# Check Year, month and day > (TRUE)
is.factor(dt$Year) | is.integer(dt$Year)
is.factor(dt$Month) | is.integer(dt$Month)
is.factor(dt$Day) | is.integer(dt$Day)

# Change 'Cells per ml' to abundance and check is numeric (TRUE)
colnames(dt)[7] = "Abundance"
is.numeric(dt$Abundance)

# Now test Species_name > Originally, said FALSE, now says TRUE
is.factor(dt$Species_name)
dt$Species_name <- as.factor(dt$Species_name)

# Rename "station" to "plot"
colnames(dt)[4] = "Plot"

# Then, we can get rid of any columns we dont need for BioTime 

# 'Class' is not included in the BioTime layout so we can get rid of this column from the data table 
dt = subset(dt, select = -c(5))

# ----------------------------

# 4. Adding the additional information provided by the authors

# Going to start by splitting the data up into the relevant 'stations' (plots)
dt.1 <- subset(dt, Plot=="1")
dt.2 <- subset(dt, Plot=="2")
dt.3 <- subset(dt, Plot=="3")
dt.4 <- subset(dt, Plot=="4")

# To each subset of the data, I am now going to add the latitude, longitude, and correct name of the plot
# For coordinates in latitude and longitude, we work primarily in WGS84

# "Aso-bashi"
dt.1$Latitude <- as.numeric(rep('35.751', nrow(dt.1)))
dt.1$Longitude <- as.numeric(rep('140.145', nrow(dt.1)))
dt.1$Plot<-replace(dt.1$Plot,dt.1$Plot=="1","Aso-bashi")
View(dt.1)

# "Jousuidou-shusuikou-shita"
dt.2$Latitude <- as.numeric(rep('35.734673', nrow(dt.2)))
dt.2$Longitude <- as.numeric(rep('140.1925', nrow(dt.2)))
dt.2$Plot<-replace(dt.2$Plot,dt.2$Plot=="2","Jousuidou-shusuikou-shita")

# "Ipponmatsu-shita"
dt.3$Latitude <- as.numeric(rep('35.766815', nrow(dt.3)))
dt.3$Longitude <- as.numeric(rep('140.209167', nrow(dt.3)))
dt.3$Plot<-replace(dt.3$Plot,dt.3$Plot=="3","Ipponmatsu-shita")

# "Kitainbanuma-chuou"
dt.4$Latitude <- as.numeric(rep('35.80003', nrow(dt.4)))
dt.4$Longitude <- as.numeric(rep('140.251944', nrow(dt.4)))
dt.4$Plot<-replace(dt.4$Plot,dt.4$Plot=="4","Kitainbanuma-chuou")

# We can then merge them back together
rbind(dt.1, dt.2, dt.3, dt.4)
dt <- rbind(dt.1, dt.2, dt.3, dt.4)
View(dt)

# We can also plot them to visually confirm the location. 
# And check that the lats and longs are correctly plotted > looks right! 
world_map <- map_data('world') 
world <- ggplot(world_map) + 
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  coord_fixed() + 
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())
points <- world + geom_point(data=dt, aes(x=Longitude, y=Latitude), shape=21)
points + coord_fixed(xlim=c(135,143), ylim=c(34,37))
detach('package:maps')

# ----------------------------

# 5. Secondary field check: Remove errors and tidy the data 

# split the species_name column into the two words:
dt$Species <- dt$Species_name
dt$Genus <- word(dt$Species, 1)
dt$Species <- word(dt$Species, start=2)

# We can now check for any family names in the 
str_which(dt$Species_name, 'idae$|eae$')
delete <- c(str_which(dt$Species_name, 'idae$|eae$'))
delete
dt_new<- dt[-delete, ]
dt <- dt_new

# Note: also need to do this in capitals as R is case sensitive and the data set has some entries which are
#       capitalized
str_which(dt$Species_name, 'IDEA$|EAE$')
delete.2 <- c(str_which(dt$Species_name, 'IDEA$|EAE$'))
delete.2
dt_new <- dt[-delete.2, ]
dt <- dt_new

# Now we can check for spelling mistakes and errors
sort(unique(word(dt$Species_name, start=2, end=-1)))

# ERROR: Var. indicates it is sampled past the species level, we dont need this
# However, when we split 'species_name' into first and second word, the irrelevant part is automatically removed
# duplex var.gracilimum" 
# falcatus var. mirabilis
# hantzschii var. fluviatile
# italica fo.spiralis
# pulchellum var.minutum
# sp. (cf.tenuis)

# ERROR: (Dolichospermum) > Anabaena (Dolichospermum) spp.
dt$Species <- as.character(dt$Species)
dt$Species[dt$Species == "(Dolichospermum)"] <- "spp."

# We can  now remove "species_name" as a column 
dt = subset(dt, select = -c(5))

# ----------------------------

# 6. Prepare data for export 

# Add in BioTime variables that are currently missing from the corrected datafile
dt$Biomass <- rep('', nrow(dt))
dt$StudyID <- rep('', nrow(dt))
dt$Family <- rep('', nrow(dt))
dt$DepthElevation <- rep('', nrow(dt))

# aggregate abundance records that are same species, plot, and survey day.
dt_merged <- dt %>% group_by(Biomass, Family, Genus, Species, Plot, Latitude, Longitude, DepthElevation, Day, Month, Year, StudyID) %>% 
  summarise(Abundance=sum(Abundance)) %>% ungroup() %>% arrange(Year, Family, Genus, Species)

dim(dt)[1]-dim(dt_merged)[1]      # 96 rows were aggregated 

# save the dataset name as an object so we save some typing
dataset_name <- 'LakeInba_Phytoplankton'

# put in as many non-blank fields unique to the sampling event
dt_merged$SampleDescription <- as.factor(with(dt_merged, paste(dataset_name, Latitude, Longitude, Day, Month, Year, sep='_')))

length(levels(dt_merged$SampleDescription))

# then we put it in the correct formatting 
dt_merged <- dt_merged[c('Abundance',
                         'Biomass',
                         'Family',
                         'Genus',
                         'Species',
                         'Plot',
                         'Latitude',
                         'Longitude',
                         'DepthElevation',
                         'Day',
                         'Month',
                         'Year',
                         'StudyID')] %>% arrange(Year, Family, Genus, Species)
head(dt_merged)

# the raw data is now ready to be exported 
# choose a file in the directory you want this saved in
setwd(dirname("Documents"))
write.csv(dt_merged, paste0(getwd(), dataset_name, '_rawdata_AS.csv'), row.names=F, na='')

# ----------------------------

# 7. Adding the meta-data 

## we then have to add the meta data, which can either be found in the data, or in the paper 
## sometimes we may have to use R for the central coordinates (`CentralLatitude, CentralLongitude`) 
## and sampling area. For datasets grained to one location, these fields should already be provided. 
## For datasets where authors provide us with multiple locations, we use R spatial analysis to calculate 
## the central point and area. This can be easily done using a few lines of code to create a convex hull 
## with the multiple locations.

# a. load libraries
library(sf)
library(clipr)
library(sp)
library(rgeos)

# b. Convert data points into point spatial object
points <- SpatialPoints(cbind(dt$Longitude,dt$Latitude))

# c. Calculate convex hull, area and centroid

convhull<-gConvexHull(points)
gArea(convhull) 
centroid<-gCentroid(convhull)    
centroid@coords

# d. We can plot this 
# > looks right!

plot(points,cex=0.5,axes=F,pch=19, xlab="Longitude", ylab="Latitude")
axis(side=2,las=1,cex.axis=0.8, tck=-.01,line=-0.2)
axis(side=1,tck=-.01,labels=NA) 
axis(side=1,lwd = 0, line = -0.6,cex.axis=.8, labels=T) 

# e. Add the convex hull
plot(convhull,add=T)

# f. Add the centroid
points(centroid,col="Red",pch=19,cex=1.2)

################################# end of code ##################################




