# Title:  Benthic invertebrates at three sites at River Salaca - Latvia
# Author: Anokhi Saha

# ----------------------------
# INDEX: 
#   I.    Loading the data      Line 12 - 72
#   II.   Criteria checks       Line 74 - 130
#   III.  Errors in the data    Line 133 - 175 
#   IV.   Prepare for export    Line 177 - 220
#   V.    Metadata              Line 223 - 264

# ----------------------------

# 1. Load the required packages
require(tidyverse)
require(readxl)
require(maps)

# 2. Read in data and check it 
setwd("~/Documents/bio/Latvia Data Set")

# Sheet one: Site = Vecate
dt.1 <- read_excel("S091-S093.xlsx", sheet=1, skip=2, col_names=T, na='')
View(dt.1)

# Sheet two: Site = Mazsalaca
dt.2 <- read_excel("S091-S093.xlsx", sheet=2, skip=2, col_names=T, na='')
View(dt.2)

# Sheet three: Site = Vecsalaca
dt.3 <- read_excel("S091-S093.xlsx", sheet=3, skip=2, col_names=T, na='')
View(dt.3)

# 3. Add in the data for the longitude, latitude, and altitude, before combining the sheets

# They have not given the lats and longs in the correct format (-180 to 180) 
#         > I need to figure out what form they are in and then use a converter
#         > For coordinates in latitude and longitude, we work primarily in WGS84
#         > I used an online converter called epsg.io to covert the co-ordinates 
#           between ESRI:102440 LKS 1992 Latvia to EPSG:4326 WGS 84

# Site 1 
dt.1$Latitude <- as.numeric(rep('57.795037', nrow(dt.1))) 
dt.1$Longitude <- as.numeric(rep('22.148004', nrow(dt.1)))
dt.1$DepthElevation <- as.numeric(rep('43', nrow(dt.1)))

# Site 2 
dt.2$Latitude <- as.numeric(rep('57.8740017', nrow(dt.2))) 
dt.2$Longitude <- as.numeric(rep('21.9964678', nrow(dt.2)))
dt.2$DepthElevation <- as.numeric(rep('58', nrow(dt.2)))

# Site 3 
dt.3$Latitude <- as.numeric(rep('57.7530955', nrow(dt.3))) 
dt.3$Longitude <- as.numeric(rep('21.4094509', nrow(dt.3)))
dt.3$DepthElevation <- as.numeric(rep('7', nrow(dt.3)))

# 4. Bind the sheets back together 
rbind(dt.1, dt.2, dt.3)
dt <- rbind(dt.1, dt.2, dt.3)
View(dt)

# 5. Check where the co-ordinates are on the map
world_map <- map_data('world') 
world <- ggplot(world_map) + 
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  coord_fixed() + 
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())
points <- world + geom_point(data=dt, aes(x=Longitude, y=Latitude), shape=21)
points + coord_fixed(xlim=c(10,30), ylim=c(45,70))
detach('package:maps')
# This has successfully plotted the three sights in the expected locations

# ----------------------------

# We can use R to check the criteria: 
# - Dataset consists of at least 2 years of sampling (they do not have to be consecutive) = YES
# - Dataset consists of entire assemblages, not just populations = YES
# - Data should record abundance, biomass or both. = YES (abundance)
# - Sampling methods *and effort* are consistent through time. = YES
# - Individuals are identified to species level where possible. = YES

dim(dt)
summary(dt)

# Does the data set record data over more than two years?
# To do this, the sampling date has to be split into day, month, and year
#   > The output says TRUE
separate(dt, "Sampling date", c("Year", "Month", "Day"), sep = "-")
dt$Year <- word(dt$`Sampling date`, sep = "-", start=1)
dt$Month <- word(dt$`Sampling date`, sep = "-", start=2)
dt$Day <- word(dt$`Sampling date`, sep = "-", start=3)
dt$`Sampling date` <- NULL
n_distinct(dt$Year) >= 2

# Make Year, Month and Day factors
#   > The output says TRUE
dt$Year <- as.factor(dt$Year) 
dt$Month <- as.factor(dt$Month)
dt$Day <- as.factor(dt$Day)
is.factor(dt$Year) | is.integer(dt$Year)
is.factor(dt$Month) | is.integer(dt$Month)
is.factor(dt$Day) | is.integer(dt$Day)

# Check that the taxon is a factor > Make taxon a factor
#   > The output says TRUE
dt$Taxon <- as.factor(dt$Taxon)
is.factor(dt$Taxon)

# Change 'Density' to 'Abundance' and check it is numeric
#   > The output says TRUE
colnames(dt)[3] = "Abundance"
is.numeric(dt$Abundance)

# We can also re-name 'site' to 'Plot' so it is the same as BioTime 
colnames(dt)[1] = "Plot"

# Check data again -> looks good 
str(dt)

# 7. Primary field check 

# Abundance needs to be more than zero for each row
# > The output says FALSE
min(dt$Abundance) > 0

# We can remove any rows where abundance = 0 
# > Output now says true (3 rows were removed)
library(dplyr)
dt<- filter_if(dt, is.numeric, all_vars((.) != 0))

# ----------------------------

# 8. Secondary field check - Errors in the data

# I am going to split the taxon into the two words:
dt$Species <- dt$Taxon
dt$Genus <- word(dt$Species, 1)
dt$Species <- word(dt$Species, start=2)
dt$Taxon <- NULL

# Now im going to look for the family ending in the genus column and we finally get results! 
str_which(dt$Genus, 'idae$|eae$')

# We need to remove these because we need them sampled down to the species level
# This has removed the rows that have only been sampled down to the family level, and we can maually check it 
# by searching the data set for rows that end 'idae' or 'eae' (4325-3902)
delete <- c(str_which(dt$Genus, 'idae$|eae$'))
delete
dt_new<- dt[-delete, ]
dt <- dt_new

# We can start by looking at any errors in the species column 
sort(unique(word(dt$Species, 1)))

# Visual check of the data shows there is 'Gen.' meaning these rows are only sampled 
# down to the genus and not the species so we can remove them (3673)
str_which(dt$Species, 'Gen.')
delete2 <- c(str_which(dt$Species, 'Gen.'))
dt_new<- dt[-delete2, ]
dt <- dt_new

# ERROR: There is also the species listed as "p." (Pisidium p.) 
# > We can change this to sp. 
dt$Species<-replace(dt$Species,dt$Species=="p.","sp.")

# We can also look at any errors in the Genus column 
sort(unique(word(dt$Genus, 1)))

# ERROR: The data set has a row called "Habroleptoides/Paraleptophlebia"
# For this row, they are not confident what the genus is, and have nothing listed for the species
# There are lots of records of the species Paraleptophlebia but no other mention of Habroleptoides
# > Going to change this row to just Paraleptophlebia
dt$Genus<-replace(dt$Genus,dt$Genus=="Habroleptoides/Paraleptophlebia","Paraleptophlebia")

# ----------------------------

# 9. Prepare curated data for export

# Add in BioTime variables that are currently missing from the corrected datafile
dt$Biomass <- rep('', nrow(dt))
dt$StudyID <- rep('', nrow(dt))
dt$Family <- rep('', nrow(dt))

# aggregate abundance records that are same species, plot, and survey day.
dt_merged <- dt %>% group_by(Biomass, Family, Genus, Species, Plot, Latitude, Longitude, DepthElevation, Day, Month, Year, StudyID) %>% 
  summarise(Abundance=sum(Abundance)) %>% ungroup() %>% arrange(Year, Family, Genus, Species)

# 4 rows were agregated 
dim(dt)[1]-dim(dt_merged)[1]

# save the dataset name as an object so we save some typing
dataset_name <- 'LTER_RiverSalaca_BenthicInvertebrates'

# put in as many non-blank fields unique to the sampling event
# If plot, depthelevation, month, and day are available, I'd add those in too
dt_merged$SampleDescription <- as.factor(with(dt_merged, paste(dataset_name, Latitude, Longitude, DepthElevation, Year, Month, Day, sep='_')))
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

# 10. Adding the meta-data 

## we then have to add the meta data, which can either be found in the data, or in the paper 
## sometimes we may have to use R for the central coordinates (`CentralLatitude, CentralLongitude`) 
## and sampling area. For datasets grained to one location, these fields should already be provided. 
## For datasets where authors provide us with multiple locations, we use R spatial analysis to calculate 
## the central point and area. This can be easily done using a few lines of code to create a convex hull 
## with the multiple locations.

# 11. load libraries
library(sf)
library(clipr)
library(sp)
library(rgeos)

# 12. Convert data points into point spatial object
points <- SpatialPoints(cbind(dt$Longitude,dt$Latitude))

# 13. Calculate convex hull, area and centroid
# > Area =  0.03233764
# > lat = 57.80738 / lon = 21.85131

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


