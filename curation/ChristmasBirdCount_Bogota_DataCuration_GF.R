# Curation Script --------------------------------------------------------------

# Dataset: Christmas Bird Count - Bogota
# Location: Bogota, Colombia 
# Curator: Garrett Fundakowski
# Date started: 23-01-2023
# Last updated: 30-06-2023


# Set up -----------------------------------------------------------------------
# load the necessary packages
require(tidyverse)
require(maps)
require(readxl)

# clear up the environment before starting
rm(list=ls())

# set the working directory via a pop-up window
setwd(file.choose() %>% dirname())

# read in the data from each sheet into separate dfs
dt89 <- read_excel('./TotalData1989_2014.xlsx', sheet=1, col_names=T, na='')
dt90 <- read_excel('./TotalData1989_2014.xlsx', sheet=2, col_names=T, na='')
dt91 <- read_excel('./TotalData1989_2014.xlsx', sheet=3, col_names=T, na='')
dt92 <- read_excel('./TotalData1989_2014.xlsx', sheet=4, col_names=T, na='')
dt93 <- read_excel('./TotalData1989_2014.xlsx', sheet=5, col_names=T, na='')
dt94 <- read_excel('./TotalData1989_2014.xlsx', sheet=6, col_names=T, na='')
dt95 <- read_excel('./TotalData1989_2014.xlsx', sheet=7, col_names=T, na='')
dt96 <- read_excel('./TotalData1989_2014.xlsx', sheet=8, col_names=T, na='')
dt97 <- read_excel('./TotalData1989_2014.xlsx', sheet=9, col_names=T, na='')
dt98 <- read_excel('./TotalData1989_2014.xlsx', sheet=10, col_names=T, na='')
dt99 <- read_excel('./TotalData1989_2014.xlsx', sheet=11, col_names=T, na='')
dt00 <- read_excel('./TotalData1989_2014.xlsx', sheet=12, col_names=T, na='')
dt01 <- read_excel('./TotalData1989_2014.xlsx', sheet=13, col_names=T, na='')
dt02 <- read_excel('./TotalData1989_2014.xlsx', sheet=14, col_names=T, na='')
dt03 <- read_excel('./TotalData1989_2014.xlsx', sheet=15, col_names=T, na='')
dt04 <- read_excel('./TotalData1989_2014.xlsx', sheet=16, col_names=T, na='')
dt05 <- read_excel('./TotalData1989_2014.xlsx', sheet=17, col_names=T, na='')
dt06 <- read_excel('./TotalData1989_2014.xlsx', sheet=18, col_names=T, na='')
dt07 <- read_excel('./TotalData1989_2014.xlsx', sheet=19, col_names=T, na='')
dt08 <- read_excel('./TotalData1989_2014.xlsx', sheet=20, col_names=T, na='')
dt09 <- read_excel('./TotalData1989_2014.xlsx', sheet=21, col_names=T, na='')
dt10 <- read_excel('./TotalData1989_2014.xlsx', sheet=22, col_names=T, na='')
dt11 <- read_excel('./TotalData1989_2014.xlsx', sheet=23, col_names=T, na='')
dt12 <- read_excel('./TotalData1989_2014.xlsx', sheet=24, col_names=T, na='')
dt13 <- read_excel('./TotalData1989_2014.xlsx', sheet=25, col_names=T, na='')
dt14 <- read_excel('./TotalData1989_2014.xlsx', sheet=26, col_names=T, na='')


# Go from wide format to long for each -----------------------------------------
dt89 <- dt89 %>% 
  pivot_longer(cols = 2:30, names_to = "Plot", values_to = "Abundance")
dt90 <- dt90 %>% 
  pivot_longer(cols = 2:30, names_to = "Plot", values_to = "Abundance")
dt91 <- dt91 %>% 
  pivot_longer(cols = 2:30, names_to = "Plot", values_to = "Abundance")
dt92 <- dt92 %>% 
  pivot_longer(cols = 2:30, names_to = "Plot", values_to = "Abundance")
dt93 <- dt93 %>% 
  pivot_longer(cols = 2:30, names_to = "Plot", values_to = "Abundance")
dt94 <- dt94 %>% 
  pivot_longer(cols = 2:30, names_to = "Plot", values_to = "Abundance")
dt95 <- dt95 %>% 
  pivot_longer(cols = 2:30, names_to = "Plot", values_to = "Abundance")
dt96 <- dt96 %>% 
  pivot_longer(cols = 2:30, names_to = "Plot", values_to = "Abundance")
dt97 <- dt97 %>% 
  pivot_longer(cols = 2:30, names_to = "Plot", values_to = "Abundance")
dt98 <- dt98 %>% 
  pivot_longer(cols = 2:30, names_to = "Plot", values_to = "Abundance")
dt99 <- dt99 %>% 
  pivot_longer(cols = 2:30, names_to = "Plot", values_to = "Abundance")
dt00 <- dt00 %>% 
  pivot_longer(cols = 2:30, names_to = "Plot", values_to = "Abundance")
dt01 <- dt01 %>% 
  pivot_longer(cols = 2:30, names_to = "Plot", values_to = "Abundance")
dt02 <- dt02 %>% 
  pivot_longer(cols = 2:30, names_to = "Plot", values_to = "Abundance")
dt03 <- dt03 %>% 
  pivot_longer(cols = 2:30, names_to = "Plot", values_to = "Abundance")
dt04 <- dt04 %>% 
  pivot_longer(cols = 2:30, names_to = "Plot", values_to = "Abundance")
dt05 <- dt05 %>% 
  pivot_longer(cols = 2:30, names_to = "Plot", values_to = "Abundance")
dt06 <- dt06 %>% 
  pivot_longer(cols = 2:30, names_to = "Plot", values_to = "Abundance")
dt07 <- dt07 %>% 
  pivot_longer(cols = 2:30, names_to = "Plot", values_to = "Abundance")
dt08 <- dt08 %>% 
  pivot_longer(cols = 2:30, names_to = "Plot", values_to = "Abundance")
dt09 <- dt09 %>% 
  pivot_longer(cols = 2:30, names_to = "Plot", values_to = "Abundance")
dt10 <- dt10 %>% 
  pivot_longer(cols = 2:30, names_to = "Plot", values_to = "Abundance")
dt11 <- dt11 %>% 
  pivot_longer(cols = 2:30, names_to = "Plot", values_to = "Abundance")
dt12 <- dt12 %>% 
  pivot_longer(cols = 2:30, names_to = "Plot", values_to = "Abundance")
dt13 <- dt13 %>% 
  pivot_longer(cols = 2:30, names_to = "Plot", values_to = "Abundance")
dt14 <- dt14 %>% 
  pivot_longer(cols = 2:30, names_to = "Plot", values_to = "Abundance")

# Add years to each
dt89$Year <-1989
dt90$Year <-1990
dt91$Year <-1991
dt92$Year <-1992
dt93$Year <-1993
dt94$Year <-1994
dt95$Year <-1995
dt96$Year <-1996
dt97$Year <-1997
dt98$Year <-1998
dt99$Year <-1999
dt00$Year <-2000
dt01$Year <-2001
dt02$Year <-2002
dt03$Year <-2003
dt04$Year <-2004
dt05$Year <-2005
dt06$Year <-2006
dt07$Year <-2007
dt08$Year <-2008
dt09$Year <-2009
dt10$Year <-2010
dt11$Year <-2011
dt12$Year <-2012
dt13$Year <-2013
dt14$Year <-2014

# merge them all together
dtALL <- rbind(dt89, dt90, dt91, dt92, dt93, dt94, dt95, dt96, dt97, dt98, dt99,
               dt00, dt01, dt02, dt03, dt04, dt05, dt06, dt07, dt08, dt09, 
               dt10, dt11, dt12, dt13, dt14)

# read in additional info ------------------------------------------------------
# read in info for coordinates and elevation
coords <- read_excel('./LocalitiesTableCBC_decimal.xls', sheet=1, skip=2, col_names=T, na='')

# remove unnecessary rows and columns
coords <- subset(coords, select=c(1:4))
coords <- coords[!is.na(coords$No.),] # keep only those that don't have NA in No.

# read in party hours per site per year
pHpSpY <- read_excel('./PartyHoursPerSitePerYear.xlsx', sheet=1, skip=2, col_names=T, na='')
pHpSpY <- pHpSpY[!is.na(pHpSpY$`Party Hours`),]
pHpSpY <- subset(pHpSpY, select=c(1:30))
summary(pHpSpY) # notice 3 plots were only visited for 1 year, will remove later


# Structure check --------------------------------------------------------------
dim(dtALL) # 180148 x 4
str(dtALL) # notice Year is num; will fix in a moment
summary(dtALL)

# Abundance and/or biomass, latitude and longitude numeric?
# No Lat/Long; will deal with coordinates later
is.numeric(dtALL$Abundance) # TRUE
# Date should be POSIXct? 
# NA
# Year, month and day must be integers or factors?
is.factor(dtALL$Year) | is.integer(dtALL$Year) # FALSE
# Secondary fields such as trawl, plot, transect etc must be factors or integers? 
is.factor(dtALL$Plot) | is.integer(dtALL$Plot) # FALSE
# Secondary fields such as elevation or depth will normally be numeric unless in a treatment format.
is.numeric(coords$Elevation) # some are ranges
# Taxonomic fields must be characters or factors? 
is.factor(dtALL$SPECIES) | is.character(dtALL$SPECIES) # TRUE


# Structure fix ----------------------------------------------------------------
# convert year, month, scode(aka - plot) to factor
dtALL$Year <- as.factor(dtALL$Year)

str(dtALL)

# convert elevation to numeric and get rid of ranges (use maximum elevation)
# remove all non-digits from Elevation column 
coords$Elevation <- gsub("\\D+","",coords$Elevation)
# keep only last 4 digits, this equates to using the maximum elevation for elevation ranges
coords$Elevation <- substring(coords$Elevation, nchar(coords$Elevation)-3, nchar(coords$Elevation))
coords$Elevation <- as.numeric(coords$Elevation)


# Primary field check ----------------------------------------------------------
# ABUNDANCE
# No negative values, zeroes, or NAs in abundance/biomass fields.
# From a quick look, there are NAs
# Remove NAs
dtALL <- dtALL[!is.na(dtALL$Abundance),]
#Check
min(dtALL$Abundance) > 0 # TRUE
sum(dtALL$Abundance == "") == 0 # TRUE

# YEAR MONTH DAY
# again, no negative values, 0s or NAs, and all are logical
# Year < 2020, month < 12, day < 31
summary(dtALL$Year) # looks good

# LAT LONG
# no blanks, no NAs
# Latitude constrained from -90 to 90.
# Longitude constrained -180 to 180.
# Create a copy of Plot variable and isolate numeric to help with joining from coords
dtALL$Plot2 <- word(dtALL$Plot,1)
dtALL$Plot2 <- substring(dtALL$Plot2, 0, nchar(dtALL$Plot2)-1)
dtALL$Plot2 <- as.numeric(dtALL$Plot2)
#dtALL$Plot2 <- as.factor(dtALL$Plot2)
# Join with coords df
dt <- left_join(dtALL,coords,by = c("Plot2" = "No."))
# Separate Lat & Long
dt$Latitude <- substring(dt$Coordinates,0,8)
dt$Longitude <- substring(dt$Coordinates,11,nchar(dt$Coordinates))
dt$Latitude <- as.numeric(dt$Latitude)
dt$Longitude <- as.numeric(dt$Longitude)

#plot to visually check whether the GPS coordinates match expectations
world_map <- map_data('world')
world <- ggplot(world_map) + 
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  coord_fixed() + 
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())
points <- world + geom_point(data=dt[1,], aes(x=Longitude, y=Latitude), shape=21)
points_zoom <- points + coord_fixed(xlim=c(-85,-65), ylim=c(-15,15))
points_zoom # looks good
points1 <- world + geom_point(data=dt, aes(x=Longitude, y=Latitude), shape=21)
points_zoom1 <- points1 + coord_fixed(xlim=c(-74.5,-73.5),ylim=c(4.5,5))
points_zoom1 # looks good

# ADJUST ABUNDANCE BY PARTY HOURS
# make pHpSpY into long format
adjustPHpSpY <- pivot_longer(pHpSpY, cols = c(2:30), names_to = "Site", values_to = "Party Hours")
# merge adjustPHpSpY to dt so we can adjust the abundances by party hours
dtAdj <- left_join(dt,adjustPHpSpY,by = c("Locality" = "Site", "Year" = "Year"))
# adjust the abundance by the party hours
dtAdj$AbundanceAdj <- dtAdj$Abundance / dtAdj$`Party Hours`

# Check for any NAs in Party Hours (this tells us which sites don't have party hours for that site-year combo)
dim(dtAdj) #10226
dim(dtAdj[(is.na(dtAdj$`Party Hours`)),]) #301 are NA
dtAdjNA <- dtAdj[(is.na(dtAdj$`Party Hours`)),] # make a new df with just the NAs
dtAdjNA$missingPH <- paste(dtAdjNA$Year, dtAdjNA$Plot2, sep = "P") # make a new variable to see site-year combos with NAs
sort(unique(dtAdjNA$missingPH))
summary(as.factor(dtAdjNA$missingPH))
# remove the entries with NAs in Party Hours
dtAdj <- dtAdj[!(is.na(dtAdj$`Party Hours`)),]
dim(dtAdj) # 9925 x 12

# Check for any site-year combos with < 1 party hour
dim(dtAdj[dtAdj$`Party Hours`< 1,]) # 74
# Remove the entries with < 1 party hour
dtAdj <- dtAdj[!(dtAdj$`Party Hours`< 1),]
dim(dtAdj) # 9851 x 12

# Secondary field --------------------------------------------------------------
# Plot and treatment must be inspected for NA, NULL, blank or values unspecified in source methods
# We must check for misspellings and revalue levels if needed
dtAdj$Plot <- paste(dtAdj$Plot2, dtAdj$Locality, sep = "-")
sort(unique(dtAdj$Plot)) # 29 

sort(unique(dtAdj$Elevation))


# Taxonomic field check --------------------------------------------------------
# No NAs in taxonomic fields, remove all non-organism records
# Misspellings check, but not taxonomic cleaning
# Separate taxon names - SPECIES is 'genus species'
dtAdj$Genus <- word(dtAdj$SPECIES, 1)
dtAdj$Species <- word(dtAdj$SPECIES, start=2)

# check for mispellings
sort(unique(dtAdj$Genus))
sort(unique(dtAdj$Species))

# Note weird case of Empidonax (trailli)(alnorum)
dtAdj$Species[dtAdj$Species == '(trailli)(alnorum)'] <- 'sp1'

sort(unique(dtAdj$Species))


# Prepare raw data --------------------------------------------------------
# Remove unnecessary columns
dtAdj$Plot2 <- NULL
dtAdj$Locality <- NULL
dtAdj$Coordinates <- NULL
dtAdj$SPECIES <- NULL
dtAdj$Abundance <- NULL
dtAdj$`Party Hours` <- NULL

#Rename columns
colnames(dtAdj)[3] <- 'DepthElevation'
colnames(dtAdj)[6] <- 'Abundance'

dtAdj <- dtAdj %>% arrange(Year, Genus, Species)
# now create empty columns needed to fit to template
dtAdj$Biomass <- rep('', dim(dtAdj)[1])
dtAdj$Family <- rep('', dim(dtAdj)[1])
dtAdj$Day <- rep('', dim(dtAdj)[1])
dtAdj$Month <- rep('', dim(dtAdj)[1])
dtAdj$StudyID <- rep('', dim(dtAdj)[1])
dtAdj$SampleDescription <- rep('', dim(dtAdj)[1])

# aggregate abundance records that are same species, plot, and survey day.
dt_merged <- dtAdj %>% group_by(Biomass, Family, Genus, Species, SampleDescription, Plot, Latitude, Longitude, DepthElevation, Day, Month, Year, StudyID) %>% 
  summarise(Abundance=sum(Abundance)) %>% ungroup() %>% arrange(Year, Genus, Species)
dim(dtAdj)[1]-dim(dt_merged)[1] # any change in aggregating? no

# save the dataset name as an object
dataset.name <- 'ChristmasBirdCount_Bogota'
# fill in sampling event with unique info
dt_merged$SampleDescription <- as.factor(with(dt_merged, paste(Year, Plot, sep='_')))
length(levels(dt_merged$SampleDescription)) # 315 samples

# reorder columns by BioTIME format
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

# final check
View(dt_merged)
summary(dt_merged)
str(dt_merged)


# Export final ------------------------------------------------------------

setwd(file.choose() %>% dirname())
write.csv(dt_merged, paste0(dataset.name, '_rawdata_GF.csv'), row.names=F)

### Spatial Geometry Calculations for BioTIME datasets --------------------

# load libraries
library(sf)
library(clipr)

# 1. Convert data points into point spatial object
dt_coord <- dt_merged %>% select(Longitude, Latitude) %>% distinct() %>%
  st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()

# 2. Calculate convex hull, area and centroid
centroid <- dt_coord %>% st_convex_hull() %>% st_centroid() %>% unlist # get centroid
write_clip(centroid[c(2,1)]) # copy as lat-long

# transform to mercator to get area in sq km
area <- st_transform(dt_coord, st_crs("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs")) %>%
  st_convex_hull() %>% st_area()  ##get area in sq km
write_clip(area)

# Plot the geometries -----------------------------------------------------

require(ggplot2)
world_map <- map_data('world') # check whether the GPS coordinates match expectations
ggplot(world_map) +
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  geom_point(data = dt_merged, aes(x = Longitude, y = Latitude), size = 2, shape = 21, colour = 'blue') +
  geom_point(aes(x = centroid[1], y = centroid[2]), size = 3, colour = 'green') +
  geom_sf(data = dt_coord %>% st_convex_hull(), colour = 'blue', fill = 'transparent') +
  coord_sf(xlim=c(-74.5,-73.5),ylim=c(4.5,5)) + # make sure these fit your points
  labs(x = NULL, y = NULL) +
  theme_minimal()

