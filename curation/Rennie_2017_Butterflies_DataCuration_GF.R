# Curation Script --------------------------------------------------------------

# Dataset: UK_ECN_Butterflies
# Location: United Kingdom
# Curator: Garrett Fundakowski
# Date started: 06-07-2023
# Last updated: 20-07-2023


# Set up -----------------------------------------------------------------------
# load the necessary packages
require(tidyverse)
require(maps)
require(readxl)
require(stringr)
require(lubridate)

# clear up the environment before starting
rm(list=ls())

# set the working directory via a pop-up window
setwd(file.choose() %>% dirname())

# read in the data from each sheet into separate dfs
dt <- read_csv('./ECN_IB1.csv')
coords <- read_excel('./SiteInformation.xlsx', col_names=T, na='')
fieldnameTable <- read_excel('./FieldNameTable.xlsx', col_names=T, na='')
sampleTime <- read_csv('./ECN_IB2.csv')


# Convert given coords to decimal ----------------------------------------------
angle2dec <- function(angle) {
  angle <- as.character(angle)
  # detect a split of spaces and/or special characters
  x <- do.call(rbind, str_split(angle, '([^\\.] & [:punct:]\\s*)|\\"\\s*|(?<=[:digit:])\\s+(?=[:digit:])' ))
  x <- apply(x, 1L, function(y) {
    y <- as.numeric(y)
    y[1] + y[2]/60 + y[3]/3600
  })
  return(x)
}

# convert to decimal
coords$latDec <- angle2dec(str_replace_all(coords$latDMS, "\\'|\\°|\"|[:symbol:]|\\'|\\s*[:upper:]$", ' '))
coords$longDec <- angle2dec(str_replace_all(coords$longDMS, "\\'|\\°|\"|[:symbol:]|\\'|\\s*[:upper:]$", ' '))
coords$longDec <- -coords$longDec # transform to E

# remove unnecessary columns
coords$latDMS <- NULL
coords$longDMS <- NULL
coords$`Site name` <- NULL


# Merge dt and sampleTime ------------------------------------------------------
# remove unnecessary columns
sampleTime$RECORDER <- NULL
sampleTime$RECCODE <- NULL
sampleTime$TEMP <- NULL
sampleTime$SUNPERC <- NULL
sampleTime$WSPEEDB <- NULL
sampleTime$WEEK <- NULL
sampleTime$SHOUR <- NULL
sampleTime$SMINS <- NULL

dt <- left_join(dt, sampleTime, by = c("SITECODE", "LCODE", "SDATE"))


# Fixes before going for checks ------------------------------------------------
# Remove fieldnames of XX (indicates no moths collected)
dt <- dt[!(dt$FIELDNAME=="XX"),]


# Structure check --------------------------------------------------------------
dim(dt) # 56694 x 7
str(dt) 
summary(dt)

# Abundance and/or biomass, latitude and longitude numeric?
colnames(dt)[6] <- 'Abundance'
is.numeric(dt$Abundance) # TRUE
# Date should be POSIXct? 
is.POSIXct(dt$SDATE) # FALSE
# Year, month and day must be integers or factors?
# NA
# Secondary fields such as trawl, plot, transect etc must be factors or integers? 
# NA
# Secondary fields such as elevation or depth will normally be numeric unless in a treatment format.
# NA
# Taxonomic fields must be characters or factors? 
is.factor(fieldnameTable$Fieldname) | is.character(fieldnameTable$Fieldname) # TRUE


# Structure fix ----------------------------------------------------------------
# convert year, month, day, transect to factor
dt$POSIX <- as.POSIXct(dt$SDATE, format = "%d-%b-%y")
dt$Day <- as.integer(format(dt$POSIX,"%d"))
dt$Month <- as.integer(format(dt$POSIX, "%m"))
dt$Year <- as.integer(format(dt$POSIX, "%Y"))

str(dt)


# Primary field check ----------------------------------------------------------
# ABUNDANCE
# No negative values, zeroes, or NAs in abundance/biomass fields.
# From a quick look, there are 0s
# Set 0 values to NAs
dt["Abundance"][dt["Abundance"] == 0] <- NA
# Remove NAs
dt <- dt[!is.na(dt$Abundance),]
#Check
min(dt$Abundance) > 0 # TRUE
sum(dt$Abundance == "") == 0 # TRUE

# YEAR MONTH DAY
# again, no negative values, 0s or NAs, and all are logical
# Year < 2020, month < 12, day < 31
summary(dt$Year) # looks good
summary(dt$Month) # looks good
summary(dt$Day) # looks good

# LAT LONG
# no blanks, no NAs
# Latitude constrained from -90 to 90.
# Longitude constrained -180 to 180.
dt <- left_join(dt, coords, by = c("SITECODE" = "Site code"))
colnames(dt)[12] <- 'Latitude'
colnames(dt)[13] <- 'Longitude'

#plot to visually check whether the GPS coordinates match expectations
world_map <- map_data('world')
world <- ggplot(world_map) + 
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  coord_fixed() + 
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())
points <- world + geom_point(data=dt[1,], aes(x=Longitude, y=Latitude), shape=21)
points_zoom <- points + coord_fixed(xlim=c(-15,5), ylim=c(45,65))
points_zoom # looks good
points1 <- world + geom_point(data=dt, aes(x=Longitude, y=Latitude), shape=21)
points_zoom1 <- points1 + coord_fixed(xlim=c(-7,0),ylim=c(50,57))
points_zoom1 # looks good


# Secondary field --------------------------------------------------------------
# Plot and treatment must be inspected for NA, NULL, blank or values unspecified in source methods
# We must check for misspellings and revalue levels if needed
dt$Plot <- as.factor(with(dt, paste(SITECODE, LCODE, sep='_')))
length(levels(dt$Plot)) # 14


# Taxonomic field check --------------------------------------------------------
# No NAs in taxonomic fields, remove all non-organism records
# Misspellings check, but not taxonomic cleaning

# Clean fieldnameTable to only include those observed in the dataset
fieldnameTable <- fieldnameTable[(fieldnameTable$Fieldname %in% dt$FIELDNAME),]
# Check fieldnameTable for NA's in the Description (Species name) field
table(is.na(fieldnameTable$`Latin name`)) # 2 are NA
# manual checking reveals they are Fieldname = 197 & 200
# replace the missing fields
fieldnameTable$Family <-  rep('', dim(fieldnameTable)[1])
fieldnameTable[fieldnameTable$Fieldname==197, "Family"] <- "Pieridae" # Mixed whites
fieldnameTable[fieldnameTable$Fieldname==200, "Latin name"] <- "Thymelicus sp" # small/essex skipper
# due to not being able to distinguish small/essex whites reliably through time
# and due to change of methods on how to treat unindentifiable ones
# (93-96 for some sites, they lumped unidentifiable ones with small skippers)
# we will aggregate all counts for small, essex, and small/essex
# rename small [120] and essex [119] to be Thymelicus sp
fieldnameTable[fieldnameTable$Fieldname==119, "Latin name"] <- "Thymelicus sp"
fieldnameTable[fieldnameTable$Fieldname==120, "Latin name"] <- "Thymelicus sp"

sort(unique(fieldnameTable$`Latin name`))
# Separate taxon names - Latin name is 'Genus species'
fieldnameTable$Genus <- word(fieldnameTable$`Latin name`, 1)
fieldnameTable$Species <- word(fieldnameTable$`Latin name`, start=2)

# check for mispellings
sort(unique(fieldnameTable$Genus)) 
sort(unique(fieldnameTable$Species)) 

# left join fieldnameTable with dt
# FINALLY!
dtALL <- left_join(dt, fieldnameTable, by = c("FIELDNAME" = "Fieldname"))


# Prepare raw data --------------------------------------------------------
# Remove unnecessary columns
dtALL$SITECODE <- NULL
dtALL$LCODE <- NULL
dtALL$FIELDNAME <- NULL
dtALL$SDATE <- NULL
dtALL$SECTION <- NULL
dtALL$BROODED <- NULL
dtALL$POSIX <- NULL
dtALL$`Latin name` <- NULL
dtALL$`Common name` <- NULL

dtALL <- dtALL %>% arrange(Year, Genus, Species)
# now create empty columns needed to fit to template
dtALL$StudyID <- rep('', dim(dtALL)[1])
dtALL$SampleDescription <- rep('', dim(dtALL)[1])
dtALL$DepthElevation <- rep('', dim(dtALL)[1])
dtALL$Biomass <- rep('', dim(dtALL)[1])

# save the dataset name as an object
dataset.name <- 'Rennie_2017_butterflies'
# fill in sampling event with unique info
dtALL$SampleDescription <- as.factor(with(dtALL, paste(Year, Month, Day, Plot, sep='_')))
length(levels(dtALL$SampleDescription)) # 4301 samples

# aggregate abundance records that are same species, plot, and survey day.
dt_merged <- dtALL %>% group_by(Biomass, Family, Genus, Species, SampleDescription, Plot, Latitude, Longitude, DepthElevation, Day, Month, Year, StudyID) %>% 
  summarise(Abundance=sum(Abundance)) %>% ungroup() %>% arrange(Year, Genus, Species)
dim(dtALL)[1]-dim(dt_merged)[1] # any change in aggregating? yes, 36959

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
# 53.6834425867005, -3.15784642120691

# transform to mercator to get area in sq km
area <- st_transform(dt_coord, st_crs("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs")) %>%
  st_convex_hull() %>% st_area()  ##get area in sq km
write_clip(area) # 464942.532358045 sq km 

# Plot the geometries -----------------------------------------------------

require(ggplot2)
world_map <- map_data('world') # check whether the GPS coordinates match expectations
ggplot(world_map) +
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  geom_point(data = dt_merged, aes(x = Longitude, y = Latitude), size = 2, shape = 21, colour = 'blue') +
  geom_point(aes(x = centroid[1], y = centroid[2]), size = 3, colour = 'green') +
  geom_sf(data = dt_coord %>% st_convex_hull(), colour = 'blue', fill = 'transparent') +
  coord_sf(xlim = c(-10,0), ylim = c(50,60)) + # make sure these fit your points
  labs(x = NULL, y = NULL) +
  theme_minimal()

