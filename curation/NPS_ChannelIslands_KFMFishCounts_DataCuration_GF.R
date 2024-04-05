# Curation Script --------------------------------------------------------------

# Dataset: Channel Islands National Park Kelp Forest Monitoring Program -
#           Roving Diver Fish Counts
# Location: Channel Islands National Park, California, USA 
# Curator: Garrett Fundakowski
# Date started: 16-01-2023
# Last updated: 04-07-2023


# Set up -----------------------------------------------------------------------
# load the necessary packages
require(tidyverse)
require(maps)
require(readxl)
require(lubridate)

# clear up the environment before starting
rm(list=ls())

# set the working directory via a pop-up window
setwd(file.choose() %>% dirname())

# read in the data from the txt file 
dt <- read.delim("./KFM FishCount for BioTime Paper 2022 Expert only.txt", header = FALSE, sep = ",", )
colnames(dt) <- c("siteCode", "islandCode", "islandName", "reefCode", "reefName", "Year", "Date",
                 "observerCode", "observerLevel", "species", "commonName", "abundanceCat", "Abundance")


# read in additional info ------------------------------------------------------
coords <- read.delim("./KFM_SiteInformation.txt", header = FALSE, sep = ",")

# remove unnecessary rows and columns
coords <- subset(coords, select=c(1,6:8,10))
colnames(coords) <- c("siteCode", "Inside/OutsideMarineReserve", "Latitude", "Longitude","meanDepth_FT")


# Structure check --------------------------------------------------------------
dim(dt) # 129570 x 13
str(dt) # notice Date is chr
summary(dt)

# Abundance and/or biomass, latitude and longitude numeric?
is.numeric(dt$Abundance) # TRUE
is.numeric(coords$Latitude) # TRUE
is.numeric(coords$Longitude) # TRUE
# Date should be POSIXct? 
is.POSIXct(dt$Date) # FALSE, but going to separate into Day Month Year columns as time isn't crucial here
# Year, month and day must be integers or factors?
is.factor(dt$Year) | is.integer(dt$Year) # TRUE
# Secondary fields such as trawl, plot, transect etc must be factors or integers? 
# NA
# Secondary fields such as elevation or depth will normally be numeric unless in a treatment format.
is.numeric(coords$meanDepth_FT) # TRUE
# Taxonomic fields must be characters or factors? 
is.factor(dt$species) | is.character(dt$species) # TRUE


# Structure fix ----------------------------------------------------------------
# convert Date to POSIXct and make separate Year, Month, and Day columns
dt$DatePOSIXct <- as.POSIXct(dt$Date, format = "%m/%d/%Y %H:%M:%S", tz = "America/Los_Angeles")
dt$Day <- as.integer(format(dt$DatePOSIXct,"%d"))
dt$Month <- as.integer(format(dt$DatePOSIXct, "%m"))
dt$Year <- as.integer(format(dt$DatePOSIXct, "%Y"))
str(dt)


# Primary field check ----------------------------------------------------------
# ABUNDANCE
# No negative values, zeroes, or NAs in abundance/biomass fields.
# From a quick look, there are NAs and 0s
# Set 0 values to NAs
dt["Abundance"][dt["Abundance"] == 0] <- NA
# Remove NAs
dt <- dt[!is.na(dt$Abundance),]
#Check
min(dt$Abundance) > 0 # TRUE
# Remove 0s
sum(dt$Abundance == "") == 0 # TRUE

# YEAR MONTH DAY
# again, no negative values, 0s or NAs, and all are logical
# Year < 2022, month < 12, day < 31
summary(dt$Year) # looks good
summary(dt$Month) # looks good
summary(dt$Day) # looks good

# LAT LONG
# no blanks, no NAs
# Latitude constrained from -90 to 90.
# Longitude constrained -180 to 180.
# Join with coords df
dt <- left_join(dt, coords, by = "siteCode")

# DEPTH
colnames(dt)[20] <- 'DepthElevation'

#plot to visually check whether the GPS coordinates match expectations
world_map <- map_data('world')
world <- ggplot(world_map) + 
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  coord_fixed() + 
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())
points <- world + geom_point(data=dt[1,], aes(x=Longitude, y=Latitude), shape=21)
points_zoom <- points + coord_fixed(xlim=c(-140,-110), ylim=c(15,45))
points_zoom # looks good
points1 <- world + geom_point(data=dt, aes(x=Longitude, y=Latitude), shape=21)
points_zoom1 <- points1 + coord_fixed(xlim=c(-121.0,-118.0),ylim=c(32.5,34.5))
points_zoom1 # looks good


# Secondary field --------------------------------------------------------------
# Plot and treatment must be inspected for NA, NULL, blank or values unspecified in source methods
# We must check for misspellings and revalue levels if needed
dt$Plot <- paste(dt$islandCode, dt$reefCode, dt$`Inside/OutsideMarineReserve`, sep='_')
sort(unique(dt$Plot))
dt$Plot <- as.factor(dt$Plot)

# make Sample Description to keep the various observer transects separate sampling events
# save dataset name as an object
dataset.name <- 'NPS_ChannelIslands_KFMFishCounts'
# fill in sampling event with unique info
dt$SampleDescription <- as.factor(with(dt, paste(Year, Month, Day, Plot, sep='_')))
length(levels(dt$SampleDescription)) # 605 samples


# Taxonomic field check --------------------------------------------------------
# No NAs in taxonomic fields, remove all non-organism records
# Misspellings check, but not taxonomic cleaning

sort(unique(dt$species))
# Note some entries in species are not typical genus species names
# Bathymasteridae, Citharichthys, Cottidae, Embiotocidae, Gibbonsia,
# Gobiesox, Gobiidae, Neoclinus, Ophidiidae, Pholidae, Porichthys,
# Sebastes, & Stichaeidae all need to add sp to end
# other weird cases...
# baitfish, unidentified
# larval fish spp.
# Sebastes atrovirens/carnatus/caurinus/chrysomelas (all noted as kelp/gopher/copper/black and yellow rockfish, juvenile)
# Sebastes chrysomelas/carnatus (all black and yellow/gopher rockfish, juvenile)
# Sebastes serranoides/flavidus (all olive/yellowtail rockfish, juvenile)

# Find all that end in -idae and create Family column with this information
# Replace all family names in genus column with NA in next two steps
dt <- dt %>% mutate(Family = case_when(endsWith(species,"idae")~species))

# make a replacement vector to change some names; label Family names with 'family'
replace_species <- c('Bathymasteridae' = 'family',
                     '^Citharichthys$' = 'Citharichthys sp',
                     'Cottidae' = 'family',
                     'Embiotocidae' = 'family',
                     '^Gibbonsia$' = 'Gibbonsia sp',
                     'Gobiesox' = 'Gobiesox sp',
                     'Gobiidae' = 'family',
                     '^Neoclinus$' = 'Neoclinus sp',
                     'Ophidiidae' = 'family',
                     'Phanerodon spp.' = 'Phanerodon sp',
                     'Pholidae' = 'family',
                     '^Porichthys$' = 'Porichthys sp',
                     '^Sebastes$' = 'Sebastes sp',
                     'Stichaeidae' = 'family',
                     'Sebastes atrovirens/carnatus/caurinus/chrysomelas' = 'Sebastes sp1',
                     'Sebastes chrysomelas/carnatus' = 'Sebastes sp2',
                     'Sebastes serranoides/flavidus' = 'Sebastes sp3',
                     'baitfish, unidentified' = 'Baitfish sp',
                     'larval fish spp.' = 'Larvalfish sp')

# replace species names
dt$species <- str_replace_all(dt$species, replace_species)
# replace 'family' in species column with NAs
dt$species <- na_if(dt$species,"family")

# Separate taxon names - SPECIES is 'genus species'
dt$Genus <- word(dt$species, 1)
dt$Species <- word(dt$species, start=2)

# check for mispellings
sort(unique(dt$Family))
sort(unique(dt$Genus))
sort(unique(dt$Species))


# Only keep 1 replicate of each transect per year ------------------------------
# make a new data table of sampleDescription and observerCode
obsrvrs <- dt %>% arrange(Year, Month, Day) %>% distinct(SampleDescription, observerCode)
table(obsrvrs$observerCode)

# randomly choose 1 observerCode per sampleDescription
set.seed(62319) # set seed for reproducibility
obsrvrsUnique <- obsrvrs %>% group_by(SampleDescription) %>% sample_n(size = 1)
table(obsrvrsUnique$observerCode)
length(levels(obsrvrsUnique$SampleDescription)) # 605 levels

# Make unique sampleDescription_observerCode field for comparison
obsrvrsUnique$sD_oC <- paste(obsrvrsUnique$SampleDescription, obsrvrsUnique$observerCode, sep='_')
length(levels(as.factor(obsrvrsUnique$sD_oC)))
dt$sD_oC <- paste(dt$SampleDescription, dt$observerCode, sep='_')
length(levels(as.factor(dt$sD_oC)))

# keep only these observerCode x SampleDescription pairs
dtOne <- dt[(dt$sD_oC %in% obsrvrsUnique$sD_oC),]
length(levels(as.factor(dtOne$sD_oC)))


# Aggregate abundance records----------------------------------------------
# add column that checks if commonName ends with all
dtOne$match <- grepl(pattern = "^.+(all)$", dtOne$commonName)
# copy TRUEs to new df
dtALL <- dtOne[dtOne$match == TRUE,]
# subset dt to show only FALSE
dtOne <- dtOne[dtOne$match == FALSE,] # now 12218 x 27

# aggregate 
dt_merged <- dtOne %>% group_by(Family, Genus, Species, SampleDescription, Plot, DepthElevation, Latitude, Longitude, Day, Month, Year) %>% 
  summarise(Abundance=sum(Abundance)) %>% ungroup() %>% arrange(Year, Genus, Species)
dim(dtOne)[1]-dim(dt_merged)[1] # any change in aggregating? yes, 1978

# Prepare raw data --------------------------------------------------------
# now create empty columns needed to fit to template
dt_merged$Biomass <- rep('', dim(dt_merged)[1])
dt_merged$StudyID <- rep('', dim(dt_merged)[1])


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

#setwd(file.choose() %>% dirname())
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
# 33.6304940796426, -119.395587686465

# transform to mercator to get area in sq km
area <- st_transform(dt_coord, st_crs("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs")) %>%
  st_convex_hull() %>% st_area()  ##get area in sq km
write_clip(area) # 9902.15453168296 sq km 

# Plot the geometries -----------------------------------------------------

require(ggplot2)
world_map <- map_data('world') # check whether the GPS coordinates match expectations
ggplot(world_map) +
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  geom_point(data = dt_merged, aes(x = Longitude, y = Latitude), size = 2, shape = 21, colour = 'blue') +
  geom_point(aes(x = centroid[1], y = centroid[2]), size = 3, colour = 'green') +
  geom_sf(data = dt_coord %>% st_convex_hull(), colour = 'blue', fill = 'transparent') +
  coord_sf(xlim = c(-122, -118), ylim = c(32,36)) + # make sure these fit your points
  labs(x = NULL, y = NULL) +
  theme_minimal()
