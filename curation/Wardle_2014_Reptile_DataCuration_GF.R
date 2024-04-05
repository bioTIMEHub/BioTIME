# Curation Script --------------------------------------------------------------

# Dataset: Wardle_2014_Reptiles
# Location: Simpson Desert, Western Queensland, Australia
# Curator: Garrett Fundakowski
# Date started: 06-07-2023
# Last updated: 19-07-2023


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
dt <- read_csv('./derg_reptile_data_1990+_p902t1207.csv')


# Structure check --------------------------------------------------------------
dim(dt) # 6956 x 13
str(dt) 
summary(dt)

# Abundance and/or biomass, latitude and longitude numeric?
is.numeric(dt$captures_100tn) # TRUE
# Date should be POSIXct? 
is.POSIXct(dt$month_year) # FALSE
# Year, month and day must be integers or factors?
is.numeric(dt$year) | is.factor(dt$year) # TRUE
# Secondary fields such as trawl, plot, transect etc must be factors or integers? 
# NA
# Secondary fields such as elevation or depth will normally be numeric unless in a treatment format.
# NA
# Taxonomic fields must be characters or factors? 
is.factor(dt$species) | is.character(dt$species) # TRUE
is.factor(dt$family) | is.character(dt$family) # TRUE


# Structure fix ----------------------------------------------------------------
# convert year, month, day, transect to factor
dt$date <- parse_date(dt$month_year, format = "%b.%y")
dt$Month <- month(dt$date)

colnames(dt)[1] <- 'Year'
colnames(dt)[12] <- 'Abundance'
colnames(dt)[13] <- 'Family'

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

# LAT LONG
# no blanks, no NAs
# Latitude constrained from -90 to 90.
# Longitude constrained -180 to 180.
dt$Latitude <- -23.59983
dt$Longitude <- 138.235505

#plot to visually check whether the GPS coordinates match expectations
world_map <- map_data('world')
world <- ggplot(world_map) + 
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  coord_fixed() + 
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())
points <- world + geom_point(data=dt[1,], aes(x=Longitude, y=Latitude), shape=21)
points_zoom <- points + coord_fixed(xlim=c(100,175), ylim=c(-45,-5))
points_zoom # looks good
points1 <- world + geom_point(data=dt, aes(x=Longitude, y=Latitude), shape=21)
points_zoom1 <- points1 + coord_fixed(xlim=c(130,145),ylim=c(-30,-15))
points_zoom1 # looks good


# Secondary field --------------------------------------------------------------
# Plot and treatment must be inspected for NA, NULL, blank or values unspecified in source methods
# We must check for misspellings and revalue levels if needed
colnames(dt)[4] <- 'Plot'
dt$Plot <- as.factor(dt$Plot)
length(levels(dt$Plot)) # 25


# Taxonomic field check --------------------------------------------------------
# No NAs in taxonomic fields, remove all non-organism records
# Misspellings check, but not taxonomic cleaning
dt <- dt[!(dt$species == 'No captures'),]
sort(unique(dt$species)) # Note: Geckos, Frogs, Skinks, Snakes, Unknown
# Skinks, Geckos, & Elapids have Family name
# remove Skinks, Geckos, & Elapids from species column
dt$species[dt$species=='Skinks'] <- NA
dt$species[dt$species=='Geckos'] <- NA
dt$species[dt$species=='Elapids'] <- NA
# Unknowns[17], Snakes[3], and Frogs[1] don't have Family name
dt$Family[dt$species=='Snakes'] <- 'Serpentes'
dt$species[dt$species=='Snakes'] <- NA
dt$Family[dt$species=='Frogs'] <- 'Anura'
dt$species[dt$species=='Frogs'] <- NA
dt$species[dt$species=='Unknown'] <- NA
dt <- dt[!(is.na(dt$species) & is.na(dt$Family)),]

# Separate taxon names - species is 'Genus species'
dt$Genus <- word(dt$species, 1)
dt$Species <- word(dt$species, start=2)

# check for mispellings
sort(unique(dt$Family))
sort(unique(dt$Genus))
sort(unique(dt$Species))
dt$Species[dt$Species=='sp.'] <- 'sp'

# Prepare raw data --------------------------------------------------------
# Remove unnecessary columns
dt$month_year <- NULL
dt$site_name <- NULL
dt$trip_no <- NULL
dt$nights <- NULL
dt$no_traps <- NULL
dt$total_trap_nights <- NULL
dt$species <- NULL
dt$recap_this_trip <- NULL
dt$captures <- NULL
dt$date <- NULL

dt <- dt %>% arrange(Year, Genus, Species)
# now create empty columns needed to fit to template
dt$StudyID <- rep('', dim(dt)[1])
dt$SampleDescription <- rep('', dim(dt)[1])
dt$DepthElevation <- rep('', dim(dt)[1])
dt$Biomass <- rep('', dim(dt)[1])
dt$Day <- rep('', dim(dt)[1])

# save the dataset name as an object
dataset.name <- 'Wardle_2014_Reptile'
# fill in sampling event with unique info
dt$SampleDescription <- as.factor(with(dt, paste(Year, Month, Plot, sep='_')))
length(levels(dt$SampleDescription)) # 1287 samples

# aggregate abundance records that are same species, plot, and survey day.
dt_merged <- dt %>% group_by(Biomass, Family, Genus, Species, SampleDescription, Plot, Latitude, Longitude, DepthElevation, Day, Month, Year, StudyID) %>% 
  summarise(Abundance=sum(Abundance)) %>% ungroup() %>% arrange(Year, Genus, Species)
dim(dt)[1]-dim(dt_merged)[1] # any change in aggregating? yes, 2

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
summary(dt_merged) # Note NA's
# check reveals it's a row of NA's
dt_merged <- dt_merged[!(is.na(dt_merged$Abundance)),]

str(dt_merged)


# Export final ------------------------------------------------------------

setwd(file.choose() %>% dirname())
write.csv(dt_merged, paste0(getwd(), dataset.name, '_rawdata_GF.csv'), row.names=F)
