# Curation Script ----------------------------------------------------------

# Dataset: Chingaza, Colombian Birds - Mist Net Capture
# Location: Parque Nacional Natural Chingaza, Cudinamarca, Colombia 
# Curator: Garrett Fundakowski
# Date started: 17-01-2023
# Last updated: 21-07-2023


# Set up -------------------------------------------------------------------
# load the necessary packages
require(tidyverse)
require(maps)
require(readxl)

# clear up the environment before starting
rm(list=ls())

# set the working directory via a pop-up window
setwd(file.choose() %>% dirname())

# read in the data
dt <- read_excel('./ChingazaData.xlsx', sheet=3, skip=1, col_names=T, na='')


# Remove unnecessary columns -----------------------------------------------
# subset the columns that we need
dt <- subset(dt, select=c(1,16,18))


# Go from wide format to long ----------------------------------------------
dt <- dt %>% 
  pivot_longer(cols = 2:3, names_to = "Year", names_prefix = "Captures per 1000 mist-net hours ", values_to = "Abundance")

# Fixing values of Year column
dt$Year[dt$Year == '(total) 2015/16'] <- 2016
dt$Year[dt$Year == '91/92'] <- 1992



# Structure check ----------------------------------------------------------
dim(dt) # 110 x 3
str(dt) # notice Year is chr; will fix in a moment
summary(dt)

# Abundance and/or biomass, latitude and longitude numeric?
# No Lat/Long; will deal with coordinates later
is.numeric(dt$Abundance) # TRUE
# Date should be POSIXct? 
# NA
# Year, month and day must be integers or factors?
is.factor(dt$Year) | is.integer(dt$Year) # FALSE
# Secondary fields such as trawl, plot, transect etc must be factors or integers? 
# NA
# Secondary fields such as elevation or depth will normally be numeric unless in a treatment format.
# NA
# Taxonomic fields must be characters or factors? 
is.factor(dt$Species) | is.character(dt$Species) # TRUE


# Structure fix ------------------------------------------------------------
# convert year, month, scode(aka - plot) to factor
dt$Year <- as.factor(dt$Year)

str(dt)


# Primary field check ------------------------------------------------------
# ABUNDANCE
# No negative values, zeroes, or NAs in abundance/biomass fields.
# From a quick look, there are 0s and NAs
# Replace all 0s with NAs
dt[dt == 0] = NA
# Remove NAs
dt <- dt[!is.na(dt$Abundance),]
#Check
min(dt$Abundance) > 0 # TRUE
sum(dt$Abundance == "") == 0 # TRUE

# YEAR MONTH DAY
# again, no negative values, 0s or NAs, and all are logical
# Year < 2020, month < 12, day < 31
summary(dt$Year) # looks good


# LAT LONG
# no blanks, no NAs
# Latitude constrained from -90 to 90.
# Longitude constrained -180 to 180.
# Manually input centroid coordinates from metadata provided by Haase
dt$Latitude <- as.numeric(rep('4.67', nrow(dt)))
dt$Longitude <- as.numeric(rep('-73.85', nrow(dt)))

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

# Secondary field ----------------------------------------------------------
# Plot and treatment must be inspected for NA, NULL, blank or values unspecified in source methods
# We must check for misspellings and revalue levels if needed
# NA


# Taxonomic field check ---------------------------------------------------
# No NAs in taxonomic fields, remove all non-organism records
# Misspellings check, but not taxonomic cleaning

# Check the species list for misspellings or non-BioTIME taxonomic convention names.
# Separate taxon names - Species is 'genus species'
dt$Genus <- word(dt$Species, 1)
dt$Species <- word(dt$Species, start=2)

# check again for mispellings
sort(unique(dt$Genus))
sort(unique(dt$Species))


# Prepare raw data --------------------------------------------------------
dt <- dt %>% arrange(Year, Genus, Species)
# now create empty columns needed to fit to template
dt$Biomass <- rep('', dim(dt)[1])
dt$Family <- rep('', dim(dt)[1])
dt$Plot <- rep('', dim(dt)[1])
dt$DepthElevation <- rep('', dim(dt)[1])
dt$Day <- rep('', dim(dt)[1])
dt$Month <- rep('', dim(dt)[1])
dt$StudyID <- rep('', dim(dt)[1])
dt$SampleDescription <- rep('', dim(dt)[1])

# aggregate abundance records that are same species, plot, and survey day.
dt_merged <- dt %>% group_by(Biomass, Family, Genus, Species, SampleDescription, Plot, Latitude, Longitude, DepthElevation, Day, Month, Year, StudyID) %>% 
  summarise(Abundance=sum(Abundance)) %>% ungroup() %>% arrange(Year, Genus, Species)
dim(dt)[1]-dim(dt_merged)[1] # any change in aggregating? no

# save the dataset name as an object
dataset.name <- 'Chingaza_Colombia_Birds_MistNetCapture'
# fill in sampling event with unique info
dt_merged$SampleDescription <- as.factor(with(dt_merged, paste(Year, sep='_')))
length(levels(dt_merged$SampleDescription)) # 2 samples

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
