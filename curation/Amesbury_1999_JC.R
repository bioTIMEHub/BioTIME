# Curation Script ---------------------------------------------------------

# Dataset: Amesbury_1999 (War-in-the-Pacific National Historical Park marine biological survey)
# Location: Guam
# Curator: James Cant
# Date: 06-Jun-2023

# ------------------------------------------------------------------
# load the necessary packages
require(tidyverse)
require(maps)
require(readxl)
require(stringr)
require(dplyr)
require(clipr)

# clear workspace
rm(list=ls())

# assign book-keeping details.
data.name <- 'Amesbury_1999'
curator <- 'JC'

# READ IN DATA ---------------------------------------------------
# identify the folder that the data is located in
mypath <- 'E:/BioTime/Amesbury 1999/'
# identify folder for saving
fileSave <- 'E:/BioTime/CuratedData/'
# read in data
dt <- readRDS(paste0(mypath, data.name, '.rds'))
View(dt)
# This file contains reef fish counts across 6 transects repeated in 1974 and 1999 in Agat Bay, Guam.

# RESTRUCTURE DATA ---------------------------------------------------
# restructure data into 'long' format 
# save transect and year details
Transect <- word(gsub('_', ' ', names(dt)[-1]), 1)
Year <- word(gsub('_', ' ', names(dt)[-1]), 2)
# reshape data
dt <- reshape(dt, varying = list(dput(names(dt)[-1])), v.names = 'Abundance', idvar = 'species', direction = 'long')
# clean up row names
rownames(dt) <- 1:dim(dt)[1]
# duplicate 'times' variable generated in stacking and rename appropriately
dt$Sample <- dt$time
colnames(dt)[2] <- 'Year'
# reassign survey metadata
index <- min(dt$Sample):max(dt$Sample)
dt <- dt %>%
  mutate(Sample = recode(Sample, !!!setNames(Transect, index)),
         Year = recode(Year, !!!setNames(Year, index)))

# ADD SPATIAL INFORMATION ---------------------------------------------------
dt$Latitude <- rep(13.401193, dim(dt)[1])
dt$Longitude <- rep(144.662870, dim(dt)[1]) # coordinates correspond with Agat Bay, sourced from google.
dt$DepthElevation <- rep(NA, dim(dt)[1])

# REFORMAT TAXONIMIC DETAILS ---------------------------------------------------
# No NAs in taxonomic fields, remove all non-organism records
sum(is.na(dt$species) + 0) == 0

# misspellings check, but not taxonomic cleaning
# check that genera are genera, not family names (-idae/eae)
dt$Genus <- word(dt$species, 1)
dt$Genus %>% str_detect(., 'idae$|ini$') %>% sum(. + 0)
# Manually check the offending entries 
dt[which(str_detect(dt$Genus, 'idae$|ini$')),] # most of these entries are not associated with any abundance counts, and so will be dropped
dt[which(str_detect(dt$Genus, 'idae$|ini$')),]$Genus <- NA
dt <- dt[!(is.na(dt$Genus)),]
sort(unique(dt$Genus)) # Manual inspection to confirm
# check species names
dt$Species <- word(dt$species, 2)
sort(unique(dt$Species))
# unify notation for unknown species
dt$Species <- dt$Species %>% str_replace_all(., 'sp.', 'sp')
# remove unnecessary variable
dt$species <- NULL

# CHECK STRUCTURE OF VARIABLES ---------------------------------------------------------
nrow(dt) # check dimensions
# 1376 records
str(dt) # check structure

# ABUNDANCE
# This dataset includes abundance counts and presence absence identifiers.
# Remove presence only data and convert abundance counts to numeric
dt$Abundance <- as.numeric(dt$Abundance)
# No negative values, zeroes, or NAs in abundance/biomass fields.
dt <- dt[!(is.na(dt$Abundance)),]
min(dt$Abundance) > 0 # no zeroes?
sum(dt$Abundance == "" | dt$Abundance == ' ') == 0 # no blanks?

# DATES
# Ensure Year is a factor
dt$Year <- as.factor(dt$Year)
dt$Day <- rep(NA, dim(dt)[1]) # add missing variables
dt$Month <- rep(NA, dim(dt)[1])
# No negative values, 0s or NAs, and all are logical
# Year < 2020, month < 12, day < 31
summary(dt[c('Year', 'Month', 'Day')])

# PLOT DETAILS
dt$Sample <- as.factor(dt$Sample)

# TAXONOMIC DETAILS
dt$Genus <- as.factor(dt$Genus)
dt$Species <- as.factor(dt$Species)

# LAT LONG
# check coordinates are in numeric format
is.numeric(dt$Latitude); is.numeric(dt$Longitude) 
sum(dt[c('Latitude', 'Longitude')] == "") == 0 # no blanks?
str_detect(dt %>% pull(Latitude, Longitude), '\\s') %>% sum() == 0 # no spaces?
sum(is.na(dt[c('Latitude', 'Longitude')])) == 0 # no NAs?
# Latitude constrained from -90 to 90.
# Longitude constrained -180 to 180.
summary(dt[c('Latitude', 'Longitude')]) # only one set of coordinates

# Check the coordinates match with the expected location of the data
world_map <- map_data('world')
world <- ggplot(world_map) + 
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  coord_fixed() + 
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())
points <- world + geom_point(data=dt %>% distinct(Longitude, Latitude), aes(x=Longitude, y=Latitude), shape=21)

points_zoom <- points + coord_fixed(xlim=c(135,155), ylim=c(0,20))
points_zoom # all looks good
# clean memory
rm(list=c('world_map', 'world', 'points', 'points_zoom'))

# PREPARE RAW DATA --------------------------------------------------------

# aggregate abundance records that are same species, plot, and survey day.
dt_merged <- dt %>% group_by(Latitude, Longitude, DepthElevation, Sample, Year, Genus, Species) %>% 
  summarise(Abundance=sum(Abundance)) %>% ungroup() %>% arrange(Year, Sample, Genus, Species)
nrow(dt) - nrow(dt_merged) # any change in aggregating? 4 records condensed
# now create empty columns needed to fit to template
dt_merged$Plot <- rep('', nrow(dt_merged))
dt_merged$Biomass <- rep('', nrow(dt_merged))
dt_merged$Family <- rep('', nrow(dt_merged))
dt_merged$StudyID <- rep('', nrow(dt_merged))
dt_merged$Day <- rep('', nrow(dt_merged))
dt_merged$Month <- rep('', nrow(dt_merged))
dt_merged$SampleDescription <- as.factor(with(dt_merged, paste(data.name, Latitude, Longitude, Sample, Year, sep='_')))
length(levels(dt_merged$SampleDescription)) # 8 samples (corresponds with the different transects run)

# Remove columns that don't fit within BIOTIME format (after ensuring necessary details retained in sample description)
dt_merged$Sample <- NULL

# reorder columns to match BioTIME format
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
                         'StudyID')] %>% arrange(Year, Month, Day, Genus, Species)
View(dt_merged) # final check :)
summary(dt_merged)
str(dt_merged)

# Export final ------------------------------------------------------------

write.csv(dt_merged, paste0(fileSave, data.name, '_rawdata_', curator, '.csv'), row.names=F)
write_clip(dt_merged) # save data to clipboard

# -------------------------------------------- End of Code ------------