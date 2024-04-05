

# Curation Script ---------------------------------------------------------

# Dataset: Bulgaria Wintering Waterbirds Srebarna Lake
# Location: Bulgaria
# Curator: Cher Chow
# Date: 16-Mar-2021

# Set up ------------------------------------------------------------------
# load the necessary packages
require(dplyr)
require(ggplot2)
require(stringr)
require(maps)
require(readxl)

rm(list=ls())
data.name <- 'Bulgaria_WinteringWaterbirds_SrebarnaLake'
curator <- 'CC'

# make sure your working directory is set before running these lines
mypath <- getwd() # get the folder that the data's located in
dt <- read_excel('Originals/HaasePilotto/S011-S019.xlsx', sheet=10, skip=2, col_names=T, na='')
sites <- read_excel('Originals/HaasePilotto/S011-S019.xlsx', sheet=11, skip=1, col_names=T, na='')
# this file is on wintering waterbirds in Bulgaria, with each sheet at a different site.
# remove columns we don't need
sites[,5:7] <- NULL

View(dt)

# add the metadata from the sites sheet
colnames(sites)[2:4] <- c('Latitude', 'Longitude', 'DepthElevation')
dt <- left_join(dt, sites, by="Site")
View(dt) # looks ok

# Structure check ---------------------------------------------------------

nrow(dt) # check dimensions
# 186 records
str(dt) # check structure

# we can remove the Site column since this is a single plot study
dt <- dt %>% select(!Site)

summary(dt)
# Abundance and/or biomass, latitude and longitude numeric? Y

# some renaming
colnames(dt)[which(colnames(dt)== 'Counts')] <- 'Abundance'
colnames(dt)[which(colnames(dt)== 'Sampling date')] <- 'Date'

# dates need splitting first
dt$Year <- year(dt$Date) %>% as.integer()
dt$Month <- month(dt$Date) %>% as.integer()
dt$Day <- day(dt$Date) %>% as.integer()
dt$Date <- NULL

# Year, month and day must be integers or factors?
str(dt[,c('Year', 'Month', 'Day')]) # yes

# Secondary fields such as trawl, plot, transect etc must be factors or integers? NA
# Secondary fields such as elevation or depth will normally be numeric unless in a treatment format. Y
# Taxonomic fields must be characters or factors? Y

# Primary field check -----------------------------------------------------

# ABUNDANCES
# No negative values, zeroes, or NAs in abundance/biomass fields.
min(dt$Abundance) > 0 # no zeroes? Y
sum(dt$Abundance == "" | dt$Abundance == ' ') == 0 # no blanks? Y

# YEAR MONTH DAY
# again, no negative values, 0s or NAs, and all are logical
# Year < 2020, month < 12, day < 31

summary(dt[c('Year', 'Month', 'Day')]) # looks good to me

# LAT LONG
sum(dt[c('Latitude', 'Longitude')] == "") == 0 # no blanks?
str_detect(dt %>% pull(Latitude, Longitude), '\\s') %>% sum() == 0 # no spaces?
sum(is.na(dt[c('Latitude', 'Longitude')])) == 0 # no NAs?

# Latitude constrained from -90 to 90.
# Longitude constrained -180 to 180.
summary(dt[c('Latitude', 'Longitude')]) # only one set of coordinates

world_map <- map_data('world') # check whether the GPS coordinates match expectations
world <- ggplot(world_map) + 
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  coord_fixed() + 
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())
points <- world + geom_point(data=dt %>% distinct(Longitude, Latitude), aes(x=Longitude, y=Latitude), shape=21)

points_zoom <- points + coord_fixed(xlim=c(25,35), ylim=c(40,50))
points_zoom # all looks good

rm(list=c('world_map', 'world', 'points', 'points_zoom'))

# Taxonomic field check ---------------------------------------------------

# No NAs in taxonomic fields, remove all non-organism records
sum(is.na(dt$Taxon) + 0) == 0

# misspellings check, but not taxonomic cleaning
# check that genera are genera, not family names (-idae/eae)
dt$Genus <- word(dt$Taxon, 1)
dt$Genus %>% str_detect(., 'idae$|ini$') %>% sum(. + 0)
dt$Species <- dt$Taxon # make a copy
# Check the species records
sort(unique(dt$Species))
# replace uncertain species with slashes
dt$Species <- dt$Species %>% str_replace_all(., 'cachinnans/ michahellis', 'sp1')

# manual inspection scan for potential typos/dupes
sort(unique(dt$Species))
dt$Species <- word(dt$Species, start=2, end=-1) # keep only specific epithet
sort(unique(dt$Genus))
dt$Taxon <- NULL

# Prepare raw data --------------------------------------------------------

# aggregate abundance records that are same species, plot, and survey day.
dt_merged <- dt %>% group_by_at(vars(-Abundance)) %>% 
  summarise(Abundance=sum(Abundance)) %>% ungroup() %>% arrange(Year, Genus, Species)
nrow(dt) - nrow(dt_merged) # any change in aggregating? 0 records
# now create empty columns needed to fit to template
dt_merged$Biomass <- ''
dt_merged$Plot <- ''
dt_merged$Family <- ''
dt_merged$SampleDescription <- as.factor(dt_merged$Year)
length(levels(dt_merged$SampleDescription)) # 33 samples

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
                         'Year')] %>% arrange(Year, Genus, Species)
View(dt_merged) # final check :)
summary(dt_merged)
str(dt_merged)

# Export final ------------------------------------------------------------

write.csv(dt_merged, paste0(data.name, '_rawdata_', curator, '.csv'), row.names=F)
library(clipr)
write_clip(dt_merged)
