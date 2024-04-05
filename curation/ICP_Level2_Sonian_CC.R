

# Curation Script ---------------------------------------------------------

# Dataset: Ground vegetation data ICP Forests Level II plot Sonian
# Location: Belgium
# Curator: Cher Chow
# Note* not a whole lot to clean bc contributor filled out template

# Set up ------------------------------------------------------------------
# load the necessary packages
require(tidyverse)
require(maps)
require(stringr)
require(lubridate)
require(readxl)

# make sure your working directory is set before running these lines
dt <- readxl::read_excel('Originals/INBO_Sonian.xlsx', sheet=1, col_names=T, na='')


# Structure check ---------------------------------------------------------

dim(dt) # check dimensions
str(dt) # check structure
summary(dt)
# Abundance and/or biomass, latitude and longitude numeric? Y
# Year, month and day must be integers or factors? N
dt$Year <- as.integer(dt$Year)
dt$Month <- as.integer(dt$Month)
dt$Day <- as.integer(dt$Day)
# Secondary fields such as trawl, plot, transect etc must be factors or integers? N
dt$Plot <- as.factor(dt$Plot)
str(dt)
# Secondary fields such as elevation or depth will normally be numeric unless in a treatment format. NA here
# Date should be POSIXct (not applicable in this case) NA here
# Taxonomic fields must be characters or factors? Y


# Primary field check -----------------------------------------------------

# ABUNDANCES
# No negative values, zeroes, or NAs in abundance/biomass fields.

min(dt$Abundance) # check the minimum (no zeroes)
sum(dt$Abundance=="") # should have no blanks

# YEAR MONTH DAY
# again, no negative values, 0s or NAs, and all are logical
# Year < 2020, month < 12, day < 31

summary(dt[,11:13]) # looks good to me

# LAT LONG
# no blanks, no NAs
# Latitude constrained from -90 to 90.
# Longitude constrained -180 to 180.

min(dt$Latitude)
min(dt$Longitude)
max(dt$Latitude)
max(dt$Longitude)
count(dt %>% filter(Latitude > 90 | Latitude < -90 | Longitude > 180 | Longitude < -180))

world_map <- map_data('world') # check whether the GPS coordinates match expectations
world <- ggplot(world_map) + 
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  coord_fixed() + 
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())
points <- world + geom_point(data=dt, aes(x=Longitude, y=Latitude), shape=21)
points

# Lat and long are switched
colnames(dt)[8:9] <- c('Longitude', 'Latitude') # switch
world <- ggplot(world_map) + # replot to double check the fix
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  coord_fixed() + 
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())
points <- world + geom_point(data=dt, aes(x=Longitude, y=Latitude), shape=21)
points

points_zoom <- points + coord_fixed(xlim=c(-10,20), ylim=c(35,60))
points_zoom

# Taxonomic field check ---------------------------------------------------

# No NAs in taxonomic fields, remove all non-organism records
# misspellings check, but not taxonomic cleaning
# check that genera are genera, not family names (-idae/eae)

# first, records with undefined species epithets into the BioTIME format of sp without period
dt$Species <- dt$Species %>% str_replace_all(., 'sp.$', 'sp')
sort(unique(dt$Species))
# there is a species aggregate group
dim(dt %>% filter(Species == 'fructicosus group'))[1] # number of records that fall under this group
# How to deal with fructicosus group?
dt$Species <- dt$Species %>% str_replace_all(., 'fructicosus group$', 'fructicosus agg.')
sort(unique(dt$Genus))

dt %>% filter(Species == 'sp') %>% select(Genus, Species) # check undefined species records
# all looks ok

# Prepare raw data --------------------------------------------------------
dt$Biomass <- dt$Abundance
dt$Abundance <- NA
# aggregate abundance records that are same species, plot, and survey day.
dt_merged <- dt %>% group_by_at(vars(-Biomass)) %>% 
  summarise(Biomass=sum(Biomass)) %>% ungroup()
nrow(dt) - nrow(dt_merged)
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
                         'StudyID')] %>% arrange(Year, Plot, Genus, Species)
View(dt_merged) # final check :)
summary(dt_merged)
str(dt_merged)
dt_merged$SampleDescription <- as.factor(with(dt_merged, paste(Year, Plot, sep='_')))
summary(dt_merged$SampleDescription)

# Export final ------------------------------------------------------------

write.csv(dt_merged, 'ICP_Level2_Sonian_rawdata_CC.csv', row.names=F)
library(clipr)
write_clip(dt_merged)

