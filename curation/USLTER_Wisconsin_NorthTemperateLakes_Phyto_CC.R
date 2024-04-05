## carpenter_2016
require(dplyr)
require(stringr)
require(lubridate)

rm(list = ls())

dataset_id <- 'USLTER_Wisconsin_NorthTemperateLakes_Phyto'
dt <- read.csv('Originals/USLTER_Wisconsin_NorthTemperateLakes_Phyto/cascade_phytoplankton_v0.1.csv_upload.csv', header = T)

# Structure check ---------------------------------------------------------

dim(dt) # check dimensions, 15988 records
str(dt) # check structure

# simple column reorganise
dt <- dt[1:10] # remove the other units of abundance/volume
# concentration as density

# only keep Paul Lake (control)
dt <- dt %>% filter(lakename == 'Paul Lake') %>% select(!c(lakename, lakeid))

summary(dt)

# split date
dt$Year <- year(dt$sampledate)
dt$Month <- month(dt$sampledate)
dt$Day <- day(dt$sampledate)
summary(dt)
# 860 NAs in date
dt %>% filter(is.na(Year)) %>% distinct(year4, Year, Month, Day) # only 2013-2015 for three lakes...
# check sampling consistency
dt %>% summarise(nyear = n_distinct(year4), 
                                        nmonth = n_distinct(Month),
                                        minY = min(year4),
                                        maxY = max(year4))

dt$sampledate <- NULL
dt$Year <- NULL
colnames(dt)[1] <- 'Year'

str(dt)
# Abundance and/or biomass, latitude and longitude numeric? Y
# Year, month and day must be integers or factors? Y
# Secondary fields such as trawl, plot, transect etc must be factors or integers? Y
# Secondary fields such as elevation or depth will normally be numeric unless in a treatment format. NA here
# Date should be POSIXct. NA. split already.
# Taxonomic fields must be characters or factors? Y

# Primary field check -----------------------------------------------------

# ABUNDANCES
colnames(dt)[7] <- 'Abundance'
# No negative values, zeroes, or NAs in abundance/biomass fields.
sum(dt$Abundance == "", na.rm=T) # no blanks
summary(dt)

View(dt %>% filter(Abundance == 0)) # check the 0s in biomass
# common with certain species
dt %>% filter(genus == 'Synura') %>% View # not always
View(dt %>% filter(is.na(Abundance)))
# eliminate 0 or NA records
dt <- dt %>% filter(Abundance > 0)

# YEAR MONTH DAY
# again, no negative values, 0s or NAs, and all are logical
# Year < 2020, month < 12, day < 31

summary(dt)

# LAT LONG
# Exclusion of lakes with unknown disturbance history (Alban)
dt$Latitude <- 46.2512
dt$Longitude <- -89.5034

# visual check
# require(ggplot2)
# world_map <- map_data('world') # check whether the GPS coordinates match expectations
# world <- ggplot(world_map) +
#    geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
#    coord_fixed() +
#    labs(x='Longitude', y='Latitude') +
#    theme_bw() + theme(panel.grid=element_blank())
# points <- world + geom_point(data=sites, aes(x=Longitude, y=Latitude), shape=21)
# points
# points + coord_fixed(xlim=c(-93,-85), ylim=c(40,50))
# rm(points, world_map, world)

# Taxonomic field check ---------------------------------------------------

# No NAs in taxonomic fields, remove all non-organism records
sum(is.na(with(dt, paste(division, genus, species, sep = ' ')))+0)
# misspellings check, but not taxonomic cleaning
# check that genera are genera, not family names (-idae/eae)
dt$species[is.na(dt$genus) == F & is.na(dt$species)] <- 'sp'
sort(unique(with(dt, paste(genus, species, sep = ' '))))
dt %>% distinct(division, genus, species, description) %>% 
  arrange(division, genus, species) %>% View
# some of the descriptions suggest there are consistent morphospecies being named here
# put them into species column
dt$description[str_detect(dt$description, 'sp\\.\\s*[:digit:]+') & is.na(dt$description) == F]
dt$species[str_detect(dt$description, 'sp\\.\\s*[:digit:]+') & is.na(dt$description) == F] <- dt$description[str_detect(dt$description, 'sp\\.\\s*[:digit:]+') & is.na(dt$description) == F] %>% str_remove(., '(?<=sp)\\.\\s*(?=[:digit:]+)') %>% word(., 1) %>% str_remove(., '[:punct:]$')

sort(unique(with(dt, paste(genus, species, sep = ' '))))
dt$species <- str_replace_all(dt$species, 'uvella\\/sphagnicola', 'sp')
sort(unique(with(dt, paste(genus, species, sep = ' '))))

# check higher level
sort(unique(dt$genus))
sort(unique(dt$division))
dt$division[dt$division == 'Miscellaneous'] <- NA
dt$daynum <- NULL
dt$description <- NULL
colnames(dt)[2:4] <- c('Family', 'Genus', 'Species')
dt <- dt %>% filter(is.na(Family) == F, is.na(Genus) == F, is.na(Species) == F) # remove taxonomic NA records

# Prepare raw data --------------------------------------------------------

dt_2013 <- dt %>% filter(Year >= 2013)
dt_1995 <- dt %>% filter(Year <= 1995)
nrow(dt_2013) + nrow(dt_1995) == nrow(dt) # all rows accounted for?

# sampling is done monthly for the two Long Lakes in 1989 for unknown reasons, 
# weekly for most sites-years
# after 2013 sampling frequency is yearly

# aggregate abundance records that are same species, plot, and survey day.
dt_1995_m <- dt_1995 %>% group_by_at(vars(-c(Abundance))) %>%
   summarise(Abundance=sum(Abundance)) %>% ungroup() %>% arrange(Year, Month, Day, Family, Genus, Species)
nrow(dt_1995) - nrow(dt_1995_m) # check if there's any difference, 1600 rows "lost" from merge

dt_2013_m <- dt_2013 %>% group_by_at(vars(-c(Abundance))) %>%
  summarise(Abundance=sum(Abundance)) %>% ungroup() %>% arrange(Year, Month, Day, Family, Genus, Species)
nrow(dt_2013) - nrow(dt_2013_m) # check if there's any difference, 498 rows "lost" from merge

dt_1995_m$SampleDescription <- as.factor(with(dt_1995_m, paste(Year, Month, Day, sep='_')))
# check number of samples per year-site
dt_1995_m %>% distinct(SampleDescription, Year, Month, Day) %>% 
  group_by(Year) %>% summarise(samplen = n_distinct(SampleDescription)) %>% View

dt_2013_m$SampleDescription <- dt_2013_m$Year

# add in blank columns
dt_2013_m <- dt_2013_m %>% mutate(Plot = '', Biomass = '', DepthElevation = '', StudyID = '')
dt_1995_m <- dt_1995_m %>% mutate(Plot = '', Biomass = '', DepthElevation = '', StudyID = '')

dt_1995_m <- dt_1995_m[c('Abundance',
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
                         'StudyID')]

dt_2013_m <- dt_2013_m[c('Abundance',
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
                         'StudyID')]

# everything looks good

# Export final raw data ------------------------------------------------------------

write.csv(dt_1995_m, paste0(dataset_id, '_1984-1995_rawdata_CC.csv'), row.names=F)
write.csv(dt_2013_m, paste0(dataset_id, '_2013-2015_rawdata_CC.csv'), row.names=F)

# Calculate spatial metadata ----------------------------------------------

# load libraries
library(sf)
library(clipr)

# 1. Convert data points into point spatial object
# assumes your rawdata dataframe is called rawdata
dt_coord <- dt_2013_m %>% select(Longitude, Latitude) %>% distinct() %>%
   st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()

# 2. Calculate convex hull, area and centroid
centroid <- dt_coord %>% st_convex_hull() %>% st_centroid() %>% unlist # get centroid
write_clip(centroid[c(2,1)]) # copy as lat-long

# transform to mercator to get area in sq km
area <- st_transform(dt_coord, st_crs("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs")) %>%
   st_convex_hull() %>% st_area()  ##get area in sq km
write_clip(area)
