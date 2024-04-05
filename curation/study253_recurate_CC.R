## carpenter_2016
require(dplyr)
require(stringr)
require(lubridate)

dataset_id <- 'study253_recurate'
dt <- read.csv('Originals/study253/ntl37_v11.csv', header = T)
sites <- read.csv('Originals/study253/sites.csv', header = T)

# Structure check ---------------------------------------------------------

dim(dt) # check dimensions, 43666 records
str(dt) # check structure

# simple column reorganise
dt <- dt %>% select(!c(species_code)) %>%
   rename(Abundance = density, Biomass = avg_length, Year = year4)
# split date
dt$sampledate <- as.POSIXct(dt$sample_date)
dt$Month <- month(dt$sampledate)
dt$Day <- day(dt$sampledate)
summary(dt)
# Abundance and/or biomass, latitude and longitude numeric? Y
dt$individuals_measured <- NULL # far more NAs than density
# Year, month and day must be integers or factors? Y
# Secondary fields such as trawl, plot, transect etc must be factors or integers? Y
dt$station <- as.factor(dt$station)
n_distinct(dt$station) # just 1 and 2
sort(unique(dt$lakeid))
dt$lakeid <- str_to_upper(dt$lakeid)
# Secondary fields such as elevation or depth will normally be numeric unless in a treatment format. NA here
# Date should be POSIXct. NA. split already.
dt$sample_date <- NULL
# Taxonomic fields must be characters or factors? Y

# join in sites
dt <- left_join(dt, sites[-1], by="lakeid")

# Primary field check -----------------------------------------------------

# ABUNDANCES
# No negative values, zeroes, or NAs in abundance/biomass fields.
sum(dt$Abundance == "", na.rm=T) # no blanks
sum(dt$Biomass == "", na.rm=T) # no blanks
summary(dt)

dt %>% filter(Biomass == 0) # check the 0s in biomass
dt %>% filter(Abundance == 0) # check the 0s in biomass
dt %>% filter(is.na(Abundance)) %>% View
dt %>% filter(Abundance == 0) %>% View
# eliminate 0 or NA records
dt <- dt[-which(with(dt, Abundance == 0 & is.na(Biomass))),]
43666 - nrow(dt) # removed 6 records with zeroes in Abundance
summary(dt)

# YEAR MONTH DAY
# again, no negative values, 0s or NAs, and all are logical
# Year < 2020, month < 12, day < 31
# dates all good

# LAT LONG
# manually import lat longs from US LTER website
summary(sites)

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
# rm(points,world, world_map, points_zoom)

# Taxonomic field check ---------------------------------------------------

# No NAs in taxonomic fields, remove all non-organism records
sum(is.na(dt$species_name)+0)
# misspellings check, but not taxonomic cleaning
# check that genera are genera, not family names (-idae/eae)
dt$Species <- dt$species_name %>% str_to_lower # make a copy where we make changes
sort(unique(dt$Species))
# two stage checking
dt$Species[str_count(dt$Species, '\\s') == 1] %>% unique %>% sort
# visual check the two word taxa first
'copepod nauplii' = 'Copepoda'
dt$Species <- str_replace(dt$Species, 'skistodiaptomus ', 'skistodiaptomus sp')
dt$Family <- '' # empty column to populate
dt$Family[str_detect(dt$Species, 'copepod')] <- 'Copepoda' # move to familly
dt$Family[str_detect(dt$Species, 'rotifer$')] <- 'Rotifera'
dt$Species[str_detect(dt$Species, 'copepod|rotifer$')] <- '' # delete from species

dt$Species[str_count(dt$Species, '\\s') == 0] %>% unique %>% sort
dt$Family[str_detect(dt$Species, 'idae$')] <- dt$Species[str_detect(dt$Species, 'idae$')]
dt$Species[str_detect(dt$Species, 'idae$')] <- ''
sort(unique(dt$Family))
dt <- dt %>% filter(!str_detect(Species, 'unidentified$|unknown$')) # remove
# now, I can assume all single word taxa names are uncertain genus IDs
sort(unique(dt$Species))
dt$Species[str_count(dt$Species, '\\s') > 1] %>% sort %>% unique

dt$Genus <- word(dt$Species, 1) %>% str_to_title # split genus
sort(unique(dt$Genus))
dt$Species <- word(dt$Species, 2) # everything but the first word
sort(unique(dt$Species))
dt[is.na(dt$Species) & dt$Genus != '', c('Genus', 'Species')] %>% distinct %>% View
dt$Species[is.na(dt$Species) & dt$Genus != ''] <- 'sp'
View(dt %>% distinct(Family, Genus, Species))
dt$Family <- str_to_title(dt$Family)
dt$species_name <- NULL # remove original column

# Prepare raw data --------------------------------------------------------

# check potential sample structure before merge
dt %>% distinct(Year, Month, Day, lakeid, station) %>% arrange(Year, Month, Day, lakeid, station) # too many
dt %>% group_by(lakeid, station) %>% summarise(nyears = n_distinct(Year), nmonth = n_distinct(Month))
# pretty much monthly with full annual coverage
# station not informative
dt$station <- NULL
# aggregate abundance records that are same species, plot, and survey day.
dt_merged <- dt %>% group_by_at(vars(-c(Abundance, Biomass))) %>%
   summarise(Abundance=sum(Abundance), Biomass=sum(Biomass)) %>% ungroup() %>% 
  arrange(Year, Month, lakeid, Family, Genus, Species)
nrow(dt) - nrow(dt_merged) # check if there's any difference, 140 rows "lost" from merge

dt_merged$SampleDescription <- as.factor(with(dt_merged, paste(Year, Month, lakeid, sep='_')))
length(levels(dt_merged$SampleDescription)) # check number of sampling events, 2091

# add in blank columns
dt_merged$Plot <- ''
dt_merged$StudyID <- ''

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
                         'StudyID')]
View(dt_merged) # final check :)

# Export final raw data ------------------------------------------------------------

write.csv(dt_merged, paste0(dataset_id, '_rawdata.csv'), row.names=F)
clipr::write_clip(dt_merged)

# Calculate spatial metadata ----------------------------------------------

# load libraries
library(sf)
library(clipr)

# 1. Convert data points into point spatial object
# assumes your rawdata dataframe is called rawdata
dt_coord <- dt_merged %>% select(Longitude, Latitude) %>% distinct() %>%
   st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()

# 2. Calculate convex hull, area and centroid
centroid <- dt_coord %>% st_convex_hull() %>% st_centroid() %>% unlist # get centroid
write_clip(centroid[c(2,1)]) # copy as lat-long

# transform to mercator to get area in sq km
area <- st_transform(dt_coord, st_crs("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs")) %>%
   st_convex_hull() %>% st_area()  ##get area in sq km
write_clip(area)
