

# Curation Script ---------------------------------------------------------

# Dataset: SeagrassNet monitoring, southern Andaman coast of Thailand, 2006-2009
# Location: Thailand
# Curator: Cher Chow
# Date: 02-Jun-2021

# Set up ------------------------------------------------------------------
# load the necessary packages
require(dplyr)
require(ggplot2)
require(stringr)
require(lubridate)
require(maps)
require(readxl)

rm(list=ls())
data.name <- 'SeagrassNet_SThailand_Seagrass'
curator <- 'CC'

# make sure your working directory is set before running these lines
dt <- read.csv('Originals/SeagrassNet_SThailand/ERDP-2020-10.2.1-data.csv', header=T)
sites <- read.csv('Originals/SeagrassNet_SThailand/ERDP-2020-10.4.1-sites.csv', header=T)
View(dt)


# Structure organisation --------------------------------------------------

# add the metadata from the sites sheet
colnames(sites)[1:2] <- c('Longitude', 'Latitude')
# transect coordinates are finer than what is present in the data.
# I'll keep the midpoint coord of the transect (A/B/C 25)
n_distinct(sites$Site)
sites <- sites %>% filter(str_detect(Transect, '[:upper:]{1}\\s*25')) # should result in 21 rows, 3 transects for 7 sites
sites$Transect <- str_remove_all(sites$Transect, '\\s*25')

sort(unique(sites$Site))
sort(unique(dt$Site))
# site names don't match. Condense them while we're at it
# NationalPark_sitenumber, e.g. HCM_1
sites$Site <- str_remove_all(sites$Site, '\\s*NP Site\\s*')
dt$Site <- str_remove_all(dt$Site, '\\s*NP Site\\s*')
dt$Site <- str_replace_all(dt$Site, 'Phetra', 'Petra')

sum((sort(unique(sites$Site)) == sort(unique(dt$Site))) + 0) # match?
sites <- sites[-c(3,4)] # get rid of UTM coordinates
# transect also has to match for joining
sort(unique(dt$Transect))
dt$Transect <- str_remove_all(dt$Transect, '^Transect\\s*') # Transect and space
dt$Transect <- str_remove_all(dt$Transect, '\\s+$') # spaces at the end
sort(unique(dt$Transect)) # check

dt <- left_join(dt, sites, by=c('Site', 'Transect'))
View(dt) # looks ok
rm(sites) # remove now that we've joined

# remove the biomass measurements for core species
dt <- dt[-c(7,8)]

# concatenate quadrat, transect, site for plot name
colnames(dt)[6] <- 'Biomass'

# Species in codes, will need to create a replacement object
sort(unique(dt$Species))
species <- c('0', # keep the zero for now until our checks later
             'Cymodocea rotunda',
             'Enhalus acoroides',
             'Halophila minor',
             'Halophila ovalis',
             'Halodule pinifolia',
             'Halodule uninervis',
             'Thalassia hemprichii')
names(species) <- sort(unique(dt$Species))
dt$Species <- str_replace_all(dt$Species, species)
View(dt) # check

# Structure check ---------------------------------------------------------

nrow(dt) # check dimensions
# 3081 records
str(dt) # check structure
summary(dt)
# Abundance and/or biomass, latitude and longitude numeric? Y

# dates need to be coerced
dt$Date <- as.POSIXct(dt$Date, tryFormats=('%d.%m.%Y'))
summary(dt$Date)

# Year, month and day must be integers or factors?
dt$Year <- year(dt$Date) %>% as.integer()
dt$Month <- month(dt$Date) %>% as.integer()
dt$Day <- day(dt$Date) %>% as.integer()
dt$Date <- NULL

# logical?
summary(dt[,c('Year', 'Month', 'Day')]) # yes

# Secondary fields such as trawl, plot, transect etc must be factors or integers?

# Secondary fields such as elevation or depth will normally be numeric unless in a treatment format. NA
# Taxonomic fields must be characters or factors? Y

# Primary field check -----------------------------------------------------

# ABUNDANCES
# No negative values, zeroes, or NAs in abundance/biomass fields.
sum(is.na(dt$Biomass)+0)
min(dt$Biomass, na.rm=T) # no zeroes? Y
dt <- dt %>% filter(!is.na(Biomass)) %>% filter(!Biomass == 0)
str_which(dt$Biomass, '\\s') %>% length() # no blanks? Y

# YEAR MONTH DAY
# again, no negative values, 0s or NAs, and all are logical
# Year < 2020, month < 12, day < 31

summary(dt[c('Year', 'Month', 'Day')]) # looks good to me

# LAT LONG
sum(dt[c('Latitude', 'Longitude')] == "") == 0 # no blanks?
str_which(dt$Biomass, '\\s') %>% length()
sum(is.na(dt[c('Latitude', 'Longitude')])) == 0 # no NAs?

# Latitude constrained from -90 to 90.
# Longitude constrained -180 to 180.
summary(dt[c('Latitude', 'Longitude')]) # good

world_map <- map_data('world') # check whether the GPS coordinates match expectations
world <- ggplot(world_map) + 
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  coord_fixed() + 
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())
points <- world + geom_point(data=dt %>% distinct(Longitude, Latitude), aes(x=Longitude, y=Latitude), shape=21)

points_zoom <- points + coord_fixed(xlim=c(80,120), ylim=c(0,15))
points_zoom # all looks good

rm(list=c('world_map', 'world', 'points', 'points_zoom'))

# Taxonomic field check ---------------------------------------------------

# No NAs in taxonomic fields, remove all non-organism records
sum(is.na(dt$Species) + 0) == 0
sum(str_detect(dt$Species, '^\\s+$') + 0) == 0

# misspellings check, but not taxonomic cleaning
# check that genera are genera, not family names (-idae/eae)
dt$Genus <- word(dt$Species, 1)
dt$Genus %>% str_detect(., 'idae$|ini$') %>% sum(. + 0)

# Check the species records
sort(unique(dt$Species)) # looks good

dt$Species <- word(dt$Species, start=2, end=-1) # keep only specific epithet
sort(unique(dt$Genus))

# Prepare raw data --------------------------------------------------------
dt %>% distinct(Year, Month, Site, Transect) %>% arrange(Year, Month, Site, Transect) %>% View

# aggregate abundance records that are same species, plot, and survey day.
dt_merged <- dt %>% group_by_at(vars(-Biomass)) %>% 
  summarise(Biomass=sum(Biomass)) %>% ungroup() %>% 
  arrange(Year, Month, Transect, Quadrat, Genus, Species)
nrow(dt) - nrow(dt_merged) # any change in aggregating? 3 records
# now create empty columns needed to fit to template
dt_merged$Abundance <- ''
dt_merged$Family <- ''
dt_merged$DepthElevation <- ''
dt_merged$StudyID <- ''
dt_merged$Plot <- ''
dt_merged$SampleDescription <- as.factor(with(dt_merged, paste(Year, Month, Site, Transect, Quadrat, sep='_')))
length(levels(dt_merged$SampleDescription)) # 1866 samples

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
                         'StudyID')]
View(dt_merged) # final check :)
summary(dt_merged)
str(dt_merged)

# Export final ------------------------------------------------------------

write.csv(dt_merged, paste0(data.name, '_rawdata_', curator, '.csv'), row.names=F)

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
