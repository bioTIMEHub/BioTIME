

# Curation Script ---------------------------------------------------------

# Dataset: Bulgaria Wintering Waterbirds Black Sea coast
# Location: Bulgaria
# Curator: Cher Chow

# Set up ------------------------------------------------------------------
# load the necessary packages
require(dplyr)
require(ggplot2)
require(maps)
require(stringr)
require(lubridate)
require(readxl)

# make sure your working directory is set before running these lines
dt1 <- read_excel('Originals/HaasePilotto/S011-S019.xlsx', sheet=6, skip=2, col_names=T, na='')
dt2 <- read_excel('Originals/HaasePilotto/S011-S019.xlsx', sheet=7, skip=2, col_names=T, na='')
dt3 <- read_excel('Originals/HaasePilotto/S011-S019.xlsx', sheet=8, skip=2, col_names=T, na='')
dt4 <- read_excel('Originals/HaasePilotto/S011-S019.xlsx', sheet=9, skip=2, col_names=T, na='')
sites <- read_excel('Originals/HaasePilotto/S011-S019.xlsx', sheet=11, skip=1, col_names=T, na='')
# this file is on wintering waterbirds in Bulgaria, with each sheet at a different site.
# remove columns we don't need
sites[,5:7] <- NULL

# metadata from authors show that these four sea coast sites can be considered one dataset
# but they differ in the length of the transect --> sampling effort
# scale the counts by transect length

sites$length <- NA
sites$length[5:8] <- c(20,20,43,16) # add in transect lengths (km)

# join rows after checking the columns are identical
dt <- bind_rows(dt1, dt2, dt3, dt4)
View(dt)
rm(dt1,dt2,dt3,dt4)
# add the metadata from the sites sheet
colnames(sites)[2:4] <- c('Latitude', 'Longitude', 'DepthElevation')
dt <- left_join(dt, sites, by="Site")
View(dt) # looks ok
dt$Counts <- dt$Counts / dt$length # density per km
summary(dt)
dt$length <- NULL

# Structure check ---------------------------------------------------------

nrow(dt) # check dimensions
# 1165 records
str(dt) # check structure
summary(dt)
# Abundance and/or biomass, latitude and longitude numeric? Y
colnames(dt)[4] <- 'Abundance'
colnames(dt)[2] <- 'Date'
# Year, month and day must be integers or factors? needs splitting first
dt$Year <- year(dt$Date) %>% as.integer()
dt$Month <- month(dt$Date) %>% as.integer()
dt$Day <- day(dt$Date) %>% as.integer()
dt$Date <- NULL
# Secondary fields such as trawl, plot, transect etc must be factors or integers? N
dt$Site <- str_remove_all(dt$Site, 'Black sea coast:\\s') %>% 
  str_replace_all(., '\\s\\-\\s', '_') %>% as.factor
# Secondary fields such as elevation or depth will normally be numeric unless in a treatment format. Y
# Taxonomic fields must be characters or factors? Y
str(dt)


# Primary field check -----------------------------------------------------

# ABUNDANCES
# No negative values, zeroes, or NAs in abundance/biomass fields.
min(dt$Abundance) > 0 # check the minimum (no zeroes) Y
sum(dt$Abundance=="") == 0 # no blanks Y

# YEAR MONTH DAY
# again, no negative values, 0s or NAs, and all are logical
# Year < 2020, month < 12, day < 31

summary(dt[c('Year', 'Month', 'Day')]) # looks good to me

# LAT LONG
# no blanks, no NAs
sum(dt[c('Latitude', 'Longitude')]=="") == 0
sum(is.na(dt[c('Latitude', 'Longitude')]) + 0) == 0

# Latitude constrained from -90 to 90.
# Longitude constrained -180 to 180.
summary(dt[c('Latitude', 'Longitude')])

world_map <- map_data('world') # check whether the GPS coordinates match expectations
world <- ggplot(world_map) + 
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  coord_fixed() + 
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())
points <- world + geom_point(data=dt %>% distinct(Longitude, Latitude), aes(x=Longitude, y=Latitude), shape=21)

points_zoom <- points + coord_fixed(xlim=c(25,35), ylim=c(40,50))
points_zoom

# all looks good

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
dt$Species <- word(dt$Species, start=2, end=-1)
sort(unique(dt$Genus))
dt$Taxon <- NULL

# Prepare raw data --------------------------------------------------------

# aggregate abundance records that are in the same sample
# but still grouping by redundant columns because dplyr drops columns
dt_merged <- dt %>% group_by_at(vars(-Abundance)) %>% 
  summarise(Abundance=sum(Abundance)) %>% ungroup() %>% 
  arrange(Year, Month, Day, Site, Genus, Species)
nrow(dt) - nrow(dt_merged) # any change in aggregating? 3 records
# now create empty columns needed to fit to template
dt_merged$Biomass <- ''
dt_merged$Family <- ''
dt_merged$Plot <- ''
dt_merged$SampleDescription <- as.factor(with(dt_merged, paste(Year, Site, sep='_')))
dt_merged$Site <- NULL
length(levels(dt_merged$SampleDescription)) # 122 samples

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
                         'Year')] %>% 
  arrange(Year, Month, Day, Plot, Genus, Species)
View(dt_merged) # final check :)
summary(dt_merged)
str(dt_merged)

# Export final ------------------------------------------------------------

write.csv(dt_merged, 'Bulgaria_WinteringWaterbirds_BlackSeaCoast_rawdata_CC.csv', row.names=F)


### Spatial Geometry Calculations for BioTIME datasets
# paste this chunk into the end of your curation script

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

# Plot the geometries -----------------------------------------------------

require(ggplot2)
world_map <- map_data('world') # check whether the GPS coordinates match expectations
ggplot(world_map) +
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  geom_point(data = dt_merged, aes(x = Longitude, y = Latitude), size = 2, shape = 21, colour = 'blue') +
  geom_point(aes(x = centroid[1], y = centroid[2]), size = 3, colour = 'green') +
  geom_sf(data = dt_coord %>% st_convex_hull(), colour = 'blue', fill = 'transparent') +
  coord_sf(xlim = c(-115, -110), ylim = c(30,35)) + # make sure these fit your points
  labs(x = NULL, y = NULL) +
  theme_minimal()


