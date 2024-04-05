

# Curation Script ---------------------------------------------------------

# Dataset: LTER Lange Bramke Vegetation
# Location: Germany
# Curator: Cher Chow
# Date: 05-Feb-2021

# Set up ------------------------------------------------------------------
# load the necessary packages
require(dplyr)
require(ggplot2)
require(maps)
require(stringr)
require(lubridate)
require(readxl)

# clear environment
rm(list = ls(all.name=T))

# make sure your working directory is set before running these lines
# North slope
dt <- read_excel('Originals/HaasePilotto/S040.xlsx', sheet=2, skip=3, col_names=T, na='')
# South slope
dt2 <- read_excel('Originals/HaasePilotto/S040.xlsx', sheet=1, skip=3, col_names=T, na='')
# Ridge
dt3 <- read_excel('Originals/HaasePilotto/S040.xlsx', sheet=3, skip=3, col_names=T, na='')
dt_sites <- read_excel('Originals/HaasePilotto/S040.xlsx', sheet=4, skip=1, col_names=T, na='')
dt_sites$Site <- c('North', 'South', 'Ridge')
dt[,8:9] <- NULL
dt2[,8:9] <- NULL
dt3[,c(8:9,11)] <- NULL
dt$Site <- rep('North', dim(dt)[1])
dt2$Site <- rep('South', dim(dt2)[1])
dt3$Site <- rep('Ridge', dim(dt3)[1])

# Initial data check ---------------------------------------------------------

# mash them together properly first
names(dt)[4:7] <- c('Plot1', 'Plot2', 'Plot3', 'Plot4')
names(dt2)[4:7] <- c('Plot1', 'Plot2', 'Plot3', 'Plot4')
names(dt3)[4:7] <- c('Plot1', 'Plot2', 'Plot3', 'Plot4')
str(dt2) # check that all values in plots are numeric before pivoting
str(dt3)
str(dt)

# North
dt$Plot4 <- as.numeric(dt$Plot4)
dt <- dt %>% pivot_longer(cols=starts_with('Plot'), names_to = "Plot", values_to="Biomass") # long format
dt <- dt %>% filter(!is.na(Biomass))

# South
dt2 <- dt2 %>% pivot_longer(cols=starts_with('Plot'), names_to = "Plot", values_to="Biomass") # long format
dt2 <- dt2 %>% filter(!is.na(Biomass))

# Ridge
dt3 <- dt3 %>% pivot_longer(cols=starts_with('Plot'), names_to = "Plot", values_to="Biomass") # long format
dt3 <- dt3 %>% filter(!is.na(Biomass))
# Layers here refer to
# 2 = shrub layer
# 3 = herb layer
# 4 = moss layer

merged <- bind_rows(dt, dt2, dt3)
View(merged)
dt <- merged
rm(list=c('dt2', 'dt3', 'merged'))

# Abundance and/or biomass, latitude and longitude numeric? Y
# Year, month and day must be integers or factors? NA here
# Secondary fields such as trawl, plot, transect etc must be factors or integers? NA
str(dt)
dt$Plot <- as.factor(paste(dt$Site, dt$Plot)) # 26 levels
# Secondary fields such as elevation or depth will normally be numeric unless in a treatment format. NA here
colnames(dt_sites)[2:4] <- c('Latitude', 'Longitude', 'DepthElevation')
dt_m <- full_join(dt, dt_sites[1:4], by='Site')
View(dt_m) # check before replacing dt
dt <- dt_m
rm(dt_m)

str(dt)
# now we don't need Site anymore
dt$Site <- NULL

dt %>% distinct(Plot, Year, Month, Day) %>% arrange(Year, Plot) %>% View # Check the real sampling intervals
dt %>% group_by(Plot) %>% summarise(nyear = n_distinct(Year)) %>% View
# Ridge might need isolating
dt %>% group_by(Year) %>% summarise(nplot = n_distinct(Plot)) %>% View

# Date should be POSIXct Y
# Taxonomic fields must be characters or factors? Y

# ABUNDANCES
# No negative values, zeroes, or NAs in abundance/biomass fields.
sum(dt$Biomass == "") # no blanks
summary(dt)

# YEAR MONTH DAY
# again, no negative values, 0s or NAs, and all are logical
# Year < 2020, month < 12, day < 31

dt$Year <- year(dt$Date)
dt$Month <- month(dt$Date)
dt$Day <- day(dt$Date)
dt$Date <- NULL
summary(dt[,8:10])

# LAT LONG
# no blanks, no NAs
# Latitude constrained from -90 to 90.
# Longitude constrained -180 to 180.

world_map <- map_data('world') # check whether the GPS coordinates match expectations
world <- ggplot(world_map) + 
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  coord_fixed() + 
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())
points <- world + geom_point(data=dt %>% distinct(Latitude, Longitude), aes(x=Longitude, y=Latitude), shape=21)

points_zoom <- points + coord_fixed(xlim=c(5,15), ylim=c(45,55))

# No NAs in taxonomic fields, remove all non-organism records
sum(is.na(dt$Taxon)+0)
# misspellings check, but not taxonomic cleaning
sort(unique(dt$Taxon))
# check that genera are genera, not family names (-idae/eae)
dt$Species <- dt$Taxon # make a copy in case
dt$Species <- dt$Species %>% str_replace_all(., ' [:punct:]|[:punct:]', '') # remove asterisks
replace_sp <- c(
  'arundinacea' = 'arundinacaea',
  'cüpressiforme' = 'cupressiforme'
)
dt$Species <- dt$Species %>% str_replace_all(., replace_sp)
sort(unique(dt$Species))
dt$Genus <- word(dt$Species, 1)
dt$Species <- word(dt$Species, start=2, end=-1) # everything but the first word
sort(unique(dt$Genus))
dt$Taxon <- NULL

# Prepare raw data --------------------------------------------------------

# aggregate abundance records that are same species, plot, and survey day.
dt_merged <- dt %>% group_by_at(vars(-Biomass)) %>% 
  summarise(Biomass=sum(Biomass)) %>% ungroup() %>% arrange(Year, Plot, Genus, Species)
dim(dt)[1]-dim(dt_merged)[1] # check if there's any difference

# add in blank columns
dt_merged$StudyID <- ''
dt_merged$Abundance <- ''
dt_merged$Family <- ''

dt_merged$SampleDescription <- as.factor(with(dt_merged, paste(Year, Plot, Layer, sep='_')))
length(levels(dt_merged$SampleDescription)) # check number of sampling events

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

# Export final ------------------------------------------------------------

write.csv(dt_merged, 'LTER_ICP_LangeBramke_Vegetation_rawdata_CC.csv', row.names=F)
library(clipr)
write_clip(dt_merged)

# load libraries
library(sf)


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