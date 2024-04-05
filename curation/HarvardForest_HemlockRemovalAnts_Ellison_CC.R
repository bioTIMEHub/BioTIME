
## Data curation script
## Ants and Ecosystem Function in Hemlock Removal Experiment at Harvard Forest 2006-2014
## Aaron Ellison
## Revision 23 from LTER EDI portal

require(dplyr)
require(stringr)
require(lubridate)

dataset_id <- 'HarvardForest_HemlockRemovalAnts_Ellison'
load(file = 'sDiv/BioTIMEx/data/raw data/ellison_2018/ddata')
dt2 <- read.csv('sDiv/BioTIMEx/data/raw data/ellison_2018/hf160-02-subplots.csv', header = T)

# Structure check ---------------------------------------------------------

dim(ddata) # check dimensions, 10319 records originally
str(ddata) # check structure

# 8 experimental plots divided each into 2 subplots
# plots are grouped into ridge or valley locations.
# treatments that alter hemlock population (girdling or logging). 2 controls: hemlock control, hardwood control.

# only keep controls
ddata <- ddata %>% filter(str_detect(treatment, 'control'), subplot.t == 'control')
n_distinct(ddata$plot) # check that it lines up with methods described
n_distinct(ddata$subplot)

# merge location data in
# needs conversion from NAD83 to WGS84 first
# Original data coordinate system metadata from EDI portal. Not in data files.
require(sf)
sites <- dt2 %>% st_as_sf(., coords = c("x.m", "y.m"), crs = 26986) %>%
   st_transform(crs = 4326)

require(ggplot2)
world_map <- map_data('world') # check whether the GPS coordinates match expectations
world <- ggplot(world_map) +
   geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
   coord_fixed() +
   labs(x='Longitude', y='Latitude') +
   theme_bw() + theme(panel.grid=element_blank())
points <- world + geom_sf(data=sites, shape=21)
points
points + coord_sf(xlim=c(-75, -70), ylim=c(40,45)) # check projection transformation

# back into dataframe for merge
dt2$Longitude <- st_coordinates(sites)[,1]
dt2$Latitude <- st_coordinates(sites)[,2]

ddata$plot <- as.integer(ddata$plot)
ddata <- dt2 %>% select(!c(block, treatment, x.m, y.m)) %>%
   left_join(ddata, ., by=c('plot', 'subplot', 'subplot.t'))
# simple column reorganise
ddata <- ddata %>% select(!c(vial.num, subfamily, spec.code)) %>%
   rename(Abundance = count, DepthElevation = elevation)

# create plot
ddata$Plot <- with(ddata, paste(plot, subplot, sep='-')) %>% as.factor #  treatment plots
# plot number links with ridge or valley block
# subplot links with hardwood vs hemlock controls
ddata <- ddata %>% select(!c(block, plot, treatment, subplot, subplot.t))

ddata$Year <- year(ddata$date)
ddata$Month <- month(ddata$date)
ddata$Day <- day(ddata$date)
ddata$date <- NULL

# Abundance and/or biomass, latitude and longitude numeric? Y
# Year, month and day must be integers or factors? Y
# Secondary fields such as trawl, plot, transect etc must be factors or integers? Y

# Secondary fields such as elevation or depth will normally be numeric unless in a treatment format. Y
# Date should be POSIXct. NA. split already.
# Taxonomic fields must be characters or factors? Y

# Primary field check -----------------------------------------------------

# ABUNDANCES
# No negative values, zeroes, or NAs in abundance/biomass fields.
sum(ddata$Abundance == "", na.rm=T) # no blanks
summary(ddata)

View(ddata %>% filter(is.na(Abundance))) # 113 records of NAs in abundance and taxon
# eliminate 0 or NA records
ddata <- ddata %>% filter(!Abundance == 0, !is.na(Abundance))

# YEAR MONTH DAY
# again, no negative values, 0s or NAs, and all are logical
# Year < 2020, month < 12, day < 31

summary(ddata) # dates all good

# LAT LONG
# already checked above when we converted them over from NAD83

# Taxonomic field check ---------------------------------------------------

# No NAs in taxonomic fields, remove all non-organism records
sum(is.na(ddata$genus)+0)
sum(is.na(ddata$species)+0)
# ddata %>% filter(is.na(genus) | is.na(species)) %>% View # check them out
ddata$Family <- 'Formicidae'

# misspellings check, but not taxonomic cleaning
# check that genera are genera, not family names (-idae/eae)
sort(unique(as.character(ddata$genus))) # manual check genera
# no problems here

sort(unique(paste(ddata$genus, ddata$species, sep=' '))) # check species names
colnames(ddata) <- str_to_title(colnames(ddata)) # rename to match BioTIME
colnames(ddata)[4] <- 'DepthElevation'

# Prepare raw data --------------------------------------------------------

# aggregate abundance records that are same species, plot, and survey day.
ddata_merged <- ddata %>% group_by_at(vars(-Abundance)) %>%
   summarise(Abundance=sum(Abundance)) %>% ungroup() %>% arrange(Year, Month, Plot, Genus, Species)
nrow(ddata) - nrow(ddata_merged) # check if there's any difference, 14 rows "lost"]

# add in blank columns
ddata_merged$Biomass <- ''
ddata_merged$StudyID <- ''
ddata_merged$SampleDescription <- as.factor(with(ddata_merged, paste(Year, Month, Plot, sep='_')))
n_distinct(ddata_merged$SampleDescription) # 108
ddata_merged %>% distinct(Year, Month, Plot)
ddata_merged <- ddata_merged[c('Abundance',
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

View(ddata_merged) # final check :)

# Export final raw data ------------------------------------------------------------

write.csv(ddata_merged, paste0(dataset_id, '_rawdata_CC.csv'), row.names=F)
clipr::write_clip(ddata_merged)

# Calculate spatial metadata ----------------------------------------------

# load libraries
library(sf)
library(clipr)

# 1. Convert data points into point spatial object
# assumes your rawdata dataframe is called rawdata
dt_coord <- ddata_merged %>% select(Longitude, Latitude) %>% distinct() %>%
   st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()

# 2. Calculate convex hull, area and centroid
centroid <- dt_coord %>% st_convex_hull() %>% st_centroid() %>% unlist # get centroid
write_clip(centroid[c(2,1)]) # copy as lat-long

# transform to mercator to get area in sq km
area <- st_transform(dt_coord, st_crs("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs")) %>%
   st_convex_hull() %>% st_area()  ##get area in sq km
write_clip(area)

