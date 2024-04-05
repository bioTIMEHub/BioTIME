## carpenter_2016
require(dplyr)
require(stringr)
require(lubridate)

dataset_id <- 'USLTER_Wisconsin_NorthTemperateLakesZooplankton1984-2016_CC'
load(file='sDiv/BioTIMEx/data/raw data/carpenter_2016/ddata')

# Structure check ---------------------------------------------------------

dim(ddata) # check dimensions
str(ddata) # check structure

# simple column reorganise
ddata <- ddata %>% select(number_per_net, biomass, lakename, taxon_name, sampledate) %>%
   rename(Abundance = number_per_net, Biomass = biomass)
# split date
ddata$Year <- year(ddata$sampledate)
ddata$Month <- month(ddata$sampledate)
ddata$Day <- day(ddata$sampledate)
summary(ddata)
# Abundance and/or biomass, latitude and longitude numeric? Y
# Year, month and day must be integers or factors? Y
# Secondary fields such as trawl, plot, transect etc must be factors or integers? Y

# Secondary fields such as elevation or depth will normally be numeric unless in a treatment format. NA here
# Date should be POSIXct. NA. split already.
ddata$sampledate <- NULL
# Taxonomic fields must be characters or factors? Y

# Primary field check -----------------------------------------------------

# ABUNDANCES
# No negative values, zeroes, or NAs in abundance/biomass fields.
sum(ddata$Abundance == "", na.rm=T) # no blanks
sum(ddata$Biomass == "", na.rm=T) # no blanks
summary(ddata)

View(ddata %>% filter(Biomass == 0)) # check the 0s in biomass
View(ddata %>% filter(Abundance == 0)) # check the 0s in biomass
View(ddata %>% filter(is.na(Abundance)))
# eliminate 0 or NA records
ddata <- ddata %>% filter(!Biomass == 0, !Abundance == 0, !is.na(Abundance), !is.na(Biomass))
20822 - 16043 # removed almost 5000 records

# YEAR MONTH DAY
# again, no negative values, 0s or NAs, and all are logical
# Year < 2020, month < 12, day < 31

summary(ddata) # dates all good

# LAT LONG
# Exclusion of lakes with unknown disturbance history (Alban)
ddata <- ddata %>% filter(!lakename %in% c('Ward Lake','Hummingbird Lake'))
sort(unique(ddata$lakename))
# manually import lat longs from US LTER website
sites <- read.csv('sDiv/BioTIMEx/data/raw data/carpenter_2016/lakesites.csv', header = T)
colnames(sites)[1] <- 'lakename'
ddata$lakename <- as.character(ddata$lakename)
ddata <- left_join(ddata, sites, by="lakename")
summary(ddata[8:9])

# Depth elevation exceptions and changes
# The standard depths are as follows: Peter, East Long, West Long, Crampton and Tuesday Lakes: 12m, Paul Lake: 8m, Ward Lake: 6m;
#exceptions are: for 2012 and beyond Tuesday Lake was sampled at 10m, Peter was sampled at 10m from 1984-1986, Paul was sampled at 7.5m in 1995.
ddata$DepthElevation[which(ddata$Year >= 2012 & ddata$lakename == 'Tuesday Lake')] <- 10
ddata$DepthElevation[which(ddata$Year >= 1984 & ddata$Year <= 1986 & ddata$lakename == 'Peter Lake')] <- 10
ddata$DepthElevation[which(ddata$Year == 1995 & ddata$lakename == 'Paul Lake')] <- 7.5

# visual check
require(ggplot2)
world_map <- map_data('world') # check whether the GPS coordinates match expectations
world <- ggplot(world_map) +
   geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
   coord_fixed() +
   labs(x='Longitude', y='Latitude') +
   theme_bw() + theme(panel.grid=element_blank())
points <- world + geom_point(data=sites, aes(x=Longitude, y=Latitude), shape=21)
points
points + coord_fixed(xlim=c(-93,-85), ylim=c(40,50))


# Taxonomic field check ---------------------------------------------------

# No NAs in taxonomic fields, remove all non-organism records
sum(is.na(ddata$taxon_name)+0)
# misspellings check, but not taxonomic cleaning
# check that genera are genera, not family names (-idae/eae)
sort(unique(ddata$taxon_name))
ddata$Species <- ddata$taxon_name # make a copy where we make changes
# first, records with undefined species epithets into the BioTIME format of sp without period
ddata$Species <- ddata$Species %>% str_replace_all(., 'sp.$', 'sp')
# remove records that are above family level, or mixed species
ddata <- ddata %>% filter(str_detect(Species, 'oid$|Nauplii|Copepodite|\\+', negate = T))
# visually/manually check single word taxon names are genus names
replace_sp <- c(# pattern = replacement
   'colonial ' = '',
   'single ' = '',
   'Cyclops varicans rubellus' = 'Cyclops varicans',
   'Tropocyclops prasinus mexicanus' = 'Tropocyclops prasinus'
)
ddata$Family <- ''
ddata$Species <- ddata$Species %>% str_replace_all(., replace_sp) # fix uncertain species conventions
sort(unique(ddata$Species)) # all good. just need to add " sp" for the single word genus records
ddata$Genus <- word(ddata$Species, 1)
ddata$Species <- word(ddata$Species, start=2, end=-1) # everything but the first word
sort(unique(ddata$Genus))
View(ddata %>% filter(is.na(Species)) %>% distinct(Genus, .keep_all = T))
ddata$Species[which(is.na(ddata$Species) & is.na(ddata$Genus)==F )] <- 'sp'
View(ddata %>% distinct(Genus, Species))
ddata$taxon_name <- NULL # remove original column

# Prepare raw data --------------------------------------------------------


# aggregate abundance records that are same species, plot, and survey day.
ddata_merged <- ddata %>% group_by_at(vars(-c(Abundance, Biomass))) %>%
   summarise(Abundance=sum(Abundance), Biomass=sum(Biomass)) %>% ungroup() %>% arrange(Year, Month, Day, lakename, Genus, Species)
dim(ddata)[1]-dim(ddata_merged)[1] # check if there's any difference, 19 rows "lost" from merge

ddata_merged %>% distinct(Year, Month, Day, lakename) %>% View
ddata_merged$SampleDescription <- as.factor(with(ddata_merged, paste(Year, Month, Day, lakename, sep='_')))
length(levels(ddata_merged$SampleDescription)) # check number of sampling events

# add in blank columns
ddata_merged$Plot <- ''
ddata_merged$StudyID <- ''

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

write.csv(ddata_merged, paste0(dataset_id, '_rawdata.csv'), row.names=F)


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
