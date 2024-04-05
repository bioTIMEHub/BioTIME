

# Curation Script ---------------------------------------------------------

# Dataset: NIES Fish and shrimp monitoring in Lake Kasumigaura
# Location: Japan
# Curator: Cher Chow
# Date: 06-Jul-2021

# Set up ------------------------------------------------------------------
# load the necessary packages
require(tidyverse)
require(lubridate)
require(maps)
require(CoordinateCleaner)
require(readxl)

rm(list=ls())
data.name <- 'NIES_Fish_LakeKasumigaura'
curator <- 'CC'

# make sure your working directory is set before running these lines
# Darwin Core Archive format
dt <- read.table('CurationProgress/Originals/LakeKasumigaura/LakeKasumigaura_fishdensity/occurrence.txt', header=T, sep='\t')
biomass <- read_excel(path="CurationProgress/Originals/LakeKasumigaura/LakeKasumigaura_fishdensity/15-2_fish_biomass_eng.xls", skip = 16)
View(dt)

# Structure organisation --------------------------------------------------

# just grab the fields we need
dt <- dt[c(8,10,14,15,16,23:25,27)]

# rename fields
colnames(dt)[1:8] <- c('Abundance', 'Date', 'Plot', 'Latitude', 'Longitude', 'Family', 'Genus', 'Species')
biomass$Species <- NULL
biomass <- biomass %>% pivot_longer(!c(Date, Station), names_to="Species", values_to="Biomass") %>% filter(Biomass > 0)
colnames(biomass)[2] <- 'Plot'
# some mismatched synonyms
biomass$Species <- str_replace_all(biomass$Species, 'Rana catesbeiana', 'Lithobates catesbeianus')
biomass$Genus <- word(biomass$Species, 1)
biomass$Species <- word(biomass$Species, 2)
biomass$Species[which(is.na(biomass$Species))] <- ''

dt$Plot <- dt$Plot %>% word(., 1) %>% str_remove_all(., '[:punct:]') %>% as.factor(.)
levels(dt$Plot)

# dates need to be coerced
dt$Date <- as.POSIXct(dt$Date, tryFormats=('%Y-%m-%d'))
dt$Year <- year(dt$Date) %>% as.integer()
dt$Month <- month(dt$Date) %>% as.integer()

biomass$Year <- year(biomass$Date) %>% as.integer()
biomass$Month <- month(biomass$Date) %>% as.integer()

dataset <- full_join(dt[-2], biomass[-1], by=c('Year','Month','Plot','Genus','Species'))
dataset[which(is.na(dataset)),] %>% View()

# fix the NAs from biomass records without coordinates
dataset$Latitude[which(dataset$Plot == 'Futto')] <- 35.98595
dataset$Longitude[which(dataset$Plot == 'Futto')] <- 140.3617
dataset$Latitude[which(dataset$Plot == 'Dosakibana')] <- 35.98707
dataset$Longitude[which(dataset$Plot == 'Dosakibana')] <- 140.3537
dataset <- dataset[1:1507,]

dt <- dataset
rm(dataset)

# Structure check ---------------------------------------------------------

nrow(dt) # check dimensions
# 1507 records
str(dt) # check structure
summary(dt)
# Abundance and/or biomass, latitude and longitude numeric? Y
# logical?
summary(dt[,c('Year', 'Month')]) # yes

summary(dt)
# Secondary fields such as elevation or depth will normally be numeric unless in a treatment format. NA
# Taxonomic fields must be characters or factors? Y

# Primary field check -----------------------------------------------------

# ABUNDANCES
# No negative values, zeroes, or NAs in abundance/biomass fields.
dt %>% filter(is.na(Abundance) & is.na(Biomass))
dt %>% filter(is.na(Biomass)) # blanks in biomass? biomass more complete
min(dt$Abundance, na.rm=T) # no zeroes? Y
min(dt$Biomass, na.rm=T) # no zeroes? Y
str_which(dt$Abundance, '\\s') %>% length() # no blanks? Y
str_which(dt$Biomass, '\\s') %>% length() # no blanks? Y

# LAT LONG
str_which(dt$Latitude, '\\s') %>% length() # no blanks?
str_which(dt$Longitude, '\\s') %>% length()
sum(is.na(dt[c('Latitude', 'Longitude')])) == 0 # no NAs?

# coordinate cleaner works best against Darwin Core Archive formats
coord.test <- clean_coordinates(dt %>% mutate(countryCode=rep('JP', nrow(dt)), Name=paste(Genus, specificEpithet, sep=' ')),
                                lon='Longitude', lat='Latitude', species='Name', countries='countryCode', verbose=T)
coord.test %>% filter(.summary == F) %>% View()

# WGS84 coordinates? Y
# Latitude constrained from -90 to 90.
# Longitude constrained -180 to 180.
summary(dt[c('Latitude', 'Longitude')]) # good

world_map <- map_data('world') # check whether the GPS coordinates match expectations
world <- ggplot(world_map) + 
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  coord_fixed() + 
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())
points <- world + 
  geom_point(data=dt %>% distinct(Longitude, Latitude), aes(x=Longitude, y=Latitude)) +
  geom_point(data=coord.test %>% distinct(Longitude, Latitude, .keep_all=T) %>% filter(.summary == F),
             aes(x=Longitude, y=Latitude), shape=21, fill='red')

points_zoom <- points + coord_fixed(xlim=c(130,150), ylim=c(30,40))
points_zoom # all looks good

rm(list=c('world_map', 'world', 'points', 'points_zoom'))
rm(coord.test, original)

# Taxonomic field check ---------------------------------------------------

# No NAs in taxonomic fields, remove all non-organism records
sum(is.na(dt$Species) + 0) == 0
sum(str_detect(dt$Species, '^\\s+$') + 0) == 0
sum(is.na(dt$Genus) + 0) == 0

dt %>% filter(str_detect(Genus, '^\\s+$') | str_detect(Species, '^\\s+$'))

# genus level add sp
dt$Species[which(dt$taxonRank == 'genus')] <- 'sp'
dt <- dt[-8] # get rid of taxon rank column now that we've used it

# misspellings check, but not taxonomic cleaning
# check that genera are genera, not family names (-idae/eae)
dt$Genus %>% str_detect(., 'idae$|ini$') %>% sum(. + 0)

# Check the species records
with(dt, paste(Genus, Species, sep=' ')) %>% sort() %>% unique()
unique(dt$Genus) %>% sort()
dt$Species[str_which(dt$Species, '\\s')] %>% sort(.) %>% unique(.) # view all the subspecies
# nothing to fix

dt <- dt %>% filter(!Genus == 'Others') # remove unknown IDs

# Prepare raw data --------------------------------------------------------

# aggregate abundance records that are same species, plot, and survey day.
dt_merged <- dt %>% group_by(Plot, Latitude, Longitude, Family, Genus, Species, Year, Month) %>% 
  summarise(Abundance=sum(Abundance), Biomass=sum(Biomass)) %>% ungroup() %>% arrange(Year, Month, Plot, Family, Genus, Species)
nrow(dt) - nrow(dt_merged) # any change in aggregating? no

# now create empty columns needed to fit to template
dt_merged$DepthElevation <- rep('', nrow(dt_merged))
dt_merged$StudyID <- rep('', nrow(dt_merged))
dt_merged$Day <- rep('', nrow(dt_merged))
dt_merged$SampleDescription <- as.factor(with(dt_merged, paste(data.name, Plot, Latitude, Longitude, Year, Month, sep='_')))
length(levels(dt_merged$SampleDescription)) # 92 samples

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
                         'StudyID')] %>% arrange(Year, Month, Plot, Family, Genus, Species)
View(dt_merged) # final check :)


# Export final ------------------------------------------------------------

write.csv(dt_merged, paste0('CurationProgress/', data.name, '_rawdata_', curator, '.csv'), row.names=F)

# Convex Hull for centroid ------------------------------------------------

##load libraries
require(sp)
require(rgeos)
require(clipr)
write_clip(dt_merged)

# 1. Convert data points into point spatial object
# assumes your rawdata dataframe is called rawdata
dataset_coords<- SpatialPoints(dt_merged %>% select(Longitude, Latitude) %>% distinct(), proj4string=CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')) 

# 2. Calculate convex hull, area and centroid
centroid <- gConvexHull(dataset_coords) %>% gCentroid() # get centroid
centroid@coords # coordinates
centroid@coords[c(2,1)] %>% write_clip()

# use lake area. two points not sufficient.
