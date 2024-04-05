

# Curation Script ---------------------------------------------------------

# Dataset: STUDY 45 MCR LTER: Coral Reef: Long-term Population and Community Dynamics: Fishes, ongoing since 2005
# Location: Moorea
# Curator: Cher Chow
# Date: 25-Oct-2021

# Set up ------------------------------------------------------------------
# load the necessary packages
require(tidyverse)
require(lubridate)
require(maps)
require(CoordinateCleaner)
require(readxl)

rm(list=ls())
data.name <- 'Study45_MooreaCoralReefLTER'
curator <- 'CC'

# make sure your working directory is set before running these lines
dt <- read.csv('CurationProgress/sTeTra/STUDY45/MCR_LTER_Annual_Fish_Survey_20210108.csv', header=T, sep=',')
dt.og <- read.csv('CurationProgress/sTeTra/STUDY45/MCR_LTER_Annual_Fish_Survey_20210108.csv', header=T, sep=',')
View(dt)
summary(dt)

# Structure organisation --------------------------------------------------

unique(dt$Site) # see an NA
n_distinct(dt$Location)
dt[6:9] <- dt[6:9] %>% mutate(across(.cols=everything(), .fns=as.factor))
summary(dt[6:9]) # habitat has an error?

dt[which(is.na(dt$Site)),] # just the row total at the end. remove.
dt <- dt[-which(is.na(dt$Site)),]

dt %>% filter(Habitat == 'error') %>% select(Location, Habitat) # all forereef.
dt$Habitat[which(dt$Habitat == 'error')] <- 'FO' # fix
with(dt, paste0(Site, Habitat, Transect, Swath)) %>% n_distinct()
# I'll decide to use Site Habitat Transect Swatch as Plot instead of Location

dt$Plot <- with(dt, paste(Site, Habitat, Transect, Swath, sep="_"))

# add approximate depth information from methods
depth <- data.frame(Habitat=sort(unique(dt$Habitat)), DepthElevation=NA)
depth[2] <- c(1.5, 12, 10)

# I have to take the bounding coordinates for each site and get a centroid. This is only applicable for the site level
sites <- data.frame(Site=sort(unique(dt$Site)), Latitude=NA, Longitude=NA)
sites[1,2:3] <- c(-17.47659, -149.8377) #LTER 0
sites[2,2:3] <- c(-17.47659, -149.8377) #LTER 1, and so on.
sites[3,2:3] <- c((-17.46576169 + -17.48131958)/2, (-149.8116849 + -149.7961685)/2)
sites[4,2:3] <- c((-17.46576169 + -17.48131958)/2, (-149.8116849 + -149.7961685)/2)
sites[5,2:3] <- c((-17.50382025 + -17.52087158)/2, (-149.7708619 + -149.7519968)/2)
sites[6,2:3] <- c((-17.53305021 + -17.55064263)/2, (-149.7772857 + -149.7566866)/2)
sites[7,2:3] <- c((-17.53305021 + -17.55064263)/2, (-149.7772857 + -149.7566866)/2)
sites[8,2:3] <- c((-17.53305021 + -17.55064263)/2, (-149.7772857 + -149.7566866)/2)
sites[9,2:3] <- c((-17.56818162 + -17.59182383)/2, (-149.8869755 + -149.8561009)/2)
sites[10,2:3] <- c((-17.56818162 + -17.59182383)/2, (-149.8869755 + -149.8561009)/2)
sites[11,2:3] <- c((-17.56818162 + -17.59182383)/2, (-149.8869755 + -149.8561009)/2)
sites[12,2:3] <- c((-17.50735955 + -17.52839766)/2, (-149.934537 + -149.9115336)/2)

dt <- left_join(dt, sites, by="Site")
dt <- left_join(dt, depth, by="Habitat")

# just grab the fields we need
dt <- dt[c(1,2,6:10,11,12,15,25:28)]
View(dt) # check the joining and filtering

# rename fields
colnames(dt)[9] <- 'Abundance'
str(dt)

# dates need to be coerced
dt$Date <- as.POSIXct(dt$Date, tryFormats=('%d/%m/%Y'))
dt$Day <- day(dt$Date) %>% as.integer()
dt$Month <- month(dt$Date) %>% as.integer()
dt$Date <- NULL

# before we run any checks, the data contributor noted sampling inconsistencies in 2005 as well as variability in data recorder/diver in transect 3
# I'd omit these before running any checks

dt <- dt %>% filter(!Year == 2005, !Transect == 3)
dt %>% filter(Transect == 3)
min(dt$Year) # yep
dt <- dt %>% select(!c(Site, Habitat, Transect, Swath)) # we don't need these columns after the filtering

# Structure check ---------------------------------------------------------

nrow(dt) # check dimensions
# 65334 records. removed 21410 rows in our filtering above.
str(dt) # check structure

# do coercions first
dt$Plot <- as.factor(dt$Plot)
dt$Year <- as.integer(dt$Year)

summary(dt)
# Abundance and/or biomass, latitude and longitude numeric? N

dt$Biomass <- dt$Biomass %>% as.numeric() # fish biomass is in grams

# logical?
summary(dt[,c('Year', 'Month', 'Day')]) # yes

summary(dt)
# Secondary fields such as elevation or depth will normally be numeric unless in a treatment format. NA
# Taxonomic fields must be characters or factors? Y

# Primary field check -----------------------------------------------------

# ABUNDANCES
# No negative values, zeroes, or NAs in abundance/biomass fields.
dt %>% filter(is.na(Biomass))
dt %>% filter(is.na(Abundance)) # blanks in biomass? biomass more complete
min(dt$Abundance, na.rm=T) # no zeroes? Y
min(dt$Biomass, na.rm=T) # no zeroes? Y
str_which(dt$Abundance, '\\s') %>% length() # no blanks? Y
str_which(dt$Biomass, '\\s') %>% length() # no blanks?

dt[which(is.na(dt$Biomass)|dt$Biomass <= 0),] %>% View # some issues with -1, zeroes, and NA in biomass
dt$Biomass[which(is.na(dt$Biomass)|dt$Biomass <= 0)] <- NA
# keep these records as there's still abundance data, but NAs in biomass

# LAT LONG
str_which(dt$Latitude, '\\s') %>% length() # no blanks?
str_which(dt$Longitude, '\\s') %>% length()
sum(is.na(dt[c('Latitude', 'Longitude')])) == 0 # no NAs?
summary(dt[8:9])

# coordinate cleaner works best against Darwin Core Archive formats
coord.test <- clean_coordinates(dt %>% select(Latitude, Longitude, Taxonomy),
                                lon='Longitude', lat='Latitude', species='Taxonomy', verbose=T)
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
points
points_zoom <- points + coord_fixed(xlim=c(-130,-150), ylim=c(-25,-10))
points_zoom # all looks good

rm(list=c('world_map', 'world', 'points', 'points_zoom'))
rm(coord.test)

# Taxonomic field check ---------------------------------------------------

# No NAs in taxonomic fields, remove all non-organism records
sum(is.na(dt$Taxonomy) + 0) == 0
sum(str_detect(dt$Taxonomy, '^\\s+$') + 0) == 0

# genus level add sp
dt[str_which(dt$Taxonomy, 'sp.$'),]
dt$Taxonomy <- str_replace_all(dt$Taxonomy, 'sp.$', 'sp')

# misspellings check, but not taxonomic cleaning
# check that genera are genera, not family names (-idae/eae)
dt$Taxonomy %>% str_detect(., 'idae$|ini$') %>% sum(. + 0)
dt[str_which(dt$Taxonomy, 'idae$|ini$'),] # not a family name. just a species

# Check the species records
sort(unique(dt$Taxonomy))

dt[str_which(dt$Taxonomy, 'unidentified *'),] %>% unique() %>% View
dt$Species <- dt$Taxonomy # make a copy
dt$Species <- str_replace_all(dt$Species, 'unidentified *', 'sp') # uncertain records to the family level
dt$Family[str_which(dt$Species, 'idae \\w')] <- dt$Species[str_which(dt$Species, 'idae \\w')] %>% word(., 1) # correct their family level taxon info
dt <- dt[-str_which(dt$Species, '^Osteichthyes'),] # remove the record that is too uncertain
dt <- dt[-str_which(dt$Taxonomy, 'No fish observed'),] # remove zeroes
dt[str_which(dt$Family, 'ormes'),] # check that family column doesn't have any more non-family records
dt$Species[str_which(dt$Species, 'idae sp$')] <- '' # if there's no unidentified species distinction, just remove the species record and keep it at family level

# check again
sort(unique(dt$Species))
dt$Species[str_which(dt$Species, 'sp\\.\\s*[:digit:]*')] # see all records that have some variation of e.g. "sp. 3" or "sp.3"
dt$Species <- str_replace_all(dt$Species, 'sp\\.\\s*', 'sp') # change to "sp3"
dt$Species[str_which(dt$Species, '\\s+\\(cf\\)')] # cf uncertainties
dt$Species <- str_remove_all(dt$Species, '\\s+\\(cf\\)') # get rid of (cf) and white space.
sort(unique(dt$Species))

dt$Genus <- word(dt$Species, 1) # migrate the first word in the species column to genus
dt %>% filter(str_detect(Genus, 'idae$')) # make sure these records have species distinctions, i.e. not just uncertain at family level but Family sp1
sort(unique(dt$Genus)) # looks good

sort(unique(dt$Species))
dt$Species <- word(dt$Species, start=2, end=-1)
sort(unique(dt$Genus))
dt$Taxonomy <- NULL

# Prepare raw data --------------------------------------------------------

# aggregate abundance records that are same species, plot, and survey day.
dt_merged <- dt %>% group_by(across(c(-Abundance,-Biomass))) %>% 
  summarise(Abundance=sum(Abundance), Biomass=sum(Biomass)) %>% ungroup() %>% arrange(Year, Month, Day, Plot, Family, Genus, Species)
nrow(dt) - nrow(dt_merged) # any change in aggregating? 33437 rows because each row corresponded with a body length in original

# now create empty columns needed to fit to template
dt_merged$StudyID <- rep('', nrow(dt_merged))
dt_merged$SampleDescription <- as.factor(with(dt_merged, paste(data.name, Plot, Latitude, Longitude, Year, Month, Day, sep='_')))
length(levels(dt_merged$SampleDescription)) # 1539 samples

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
                         'StudyID')] %>% arrange(Year, Month, Day, Plot, Family, Genus, Species)
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
