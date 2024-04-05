

# Curation Script ---------------------------------------------------------

# Dataset: Long-term (2005–2017) macromoth community monitoring at Mt. Jirisan National Park, South Korea
# Location: Mt Jirisan, South Korea
# Curator: Cher Chow
# Date: 05-Jul-2021

# Set up ------------------------------------------------------------------
# load the necessary packages
require(tidyverse)
require(lubridate)
require(maps)
require(CoordinateCleaner)

rm(list=ls())
data.name <- 'KLTER_Macromoths_MtJirisan-SKorea_20052017'
curator <- 'CC'

# make sure your working directory is set before running these lines
# Darwin Core Archive format
dt <- read.csv('Originals/KoreanMacromoth/ERDP-2019-02.2.2-Jirisan_data.csv', header=T)
sites <- read.table('Originals/KoreanMacromoth/ERDP-2019-02.3.1-Jirisan_info.txt', sep=',', header=T)
taxa <- read.table('Originals/KoreanMacromoth/ERDP-2019-02.4.2-Jirisan_taxa.txt', sep=',', header=T)
View(dt)
View(sites)
View(taxa)

dt <- dt[-13529,] # remove the grand total row

angle2dec <- function(angle) {
  angle <- as.character(angle)
  x <- do.call(rbind, str_split(angle, pattern='‘|’|“'))
  x <- apply(x, 1L, function(y) {
    y <- as.numeric(y)
    y[1] + y[2]/60 + y[3]/3600
  })
  return(x)
}

# Structure organisation --------------------------------------------------

# add the metadata from the sites sheet
dt[,c(1,8)] <- NULL # remove some of the unnecessary fields
# convert the coordinates to decimal degrees
sites$decLat <- angle2dec(sites$Latitude)
sites$decLon <- angle2dec(sites$Longitude)
colnames(sites)[1] <- 'Site'

# check that plot names will line up before joining
sort(unique(dt$Site))
sort(unique(sites$Site))
sites$Site[1] <- 'CEH'

dt <- left_join(dt, sites[-c(2:4)], by='Site')

# rename fields
colnames(dt) <- c('Site', 'Taxa', 'Abundance', 'Date', 'Year', 'Month', 'DepthElevation', 'Latitude', 'Longitude')


# Structure check ---------------------------------------------------------

nrow(dt) # check dimensions
# 13528 records
str(dt) # check structure
summary(dt)
# Abundance and/or biomass, latitude and longitude numeric? Y

# dates need to be coerced
dt$Date <- as.POSIXct(dt$Date, tryFormats=('%Y-%m-%d'))
summary(dt$Date)

# Year, month and day must be integers or factors?
dt$Day <- day(dt$Date) %>% as.integer()
dt$Date <- NULL

# logical?
summary(dt[,c('Year', 'Month', 'Day')]) # yes

# Secondary fields such as trawl, plot, transect etc must be factors or integers?
dt$Site <- as.factor(dt$Site)
levels(dt$Site) %>% length() # 6 plots

# Secondary fields such as elevation or depth will normally be numeric unless in a treatment format. Y
# Taxonomic fields must be characters or factors? Y

# Primary field check -----------------------------------------------------

# ABUNDANCES
# No negative values, zeroes, or NAs in abundance/biomass fields.
sum(is.na(dt$Abundance)+0)
min(dt$Abundance, na.rm=T) # no zeroes? Y
dt %>% filter(is.na(Abundance)) %>% filter(Abundance == 0)
str_which(dt$Abundance, '\\s') %>% length() # no blanks? Y

# LAT LONG
str_which(dt$Latitude, '\\s') %>% length() # no blanks?
str_which(dt$Longitude, '\\s') %>% length()
sum(is.na(dt[c('Latitude', 'Longitude')])+0) # no NAs?

# WGS84 coordinates?
# Latitude constrained from -90 to 90.
# Longitude constrained -180 to 180.
summary(dt[c('Latitude', 'Longitude')]) # good

# world_map <- map_data('world') # check whether the GPS coordinates match expectations
# world <- ggplot(world_map) + 
#   geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
#   coord_fixed() + 
#   labs(x='Longitude', y='Latitude') +
#   theme_bw() + theme(panel.grid=element_blank())
# points <- world + geom_point(data=coord.test %>% distinct(Longitude, Latitude, .keep_all=T), aes(x=Longitude, y=Latitude), shape=21, fill="orange") +
#   geom_point(data=dt %>% distinct(Longitude, Latitude), aes(x=Longitude, y=Latitude), shape=21)
# 
# points_zoom <- points + coord_fixed(xlim=c(120,135), ylim=c(30,45))
# points_zoom # all looks good
# 
# rm(list=c('world_map', 'world', 'points', 'points_zoom'))

# Taxonomic field check ---------------------------------------------------

# No NAs in taxonomic fields, remove all non-organism records
sum(is.na(dt$Species) + 0) == 0
sum(str_detect(dt$Species, '^\\s+$') + 0) == 0
sum(is.na(dt$Genus) + 0) == 0
sum(str_detect(dt$Genus, '^\\s+$') + 0) == 0
dt <- dt[-10898,] # remove NA row

colnames(taxa)[2] <- 'Taxa'
dt <- left_join(dt, taxa[2:3], by="Taxa") # match family names provided to scientific names

# misspellings check, but not taxonomic cleaning
# too many moth species, use taxize for cleaning help (Open Tree of Life)
require(taxize)
namecheck <- tol_resolve(names=sort(unique(dt$Taxa)), context_name = 'Insects')
namecheck <- namecheck %>% filter(is_synonym == T | approximate_match == T)

# Check the species records
dt$Taxa[which(str_count(dt$Taxa, '\\s') > 1)] %>% sort(.) %>% unique(.) # any subspecies?

# create correction vector
sp_correct <- namecheck$unique_name
names(sp_correct) <- paste(namecheck$search_string %>% word(., 1) %>% str_to_title(), namecheck$search_string %>% word(., 2), sep=' ')

dt$Species <- str_replace_all(dt$Taxa, sp_correct)
# check that all specific epithets are one word
dt$Genus <- word(dt$Species, 1)
dt$Species <- word(dt$Species, 2, -1)
rm(sp_correct)

dt %>% filter(str_detect(Species, 'sp.$')) # any unidentified species?

# Prepare raw data --------------------------------------------------------

# aggregate abundance records that are same species, plot, and survey day.
dt_merged <- dt %>% group_by_at(vars(-Abundance)) %>% 
  summarise(Abundance=sum(Abundance)) %>% ungroup()
nrow(dt) - nrow(dt_merged) # any change in aggregating? 131

# now create empty columns needed to fit to template
dt_merged$Biomass <- ''
dt_merged$StudyID <- ''
dt_merged$Plot <- ''
dt_merged$SampleDescription <- as.factor(with(dt_merged, paste(Year, Month, Site, sep='_')))
length(levels(dt_merged$SampleDescription)) # 461 samples

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
                         'StudyID')] %>% arrange(SampleDescription, Family, Genus, Species)
View(dt_merged) # final check :)


# Export final ------------------------------------------------------------

write.csv(dt_merged, paste0(data.name, '_rawdata_', curator, '.csv'))
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
