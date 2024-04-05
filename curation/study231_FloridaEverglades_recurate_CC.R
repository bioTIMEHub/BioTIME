

# Curation Script ---------------------------------------------------------

# Dataset: Consumer Stocks Fish Vegetation and other Non-physical Data 
# from Everglades National Park (FCE) South Florida from February 2000 to April 2005
# Recurating to fix densities
# Curator: Cher Chow
# Date: 28 Apr 2023

# Set up ------------------------------------------------------------------
# load the necessary packages
require(dplyr)
require(stringr)
require(lubridate)
require(maps)
require(ggplot2)

rm(list=ls())

# make sure your working directory is set before running these lines
# Darwin Core Archive format
dt <- read.csv('Originals/study231/LT_CD_Trexler_001.csv', header=T)

# Structure check ---------------------------------------------------------

nrow(dt) # check dimensions
# 13624 records
str(dt) # check structure
summary(dt)

dt <- dt[-c(2,4,5,7,9,10)]
dt$Date <- as.POSIXct(dt$Date)
dt$Month <- month(dt$Date)
dt$Date <- NULL

# Abundance and/or biomass, latitude and longitude numeric? Y
# logical?
# Year, month and day must be integers or factors. Y
# Secondary fields such as trawl, plot, transect etc must be factors or integers? NA
dt$SITENAME <- as.factor(dt$SITENAME)
levels(dt$SITENAME)
dt$SITENAME <- str_remove_all(dt$SITENAME, '\\s')
# Secondary fields such as elevation or depth will normally be numeric unless in a treatment format. NA here
# Date should be POSIXct. NA
# Taxonomic fields must be characters or factors? Y
colnames(dt)[3:4] <- c('Taxon', 'Abundance')

# Primary field check -----------------------------------------------------

# ABUNDANCES
# No negative values, zeroes, or NAs in abundance/biomass fields.
dt %>% filter(is.na(Abundance)) %>% View
dt <- dt %>% filter(!is.na(Abundance)) # remove those records
min(dt$Abundance, na.rm=T) # no zeroes? Y
dt %>% filter(Abundance < 0) %>% View
dt <- dt %>% filter(!Abundance <= 0)
str_which(dt$Abundance, '\\s') %>% length() # no blanks? Y

# LAT LONG
# provider coordinates only good for one group of sites
# input coordinates from Coastal Everglades LTER website and calculate centroid per slough group

# load libraries
library(sf)

# 1. Convert data points into point spatial object
coords <- read.csv('originals/study231/FCE_LTER_site_coordinates.csv', header = T)[1:3]
colnames(coords)[2:3] <- c('Latitude', 'Longitude') 
coords_shark <- coords[1:10,] %>% st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()
coords_taylor <- coords[-(1:10),] %>% st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()

# 2. Calculate convex hull, area and centroid
cent_S <- coords_shark %>% st_convex_hull() %>% st_centroid() %>% unlist # get centroid
cent_T <- coords_taylor %>% st_convex_hull() %>% st_centroid() %>% unlist # get centroid
dt$Latitude = ''
dt$Longitude = ''
# add them in depending on site name
dt[str_which(dt$SITENAME, 'Shark'),6] <- cent_S[2]
dt[str_which(dt$SITENAME, 'Shark'),7] <- cent_S[1]
dt[str_which(dt$SITENAME, 'Taylor'),6] <- cent_T[2]
dt[str_which(dt$SITENAME, 'Taylor'),7] <- cent_T[1]

# Taxonomic field check ---------------------------------------------------

# No NAs in taxonomic fields, remove all non-organism records
sum(is.na(dt$Taxon) + 0) == 0
sum(str_detect(dt$Taxon, '^\\s+$') + 0) == 0
sum(is.na(dt$Genus) + 0) == 0

dt %>% filter(str_detect(Taxon, '^\\s+$'))

sort(unique(dt$Taxon)) # a mix of uncertainty levels, some at class/subclass level
# remove unidentified and ones at the class level
dt <- dt %>% filter(!str_detect(Taxon, 'Unidentified')) 
dt$Taxon[str_count(dt$Taxon, '\\W') == 0] %>% unique
dt <- dt %>% filter(!(str_count(dt$Taxon, '\\W') == 0 & str_detect(dt$Taxon, 'era$|nea$|aeta$')))
# move families to their own column
dt$Family[str_count(dt$Taxon, '\\W') == 0 & str_detect(dt$Taxon, 'idae$')] <- dt$Taxon[str_count(dt$Taxon, '\\W') == 0 & str_detect(dt$Taxon, 'idae$')]
dt$Taxon[str_count(dt$Taxon, '\\W') == 0 & str_detect(dt$Taxon, 'idae$')] <- '' # replace with blank

sort(unique(dt$Taxon))
dt <- dt %>% filter(!str_detect(Taxon, 'algae'))
# create a replacement correction object
# old = new
replace <- c('Ameriurus natalis' = 'Ameiurus natalis',
             '\\stp' = '',
             'spp.$' = 'sp',
             'Idataphe' = 'Idiataphe',
             'platyrhinchus' = 'platyrhincus',
             'Parva' = 'parva')

dt$Taxon <- str_replace_all(dt$Taxon, replace)
sort(unique(dt$Taxon))
dt$Genus <- word(dt$Taxon, 1)
dt$Species <- word(dt$Taxon, 2)
sort(unique(dt$Genus))
dt$Genus <- str_replace(dt$Genus, 'Henichromis', 'Henichromis')
sort(unique(dt$Species))
# final check
dt$Taxon <- NULL

# Prepare raw data --------------------------------------------------------

dt$SampleDescription <- with(dt, paste(SITENAME, Year, Month, sep = "_")) %>% as.factor
levels(dt$SampleDescription) %>% length
dt$SITENAME <- NULL

# aggregate abundance records that are same species, plot, and survey day.
dt_merged <- dt %>% group_by_at(colnames(dt)[colnames(dt) != 'Abundance']) %>% 
  summarise(Abundance=sum(Abundance)) %>% 
  ungroup() %>% arrange(Year, Month, SampleDescription, Genus, Species) %>% 
  mutate(Biomass = '', Plot = '', DepthElevation = '', Day = '') %>% 
  relocate(Abundance, Biomass, Family, Genus, Species, SampleDescription, Plot, Latitude, Longitude, DepthElevation, Day, Month, Year)
nrow(dt) - nrow(dt_merged) # any change in aggregating? 5 records

View(dt_merged) # final check :)


# Export final ------------------------------------------------------------

write.csv(dt_merged, 'study231_recurate_CC_rawdata.csv', row.names=F)

# load libraries
library(sf)
library(clipr)
write_clip(dt_merged)

# 1. Convert data points into point spatial object
# assumes your rawdata dataframe is called rawdata
dt_coord <- coords %>% select(Longitude, Latitude) %>% distinct() %>%
  st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()

# 2. Calculate convex hull, area and centroid
centroid <- dt_coord %>% st_convex_hull() %>% st_centroid() %>% unlist # get centroid
write_clip(centroid[c(2,1)]) # copy as lat-long

# transform to mercator to get area in sq km
area <- st_transform(dt_coord, st_crs("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs")) %>%
  st_convex_hull() %>% st_area()  ##get area in sq km
write_clip(area)