# Curation Script ---------------------------------------------------------

# Dataset: Reed_2022 (SBC LTER: Reef: Annual time series of biomass for kelp forest species, ongoing since 2000)
# Location: Santa Babara
# Curator: James Cant
# Date: 07-Jun-2023

# ------------------------------------------------------------------
# load the necessary packages
require(tidyverse)
require(maps)
require(readxl)
require(stringr)
require(dplyr)
require(sf)

# clear workspace
rm(list=ls())

# assign book-keeping details.
data.name <- 'Reed_2022'
curator <- 'JC'

# READ IN DATA ---------------------------------------------------
## CC overwrote 
dt <- read.csv('/Users/cher/Dropbox/towards BioTIME v2/originalData/v2/JC/Annual_All_Species_Biomass_at_transect_20230814.csv', header = T, na.strings = c('NA', '-99999'))
View(dt)
# This file contains annual estimates of biomass of approximately 225 taxa of reef algae, invertebrates and fish in permanent transects at 11 kelp forest sites in the Santa Barbara Channel.

# ADD SPATIAL INFORMATION ---------------------------------------------------
# 2-8 transects are carried out across 11 different sites. Each transect is georeferenced (has a define lat - long corresponding with starting location)
# identify unique site transect combinations
SiteList <- unique(dt[,c('SITE', 'TRANSECT')])
# store coordinates associated with each location (obtained from the SBC LTER portal https://sbclter.msi.ucsb.edu/research/sampling_sites/)
# on the portal there are gps coordinates provided for all but 4 of the individual transects - for consistency site coordinates used instead.
SiteList <- unique(dt[,c('SITE')])
Coords <- matrix(c(34.3916319, -119.5416933,
                   34.4221216, -119.95154, 
                   34.45850533, -120.33349, 
                   34.3940708, -119.72957, 
                   34.4137165, -119.8221,
                   34.402783, -119.85755,
                   34.46774988, -120.11905,
                   34.400275, -119.7445915, 
                   34.471817, -120.1426165, 
                   34.0444335, -119.71513, 
                   34.05865, -119.75763), byrow = TRUE, ncol = 2)
# assign site coordinates
dt$Latitude <- ''
dt$Longitude <- ''
for(ii in 1:length(SiteList)) {
  dt[dt$SITE==SiteList[ii],]$Latitude <- Coords[ii,1]
  dt[dt$SITE==SiteList[ii],]$Longitude <- Coords[ii,2]
}
dt$DepthElevation <- NA

# REFORMAT TAXONIMIC DETAILS ---------------------------------------------------

# check that genera are genera, not family names (-idae/eae)
dt[which(str_detect(dt$TAXON_GENUS, 'idae$|ini$')),] 
# Remove entries with missing species IDs 
dt <- dt[!(is.na(dt$TAXON_GENUS)),]
# rename variable
names(dt)[20] <- 'Genus'
sort(unique(dt$Genus)) # Manual inspection to confirm

# check species names
dt$Species <- word(dt$SCIENTIFIC_NAME, 2)
sort(unique(dt$Species))
# unify notation for unknown species
dt$Species <- dt$Species %>% str_replace_all(., 'spp.', 'sp')
dt$Species <- dt$Species %>% str_replace_all(., 'YOY', 'sp')
dt$Species <- dt$Species %>% str_replace_all(., 'sp.', 'sp')
dt$Species <- dt$Species %>% str_replace_all(., 'salma;', 'salma')
dt$Species <- dt$Species %>% str_replace_all(., 'diffusa;', 'diffusa')
dt$Species <- dt$Species %>% str_replace_all(., 'corymbiferus;', 'corymbiferus')
sort(unique(dt$Species)) # Manual inspection to confirm
# remove unnecessary taxonomic variables variable
dt$SCIENTIFIC_NAME <- NULL
dt$COMMON_NAME <- NULL
dt$SP_CODE <- NULL
dt$TAXON_KINGDOM <- NULL
dt$TAXON_PHYLUM <- NULL
dt$GROUP <- NULL
dt$MOBILITY <- NULL
dt$GROWTH_MORPH <- NULL
dt$COARSE_GROUPING <- NULL
# Rename remaining taxonomic variables
names(dt)[12] <- 'Class'
names(dt)[13] <- 'Order'
names(dt)[14] <- 'Family'
# Reassign classification of variables
dt$Class <- as.character(dt$Class)
dt$Order <- as.character(dt$Order)
dt$Family <- as.character(dt$Family)
dt$Genus <- as.character(dt$Genus)
# Condense Order classification details into family variable for instances for which no family details provided
dt[is.na(dt$Family),]$Family <- dt[is.na(dt$Family),]$Order
# Remove higher taxonomic rank variables
dt$Class <- NULL
dt$Order <- NULL


# REFORMAT ABUNDANCE/BIOMASS DATA ---------------------------------------------------
# This dataset density counts, percentage cover, and biomass (wet & dry). 
# For consistency only dry biomass will be retained as the key variable

# which biomass is more complete
sum(dt$DRY_GM2 == 0 | is.na(dt$DRY_GM2))
sum(dt$SFDM == 0 | is.na(dt$SFDM))

names(dt)[10] <- 'Biomass'
dt <- dt %>% select(!c(PERCENT_COVER:WM_GM2, SFDM))
# No negative values, zeroes, or NAs in abundance/biomass fields.
dt <- dt[!(is.na(dt$Biomass)),]
dt <- dt[which(dt$Biomass != 0),]
# check to confirm
min(dt$Biomass) > 0 # no zeroes?
sum(dt$Biomass == "" | dt$Biomass == ' ') == 0 # no blanks?

# REFORMAT DATE INFORMATION --------------------------------------------------
# Ensure Year and Month are factors
dt$Year <- as.factor(dt$YEAR); dt$YEAR <- NULL
dt$Month <- as.factor(dt$MONTH); dt$MONTH <- NULL
# isolate day details
dt$DATE <- as.character(dt$DATE)
dt$Day <- substr(dt$DATE, 9, nchar(dt$DATE)); dt$DATE <- NULL
dt$Day <- as.factor(dt$Day)

# Fix remaining column names
names(dt)[1] <- 'Site'; names(dt)[2] <- 'Transect'

# Check LAT LONGs
# check coordinates are in numeric format
is.numeric(dt$Latitude); is.numeric(dt$Longitude) 
dt$Latitude <- as.numeric(dt$Latitude); dt$Longitude <- as.numeric(dt$Longitude)
str_detect(dt %>% pull(Latitude, Longitude), '\\s') %>% sum() == 0 # no spaces?

# Check the coordinates match with the expected location of the data
world_map <- map_data('world')
world <- ggplot(world_map) + 
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  coord_fixed() + 
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())
points <- world + geom_point(data=dt %>% distinct(Longitude, Latitude), aes(x=Longitude, y=Latitude), shape=21)

points_zoom <- points + coord_fixed(xlim=c(-122,-117), ylim=c(30,36))
points_zoom # all looks good
# clean memory
rm(list=c('world_map', 'world', 'points', 'points_zoom'))

# PREPARE RAW DATA --------------------------------------------------------

# aggregate abundance records that are same species, site, transect, and survey day.
dt_merged <- dt %>% select(!c(VIS, TAXON_GENUS)) %>% 
  group_by_at(vars(-Biomass)) %>% 
  summarise(Biomass=sum(Biomass)) %>% ungroup() %>% 
  dplyr::arrange(Year, Site, Transect, Month, Day, Genus, Species) %>% 
  mutate(Abundance = '', StudyID = '', Plot = '', DepthElevation = '',
         SampleDescription = paste(Site, Transect, Year, Month, Day, sep='_'))
nrow(dt) - nrow(dt_merged) # any change in aggregating?
# now create empty columns needed to fit to template

n_distinct(dt_merged$SampleDescription) # 8 samples (corresponds with the different transects run)

# Remove variables not used in BIOTIME format
dt_merged$Site <- NULL
dt_merged$Transect <- NULL

# reorder columns to match BioTIME format
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
                         'StudyID')] %>% arrange(Year, Month, Day, Plot, Genus, Species)
View(dt_merged) # final check :)
summary(dt_merged)
str(dt_merged)

# Export final ------------------------------------------------------------

write.csv(dt_merged, paste0(fileSave, data.name, '_rawdata_', curator, '.csv'), row.names=F)
write_clip(dt_merged)

# This study has multiple gps coordinates so it is nessecary to estimate a central location for the study.
# 1. Convert data points into point spatial object
# assumes your rawdata dataframe is called rawdata
dt_coord <- dt_merged %>% select(Longitude, Latitude) %>% distinct() %>%
  st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()

# 2. Calculate convex hull, area and centroid
centroid <- dt_coord %>% st_convex_hull() %>% st_centroid() %>% unlist # get centroid

# transform to mercator to get area in sq km
area <- st_transform(dt_coord, st_crs("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs")) %>%
  st_convex_hull() %>% st_area()  ##get area in sq km

# -------------------------------------------- End of Code ------------