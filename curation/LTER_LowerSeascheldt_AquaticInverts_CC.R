

# Curation Script ---------------------------------------------------------

# Dataset: LTER Lower Seascheldt Aquatic Invertebrates
# Location: Lower sea Scheldt estuary, Belgium
# Curator: Cher Chow
# Date: 15-Dec-2020

# Set up ------------------------------------------------------------------
# load the necessary packages
require(stringr)
require(dplyr)
require(ggplot2)
require(maps)
require(readxl)

# make sure your working directory is set before running these lines

data.name <- 'LTER_INBO_LowerSeascheldt_AqInvert'
curator <- 'CC'

dt <- read_excel('Originals/HaasePilotto/S005.xlsx', sheet=1, skip=2, col_names=T, na='')
plots <- read_excel('Originals/HaasePilotto/S005.xlsx', sheet=3, skip=1, col_names=T, na='')
plots <- plots[,1:4]
dt$Site <- NULL # remove since it's all the same
colnames(dt)[2] <- 'Site'

# Structure check ---------------------------------------------------------

dim(dt) # check dimensions
str(dt) # check structure
dt$Site <- as.factor(dt$Site)
summary(dt)
# Abundance and/or biomass, latitude and longitude numeric? N
# Year, month and day must be integers or factors? Y
# Secondary fields such as trawl, plot, transect etc must be factors or integers? N
n_distinct(dt$Site) == n_distinct(plots$Site)
# make sure each plot ID has a corresponding coordinate in the plots df
str(dt)
# Secondary fields such as elevation or depth will normally be numeric unless in a treatment format. NA here
# Date should be POSIXct (not applicable in this case)
colnames(dt)[1] <- 'Year' #rename sampling date column to year
colnames(dt)[4] <- 'Abundance'
# Taxonomic fields must be characters or factors? Y


# Primary field check -----------------------------------------------------

# ABUNDANCES
# No negative values, zeroes, or NAs in abundance/biomass fields.

min(dt$Abundance) # check the minimum (no zeroes)
dt <- dt %>% filter(Abundance > 0)
sum(dt$Abundance=="") # no blanks

# LAT LONG
# no blanks, no NAs
# Latitude constrained from -90 to 90.
# Longitude constrained -180 to 180.

# first, we need to match the ones from the plots metadata sheet to the main raw data
colnames(plots) <- c('Site', 'Latitude', 'Longitude', 'DepthElevation')
plots <- plots %>% distinct(Site,Latitude,Longitude,DepthElevation) # remove the duplicate rows
dt <- left_join(dt, plots, by='Site')
summary(dt[c('Latitude', 'Longitude')]) # proper ranges

world_map <- map_data('world') # check whether the GPS coordinates match expectations
world <- ggplot(world_map) + 
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  coord_fixed() + 
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())
points <- world + geom_point(data=dt, aes(x=Longitude, y=Latitude), shape=21)
points

points_zoom <- points + coord_fixed(xlim=c(-10,10), ylim=c(40,60))
points_zoom
rm('points','points_zoom','world','world_map')

# Taxonomic field check ---------------------------------------------------

# No NAs in taxonomic fields, remove all non-organism records
# misspellings check, but not taxonomic cleaning

# first, records with undefined species epithets into the BioTIME format of sp without period
colnames(dt)[3] <- 'Species'
dt$Species <- str_replace_all(dt$Species, 'sp.$', 'sp')
sort(unique(dt$Species))

# check that genera are genera, not family names (-idae/eae)
# messy! Mixed up all in there.
dt$Genus <- word(dt$Species, 1) # create the genus column from the species column
dt$Family <- '' # create a family column to move family names into
dt$Family[str_which(dt$Species, 'idae$')] <- dt$Species[str_which(dt$Species, 'idae$')]
# migrate the family names from species to family
dt$Genus[str_which(dt$Genus, 'idae$')] <- ''
dt$Species[str_which(dt$Species, 'idae$')] <- ''
# delete incorrect rows

sort(unique(dt$Species))
dt <- dt %>% filter(!str_detect(Species, 'Nemertini')) # delete records that are only confident to phylum level
dt$Family[str_which(dt$Species, 'Tubificide zonder haren')] <- 'Tubificidae' # migrate Tubificidae
dt$Species[str_which(dt$Species, 'Tubificide zonder haren')] <- 'Tubificidae sp1' # identifiable but no name
dt$Genus[str_which(dt$Genus, 'Tubificide')] <- '' # delete from genus

# check
sort(unique(dt$Species))
dt$Species <- word(dt$Species, start=2, end=-1)
sort(unique(dt$Genus))
summary(dt)

# Prepare raw data --------------------------------------------------------

# aggregate abundance records that are same species, plot, and survey day.
dt_merged <- dt %>% group_by_at(vars(-Abundance)) %>% 
  summarise(Abundance=sum(Abundance)) %>% ungroup() %>% arrange(Year, Site, Genus, Species)
nrow(dt) - nrow(dt_merged) # any change in aggregating? no
# now create empty columns needed to fit to template
dt_merged$Biomass <- ''
dt_merged$Day <- ''
dt_merged$Month <- ''
dt_merged$StudyID <- ''
dt_merged$Plot <- ''
dt_merged$SampleDescription <- as.factor(with(dt_merged, paste(Year, Site, sep='_')))
length(levels(dt_merged$SampleDescription)) # 285 samples

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
                         'StudyID')] %>% arrange(SampleDescription, Genus, Species)

dt_merged2 <- dt_merged %>% filter(Year >= 2008)
dt_merged <- dt_merged %>% filter(Year <= 2005)

# Export final ------------------------------------------------------------

write.csv(dt_merged, 'LTER_INBO_LowerSeascheldt_AqInverts_19992005_rawdata_CC.csv', row.names=F)
write.csv(dt_merged2, 'LTER_INBO_LowerSeascheldt_AqInverts_20082015_rawdata_CC.csv', row.names=F)

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
