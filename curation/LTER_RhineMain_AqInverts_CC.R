

# Curation Script ---------------------------------------------------------

# Dataset: LTER Rhine Main Observatory Aquatic benthic invertebrates
# Location: Rhine Main Observatory, Germany
# Curator: Cher Chow
# Date: 18-Dec-2020

# Set up ------------------------------------------------------------------
# load the necessary packages
require(dplyr)
require(ggplot2)
require(maps)
require(stringr)
require(lubridate)
require(readxl)

rm(list=ls()) # clear up the environment before starting

# make sure your working directory is set before running these lines
dt1 <- read_excel('Originals/HaasePilotto/S037-S039.xlsx', sheet=1, skip=3, col_names=T, na='')
dt2 <- read_excel('Originals/HaasePilotto/S037-S039.xlsx', sheet=2, skip=3, col_names=T, na='')
dt3 <- read_excel('Originals/HaasePilotto/S037-S039.xlsx', sheet=3, skip=3, col_names=T, na='')
sites <- read_excel('Originals/HaasePilotto/S037-S039.xlsx', sheet=4, col_names=T, na='')

# Structure check ---------------------------------------------------------

colnames(sites) <- c('Site', 'Latitude', 'Longitude', 'Elevation')
sites$Site[1:2] <- c('KiO3', 'Bieb')

# bind them all together with site info
dt <- bind_rows(dt1, dt2, dt3) %>% full_join(., sites, by="Site")
rm(list=c('dt1','dt2','dt3')) # delete the separate tibbles

str(dt) # check dimensions
summary(dt)
dt$Site <- as.factor(dt$Site)
# Abundance and/or biomass, latitude and longitude numeric? Y
colnames(dt)[5] <- 'Abundance'

# check if these columns need to be kept

# Year, month and day must be integers or factors? N
unique(dt$Date)
dt$Date <- as.POSIXct(dt$Date, format='%d.%m.%Y')
dt$Month <- month(dt$Date) %>% as.factor()
dt$Day <- day(dt$Date) %>% as.factor()
dt$Year <- as.factor(dt$Year)
str(dt)
dt$Date <- NULL

summary(dt)
# Secondary fields such as trawl, plot, transect etc must be factors or integers? Y

# Secondary fields such as elevation or depth will normally be numeric unless in a treatment format. NA here
colnames(dt)[7] <- 'DepthElevation'
# Date should be POSIXct (not applicable in this case) Y
# Taxonomic fields must be characters or factors? Y

# Primary field check -----------------------------------------------------

# ABUNDANCES
# No negative values, zeroes, or NAs in abundance/biomass fields.

min(dt$Abundance) # check the minimum (no zeroes) Y
sum(dt$Abundance=="") # no blanks Y

# YEAR MONTH DAY
# again, no negative values, 0s or NAs, and all are logical
# Year < 2020, month < 12, day < 31

summary(dt[c('Year', 'Month','Day')]) # looks good to me

# LAT LONG
# no blanks, no NAs
# Latitude constrained from -90 to 90.
# Longitude constrained -180 to 180.
summary(dt[,5:6])

world_map <- map_data('world') # check whether the GPS coordinates match expectations
world <- ggplot(world_map) + 
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  coord_fixed() + 
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())
points <- world + geom_point(data=dt %>% distinct(Latitude, Longitude), aes(x=Longitude, y=Latitude), shape=21)

points_zoom <- points + coord_fixed(xlim=c(7,12), ylim=c(45,55))

# all looks good

# Taxonomic field check ---------------------------------------------------

# No NAs in taxonomic fields, remove all non-organism records
# misspellings check, but not taxonomic cleaning
# check that genera are genera, not family names (-idae/eae)
dt$Species <- dt$Taxon # make a copy
# check the species list for misspellings or non-BioTIME taxonomic convention names
sort(unique(dt$Species))
dt$Species <- str_replace_all(dt$Species, 'sp\\.', 'sp')

#move any famiy level records
dt$Family <- rep('', nrow(dt))
dt$Family[str_which(dt$Taxon, regex('gen\\. sp\\.$', T))] <- word(dt$Taxon[str_which(dt$Taxon, regex('gen\\. sp\\.$', T))], 1)
# make species column blank
dt$Species[str_which(dt$Taxon, regex('gen\\. sp\\.$', T))] <- ''

sort(unique(dt$Species))

replace_sp <- c( #old string = new string
  'Athripsodes bilineatus bilineatus' = 'Athripsodes bilineatus',
  'fuscatus\\/scambus' = 'sp1',
  'lugubris\\/polychroa' = 'sp1',
  '\\sLv.$' = '',
  '\\sAd.$' = '',
  '\\-Gr\\.' = ' agg.',
  'digitatus\\/radiatus\\/tesselatus' = 'sp1',
  'digitatus\\/tesselatus' = 'sp2',
  'dorsalis\\/nubila\\/pascoei\\/simulatrix\\/vulgaris' = 'sp', 
  's. str. sp' = 'sp',
  'casertanum casertanum' = 'casertanum',
  'posticatum/rostratum' = 'sp1',
  'nigricornis/piceus' = 'sp1',
  'aenea/maugetii/rietscheli/rioloides' = 'sp1',
  'aenea/maugetii' = 'sp2',
  'flavomaculatus flavomaculatus' = 'flavomaculatus',
  'flavicorne/personatum' = 'sp1'
)
dt$Species <- str_replace_all(dt$Species, replace_sp) # replace
sort(unique(dt$Species))

dt$Genus[str_detect(dt$Species, '\\s\\([:alpha:]+\\)\\s')] <- word(dt$Species[str_detect(dt$Species, '\\s\\([:alpha:]+\\)\\s')], start = 1, end = 2)
dt$Species[str_detect(dt$Species, '\\s\\([:alpha:]+\\)\\s')] <- word(dt$Species[str_detect(dt$Species, '\\s\\([:alpha:]+\\)\\s')], start = 2, end = -1)
dt$Genus[str_which(dt$Species, '\\(', negate = T)] <- word(dt$Species[str_which(dt$Species, '\\(', negate = T)], 1)
sort(unique(dt$Genus)) # check genera too just in case the eyes didn't pick up on that

sort(unique(dt$Genus)) # check genera too just in case the eyes didn't pick up on that
dt$Genus <- str_to_title(dt$Genus)
dt$Species <- word(dt$Species, start=2, end=-1)
dt$Taxon <- NULL # get rid of it now that we've split and checked

# check family too
sort(unique(dt$Family))
dt <- dt %>% filter(!str_detect(Family, '\\/')) # remove records where even Family level is uncertain
nrow(dt)
# now 2296 records, down from 2369

# Prepare raw data --------------------------------------------------------
# now create empty columns needed to fit to template

dt %>% distinct(Year, Month, Day, Site) %>% View # check the sampling intervals
dt %>% group_by(Site) %>% summarise(nyear = n_distinct(Year))

# aggregate abundance records that are same species, plot, and survey day.
dt_merged <- dt %>% group_by_at(vars(-Abundance)) %>% 
  summarise(Abundance=sum(Abundance)) %>% ungroup() %>% arrange(Year, Site)
nrow(dt)-nrow(dt_merged) # any change in aggregating? yep. 529 rows compressed (probably from the unknown sp records)

dt_merged$SampleDescription <- as.factor(with(dt_merged, paste(Year, Site, sep='_')))
length(levels(dt_merged$SampleDescription)) # 40 samples

dt_merged$Biomass <- ''
dt_merged$StudyID <- ''
dt_merged$Plot <- ''

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
                         'StudyID')]
View(dt_merged) # final check :)
summary(dt_merged)
str(dt_merged)

# Export final ------------------------------------------------------------

write.csv(dt_merged, 'LTER_RhineMain_AquaticInverts_rawdata_CC.csv', row.names=F)

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
