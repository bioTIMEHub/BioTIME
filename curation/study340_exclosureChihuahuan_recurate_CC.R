

# Curation Script ---------------------------------------------------------

# Dataset: Study 340 Small Mammal Exclosure Study (SMES) Vegetation Data from the Chihuahuan Desert Grassland and Shrubland at the Sevilleta National Wildlife Refuge, New Mexico (1995-2009)
# Location: New Mexico, USA
# Curator: Cher Chow
# Date: 27 Apr 2023

# Set up ------------------------------------------------------------------
# load the necessary packages
require(dplyr)
require(ggplot2)
require(maps)
require(stringr)
require(lubridate)
require(readxl)

# make sure your working directory is set before running these lines
mypath <- getwd() # get the folder that the data's located in
# read in data from LTER
dt <- read.csv('Originals/study340/sev097_smesquad_20201203.csv', header = T)
sp <- read.table('Originals/study340/usda_plants.txt', header = T, sep = ',')

# filter out unwanted columns and records first
# Only keeping the controls in this experiment
# remove records of dead plants
dt <- dt %>% filter(treatment == 'C', cond == 'L') %>% select(!c(userid, date, tapeid))


# Species code match ------------------------------------------------------

# trim the scientific names in the species code reference to only keep species level records
# so that it joins properly
sp$Scientific.Name.with.Author <- word(sp$Scientific.Name.with.Author, start = 1, end = 2)
# then only keep distinct records
sp <- sp %>% distinct(Symbol, Synonym.Symbol, Scientific.Name.with.Author)
sp$codemerge <- sp$Synonym.Symbol
sp$codemerge[sp$codemerge == ''] <- sp$Symbol[sp$codemerge == ''] # make sure something from the data matches with a code

# replace the codes in the LTER data with the correct synonym
dt[dt$kartez %in% sp$Synonym.Symbol, ] %>% View
dt$kartez[dt$kartez %in% sp$Synonym.Symbol] %>% unique # will these have a scientific name match

# how many synonym replacements
sum((unique(dt$kartez) %in% sp$codemerge)+0) # does every code in the data have a corresponding key?
sum((unique(dt$kartez) %in% sp$codemerge == F)+0) # 7 don't
dt$kartez[dt$kartez %in% sp$codemerge == F] %>% unique # what are these codes?
sp %>% filter(str_detect(codemerge, '^ASTE|^MUHL|^SPOR|^POAC|^BRIC')) %>% View # try a manual fuzzy match
# no match for seedsa nd BRIC1

replace <- c('ASTE1' = 'ASTE',
             'MUHL1' = 'MUHL',
             'SPOR1' = 'SPOR',
             'POAC1' = 'POAC')

dt$kartez_val <- str_replace_all(dt$kartez, replace)
 
colnames(sp)[3] <- 'Taxon'
dt <- left_join(dt, sp[c(4,3)], by=c("kartez_val" = "codemerge")) # match the valid codes with scientific names
# did that join work
dt %>% filter(is.na(Taxon)) %>% View
# remove the non valid species codes/species, and then get rid of species codes columns
dt <- dt %>% filter(!is.na(Taxon)) %>% select(!c(kartez, treatment, cond, comment, kartez_val, height))

# because vegetation quadrats are permanent, this information can be put into Plot
dt$Plot <- with(dt, paste(site, block, plot, quad, sep = '_'))
sites <- data.frame(site = c('C', 'G'),
           Latitude = c(34.296, 34.3331),
           Longitude = c(-106.927, -106.736))
dt <- left_join(dt, sites, by="site") # add in lat long data (site-level) from the metadata

# Initial data check ---------------------------------------------------------

str(dt)
summary(dt)

# Abundance and/or biomass, latitude and longitude numeric? Y
# get rid of 0 cover
dt %>% filter(cover == 0) %>% View # always the same species, 2007-2009
dt %>% filter(Taxon == 'Notholaena neglecta') %>% View
# remove. not sure why it's always a cover = 0
dt <- filter(dt, !Taxon == 'Notholaena neglecta')
# Year, month and day must be integers or factors? NA here
dt$record_date <- as.POSIXct(dt$record_date)
dt$Month <- month(dt$record_date)
dt$Day <- day(dt$record_date)
# check that american dates were read in right
summary(dt)
dt$record_date <- NULL # get rid

# Secondary fields such as trawl, plot, transect etc must be factors or integers? NA
dt$Plot <- as.factor(dt$Plot)
str(dt) # 287 levels

# Secondary fields such as elevation or depth will normally be numeric unless in a treatment format. NA here
# Date should be POSIXct Y
# Taxonomic fields must be characters or factors? Y

# ABUNDANCES
# No negative values, zeroes, or NAs in abundance/biomass fields.
sum(dt$cover == "") # no blanks

# YEAR MONTH DAY
# again, no negative values, 0s or NAs, and all are logical
# Year < 2020, month < 12, day < 31

# already checked

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
points

points_zoom <- points + coord_fixed(xlim=c(-115,-100), ylim=c(25,45))
points_zoom
rm(world_map, world, points, points_zoom)

# No NAs in taxonomic fields, remove all non-organism records
sum(is.na(dt$Taxon)+0)
# misspellings check, but not taxonomic cleaning
sort(unique(dt$Taxon))
# check that genera are genera, not family names (-idae/eae)
# looks fine to me. USDA database/species key is reliable
dt$Genus <- word(dt$Taxon, 1)
dt$Species <- word(dt$Taxon, 2)
sort(unique(dt$Genus))
dt$Taxon <- NULL

# Prepare raw data --------------------------------------------------------

dt$SampleDescription <- with(dt, paste(year, season, site, block, plot, quad, sep="_"))
dt <- dt %>% select(!c(season, site, block, plot, quad))

# aggregate abundance records that are same species, plot, and survey day.
dt_merged <- dt %>% group_by(year, Plot, Latitude, Longitude, Month, Day, Genus, Species, SampleDescription) %>% 
  summarise(Biomass=sum(cover)) %>% ungroup() %>% arrange(year, Month, Day, Plot, Genus, Species)
nrow(dt) - nrow(dt_merged) # check if there's any difference, 21 rows
dt_merged$Abundance <- ''
dt_merged$Family <- ''
dt_merged$DepthElevation <- ''
colnames(dt_merged)[1] <- 'Year'
dt_merged <- dt_merged %>% relocate(Abundance, Biomass, Family, Genus, 
                                    Species, SampleDescription, Plot, 
                                    Latitude, Longitude, DepthElevation,
                                    Day, Month, Year)

length(levels(as.factor(dt_merged$SampleDescription))) # check number of sampling events
View(dt_merged) # final check :)

# Export final ------------------------------------------------------------

write.csv(dt_merged, 'study340recurate_rawdata_CC.csv', row.names=F)
clipr::write_clip(dt_merged)

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

