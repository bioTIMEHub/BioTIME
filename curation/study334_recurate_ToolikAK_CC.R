

# Curation Script ---------------------------------------------------------

# Dataset: Above ground plant biomass in a mesic acidic tussock tundra experimental 
# site from 1982 to 20000 Toolik lake Alaska
# Recurating to keep only controls
# Curator: Cher Chow

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
dt <- read.csv('Originals/study334_arcticlter/1982_2015gs81tusbm.csv', header=T, na.strings = c("NA", '.')) # NA/blanks are also expressed as .
colnames(dt)
# B = block
# Q = quadrat

# super wide format needs restructuring before checks begin
dt <- dt %>% filter(Treatment == 'Control') %>% select(Date, Species, ends_with('gm2'), Species.Comments, Comments)
dt <- tidyr::pivot_longer(dt, cols = ends_with('gm2'), names_to = "Quadrat", values_to = "Biomass", values_drop_na = T)
dt$Plot <- str_extract(dt$Quadrat, '^B[:digit:]{1}')
dt$Quadrat <- str_remove_all(dt$Quadrat, 'gm2$') %>% str_remove_all(., '^B[:digit:]+')


# Structure check ---------------------------------------------------------

nrow(dt) # check dimensions
# 16556 records
str(dt) # check structure
summary(dt)

dt$Date <- as.POSIXct(dt$Date, tryFormats = "%d-%b-%Y")
# split dates
dt$Year <- year(dt$Date)
dt$Month <- month(dt$Date)
dt$Day <- day(dt$Date)
dt$Date <- NULL

# Abundance and/or biomass, latitude and longitude numeric? Y
# logical?
# Year, month and day must be integers or factors. Y
# Secondary fields such as trawl, plot, transect etc must be factors or integers? NA
dt$Plot <- as.factor(dt$Plot)
dt$Quadrat <- as.factor(dt$Quadrat)
# Secondary fields such as elevation or depth will normally be numeric unless in a treatment format. NA here
# Date should be POSIXct. NA already split
# Taxonomic fields must be characters or factors? Y

# Primary field check -----------------------------------------------------

# ABUNDANCES
# No negative values, zeroes, or NAs in abundance/biomass fields.
dt %>% filter(is.na(Biomass))
min(dt$Biomass, na.rm=T) # no zeroes? N
dt %>% filter(Biomass <= 0)
dt <- dt %>% filter(Biomass > 0)
str_which(dt$Biomass, '\\s') %>% length() # no blanks? Y
nrow(dt) # 6148 records now

# LAT LONG
# provider coordinates only good for one group of sites
# input coordinates from Coastal Everglades LTER website and calculate centroid per slough group

# NA, just a site centroid
dt$Latitude <- 68.629636
dt$Longitude <- -149.575656

# Taxonomic field check ---------------------------------------------------

# No NAs in taxonomic fields, remove all non-organism records
sum(is.na(dt$Species) + 0) == 0
sum(str_detect(dt$Species, '^\\s+$') + 0) == 0

sort(unique(dt$Species))
dt %>% filter(str_detect(Species, 'comments\\)$|^Other')) %>% 
  distinct(Species, Species.Comments, Comments) %>% View
dt$Species.Comments[str_which(dt$Species.Comments, '^Grass')] <- ''
dt$Species[str_detect(dt$Species, '^Other') & str_detect(dt$Species, 'comments\\)\\s*$')] <- paste0(word(dt$Species.Comments[str_detect(dt$Species, '^Other') & str_detect(dt$Species, 'comments\\)\\s*$')], 1), ' sp')
sort(unique(dt$Species))

dt <- dt %>% filter(!str_detect(Species, 'dead wood|litter')) # remove non-organismal records

# create a replacement correction object
# old = new
replace <- c('Other\\s*|other\\s*' = '',
             '^ sp$' = '',
             'spp.\\s*$|sp.\\s*$' = 'sp')

dt$Species <- str_replace_all(dt$Species, replace)
sort(unique(dt$Species))
dt$Family <- ''
# for general classifications that have a taxonomic name
dt$Family[str_detect(dt$Species, 'Mosses')] <- 'Bryophyta'
dt$Family[str_detect(dt$Species, 'Grasses')] <- 'Poaceae'
dt$Family[str_detect(dt$Species, 'sedges')] <- 'Cyperaceae'
# delete
dt$Species[str_detect(dt$Species, 'Mosses')] <- ''
dt$Species[str_detect(dt$Species, 'Grasses')] <- ''
dt$Species[str_detect(dt$Species, 'sedges')] <- ''
sort(unique(dt$Species))
dt$Species[str_count(dt$Species, '\\s') == 0 & dt$Species != ''] <- str_to_title(dt$Species[str_count(dt$Species, '\\s') == 0 & dt$Species != '']) %>% paste0(., ' sp')

dt$Genus <- word(dt$Species, 1)
dt$Species <- word(dt$Species, 2)

# final check
sort(unique(dt$Genus))
sort(unique(dt$Species))
dt$Comments <- NULL
dt$Species.Comments <- NULL
dt <- dt[-which(dt$Genus == '' & dt$Family == ''), ]
dt %>% filter(Genus == '') %>% distinct(Family, Genus, Species)

# Prepare raw data --------------------------------------------------------

dt %>% distinct(Year, Month, Day, Plot) %>% arrange(Year, Month, Day, Plot) %>% View
dt$SampleDescription <- with(dt, paste(Year, Month, Day, Plot, Quadrat, sep = "_"))
dt$Quadrat <- NULL
n_distinct(dt$SampleDescription)
dt %>% group_by(Year, Month, Day, Plot) %>% summarise(n = length(unique(SampleDescription))) %>% View

# aggregate abundance records that are same species, plot, and survey day.
dt_merged <- dt %>% group_by_at(vars(-Biomass)) %>% 
  summarise(Biomass = sum(Biomass)) %>% 
  ungroup() %>% arrange(Year, Month, Day, SampleDescription, Family, Genus, Species) %>% 
  mutate(Abundance = '', DepthElevation = '') %>% 
  relocate(Abundance, Biomass, Family, Genus, Species, SampleDescription, Plot, Latitude, Longitude, DepthElevation, Day, Month, Year)
nrow(dt) - nrow(dt_merged) # any change in aggregating? 5 records

View(dt_merged) # final check :)


# Export final ------------------------------------------------------------

write.csv(dt_merged, 'study334_recurate_CC_rawdata.csv', row.names=F)