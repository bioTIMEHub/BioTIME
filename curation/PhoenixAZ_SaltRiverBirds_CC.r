

# Data wrangling for Bateman 2018
# Code adapted by Cher Chow from BioTIMEx code by Alban Sagouis

require(lubridate)
require(dplyr)
require(stringr)

data_string <- 'PhoenixAZ_SaltRiverBirds_CC'
ddata <- read.csv('sDiv/BioTIMEx/data/raw data/bateman_2018/641_bird_observations.csv', header = T)
species <- read.csv('sDiv/BioTIMEx/data/raw data/bateman_2018/speciescodes.csv', header = T)
sites <- read.csv('sDiv/BioTIMEx/data/raw data/bateman_2018/sitecodes.csv', header = T)


# Structure check ---------------------------------------------------------

dim(ddata)
str(ddata)
summary(ddata)

# remove fields we don't need
ddata <- ddata %>% select(code, common_name, Abundance = bird_count, site_code, reach, survey_date)

# match species codes and site metadata
ddata <- ddata %>% left_join(., species[c(1,3)], by = c('code' = 'Species.codes'))
codeless <- ddata %>% filter(is.na(Scientific.name)) %>%
   distinct(code, .keep_all = T) %>% select(code, common_name)

# incomplete species list from source, have to use taxize
require(taxize)
matched_sp <- comm2sci(codeless %>% filter(str_detect(common_name, 'Unidentified', negate = T)) %>%
                          pull(common_name), db = 'itis', simplify = T)
codeless_sp <- data.frame(common_name = names(matched_sp))
matched_sp[which(lengths(matched_sp) == 0)] <- 'NA'
codeless_sp <- do.call(rbind, matched_sp) %>% bind_cols(codeless_sp, .)
View(codeless_sp) # check the options
codeless_sp <- codeless_sp[1:2] # first choice works for most species
colnames(codeless_sp)[2] <- 'Species'

# manual override for some species
codeless_sp$Species[c(7, 45, 28, 4,
                      23, 42, 48, 6, 52)] <- c('Anas sp1',
                                    'Anas sp2',
                                    'Calamospiza melanocorys',
                                    'Setophaga nigrescens',
                                    'Setophaga occidentalis',
                                    'Empidonax difficilis',
                                    'Geothlypis tolmiei',
                                    'Setophaga americana',
                                    'Leiothlypis virginiae')
colnames(ddata)[7] <- 'Species'

# unidentifieds, genus or family
matched_gen <- comm2sci(codeless %>% filter(str_detect(common_name, 'Unidentified')) %>%
                           pull(common_name) %>% str_remove_all(., 'Unidentified[:space:]'), db = 'itis', simplify = T)
codeless_gen <- data.frame(common_name = names(matched_gen))
matched_gen[which(lengths(matched_gen) == 0)] <- 'NA'
codeless_gen <- do.call(rbind, matched_gen) %>% bind_cols(codeless_gen, .)
codeless_gen <- codeless_gen[-3,]
#clipr::write_clip(codeless_gen) # manual override
codeless_gen <- read.csv('data/raw data/bateman_2018/manual_unidentified.csv', header=T)
codeless_gen$common_name <- paste0('Unidentified ', codeless_gen$common_name)

# ugh. new fixed species key
codeless <- bind_rows(codeless_sp, codeless_gen) %>% left_join(., codeless, by="common_name")

# match up the fixed key with the original key
species <- species[1:3]
codeless <- codeless[c(3,1,2)]
colnames(species) <- colnames(codeless)
species <- bind_rows(species, codeless)
ddata$Scientific.name <- NULL #redo name match

# match up again, remove the NAs (because they're above family level)
dt <- left_join(ddata, species[c(1,3)], by='code') %>% filter(!is.na(Species))
dt <- dt %>% select(!common_name, !code) # remove common_name and code now
View(dt) # visual check before removing species matching objects
rm(codeless, codeless_sp, matched_sp, matched_gen, codeless_gen)

# rearrange date
dt$Year <- year(dt$survey_date)
dt$Month <- month(dt$survey_date)
dt$Day <- day(dt$survey_date)
dt$survey_date <- NULL

# field type fixes
dt$site_code <- as.factor(dt$site_code)
# match site info
dt <- left_join(dt, sites[1:4], by=c('reach' = 'Reach'))
dt <- dt %>% select(!reach) %>% rename(Plot = site_code)

# Alban Sagouis curation decisions
# Selecting surveys between January and May, excluding 2018 which has only January
dt <- dt %>% filter(Month < 5, !Year == 2018)
dt$Abundance[which(is.na(dt$Abundance))] <- 1
# reading methods suggest that data format blank = abundance of 1


# Primary field check -----------------------------------------------------

str(dt)
# overall checks first
summary(dt)

# ABUNDANCES
# No negative values, zeroes, or NAs in abundance/biomass fields.
# yes

# YEAR MONTH DAY
# again, no negative values, 0s or NAs, and all are logical
# Year < 2020, month < 12, day < 31
# yes

# LAT LONG
# no blanks, no NAs
# Latitude constrained from -90 to 90.
# Longitude constrained -180 to 180.
# negative sign is not correct
dt$Longitude <- str_remove_all(dt$Longitude, '\\−')
dt$Longitude <- as.numeric(dt$Longitude) * -1

# visual map check
require(ggplot2)
world_map <- map_data('world') # check whether the GPS coordinates match expectations
ggplot(world_map) +
   geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
   geom_point(data = dt, aes(x = Longitude, y = Latitude), size = 2, shape = 21, colour = 'blue') +
   coord_fixed(xlim = c(-130, -100), ylim = c(25,40)) +
   labs(x = NULL, y = NULL) +
   theme_minimal()

# Taxonomic field check ---------------------------------------------------

# No NAs in taxonomic fields, remove all non-organism records
# misspellings check, but not taxonomic cleaning
# check that genera are genera, not family names (-idae/eae)

sum(is.na(dt$Species)+0)
dt$Family <- ''
# move family names to family column
dt$Family[str_which(dt$Species, 'idae$')] <- dt$Species[str_which(dt$Species, 'idae$')]
dt$Species[str_which(dt$Species, 'idae$')] <- NA
# add sp to Genus level IDs, doesn't add it to NAs
dt$Species[which(str_count(dt$Species, '\\s') == 0)] <- paste0(dt$Species[which(str_count(dt$Species, '\\s') == 0)], ' sp')

# check species names before splitting
sort(unique(dt$Species))
# split Genus species
dt$Genus <- word(dt$Species, 1)
dt$Species <- word(dt$Species, 2)
sort(unique(dt$Genus))

dt <- dt %>% rename(DepthElevation = DepthElevation_m)
# aggregate abundance records that are same species, plot, and survey day.
dt_merged <- dt %>% group_by(Family, Genus, Species, Plot, Latitude, Longitude, DepthElevation, Day, Month, Year) %>%
   summarise(Abundance=sum(Abundance)) %>% ungroup() %>% arrange(Year,Month,Day,Plot,Family,Genus,Species)
dim(dt)[1] - dim(dt_merged)[1] # row number change? 3570
dt_merged$SampleDescription <- as.factor(with(dt_merged, paste(data_string, Year, Month, Day, Plot, Latitude, Longitude, sep='_')))
summary(dt_merged$SampleDescription)

dt_merged$Biomass <- ''
dt_merged$StudyID <- ''
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
write.csv(dt_merged, file = paste0('../../', data_string, '_rawdata.csv'), row.names = F)

# Convex Hull for centroid ------------------------------------------------

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
