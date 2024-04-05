

# Curation Script ---------------------------------------------------------

# Dataset: Switzerland LWF Vegetation monitoring
# Location: Switzerland
# Curator: Cher Chow
# Date: 30-Dec-2020

# Set up ------------------------------------------------------------------
# load the necessary packages
require(dplyr)
require(ggplot2)
require(stringr)
require(readxl)

dataset_name <- 'LWF_Switzerland_Vegetation'
# make sure your working directory is set before running these lines
dt <- read.csv('Originals/biotime_lwf_vegetation_2022-10-04.csv', header = T, sep = ";")

# curate the fields of everything and then split quadrat plots and circular plots.

# Structure check ---------------------------------------------------------

dim(dt) # check dimensions
str(dt) # check structure
summary(dt)
colnames(dt)[1] <- 'Biomass'
# Abundance and/or biomass, latitude and longitude numeric? Y
# Year, month and day must be integers or factors? Y
# Secondary fields such as trawl, plot, transect etc must be factors or integers? N
# make a concantenated plot field with site name, sample id and vegetation layer
# check first
sort(unique(dt$name))
dt$name <- str_replace_all(dt$name, 'Sch\xe4nis', 'Schanis') # fix encoding error with special character
sort(unique(dt$layer_text))
dt$Plot <- with(dt, paste(name, sample_id, sep="_")) %>% as.factor
sort(unique(dt$Plot)) # check
str(dt)
dt$layer_text <- str_remove_all(dt$layer_text, '\\slayer')

dt <- dt %>% select(!c(name, layer_code, sample_id, layer_cover_pct, layer_height_m))
# Secondary fields such as elevation or depth will normally be numeric unless in a treatment format. Y
# Date should be POSIXct (not applicable in this case)
# Taxonomic fields must be characters or factors? Y

# Primary field check -----------------------------------------------------

# BIOMASS
# No negative values, zeroes, or NAs in abundance/biomass fields.

min(dt$Biomass) # check the minimum (no zeroes)
sum(dt$Biomass=="") # no blanks

# YEAR MONTH DAY
# again, no negative values, 0s or NAs, and all are logical
# Year < 2020, month < 12, day < 31

summary(dt)

# LAT LONG
# no blanks, no NAs
# Latitude constrained from -90 to 90.
# Longitude constrained -180 to 180. Y

colnames(dt) <- str_to_title(colnames(dt))
colnames(dt)[7] <- 'DepthElevation'

world_map <- map_data('world') # check whether the GPS coordinates match expectations
world <- ggplot(world_map) + 
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  coord_fixed() + 
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())
points <- world + geom_point(data=dt %>% distinct(Latitude, Longitude), 
                             aes(x = Longitude, y = Latitude), shape=21, size = 3, color = 'blue')
points

points_zoom <- points + coord_fixed(xlim=c(min(dt$Longitude)-10, max(dt$Longitude)+10), ylim=c(min(dt$Latitude)-10, max(dt$Latitude)+10))
points_zoom
rm(world, points, points_zoom, world_map)

# Taxonomic field check ---------------------------------------------------

# No NAs in taxonomic fields, remove all non-organism records
# misspellings check, but not taxonomic cleaning
# check that genera are genera, not family names (-idae/eae)
sort(unique(dt$Genus)) # genus
dt %>% distinct(Genus, Species) %>% arrange(Genus, Species) %>% pull(Species) # specific epithets in order of genus species
# first, records with undefined species epithets into the BioTIME format of sp without period
dt$Species <- dt$Species %>% str_replace_all(., 'sp.$', 'sp')

sort(unique(dt$Family))

# Prepare raw data--------------------------------------------------------
dt %>% distinct(Year, Month, Plot) %>% arrange(Year, Plot) %>% View
dt <- dt %>% arrange(Year, Plot, Layer_text, Family, Genus, Species) # sort the original dataset

# split circular plots from quadrats
dt500 <- dt %>% filter(Sampled_area_m2 == 500)
dt1 <- dt %>% filter(Sampled_area_m2 == 1)

## Quadrats (dt1)--------------------------------------------------------
dt1$Sampled_area_m2 <- NULL
dt500$Sampled_area_m2 <- NULL
dt1$Other_observations <- NULL
dt500$Other_observations <- NULL

# aggregate abundance records that are same species, plot, and survey day.
dt1_merged <- dt1 %>% group_by_at(vars(-Biomass)) %>% 
  summarise(Biomass=sum(Biomass)) %>% ungroup()
dim(dt1)[1]-dim(dt1_merged)[1] # any change in aggregating? no
# now create empty columns needed to fit to template

dt1_merged$Abundance <- ''
dt1_merged$StudyID <- ''
dt1_merged$SampleDescription <- as.factor(with(dt1_merged, paste(Year, Plot, Layer_text, sep='_')))
length(levels(dt1_merged$SampleDescription)) # 4356 samples

# reorder columns by BioTIME format
dt1_merged <- dt1_merged[c('Abundance',
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
                         'Year')] %>% arrange(Year, Plot, Family, Genus, Species)
View(dt1_merged) # final check :)

## Large plots (dt500)--------------------------------------------------------

# aggregate abundance records that are same species, plot, and survey day.
dt500_merged <- dt500 %>% group_by_at(vars(-Biomass)) %>% 
  summarise(Biomass=sum(Biomass)) %>% ungroup() %>% arrange(Year, Plot, Family, Genus, Species)
dim(dt500)[1]-dim(dt500_merged)[1] # any change in aggregating? no
# now create empty columns needed to fit to template

dt500_merged$Abundance <- ''
dt500_merged$StudyID <- ''
dt500_merged$SampleDescription <- as.factor(with(dt500_merged, paste(Year, Plot, Layer_text, sep='_')))
length(levels(dt500_merged$SampleDescription)) # 484 samples

# reorder columns by BioTIME format
dt500_merged <- dt500_merged[c('Abundance',
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
                           'Year')]
View(dt500_merged) # final check :)

# Export final ------------------------------------------------------------

write.csv(dt1_merged, paste(dataset_name, 'quadrats', 'rawdata_CC.csv', sep="_"), row.names=F)
write.csv(dt500_merged, paste(dataset_name, 'circular500', 'rawdata_CC.csv', sep="_"), row.names=F)

# clipr::write_clip(dt1_merged)

# Convex Hull for centroid ------------------------------------------------

# load libraries
library(sf)
library(clipr)

# 1. Convert data points into point spatial object
# assumes your rawdata dataframe is called rawdata
dt_coord <- dt1_merged %>% select(Longitude, Latitude) %>% distinct() %>%
  st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()

# 2. Calculate convex hull, area and centroid
centroid <- dt_coord %>% st_convex_hull() %>% st_centroid() %>% unlist # get centroid
write_clip(centroid[c(2,1)]) # copy as lat-long


