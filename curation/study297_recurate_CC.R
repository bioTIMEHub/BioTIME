
## STUDY_ID 297
## MCR LTER: Coral Reef: Long-term Population and Community Dynamics: Other Benthic Invertebrates, ongoing since 2005
## Recurate for fixes of BioTIME v1
## Curator: CC

require(dplyr)
require(stringr)
require(lubridate)

rm(list = ls())

dataset_id <- 'study297_Moorea_recurate_CC'
dt <- read.csv('Originals/study297_knb-lter-mcr.7.28/MCR_LTER_Annual_Survey_Herbiv_Invert_20230619.csv', header = T)
sites <- read.csv('Originals/study297_knb-lter-mcr.7.28/sitesfromxml.csv', header = T)

# Structure check ---------------------------------------------------------

nrow(dt) # check dimensions, 22681 records originally
str(dt) # check structure

dt <- dt %>% select(!Location)
# match in site info
sites$geographicDescription <- str_extract(sites$geographicDescription, '(LTER)\\s[:digit:]')
colnames(sites)[3:6] <- c('Longitude.1', 'Longitude.2', 'Latitude.1', 'Latitude.2')
sites <- tidyr::pivot_longer(sites, cols = Longitude.1:Latitude.2, names_to = c('.value', 'corner'), names_sep = '\\.')

coords <- st_as_sf(sites, coords = c('Longitude', 'Latitude')) %>% group_by(geographicDescription) %>% 
  summarise(geometry = st_union(geometry) %>% st_convex_hull() %>% st_centroid()) %>% 
  mutate(Latitude = st_coordinates(geometry)[,2], Longitude = st_coordinates(geometry)[,1]) %>% 
  rename(Site = geographicDescription)
dt <- left_join(dt, st_drop_geometry(coords), by="Site")

dt$Month <- month(dt$Date)
dt$Day <- day(dt$Date)
dt$Date <- NULL

names(dt$Latitude) <- NULL
names(dt$Longitude) <- NULL

str(dt)
# Abundance and/or biomass, latitude and longitude numeric? Y
# Year, month and day must be integers or factors? Y
# Secondary fields such as trawl, plot, transect etc must be factors or integers? Y
dt$Transect <- as.factor(dt$Transect)
dt$Quadrat <- as.factor(dt$Quadrat)

dt %>% group_by(Site) %>% summarise(nTQ = n_distinct(paste0(Transect, Quadrat)))
# EQUAL :D

# Secondary fields such as elevation or depth will normally be numeric unless in a treatment format. NA here
# Date should be POSIXct. NA. split already.
# Taxonomic fields must be characters or factors? Y

# Primary field check -----------------------------------------------------

# ABUNDANCES
# No negative values, zeroes, or NAs in abundance/biomass fields.
summary(dt)
colnames(dt)[7] <- 'Abundance'
sum(dt$Abundance == "", na.rm=T) # no blanks
dt %>% filter(is.na(Abundance))
dt %>% filter(Abundance == 0) %>% head
dt <- dt %>% filter(!Abundance == 0)
# eliminate 0 or NA records

# YEAR MONTH DAY
# again, no negative values, 0s or NAs, and all are logical
# Year < 2020, month < 12, day < 31
# dates all good

# LAT LONG
summary(coords)

# world_map <- map_data('world') # check whether the GPS coordinates match expectations
# world <- ggplot(world_map) +
#   geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
#   coord_fixed() +
#   labs(x='Longitude', y='Latitude') +
#   theme_bw() + theme(panel.grid=element_blank())
# points <- world + geom_point(data=sites, aes(x=Longitude, y=Latitude), shape=21)
# points

# Taxonomic field check ---------------------------------------------------

# need to coerce common names to scientific names
# check if there is a single scientific name that fits

dt$Species <- dt$Taxonomy
sort(unique(dt$Species))

dt$Species <- str_remove_all(dt$Species, '\\s*\\(1m away\\)')
sort(unique(dt$Species))
dt$Genus <- word(dt$Species, 1)
dt$Species <- word(dt$Species, 2)
sort(unique(dt$Genus))
dt$Taxonomy <- NULL

# Prepare raw data --------------------------------------------------------
dt$Habitat <- str_remove(dt$Habitat, '\\s')
dt %>% group_by(Year, Month) %>% summarise(nsites = paste0(Site, Habitat, Transect, Quadrat) %>% n_distinct)
dt %>% group_by(Year) %>% summarise(nsites = paste0(Site, Habitat, Transect, Quadrat) %>% n_distinct)
# not quite so even throughout years
dt %>% group_by(Year) %>% summarise(nsites = paste0(Site, Habitat) %>% n_distinct)
# some sites were missed in 2005, 2006, 2021
# 2021 due to COVID
# 2006 and 2007 due to zeroes
dt_merged <- dt %>% group_by_at(vars(-Abundance)) %>%
  summarise(Abundance=sum(Abundance)) %>% ungroup() %>% 
  arrange(Year, Month, Day, Site, Habitat, Transect, Quadrat, Genus, Species)
nrow(dt) - nrow(dt_merged) # check if there's any difference, 2 rows "lost"]

# add in blank columns
dt_merged$SampleDescription <- with(dt_merged, paste(Year, Site, Habitat, Transect, Quadrat, sep = "_"))
n_distinct(dt_merged$SampleDescription) # 4149 samples

dt_merged <- dt_merged %>% mutate(Biomass = '', Family = '', Plot = '', 
                                  DepthElevation = str_extract(dt_merged$Habitat, '[:digit:]{2}$'), 
                                  StudyID = '')
# methods specify depth for some habitats

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

# Export final raw data ------------------------------------------------------------

write.csv(dt_merged, paste0(dataset_id, '_rawdata.csv'), row.names=F)
write_clip(dt_merged)



