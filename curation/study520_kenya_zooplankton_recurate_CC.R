

# Curation Script ---------------------------------------------------------

# Dataset: Seasonal variation of the zooplankton community at Gazi, Lamu and Malindi (Kenya) sampled between 1990 and 1992
# Location: Kenya
# Curator: Cher Chow

# Set up ------------------------------------------------------------------
# load the necessary packages
require(dplyr)
require(stringr)
require(lubridate)
require(readxl)
require(sf)

rm(list=ls())
data.name <- 'study520_kenya_zooplankton_recurate'
curator <- 'CC'

# make sure your working directory is set before running these lines
# Darwin Core Archive format
dt <- read_excel('Originals/study520_kenya_zooplankton/id520KMFRI_data-arch refID222539.xls', sheet = 2, skip =2)
dt <- dt %>% select(LocationID, WKTFootPrint, SamplingProtocol, Eventdate, ScientificName,
                    `Monthly Average (Spring Tide) Density (#/m^3)`,
                    `Monthly Average (Neap Tide) Density (#/m^3)`)

View(dt)

unique(dt$SamplingProtocol)
dt <- dt %>% filter(!is.na(SamplingProtocol)) # remove records without sampling Protocol described
# will have to split by protocol later on

# Structure check ---------------------------------------------------------

nrow(dt) # check dimensions
# 2156 records
str(dt) # check structure
summary(dt)
# Abundance and/or biomass, latitude and longitude numeric? Y

# dates need to be coerced
sort(unique(dt$Eventdate))
dt$Eventdate <- str_replace_all(dt$Eventdate, '1992-11-19/1992-12-05', '12-1992')
dt$Date <- parse_date_time(dt$Eventdate, '%m-%Y')
summary(dt$Date)

# Year, month and day must be integers or factors?
dt$Year <- year(dt$Date) %>% as.integer()
dt$Month <- month(dt$Date) %>% as.integer()
dt$Eventdate <- NULL
dt$Date <- NULL

# Parse lat longs
dt$WKTFootPrint <- st_as_sfc(dt$WKTFootPrint)
dt$Latitude <- ''
dt$Longitude <- ''

# calculate polygon centroid for polygon rows
dt$Latitude[st_geometry_type(dt$WKTFootPrint) == 'POLYGON'] <- dt$WKTFootPrint[st_geometry_type(dt$WKTFootPrint) == 'POLYGON'] %>% unique %>% .[[1]] %>% st_centroid() %>% unlist %>% .[2]
dt$Longitude[st_geometry_type(dt$WKTFootPrint) == 'POLYGON'] <- dt$WKTFootPrint[st_geometry_type(dt$WKTFootPrint) == 'POLYGON'] %>% unique %>% .[[1]] %>% st_centroid() %>% unlist %>% .[1]

# then convert points to lat-longs
dt$Latitude[st_geometry_type(dt$WKTFootPrint) == 'POINT'] <- dt$WKTFootPrint[st_geometry_type(dt$WKTFootPrint) == 'POINT'] %>% st_coordinates() %>% .[,2]
dt$Longitude[st_geometry_type(dt$WKTFootPrint) == 'POINT'] <- dt$WKTFootPrint[st_geometry_type(dt$WKTFootPrint) == 'POINT'] %>% st_coordinates() %>% .[,1]

# check outputs
summary(dt[c('Latitude', 'Longitude')])
dt$Latitude <- as.numeric(dt$Latitude)
dt$WKTFootPrint <- NULL

# Check sampling consistency across space and time
# and by sampling protocol
dt %>% distinct(SamplingProtocol, Latitude, Longitude, LocationID, Year, Month) %>% 
  arrange(SamplingProtocol, Year, Month, LocationID, Latitude, Longitude) %>% View
# only time series in one protocol, filter and curate
dt <- dt %>% filter(LocationID == 'Gazi Bay') %>% select(!c(SamplingProtocol, LocationID))
# 1518 records left, monthly sampling isn't even throughout the years

# logical?
summary(dt[,c('Year', 'Month')]) # yes

# Secondary fields such as trawl, plot, transect etc must be factors or integers?

# Secondary fields such as elevation or depth will normally be numeric unless in a treatment format. NA
# Taxonomic fields must be characters or factors? Y

# spring neap tide to long
# they're separate sampling events so we can keep them and just remove NAs
colnames(dt)[2:3] <- c('Abundance_spring', 'Abundance_neap')
dt <- dt %>% tidyr::pivot_longer(cols = starts_with("Abundance"), names_to = c('.value', 'tide'), names_sep ="\\_")

# Primary field check -----------------------------------------------------

# ABUNDANCES
# No negative values, zeroes, or NAs in abundance/biomass fields.
sum(is.na(dt$Abundance)+0)
dt <- dt %>% filter(!is.na(Abundance)) # remove the NAs
min(dt$Abundance, na.rm=T) # no zeroes? Y
dt %>% filter(Abundance == 0)
dt <- dt %>% filter(Abundance > 0)
str_which(dt$Abundance, '\\s') %>% length() # no blanks? Y

# LAT LONG
# already checked when we did the conversions

# require(ggplot2)
# world_map <- map_data('world') # check whether the GPS coordinates match expectations
# world <- ggplot(world_map) +
#   geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
#   coord_fixed() +
#   labs(x='Longitude', y='Latitude') +
#   theme_bw() + theme(panel.grid=element_blank())
# points <- world + geom_point(data=dt %>% distinct(Longitude, Latitude, .keep_all=T), 
#                              aes(x=Longitude, y=Latitude), shape=21, fill="orange") +
#   geom_point(data=dt %>% distinct(Longitude, Latitude), aes(x=Longitude, y=Latitude), shape=21)
# 
# points_zoom <- points + coord_fixed(xlim=c(35,45), ylim=c(-8,0))
# points_zoom # all looks good
# 
# rm(list=c('world_map', 'world', 'points', 'points_zoom'))

# Taxonomic field check ---------------------------------------------------

# No NAs in taxonomic fields, remove all non-organism records
sum(is.na(dt$ScientificName) + 0) == 0
sum(str_detect(dt$ScientificName, '^\\s+$') + 0) == 0

# misspellings check, but not taxonomic cleaning
# manual check potential family names
sort(unique(dt$ScientificName[str_count(dt$ScientificName, '\\s') == 0]))

# manual check genus/sp level names
sort(unique(dt$ScientificName[str_count(dt$ScientificName, '\\s') > 0]))

dt$Family <- NA
# move into family column
dt$Family[str_count(dt$ScientificName, '\\s') == 0] <- dt$ScientificName[str_count(dt$ScientificName, '\\s') == 0]
# remove from scientificname
dt$ScientificName[str_count(dt$ScientificName, '\\s') == 0] <- NA
dt$Species <- str_replace_all(dt$ScientificName, 'spp\\.|sp\\.|ssp\\.', 'sp')

sort(unique(dt$Species))

# everything looks ok, separate to Genus and Species
dt$Genus <- word(dt$Species, 1)
dt$Species <- word(dt$Species, 2, -1)
sort(unique(dt$Genus))
sort(unique(dt$Species))
dt$ScientificName <- NULL # everything looks good

# Prepare raw data --------------------------------------------------------
dt %>% group_by(Year) %>% summarise(nsample = n_distinct(paste0(Month, tide)))

# aggregate abundance records that are same species, plot, and survey day.
dt_merged <- dt %>% group_by_at(vars(-Abundance)) %>% 
  summarise(Abundance=sum(Abundance)) %>% ungroup() %>% 
  arrange(Year, Month, tide, Family, Genus, Species)
nrow(dt) - nrow(dt_merged) # any change in aggregating? 49 records
summary(dt$Abundance)

# now create empty columns needed to fit to template
dt_merged <- dt_merged %>% mutate(Biomass = '', Plot = '', DepthElevation = '', Day = '', StudyID = '',
                    SampleDescription = paste(Year, Month, tide, sep='_'))
n_distinct(dt_merged$SampleDescription) # 31 samples

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

# Export final ------------------------------------------------------------

write.csv(dt_merged, paste0(data.name, '_rawdata_', curator, '.csv'), row.names=F)
clipr::write_clip(dt_merged)
### Spatial Geometry Calculations for BioTIME datasets
# paste this chunk into the end of your curation script

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