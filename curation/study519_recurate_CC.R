

# Curation Script ---------------------------------------------------------

# Dataset: Swedish Bird Surveys: Fixed routes
# Location: Sweden
# Curator: Cher Chow
# Date: 01-Jul-2021

# Set up ------------------------------------------------------------------
# load the necessary packages
require(dplyr)
require(stringr)
require(lubridate)
require(readxl)
require(sf)

rm(list=ls())
data.name <- 'study519_recurate'
curator <- 'CC'

# make sure your working directory is set before running these lines
# Darwin Core Archive format
dt <- read_excel('./Originals/study519/id519KMFRI_data-arch refID12837.xls', skip = 3, sheet = 2)
dt <- dt %>% select(ObservedIndividualCount, ScientificName, EventID, Latitude, Longitude, Eventdate, SamplingProtocol)
View(dt)

unique(dt$SamplingProtocol)

# Structure check ---------------------------------------------------------

nrow(dt) # check dimensions
# 55151 records
str(dt) # check structure
summary(dt)
# Abundance and/or biomass, latitude and longitude numeric? Y

# dates need to be coerced
dt$Date <- parse_date_time(dt$Eventdate, '%m-%Y')
summary(dt$Date)

# Year, month and day must be integers or factors?
dt$Year <- year(dt$Date) %>% as.integer()
dt$Month <- month(dt$Date) %>% as.integer()
dt$Date <- NULL
dt$Eventdate <- NULL

# check if event ID lines up with coordinates and dates
dt %>% distinct(EventID, Latitude, Longitude, Year, Month) %>% 
  arrange(EventID, Year, Month) %>% View
# only BT ones, but not the stations. coordinates are not unique across event IDs
dt$EventID
# logical?
summary(dt[,c('Year', 'Month')]) # yes

# Secondary fields such as trawl, plot, transect etc must be factors or integers?

# Secondary fields such as elevation or depth will normally be numeric unless in a treatment format. NA
# Taxonomic fields must be characters or factors? Y

colnames(dt)[1:2] <- c('Abundance', 'Taxon')

# Primary field check -----------------------------------------------------

# ABUNDANCES
# No negative values, zeroes, or NAs in abundance/biomass fields.
sum(is.na(dt$Abundance)+0)
min(dt$Abundance, na.rm=T) # no zeroes? Y
dt %>% filter(Abundance == 0)
str_which(dt$Abundance, '\\s') %>% length() # no blanks? Y
dt <- dt %>% filter(Abundance > 0)

# LAT LONG
str_which(dt$Latitude, '\\s') %>% length() # no blanks?
str_which(dt$Longitude, '\\s') %>% length()
sum(is.na(dt[c('Latitude', 'Longitude')])) == 0 # no NAs?

# WGS84 coordinates?
# Latitude constrained from -90 to 90.
# Longitude constrained -180 to 180.
summary(dt[c('Latitude', 'Longitude')]) # good

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
sum(is.na(dt$Taxon) + 0) == 0
sum(str_detect(dt$Taxon, '^\\s+$') + 0) == 0
# misspellings check, but not taxonomic cleaning
sort(unique(dt$Taxon))

# create correction vector
sp_correct <- c(
  'sp\\.$|sp\\.' = 'sp',
  'nigripes\\/nigripinnis\\?' = 'sp',
  '\\svar. longicollis' = '',
  'spp.' = 'sp',
  'mahsenoides\\/mahsena\\?' = 'sp',
  'albescens gemmuliferus' = 'albescens'
)
dt$Taxon <- str_replace_all(dt$Taxon, sp_correct)
sort(unique(dt$Taxon))
dt <- dt %>% filter(!Taxon == 'Plat lati?') # remove record for this unknown species

# Now I have to check the one word entries and isolate genera from family +
dt %>% filter(str_count(Taxon, '\\s') == 0) %>% distinct(Taxon)
dt$Family <- ''
dt$Family[str_detect(dt$Taxon, 'idea$|idae$|oda$|ria$') & str_count(dt$Taxon, '\\s') == 0] <- dt$Taxon[str_detect(dt$Taxon, 'idea$|idae$|oda$|ria$') & str_count(dt$Taxon, '\\s') == 0]
dt$Taxon[str_detect(dt$Taxon, 'idea$|idae$|oda$|ria$') & str_count(dt$Taxon, '\\s') == 0] <- ''
dt$Taxon[dt$Taxon == 'Brachyura'] <- 'Brachyura sp'

sort(unique(dt$Taxon))
# check that all specific epithets are one word
rm(sp_correct)

# everything looks ok, separate to Genus and Species
dt$Genus <- word(dt$Taxon, 1)
dt$Species <- word(dt$Taxon, 2, -1)
sort(unique(dt$Genus))
possfam <- dt$Genus[str_detect(dt$Genus, 'idea$|idae$|oda$|ea$')] %>% unique %>% sort
require(taxize)
possfam <- tax_rank(possfam, db = 'worms')
possfam[[9]] <- 'order'
families <- data.frame(rank = unlist(possfam), Genus = dt$Genus[str_detect(dt$Genus, 'idea$|idae$|oda$|ea$')] %>% unique %>% sort)
rownames(families) <- NULL
families <- families %>% filter(rank != 'genus') # remove the true genera names so that this can be used as a key
dt$Family[dt$Genus %in% families$Genus] <- dt$Genus[dt$Genus %in% families$Genus]
dt$Genus[dt$Genus %in% families$Genus] <- ''
dt$Species[dt$Genus %in% families$Genus & dt$Species == 'sp'] <- ''
sort(unique(dt$Genus))

# manual moves
families <- c('Palaemoninae', 'Anguilliformes', 'Pontoniinae')
dt$Family[dt$Genus %in% families] <- dt$Genus[dt$Genus %in% families]
dt$Genus[dt$Genus %in% families] <- ''
dt$Species[dt$Genus %in% families & dt$Species == 'sp'] <- ''

sort(unique(dt$Genus))
sort(unique(dt$Species))
dt %>% filter(Family != '') %>% distinct(Family, Genus, Species) %>% View
dt$Taxon <- NULL # everything looks ok now, remove original

# Prepare raw data --------------------------------------------------------
dt %>% distinct(EventID, Year, Month, SamplingProtocol) %>% 
  arrange(Year, Month, EventID, SamplingProtocol) %>% View

dt <- dt %>% filter(!str_detect(EventID, '^BT')) 
dt$SamplingProtocol <- NULL
# that check shows the beam trawl method with a smaller mesh does not have a time series
# remove those records

# aggregate abundance records that are same species, plot, and survey day.
dt_merged <- dt %>% group_by_at(vars(-Abundance)) %>% 
  summarise(Abundance=sum(Abundance)) %>% ungroup() %>% arrange(Year, Month, EventID, Family, Genus, Species)
nrow(dt) - nrow(dt_merged) # any change in aggregating? no

# now create empty columns needed to fit to template
dt_merged$Biomass <- ''
dt_merged$DepthElevation <- ''
dt_merged$StudyID <- ''
dt_merged$Plot <- ''
dt_merged$Day <- ''
dt_merged$SampleDescription <- as.factor(with(dt_merged, paste(Year, Month, EventID, sep='_')))
length(levels(dt_merged$SampleDescription)) # 148 samples

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