

# Dataset: Finland LTER Moths
# Location: Finland, national
# Curator: Cher Chow


require(lubridate)
require(dplyr)
require(stringr)
require(readxl)

data_string <- 'FinlandLTER_Moths_1993-2016_CC'
ddata <- read_excel('Originals/HaasePilotto/S048-S060 S072.xlsx', sheet = 1, skip = 2)[1:4]
sites <- read_excel('Originals/HaasePilotto/S048-S060 S072.xlsx', sheet = 2)

# join up the site metadata
# prep field names
colnames(sites)[5] <- 'Latitude'
colnames(sites)[6] <- 'Longitude'
colnames(sites)[7] <- 'DepthElevation'

ddata <- left_join(ddata, sites[c(1,5:7)], by="trap code")
ddata <- ddata %>% rename(Year = year, Abundance = `individuals per year`)

# Structure check ---------------------------------------------------------

dim(ddata)
str(ddata) # check field types
summary(ddata) # check field values

# Primary field check -----------------------------------------------------

# ABUNDANCES
# No negative values, zeroes, or NAs in abundance/biomass fields.
# yes
sum((ddata$Abundance == '') + 0) # how many blanks
sum(is.na(ddata$Abundance) + 0) # how many NAs

# YEAR MONTH DAY
# again, no negative values, 0s or NAs, and all are logical
# Year < 2020, month < 12, day < 31
# yes

# LAT LONG
# no blanks, no NAs
# Latitude constrained from -90 to 90.
# Longitude constrained -180 to 180.
# yes, numbers look valid

# visual map check
require(ggplot2)
world_map <- map_data('world') # check whether the GPS coordinates match expectations
ggplot(world_map) +
   geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
   geom_point(data = ddata %>% distinct(Latitude, Longitude), 
              aes(x = Longitude, y = Latitude), size = 2, shape = 21, colour = 'blue') +
   labs(x = NULL, y = NULL) +
   theme_minimal()

ggplot(world_map) +
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  geom_point(data = ddata %>% distinct(Latitude, Longitude), 
             aes(x = Longitude, y = Latitude), size = 2, shape = 21, colour = 'blue') +
  coord_fixed(xlim = c(15,30), ylim = c(55,75)) +
  labs(x = NULL, y = NULL) +
  theme_minimal()

# Taxonomic field check ---------------------------------------------------

# No NAs in taxonomic fields, remove all non-organism records
# misspellings check, but not taxonomic cleaning
# check that genera are genera, not family names (-idae/eae)

sum(is.na(ddata$`species name`) + 0)
sort(unique(ddata$`species name`)) # visual check for misspellings
# replace object
replace_sp <- c(
  'Chloroclystis v-ata subsp. v-ata' = 'Chloroclystis v-ata',
  'sp.$' = 'sp'
)
# replace
ddata$Species <- str_replace_all(ddata$`species name`, replace_sp)
# split Genus species
ddata$Genus <- word(ddata$Species, 1)
ddata$Species <- word(ddata$Species, 2)
sort(unique(ddata$Genus))
ddata$`species name` <- NULL

# Prepare raw data --------------------------------------------------------

# aggregate abundance records that are same species, plot, and survey day.
dt_merged <- ddata %>% group_by_at(vars(-Abundance)) %>% 
  summarise(Abundance=sum(Abundance)) %>% ungroup()
nrow(ddata)-nrow(dt_merged) # any change in aggregating? no
# now create empty columns needed to fit to template
dt_merged$Biomass <- ''
dt_merged$Plot <- ''
dt_merged$Family <- ''
dt_merged$Month <- ''
dt_merged$Day <- ''
dt_merged$SampleDescription <- as.factor(with(dt_merged, paste(Year, `trap code`, sep='_')))
length(levels(dt_merged$SampleDescription)) # 282 samples
dt_merged$`trap code` <- NULL

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
                         'Year')] %>% arrange(SampleDescription, Genus, Species)
View(dt_merged) # final check :)
summary(dt_merged)
str(dt_merged)
write.csv(dt_merged, file = paste0(data_string, '_rawdata.csv'), row.names = F)
# clipr::write_clip(dt_merged)

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

