require(dplyr)
require(ggplot2)
require(stringr)
require(readxl)
require(maps)
require(tidyr)
require(sp)

rm(list = ls())
# setwd("../")
# dates are in the ./supporting-documents/ECN_VW_qtext.csv
setwd("./rennie_2017_bats/")


dt <- read.csv("./ECN_BA1.csv", header = TRUE) %>%
  mutate(YMD = as.Date(SDATE, "%d-%b-%y")) %>%
  mutate(Year = as.integer(format(YMD, "%Y"))) %>%
  mutate(Month = as.integer(format(YMD, "%m"))) %>%
  mutate(Day = as.integer(format(YMD, "%d"))) %>%
  filter(!str_detect(FIELDNAME, paste('XX', 'UU', sep = "|"))) %>%
  select(-c(YMD, SDATE))
  

## related tables
# tree_spec
species_code <- read_excel("./support_code.xlsx", sheet=1, range=cell_rows(1:22), col_names=T, na='') %>%
  setNames(c("FIELDNAME", "Taxon", "Common_name")) %>%
  mutate(Genus = word(Taxon, 1)) %>%
  mutate(Species = word(Taxon, 2))

site_code <- read_excel("./support_code.xlsx", sheet=2, range=cell_rows(1:21), col_names=T, na='') %>%
  setNames(c("SITECODE", "Site_name", "Coordinate")) %>%
  mutate(Coordinate = as(char2dms(Coordinate, chd = "°", chm = "'", chs = "\""), "numeric")) %>%
  tidyr::fill(SITECODE) %>%
  tidyr::fill(Site_name) %>%
  mutate(LatLong = rep(c("Latitude", "Longitude"), 10)) %>%
  pivot_wider(names_from = LatLong, values_from = Coordinate)
  

df <- dt %>%
  left_join(species_code) %>%
  ungroup() %>%
  select(-c(FIELDNAME, ACTS, ACTH, ACTF, Common_name)) %>%
  setNames(c("Site", "Transect", "BATLOC_ID",  "Abundance","Year", "Month", "Day", "Taxon", "Genus", "Species")) %>%
  rename(Plot = Transect) %>%
  mutate(Species = replace_na(Species, "sp."))


## check year
n_distinct(df$Year) >= 2


## structure check
dim(df) # check dimensions, returns row and column counts
summary(df)


## data type check

# Abundance or biomass: numeric
# Coordinates: numeric
# Dates: POSIXct or 
# Year, month, day columns,: integers or factors
# Plot: factors or integers
# DepthElevation: numeric or factors (if they're a treatment category)
# Taxonomic: characters or factors

# Abundance and/or biomass, here: Height/DBH as Biomass
is.numeric(df$Abundance) # TRUE


# Year, month and day must be integers or factors
# | means or
is.factor(df$Year) | is.integer(df$Year) # TRUE
is.factor(df$Month) | is.integer(df$Month) # TRUE
is.factor(df$Day) | is.integer(df$Day) # TRUE


# Secondary fields such as trawl, plot, transect etc must be factors or integers? NA
# Secondary fields such as elevation or depth will normally be numeric unless in a treatment format. NA here
# Date should be POSIXct (not applicable in this case) NA, just year

# Taxonomic fields must be characters or factors?
is.factor(df$Taxon) | is.character(df$Taxon) # TRUE


## fixes
df$Year <- as.factor(df$Year)
df$Month <- as.factor(df$Month)
df$Day <- as.factor(df$Day)


## fields
# Abundance/Density
min(df$Abundance) > 0 # no zeroes?
sum(df$Abundance=="") > 0 # no blanks


# Year < 2021, month < 12, day < 31
summary(df[,5])
summary(df[,6])
summary(df[,7])


## map

df <- left_join(df, site_code, by = c("Site" = "SITECODE"))

world_map <- map_data('world') # check whether the GPS coordinates match expectations
world <- ggplot(world_map) + 
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  coord_fixed() + 
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())
points <- world + geom_point(data=df %>% distinct(Latitude, Longitude, Site), 
                             aes(x=Longitude, y=Latitude, fill = Site), shape=21)

points + coord_fixed(xlim=c(-5,5), ylim=c(45,60))


## nomenclature

# check that genera are genera, not family names (-idae/eae)
# this returns the record index number if there are any
str_which(df$Taxon, 'idae$|eae$')
# check the species list for misspellings or non-BioTIME taxonomic convention names
# Do visual checks before splitting taxa up into the three columns.
sort(unique(df$Species)) %>% unique()
# this keeps IDs adjacent to their same-genus misspellings, but only looking at the last few words to check

sort(unique(word(df$Taxon, 1))) # check genera

df <- df %>% 
  mutate(Taxon = NULL)


### prepare and export
df$Biomass <- rep('', nrow(df))
df$DepthElevation <- rep('', nrow(df))
df$StudyID <- rep('', nrow(df))
df$Family <- rep('', nrow(df))

# aggregate abundance records that are same species, plot, and survey day.

df_merged <- df %>% group_by_at(vars(-Abundance)) %>% 
  summarise(Abundance=sum(Abundance)) %>% ungroup() %>% arrange(Year, Family, Genus, Species)

dim(df)[1]-dim(df_merged)[1] # any change in aggregating?

## save dataset
# save the dataset name as an object so we save some typing
dataset_name <- 'ECN_Bats_rennie_2017'
# put in as many non-blank fields unique to the sampling event
# If plot, depthelevation, month, and day are available, I'd add those in too
df_merged$SampleDescription <- as.factor(with(df_merged, paste(Site, Plot, BATLOC_ID, Site_name, Year, sep='_')))

length(levels(df_merged$SampleDescription)) # how many sampling events?

df_merged <- df_merged[c('Abundance',
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
                         'StudyID')] %>% arrange(Year, Family, Genus, Species)
head(df_merged) # final check!


## Export and spreadsheet prep

dir <- "./Curated/"
write.csv(df_merged, paste0(dir, dataset_name, '_ZY_rawdata.csv'), row.names=F, na='') # replace your initials here



### area coordinate

# load libraries
library(sf)
library(clipr)

# 1. Convert data points into point spatial object
# assumes your rawdata dataframe is called rawdata
df_coord <- df_merged %>% select(Longitude, Latitude) %>% distinct() %>%
  st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()

# 2. Calculate convex hull, area and centroid
centroid <- df_coord %>% st_convex_hull() %>% st_centroid() %>% unlist # get centroid
write_clip(centroid[c(2,1)]) # copy as lat-long

# transform to mercator to get area in sq km
# moor house: 7500 ha
# drayton: 190 ha
# north wyke: 250 ha
# wytham: 770 ha
# alice holt forest: 850 ha
# glensaugh: 1125 ha
# sourhope: 1119 ha
# porton down: 1227 ha
# hillsborough: 400 ha
# snowdon: 700 ha
area <- (7500 + 190 + 770 + 850 + 1125 + 1119 + 1227 + 400 + 700) / 100  ##get area in sq km
write_clip(area)

# Plot the geometries -----------------------------------------------------

require(ggplot2)
world_map <- map_data('world') # check whether the GPS coordinates match expectations
ggplot(world_map) +
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  geom_point(data = df_merged, aes(x = Longitude, y = Latitude), size = 2, shape = 21, colour = 'blue') +
  geom_point(aes(x = centroid[1], y = centroid[2]), size = 3, colour = 'green') +
  geom_sf(data = df_coord %>% st_convex_hull(), colour = 'blue', fill = 'transparent') +
  coord_sf(xlim = c(70, 110), ylim = c(25,55)) + # make sure these fit your coordinates!
  labs(x = NULL, y = NULL) +
  theme_minimal()
