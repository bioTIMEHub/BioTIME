require(dplyr)
require(ggplot2)
require(stringr)
require(readxl)
require(maps)
require(tidyr)

rm(list = ls())
# setwd("../")
# dates are in the ./supporting-documents/ECN_VW_qtext.csv
setwd("./rennie_2017_woodland/")

# load ecn sites map
library(sf)

sites <- st_read("./ECN_sites/data/shape/ECN_Site_Points_WGS84/ECN_Site_Points_WGS84.shp")

sites_df <- sites %>%
  mutate(Latitude = st_coordinates(.)[,2]) %>%
  mutate(Longitude = st_coordinates(.)[,1]) %>%
  filter(str_detect(CODE, "^T")) %>% # remove?
  st_drop_geometry()

sites_dt <- sites_df %>%
  select(CODE, Latitude, Longitude)

## STEMID == 999 introduces duplicates
## if there are more than one records of one sample, 
dt <- read.csv("./data/ECN_VW1.csv", header = TRUE) %>%
  filter(FIELDNAME == "HEIGHT" | FIELDNAME == "DIAMETER") %>%
  group_by(SITECODE, SYEAR, PLOTPID, CELLID, TREEID, STEMID, TREE_SPEC, FIELDNAME) %>%
  mutate(row = row_number()) %>%
  pivot_wider(names_from = c(row, FIELDNAME), values_from = VALUE) %>%
  mutate(Height = case_when(`1_HEIGHT` == 0 & !is.na(`2_HEIGHT`) ~ `2_HEIGHT`,
                            TRUE ~ `1_HEIGHT`)) %>%
  mutate(Diameter = case_when(`1_HEIGHT` == 0 & !is.na(`2_HEIGHT`) ~ `2_DIAMETER`,
                              TRUE ~ `1_DIAMETER`)) %>%
  drop_na(TREE_SPEC) %>%
  select(-c(8:11))

## related tables
# tree_spec
tree_code <- read_excel("./data/Species_codes.xlsx", sheet=1, range=cell_rows(1:1222), col_names=T, na='') %>%
  setNames(c("TREE_SPEC", "Latin_name", "BRC_concept", "Taxon"))

date <- read.csv("./supporting-documents/ECN_VW2.csv", header = TRUE) %>%
  mutate(DBHDATE = case_when(DBHDATE == '' & DOMDATE == '' ~ SEEDDATE,
                             DBHDATE == '' ~ DOMDATE,
                             TRUE ~ DBHDATE)) %>%
  select(c(SITECODE, PLOTPID, DBHDATE)) %>%
  mutate(YMD = as.Date(DBHDATE, "%d-%b-%y")) %>%
  mutate(Year = as.integer(format(YMD, "%Y"))) %>%
  mutate(Month = as.integer(format(YMD, "%m"))) %>%
  mutate(Day = as.integer(format(YMD, "%d"))) %>%
  select(-c(YMD, DBHDATE))

df <- dt %>%
  left_join(tree_code) %>%
  ungroup() %>%
  left_join(date, by = c("SITECODE" = "SITECODE", "PLOTPID" = "PLOTPID", "SYEAR" = "Year")) %>%
  ungroup() %>%
  select(-c(TREE_SPEC, Latin_name, BRC_concept)) %>%
  setNames(c("Site", "Year", "Plot", "Cell", "Tree", "Stem", "Height", "DBH", "Taxon", "Month", "Day")) %>%
  left_join(sites_dt, by = c("Site" = "CODE")) %>%
  drop_na(Taxon) %>%
  mutate(Genus = word(Taxon, 1)) %>% # separate genus to its own column
  mutate(Species = word(Taxon, 2)) %>%
  mutate(Species = case_when(is.na(Species) == TRUE ~ "sp",
                             TRUE ~ Species)) %>%
  mutate(Taxon = paste(Genus, Species))
  
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
is.numeric(df$Height) # TRUE
is.numeric(df$DBH) # TRUE


# Year, month and day must be integers or factors
# | means or
is.factor(df$Year) | is.integer(df$Year) # FALSE
is.factor(df$Month) | is.integer(df$Month) # FALSE
is.factor(df$Day) | is.integer(df$Day) # FALSE


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
min(df$Height) > 0 # no zeroes?
sum(is.na(df$Height)==TRUE) # 2546 NAs
sum(df$Height=="") > 0 # no blanks

min(df$DBH) > 0 # no zeroes?
sum(is.na(df$DBH)==TRUE) # 7 NAs
sum(df$DBH=="") > 0 # no blanks


# Year < 2021, month < 12, day < 31
summary(df[,2])
summary(df[,10])
summary(df[,11])

# if there are rows that need to be removed
df <- df[!is.na(df$DBH),]
df <- df[!df$DBH == 0,]
# check again
min(df$DBH) > 0 # no zeroes?
sum(df$DBH=="") > 0 # no blanks

## map

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
sort(unique(df$Taxon)) %>% word(., start=2, end=-1) %>% unique()
# this keeps IDs adjacent to their same-genus misspellings, but only looking at the last few words to check

sort(unique(word(df$Taxon, 1))) # check genera

# misspelling

## taxa fix

df <- df %>% 
  mutate(Taxon = NULL) %>%
  rename(Biomass = DBH)


### prepare and export
df$Abundance <- rep('', nrow(df))
df$Family <- rep('', nrow(df))
df$DepthElevation <- rep('', nrow(df))
df$StudyID <- rep('', nrow(df))

# aggregate abundance records that are same species, plot, and survey day.

df_merged <- df %>% group_by_at(vars(-Biomass)) %>% 
  summarise(Biomass=sum(Biomass)) %>% ungroup() %>% arrange(Year, Family, Genus, Species)

dim(df)[1]-dim(df_merged)[1] # any change in aggregating?

## save dataset
# save the dataset name as an object so we save some typing
dataset_name <- 'ECN_Vegetation_Woodland_VW_rennie_2017'
# put in as many non-blank fields unique to the sampling event
# If plot, depthelevation, month, and day are available, I'd add those in too
df_merged$SampleDescription <- as.factor(with(df_merged, paste(Site, Plot, Cell, Tree, Year, sep='_')))

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
# north wyke: 250 ha
# wytham: 770 ha
# alice holt forest: 850 ha
# snowdon: 330 ha
area <- (250 + 770 + 850 + 330) / 100  ##get area in sq km
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
