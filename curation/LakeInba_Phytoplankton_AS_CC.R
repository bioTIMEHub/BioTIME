# Title:  Phytoplankton species abundance in Lake Inba (Japan) from 1986 to 2016
# Author: Anokhi Saha

# ----------------------------

# INDEX: 
#   I.    Loading the data        Line 15 - 26
#   II.   Criteria checks         Line 28 - 73
#   III.  Adding Additional Info  Line 75 - 124
#   IV.   Errors in data          Line 126 - 165
#   V.    Prepare for export      Line 169 - 212
#   VI.   Metadata                Line 215 - 254

# ----------------------------

# 1. Load the required packages

require(tidyverse)
require(readxl)
require(maps)

# 2. Read in data
setwd("~/Documents/bio/Lake Inba")
dt<- read.csv(file = 'ERDP-2017-04.2.1-Inba_Phyto.csv')
View(dt)

# ----------------------------

# We can use R to check the criteria: 
# - Dataset consists of at least 2 years of sampling (they do not have to be consecutive) = YES
# - Dataset consists of entire assemblages, not just populations = YES
# - Data should record abundance, biomass or both. = YES (abundance)
# - Sampling methods *and effort* are consistent through time. = YES
# - Individuals are identified to species level where possible. = YES

# 3. Check data 

# Are there more than two distinct years? (TRUE)
n_distinct(dt$Year) >= 2

# Make sure none of the rows for Abundance are zero > TRUE
min(dt$Abundance) > 0 

# Then, lets check that the variables are in the correct form
# **Abundance or biomass**: numeric
# **Coordinates**: numeric
# **Dates**: POSIXct or 
# **Year, month, day* columns,**: integers or factors
# **Plot**: factors or integers
# **DepthElevation**: numeric or factors (if they're a treatment category)
# **Taxonomic**: characters or factors

# Check Year, month and day > (TRUE)
is.factor(dt$Year) | is.integer(dt$Year)
is.factor(dt$Month) | is.integer(dt$Month)
is.factor(dt$Day) | is.integer(dt$Day)

# Change 'Cells per ml' to abundance and check is numeric (TRUE)
colnames(dt)[7] = "Abundance"
is.numeric(dt$Abundance)

# Now test Species_name > Originally, said FALSE, now says TRUE
is.factor(dt$Species_name)
dt$Species_name <- as.factor(dt$Species_name)

# Rename "station" to "plot"
colnames(dt)[4] = "Plot"

# Then, we can get rid of any columns we dont need for BioTime 

# 'Class' is not included in the BioTime layout so we can get rid of this column from the data table 
dt = subset(dt, select = -c(5))

# ----------------------------

# 4. Adding the additional information provided by the authors

# To each subset of the data, I am now going to add the latitude, longitude, and correct name of the plot
# For coordinates in latitude and longitude, we work primarily in WGS84

sites <- data.frame(Plot = 1:4,
                    Site = c('Aso-bashi', 'Jousuidou-shusuikou-shita',
                             'Ipponmatsu-shita', 'Kitainbanuma-chuou'), 
                    Latitude = c(35.751, 35.734673, 35.766815, 35.80003),
                    Longitude = c(140.145, 140.1925, 140.209167, 140.251944))
dt <- left_join(dt, sites, by="Plot")
# check and remove
dt$Plot <- NULL

# We can also plot them to visually confirm the location. 
# And check that the lats and longs are correctly plotted > looks right! 
world_map <- map_data('world') 
world <- ggplot(world_map) + 
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  coord_fixed() + 
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())
points <- world + geom_point(data=dt, aes(x=Longitude, y=Latitude), shape=21)
points + coord_fixed(xlim=c(135,143), ylim=c(34,37))
detach('package:maps')

# ----------------------------

# 5. Secondary field check: Remove errors and tidy the data 

dt$Species <- dt$Species_name %>% as.character
sort(unique(dt$Species))
# species that need trimming
dt$Species[str_count(dt$Species, '\\s') >= 2] %>% unique %>% sort
dt$Species <- str_remove(dt$Species, '\\s+$') # get rid of trailing spaces
dt$Species <- str_remove_all(dt$Species, '\\svar\\.\\s*[:alpha:]+$')
dt$Species <- str_remove_all(dt$Species, '\\s\\([:alpha:]+\\)|\\s\\(cf\\.tenuis\\)')

# species that need downgrading
dt$Species[str_count(dt$Species, '\\s') == 0] %>% unique %>% sort
dt$Family[str_count(dt$Species, '\\s') == 0] <- dt$Species[str_count(dt$Species, '\\s') == 0] %>% str_to_title(.)
dt[str_count(dt$Species, '\\s') == 0, 9:10] %>% distinct # check migration
dt$Species[str_count(dt$Species, '\\s') == 0] <- ''
dt$Species[str_detect(dt$Species, 'idae$|eae$')] %>% unique %>% sort
dt$Species <- str_remove_all(dt$Species, '^other\\s|unknown\\s')

#repeat
dt$Species[str_count(dt$Species, '\\s') == 0] %>% unique %>% sort
dt$Family[str_count(dt$Species, '\\s') == 0] <- dt$Species[str_count(dt$Species, '\\s') == 0] %>% str_to_title(.)
dt[str_count(dt$Species, '\\s') == 0, 9:10] %>% distinct # check migration
dt$Species[str_count(dt$Species, '\\s') == 0] <- ''

sort(unique(dt$Species))
dt$Species <- str_replace_all(dt$Species, 's(p)+\\.', 'sp')

dt$Genus <- word(dt$Species, 1)
dt$Species <- word(dt$Species, start=2)

dt %>% distinct(Genus, Species) %>% arrange(Genus, Species)
dt$Species_name <- NULL

# ----------------------------

# 6. Prepare data for export 

# Add in BioTime variables that are currently missing from the corrected datafile
dt$Biomass <- rep('', nrow(dt))
dt$StudyID <- rep('', nrow(dt))
dt$DepthElevation <- -0.5

# aggregate abundance records that are same species, plot, and survey day.
dt_merged <- dt %>% group_by_at(vars(-Abundance)) %>% 
  summarise(Abundance=sum(Abundance)) %>% ungroup() %>% 
  arrange(Year, Month, Day, Site, Family, Genus, Species) %>% 
  mutate(SampleDescription = paste(Year, Month, Day, Site, sep="_"), Plot = '') %>% 
  select(!Site) %>% 
  select(Abundance, Biomass, Family, Genus, Species, SampleDescription, 
          Plot, Latitude, Longitude, DepthElevation, Day, Month, Year, StudyID)

nrow(dt) - nrow(dt_merged) # 215 merged

# save the dataset name as an object so we save some typing
dataset_name <- 'LakeInba_Phytoplankton_AS'

# the raw data is now ready to be exported 
# choose a file in the directory you want this saved in
write.csv(dt_merged, paste0('/Users/cherchow/Library/CloudStorage/GoogleDrive-cher.fyc@gmail.com/My Drive/BioTIMEcurate/NewStudies/Anokhi/', dataset_name, '_rawdata_AS_CC.csv'), row.names = F, na='')

# ----------------------------

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


################################# end of code ##################################




