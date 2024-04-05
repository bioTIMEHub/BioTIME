

# Curation Script ---------------------------------------------------------

# Dataset: LTER Hoge Kempen National Park Beetles
# Location: Hoge Kempen, Belgium
# Curator: Cher Chow
# Date: 18-Dec-2020

# Set up ------------------------------------------------------------------
# load the necessary packages
require(dplyr)
require(ggplot2)
require(maps)
require(stringr)
require(lubridate)
require(readxl)

rm(list=ls()) # clear up the environment before starting

# make sure your working directory is set before running these lines
dt <- read_excel('Originals/HaasePilotto/S006.xlsx', sheet=1, skip=3, col_names=T, na='')

# Structure check ---------------------------------------------------------

dim(dt) # check dimensions
# 777 records
str(dt) # check structure
summary(dt)
# Abundance and/or biomass, latitude and longitude numeric? Y

# check if these columns need to be kept
# remove if they're consistent for whole dataset
dt$Site <- if(length(unique(dt$Site)) == 1){NULL}
dt$Habitat <- if(length(unique(dt$Habitat)) == 1){NULL}
dt$Group <- if(length(unique(dt$Group)) == 1) {NULL}

# Year, month and day must be integers or factors? Y
colnames(dt)[1] <- 'Year' # just a rename
dt$Year <- as.factor(dt$Year)

# Secondary fields such as trawl, plot, transect etc must be factors or integers? NA

# Secondary fields such as elevation or depth will normally be numeric unless in a treatment format. NA here
# Date should be POSIXct (not applicable in this case) NA, just year
# Taxonomic fields must be characters or factors? Y


# Primary field check -----------------------------------------------------

# ABUNDANCES
# No negative values, zeroes, or NAs in abundance/biomass fields.

min(dt$Abundance) # check the minimum (no zeroes) Y
sum(dt$Abundance=="") # no blanks Y

# YEAR MONTH DAY
# again, no negative values, 0s or NAs, and all are logical
# Year < 2020, month < 12, day < 31

summary(dt[,1]) # looks good to me

# LAT LONG
# no blanks, no NAs
# Latitude constrained from -90 to 90.
# Longitude constrained -180 to 180.
# Manually input centroid coordinates from https://deims.org/dae8cf66-bdca-47db-b595-7bf6c1762e00
dt$Latitude <- '50.964926000000'
dt$Longitude <- '5.626886000000'

world_map <- map_data('world') # check whether the GPS coordinates match expectations
world <- ggplot(world_map) + 
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  coord_fixed() + 
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())
points <- world + geom_point(data=dt[1,], aes(x=Longitude, y=Latitude), shape=21)

points_zoom <- points + coord_fixed(xlim=c(-10,10), ylim=c(40,60))

# all looks good

# Taxonomic field check ---------------------------------------------------

# No NAs in taxonomic fields, remove all non-organism records
# misspellings check, but not taxonomic cleaning
# check that genera are genera, not family names (-idae/eae)
dt$Species <- dt$Taxon # make a copy
# check the species list for misspellings or non-BioTIME taxonomic convention names
sort(unique(dt$Species))
dt$Genus <- word(dt$Taxon, 1)
dt$Species <- word(dt$Species, start=2, end=-1)

sort(unique(dt$Genus)) # check genera too just in case the eyes didn't pick up on that
dt$Genus <- str_replace_all(dt$Genus, 'Anacaeana', 'Anacaena')
dt$Taxon <- NULL # get rid of it now that we've split and checked

# check family too
sort(unique(dt$Family))

# Prepare raw data --------------------------------------------------------

dt <- dt %>% arrange(Year, Family, Genus, Species)

# aggregate abundance records that are same species, plot, and survey day.
dt_merged <- dt %>% group_by_at(vars(-Abundance)) %>% 
  summarise(Abundance=sum(Abundance)) %>% ungroup() %>% arrange(Year, Family, Genus, Species)
dim(dt)[1]-dim(dt_merged)[1] # any change in aggregating? yep. 291 rows lost

# now create empty columns needed to fit to template
dt_merged$Biomass <- ''
dt_merged$Plot <- ''
dt_merged$DepthElevation <- ''
dt_merged$Day <- ''
dt_merged$Month <- ''
dt_merged$StudyID <- ''

dt_merged$SampleDescription <- as.factor(dt_merged$Year)
length(levels(dt_merged$SampleDescription)) # 301 samples

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
                         'StudyID')] %>% arrange(Year, Family, Genus, Species)
View(dt_merged) # final check :)
summary(dt_merged)
str(dt_merged)

# Export final ------------------------------------------------------------

write.csv(dt_merged, 'LTER_HogeKempenNationalPark_Beetles_rawdata_CC.csv', row.names=F)
clipr::write_clip(dt_merged)
