

# Curation Script ---------------------------------------------------------

# Dataset: ILTER Ebro Delta Bird Census
# Location: Ebro Delta, 
# Curator: Cher Chow
# Date: 16-Oct-2020
# Note* not a whole lot to clean bc contributor filled out template

# Set up ------------------------------------------------------------------
# load the necessary packages
require(dplyr)
require(ggplot2)
require(maps)
require(stringr)
require(lubridate)
require(readxl)

# make sure your working directory is set before running these lines
dt <- read.csv(file='Originals/ILTERbiodiversity _DATA_Ebro Delta.csv', header=T)


# Structure check ---------------------------------------------------------

dim(dt) # check dimensions
# 278 x 4
str(dt) # check structure
summary(dt)
# Abundance and/or biomass, latitude and longitude numeric? Y
# Year, month and day must be integers or factors? Y
# Secondary fields such as trawl, plot, transect etc must be factors or integers? N
dt$Site <- as.factor(dt$Site)
str(dt)
# Secondary fields such as elevation or depth will normally be numeric unless in a treatment format. NA here
# Date should be POSIXct (not applicable in this case) NA here
# Taxonomic fields must be characters or factors? Y


# Primary field check -----------------------------------------------------

# ABUNDANCES
# No negative values, zeroes, or NAs in abundance/biomass fields.
min(dt[,4]) # check the minimum (no zeroes)
colnames(dt)[4] <- 'Abundance'
colnames(dt)[2] <- 'Date'
dt <- dt %>% filter(Abundance > 0) # filter out any zeroes
sum(dt$Abundance=="") # should have no blanks

# YEAR MONTH DAY
# again, no negative values, 0s or NAs, and all are logical
# Year < 2020, month < 12, day < 31

date1 <- dt %>% filter(Taxon == unique(dt$Taxon)[1]) %>% select(Date)
summary(date1)
date1 <- as.POSIXct(date1$Date, format="%e/%m/%Y")
date2 <- dt %>% filter(Taxon == unique(dt$Taxon)[2]) %>% select(Date)
date2
summary(date2)
date2 <- str_replace_all(date2$Date, 'May-', '')
date2 <- as_date(date2, format='%y')
date3 <- dt %>% filter(Taxon == unique(dt$Taxon)[3]) %>% select(Date)
date3 <- str_replace_all(date3$Date, 'May-', '')
date3 <- as_date(date3, format='%y')
dt$Year <- c(year(date1), year(date2), year(date3), dt$Date[97:273])
  
# LAT LONG
# no blanks, no NAs
# Latitude constrained from -90 to 90.
# Longitude constrained -180 to 180.
dt$Latitude <- rep('40.57062777777778', dim(dt)[1])
dt$Longitude <- rep('0.6542583333333334', dim(dt)[1])

world_map <- map_data('world') # check whether the GPS coordinates match expectations
world <- ggplot(world_map) + 
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  coord_fixed() + 
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())
points <- world + geom_point(data=dt, aes(x=0.6542583333333334, y=40.57062777777778), shape=21)
points

# Coordinates look correct

points_zoom <- points + coord_fixed(xlim=c(-10,20), ylim=c(30,50))
points_zoom

# Taxonomic field check ---------------------------------------------------

# No NAs in taxonomic fields, remove all non-organism records
# misspellings check, but not taxonomic cleaning
# check that genera are genera, not family names (-idae/eae)

sort(unique(dt$Taxon)) # looks ok. Just needs to split.
dt$Genus <- word(dt$Taxon, 1)
dt$Species <- word(dt$Taxon, 2, -1)
dt$Family <- dt$Genus
dt$Family <- dt$Family %>% str_replace_all(c('Larus'='Laridae', 'Sterna'='Laridae', 'Phoenicopterus'='Phoenicopteridae'))


# Prepare raw data --------------------------------------------------------

dt <- dt %>% select(!c(Taxon, Date, Site)) # remove the original fields that don't fit BioTIME data
dt$Biomass <- ''
dt$Plot <- ''
dt$DepthElevation <- 2
dt$Day <- ''
dt$Month <- 5
dt$StudyID <- ''

# aggregate abundance records that are same species, plot, and survey day.
dt_merged <- dt %>% group_by_at(vars(-Abundance)) %>% 
  summarise(Abundance=sum(Abundance)) %>% ungroup()
dim(dt)[1] - dim(dt_merged)[1] # row number change? no
dt_merged$SampleDescription <- as.factor(dt_merged$Year)
summary(dt_merged$SampleDescription)
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
                         'StudyID')] %>% arrange(Year, Genus, Species)

# Export final ------------------------------------------------------------

write.csv(dt_merged, 'ILTER_EbroDeltaBirdCensus_rawdata_CC.csv', row.names=F)
library(clipr)
write_clip(dt_merged)
