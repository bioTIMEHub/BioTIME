

# Curation Script ---------------------------------------------------------

# Dataset: LTER Hunsruck-Hochwald Benthic Invertebrates
# Location: Germany
# Curator: Cher Chow
# Date: 15-Jan-2020

# Set up ------------------------------------------------------------------
# load the necessary packages
require(dplyr)
require(tidyr)
require(tidyverse)
require(ggplot2)
require(maps)
require(stringr)
require(lubridate)
require(readxl)

rm(list = ls(all.names = T))
# make sure your working directory is set before running these lines
dt <- read_excel('Originals/HaasePilotto/S044.xlsx', sheet=1, skip=3, col_names=T, na='')
plots <- read_excel('Originals/HaasePilotto/S044.xlsx', sheet=3, skip=1, col_names=T, na='')
plots <- plots[,1:4] %>% na.omit()

plots$Site <- word(plots$Site, start=-1, end=-1)
dt$Site <- word(dt$Site, start=-1, end=-1)
dt$Site <- as.factor(dt$Site)

# Structure check ---------------------------------------------------------

dim(dt) # check dimensions
str(dt) # check structure
summary(dt)
# Abundance and/or biomass, latitude and longitude numeric? N
# Year, month and day must be integers or factors? Y
# Secondary fields such as trawl, plot, transect etc must be factors or integers? N
sum((sort(unique(dt$Plot)) == sort(unique(plots$Plot)))+0)
# make sure each plot ID has a corresponding coordinate in the plots df
str(dt)
# Secondary fields such as elevation or depth will normally be numeric unless in a treatment format. NA here
# Date should be POSIXct (not applicable in this case)
colnames(dt)[4] <- 'Abundance'
dt$Year <- year(dt$`Sampling date`)
dt$Month <- month(dt$`Sampling date`)
dt$Day <- day(dt$`Sampling date`)
dt$`Sampling date` <- NULL
# Taxonomic fields must be characters or factors? Y

# checking sampling consistency over time
dt %>% group_by(Year) %>% summarise(sites = n_distinct(Site))
# will need splitting by plot at the end

# Primary field check -----------------------------------------------------

# ABUNDANCES
# No negative values, zeroes, or NAs in abundance/biomass fields.

min(dt$Abundance) # check the minimum (no zeroes)
sum(dt$Abundance=="") # no blanks

# LAT LONG
# no blanks, no NAs
# Latitude constrained from -90 to 90.
# Longitude constrained -180 to 180.

# first, we need to match the ones from the plots metadata sheet to the main raw data
colnames(plots) <- c('Site', 'Latitude', 'Longitude', 'DepthElevation')

# degree mins to decimal degree
lat <- str_split_fixed(plots$Latitude, '[^[:digit:].]', n=4) %>% as.data.frame()
long <- str_split_fixed(plots$Longitude, '[^[:digit:].]', n=4) %>% as.data.frame()
lat[4] <- NULL
long[4] <- NULL
lat <- sapply(lat, as.numeric)
long <- sapply(long, as.numeric)
for (i in 1:3) {
  plots$Latitude[i] <- lat[i,1] + lat[i,2]/60 + lat[i,3]/3600
  plots$Longitude[i] <- long[i,1] + long[i,2]/60 + long[i,3]/3600
}
rm(lat)
rm(long)
rm(i)

# now join it with main rawdata
dt<- left_join(dt, plots, by='Site')
dt[c('Latitude', 'Longitude')] <- sapply(dt[c('Latitude', 'Longitude')], as.numeric)
summary(dt[c('Latitude', 'Longitude')]) # proper ranges
str(dt)

world_map <- map_data('world') # check whether the GPS coordinates match expectations
world <- ggplot(world_map) + 
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  coord_fixed() + 
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())
points <- world + geom_point(data=dt, aes(x=Longitude, y=Latitude), shape=21)

points_zoom <- points + coord_fixed(xlim=c(-10,10), ylim=c(40,60))
points_zoom

# Taxonomic field check ---------------------------------------------------

# No NAs in taxonomic fields, remove all non-organism records
# misspellings check, but not taxonomic cleaning

# first, records with undefined species epithets into the BioTIME format of sp without period
dt$Species <- dt$Taxon # make a copy
sort(unique(dt$Species))

# check that genera are genera, not family names (-idae/eae)
# messy! Mixed up all in there.
dt$Genus <- word(dt$Taxon, 1) # create the genus column from the species column
dt$Family <- '' # create a family column to move family names into
dt$Family[str_which(dt$Species, 'idae$|inae$')] <- dt$Species[str_which(dt$Species, 'idae$|inae$')]
# migrate the family names from species to family
dt$Genus[str_which(dt$Genus, 'idae$|inae$')] <- ''
dt$Species[str_which(dt$Species, 'idae$|inae$')] <- ''

# identified to genus level needs " sp" added to end
# this detects single word entries by the number of word boundaries (== 2)
dt$Species[which(str_count(dt$Species, '\\b') == 2)] <- paste(dt$Taxon[which(str_count(dt$Species, '\\b') == 2)], 'sp', sep=' ')
# There's a species with a subgenus (so word boundaries > 2), so I'll just keep the subgenus in the species column
# detect the closing parentheses specifically at the end of the string
dt$Genus[str_which(dt$Species, 'Simulium \\(Nevermannia\\)')] <- 'Simulium (Nevermannia)' # deal with the subgenus records
dt$Species <- str_remove_all(dt$Species, '\\(Nevermannia\\)\\s') # remove from species
# there's a unknown species for the subgenus record
dt$Species[dt$Species == 'Simulium (Nevermannia)'] <- 'Simulium sp' # working replacement
sort(unique(dt$Species)) # okay everything is in the right format for splitting
dt$Species <- word(dt$Species, start=2, end=-1)
# delete incorrect rows

sort(unique(dt$Species))
sort(unique(dt$Genus))
summary(dt)
dt$Taxon <- NULL

# Prepare raw data --------------------------------------------------------
# aggregate abundance records that are same species, plot, and survey day.
dt_merged <- dt %>% group_by_at(vars(-Abundance)) %>% 
  summarise(Abundance=sum(Abundance)) %>% ungroup() %>% arrange(Year, Month, Day, Site, Genus, Species)
dim(dt)[1]-dim(dt_merged)[1] # any change in aggregating? no
dt_merged$Biomass <- ''
dt_merged$StudyID <- ''
dt_merged$Plot <- ''

dt_merged$SampleDescription <- dt_merged$Year

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
                         'StudyID', 'Site')] %>% arrange(Year, Family, Genus, Species)

# split by site
unique(dt_merged$Site)[1]
dt1 <- dt_merged %>% filter(Site == unique(dt_merged$Site)[1]) %>% select(!Site) # borfink
unique(dt_merged$Site)[2]
dt2 <- dt_merged %>% filter(Site == unique(dt_merged$Site)[2]) %>% select(!Site) # mundung
unique(dt_merged$Site)[3]
dt3 <- dt_merged %>% filter(Site == unique(dt_merged$Site)[3]) %>% select(!Site) # oberlauf


# Export final ------------------------------------------------------------

write.csv(dt1, 'LTER_Hunsruck-Hochwald_Borfink_Invert_rawdata_CC.csv', row.names=F)
write.csv(dt2, 'LTER_Hunsruck-Hochwald_Mundung_Invert_rawdata_CC.csv', row.names=F)
write.csv(dt3, 'LTER_Hunsruck-Hochwald_Oberlauf_Invert_rawdata_CC.csv', row.names=F)
