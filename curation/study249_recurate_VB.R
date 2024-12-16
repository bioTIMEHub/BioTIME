# Curation Script ---------------------------------------------------------

# Dataset:  study 249 recuration
# Curator: Viviana Brambilla

# Set up ------------------------------------------------------------------
# load the necessary packages
require(tidyverse)
require(maps)
require(stringr)
require(lubridate)
require(readxl)
require(sf)

##### dwca-csiro_eac_eddy_1978_1984 #####
# CSIRO, Catch records from eddy studies off the New South Wales coast, Australia 1979-1980

# clear environment
rm(list =ls())

# make sure your working directory is set
setwd("~/iCloud Drive (Archive)/Documents/BioTIME/Update datasets/study249/")

dt <- read.csv("Copenhagen lighttrap reprocessed 2024.csv"); dim(dt)
names(dt)

length(dt$Original_number== dt$Number)
length(dt$Plot_ID== dt$Plot_name)

dt<- dt[,c("Plot_name","Year","Taxon", "Number")]
colnames(dt) <- c("Plot", "Year", "GenusSpecies", "Abundance")
# Abundance and/or biomass, latitude and longitude numeric?
is.numeric(dt$Abundance)
is.factor(dt$Year) | is.integer(dt$Year)

### Fixes
summary(dt)

# Abundance
# no zeros
dim(dt[dt$Abundance==0,])
dt<- dt[dt$Abundance>0,]
dim(dt)
# Year is fine
# no biomass nor elevation

#add coords in

dt$Latitude <-rep(55.702512, nrow(dt))
dt$Longitude <-rep(12.558956, nrow(dt))

### Plot
world_map <- map_data('world') # check whether the GPS coordinates match expectations
world <- ggplot(world_map) + 
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  coord_fixed() + 
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())
points <- world + geom_point(data=dt, aes(x=Longitude, y=Latitude), shape=21)
points

# seems right
detach('package:maps')

#secondary fields

sort(unique(dt$Plot))

### Cleaning and checking objectives #Thank you Roel for cleaning this!!

# - Eliminate blanks, NAs (but uncertain IDs up to family level are allowed), and zeroes.
# - Fit uncertain IDs to our syntax or eliminate abbreviations that dataset contributors used.
# - Fit proper taxa level with the correct fields (`Family, Genus, Species`)
# - Remove non-organismal records (e.g. rock, sand) and incidental taxa records.
# - Check that genus column doesn't have family names (end in *-eae* or *-idae*) or species column has genus names.
# - Re-allocate taxonomic classifications into the appropriate column.
# - Check for duplication and misspellings in the species names  (e.g. *Puffnius sp, Puffinus sp*; unidentified/unknown; *Buenia jeffreysi / Buenia jeffreysii*; double spacing; several spaces, authors name & date, etc.). We're not validating or updating species, but we do quick searches in Google Scholar to figure out which spelling is correct.
#                                           - Subspecies and annotated species names should be disregarded (e.g. *Amphiura (acrocnida) brachiate* should be considered as *Acrocnida brachiate*)
#                                           - If a record only has data until the genus level, it must have "sp" in species column.
#                                           
### Nomenclature

#**Uncertain or unidentified IDs**: "Genus sp" (without the full stop/period). If the dataset distinguishes between different uncertain IDs, we reformat them while making sure they're recognised as separate species: like "Genus sp1" and "Genus sp2"  
#**No subspecies or varieties**: Lowest level = "Genus species"  
#**Species groups**: Some taxa are less resolved and are placed into species complexes, e.g. *Dascyllus trimaculatus agg.* or *Rubus fruticosus agg.*. We denote this with "agg." to make sure it's not confused with other specific epithets. Uncertain IDs with several potential options do not count.  

#   *Reminder*: For `Species`, we only leave in species epithets! Don't put it into the usual *Genus species* convention.

unique(dt$GenusSpecies)

dt$Genus <- stringr::str_split_i(dt$GenusSpecies, pattern = "_",1) # separate genus to its own column
dt$Species <- stringr::str_split_i(dt$GenusSpecies, pattern = "_",2) # separate genus to its own column

sum(is.na(dt$Genus))
sum(is.na(dt$Species))
sort(unique(dt$Genus))
sort(unique(dt$Species))

## Prepare for export
dt$Biomass <- rep('', nrow(dt))
dt$DepthElevation <- rep('', nrow(dt))
dt$Day <-rep('', nrow(dt))
dt$Month <-rep('', nrow(dt))
dt$StudyID <- rep(249, nrow(dt))
dt$Family <- rep('',nrow(dt))

##aggegation check

# aggregate abundance records that are same species, plot, site, year.
dt_merged <- dt %>% group_by(Biomass,Family,Genus, Species,
                             Plot,Latitude, Longitude,DepthElevation,
                             Day, Month,Year,StudyID) %>%
  summarise(Abundance = sum(Abundance)) %>% ungroup() %>% arrange(Year, Family, Genus, Species)
dt <- dt %>% group_by(Biomass,Family,Genus, Species,
                      Plot,Latitude, Longitude,DepthElevation,
                      Day, Month,Year,StudyID) %>% arrange(Year, Family, Genus, Species)

dim(dt)[1] - dim(dt_merged)[1] #  aggregated ok

########

# put in as many non-blank fields unique to the sampling event
# If plot, depthelevation, month, and day are available, I'd add those in too
dt_merged$SampleDescription <-as.factor(dt_merged$Year)
length(levels(dt_merged$SampleDescription)) # how many sampling events? 27
# Now we'll reorder the columns to the proper format:


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
dt_merged$Plot<- NA
head(dt_merged) # final check!

## Export and spreadsheet prep

#The raw data is ready to be exported!

write.csv(dt_merged, 'BioTIME_249_recurate_rawdata_RvK_VB.csv', row.names=F, na='') # replace your initials here

#Excel to fill out the curation spreadsheet. 
