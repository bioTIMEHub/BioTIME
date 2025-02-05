### IC 1367 Italy Alpine ground beetles

rm(list=ls())

# read data

dt_name <- "Italy Alpine ground beetles"
dt_ID <- "1367"
Roel_insect_local_folder <- "~/iCloud Drive (Archive)/Documents/BioTIME/BioTIME push Roel/"
setwd(paste0(Roel_insect_local_folder,dt_ID," ", dt_name))

Plots <- read.csv(paste0(dt_ID, " Plots.csv"),  header = FALSE)
names(Plots) <- Plots[1,]
Plots <- Plots[-1,]
dim(Plots)
RawData<- read.csv(paste0(dt_ID, " Data.csv")); dim(RawData)
names(RawData)
Samples<- read.csv(paste0(dt_ID, " Samples.csv")); dim(Samples)
names(Samples)
Studies<- read.csv(paste0(dt_ID, " Study details.csv")); dim(Studies)
names(Studies)
taxa<-read.csv(paste0(dt_ID, " Taxa.csv")); dim(taxa)
head(taxa)

# merge
merge1<-merge(RawData, taxa, by = "Taxon")
dim(merge1)

merge2<-(merge(merge1, Samples, by = "Sample_ID"))
dim(merge2) # all there.
length(unique(merge2$DataSource_ID)) #yep

# merge with plot # mind that column 'Datasource ID is in both
merge3<- merge(merge2, Plots , by = c("Plot_ID", "DataSource_ID") )#
dim(merge3) # all there

# merge with studies
merge4<- merge(merge3, Studies, by = "DataSource_ID")

names(merge4)[order(names(merge4))]
dim(merge4)

dt <- merge4

require(tidyverse)
require(readxl)
require(maps)

# are there 2 or more unique years in the dataset?
n_distinct(dt$Year) >= 2
summary(dt)

# only keep relevant column

dt_allraw <- dt #get copy

#get info fo metadata
names(dt)
dt$ExperimentalManipulation #but only control is kept.
dt$SamplingMethod
dt$SamplingMethodDetailed
dt$AggregationOfReplicates

## RvK comments
# This is a resampling of ground beetles communities along an elevational gradient in the Italian Alps.
# The first sampling was done in 1980, and this was repeated in 2008 by at least some of 
# the same researchers. The numbers refer to the mean number of individuals per trap per 10 days. 
# please check if this is acceptable for you. In how far the original trapping effort was replicated 
# is unclear to me, but I generally trust the authors to have done everything possible 
# to ensure replication. No selection on sites or years was done by me
## VB comments
#that's fine - will addd in methods

dt$Biomass
dt$Date
dt$Year
dt$SampleArea
dt$Family
dt$SampleArea
sum(!is.na(dt$SampleArea))
dt$Elevation
dt$SourceGeogrData
dt$Realm
dt$NationState
dt$ClimateZone

table(dt$PlotName)
table(dt$Plot_ID) #Plot_ID is fine
table(dt$Plot_ID,dt$Elevation)

dt<- dt[,c("Number","Biomass","Family", "validTaxon.x","Latitude", "Longitude", "Plot_ID", "Elevation","Year")]
colnames(dt) <- c("Abundance", "Biomass","Family","GenusSpecies","Latitude", "Longitude", "Plot","DepthElevation","Year")
# Abundance and/or biomass, latitude and longitude numeric?

is.numeric(dt$Abundance)
is.factor(dt$Year) | is.integer(dt$Year)
# Secondary fields such as trawl, plot, transect etc must be factors or integers? NA
# Secondary fields such as elevation or depth will normally be numeric unless in a treatment format. NA here
# Date should be POSIXct (not applicable in this case) NA, just year
# Taxonomic fields must be characters or factors?
is.factor(dt$Family) | is.character(dt$Family)
is.factor(dt$GenusSpecies) | is.character(dt$GenusSpecies)


### Fixes
# The year column here is numeric so convert it to factor
dt$Year <- as.factor(dt$Year)
# and characters for the taxonomic fields
dt$Family <- as.character(dt$Family)
#check again
summary(dt)

# Abundance is fine
# Year is fine
# no biomass nor elevation
# Lon lat are fine

### Plot

dt$Latitude <- as.numeric(dt$Latitude)
dt$Longitude <- as.numeric(dt$Longitude)

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

unique(dt$Family) # check family
unique(dt$GenusSpecies)

dt$Genus <- word(dt$GenusSpecies, 1) # separate genus to its own column
dt$Species <- word(dt$GenusSpecies, start=2) # species to its own column

sum(is.na(dt$Genus))
sum(is.na(dt$Species))
sort(unique(dt$Genus))
sort(unique(dt$Species))

## Prepare for export
dt$Biomass <- rep('', nrow(dt))
#dt$DepthElevation <- rep('', nrow(dt))
dt$Day <-rep('', nrow(dt))
dt$Month <-rep('', nrow(dt))
dt$StudyID <- rep('', nrow(dt))

##aggegation check

# aggregate abundance records that are same species, plot, site, year.
dt_merged <- dt %>% group_by(Biomass,Family,Genus, Species,
                             Plot,Latitude, Longitude,DepthElevation,
                             Day, Month,Year,StudyID) %>%
  summarise(Abundance = sum(Abundance)) %>% ungroup() %>% arrange(Year, Family, Genus, Species)
dt <- dt %>% group_by(Biomass,Family,Genus, Species,
                      Plot,Latitude, Longitude,DepthElevation,
                      Day, Month,Year,StudyID) %>% arrange(Year, Family, Genus, Species)

dim(dt)[1] - dim(dt_merged)[1] # there must be 2 records of the same species in the same year

#######


# save the dataset name as an object so we save some typing
dataset_name <- paste0(dt_name,"_ic",dt_ID)
# put in as many non-blank fields unique to the sampling event
# If plot, depthelevation, month, and day are available, I'd add those in too

dt$DepthElevation <- as.numeric(dt$DepthElevation)
table(dt$DepthElevation, dt$Plot)
dt_merged$SampleDescription <-as.factor(with(dt_merged,
                                             paste(Plot, Year, sep = '_')
))
length(levels(dt_merged$SampleDescription)) # 12 sampling events

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
head(dt_merged) # final check!

## coords updates in 2024
dt_merged$Latitude[dt_merged$Plot==142] <- 46.29271254
dt_merged$Longitude[dt_merged$Plot==142] <- 11.81097468
dt_merged$Latitude[dt_merged$Plot==143] <- 46.29271254
dt_merged$Longitude[dt_merged$Plot==143] <- 11.81020431
dt_merged$Latitude[dt_merged$Plot==144] <- 46.30065102
dt_merged$Longitude[dt_merged$Plot==144] <- 11.80020149
dt_merged$Latitude[dt_merged$Plot==145] <- 46.29899176
dt_merged$Longitude[dt_merged$Plot==145] <- 11.77590611
dt_merged$Latitude[dt_merged$Plot==147] <- 46.29914341
dt_merged$Longitude[dt_merged$Plot==147] <- 11.75233558
dt_merged$Latitude[dt_merged$Plot==146] <- 46.29562424
dt_merged$Longitude[dt_merged$Plot==146] <- 11.74784635
dt_merged$Plot<- NA

## Export and spreadsheet prep

#The raw data is ready to be exported!

dataset_name <- "IC1367_Italy_Alpine_ground_beetles"
write.csv(dt_merged, paste0(dataset_name, '_rawdata_RvK_VB.csv'), row.names=F, na='') # replace your initials here
write_clip(dt_merged)

#Excel to fill out the curation spreadsheet. 

#Central coordinates (`CentralLatitude, CentralLongitude`) and sampling area. 

# load libraries
library(sf)
library(clipr)
# 1. Convert data points into point spatial object
# assumes your rawdata dataframe is called rawdata
dt_coord <- dt_merged %>% select(Longitude, Latitude) %>% distinct() %>%
  st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()
# 2. Calculate convex hull, area and centroid
centroid <- dt_coord %>% st_convex_hull() %>% st_centroid() %>% unlist # get centroid
centroid[c(2,1)] # copy as lat-long
# transform to mercator to get area in sq km
area <- st_transform(dt_coord, st_crs("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs")) %>%
  st_convex_hull() %>% st_area()  ##get area in sq km
write_clip(area)


## snippet if coordinates are ever in degree minutes seconds
angle2dec <- function(angle) {
  angle <- as.character(angle)
  x <- do.call(rbind, strsplit(angle, split=' '))
  x <- apply(x, 1L, function(y) {
    y <- as.numeric(y)
    y[1] + y[2]/60 + y[3]/3600
  })
  return(x)
}
# Plot the geometries -----------------------------------------------------

require(ggplot2)
world_map <- map_data('world') # check whether the GPS coordinates match expectations
ggplot(world_map) +
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  geom_point(data = dt_merged, aes(x = Longitude, y = Latitude), size = 2, shape = 21, colour = 'blue') +
  geom_point(aes(x = centroid[1], y = centroid[2]), size = 3, colour = 'green') +
  geom_sf(data = dt_coord %>% st_convex_hull(), colour = 'blue', fill = 'transparent') +
  coord_sf(xlim=c(6.4,6.7), ylim=c(52.7,52.9)) + # make sure these fit your coordinates!
  labs(x = NULL, y = NULL) +
  theme_minimal()
