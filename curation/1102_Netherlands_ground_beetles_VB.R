### Roel push

###try collated data

setwd("~/Desktop/Biotime push Roel")
Plots <- read.csv("plots4Biotime.csv", header = FALSE)
dim(Plots)
names(Plots)

RawData<- read.csv("rawData4Biotime.csv"); 
dim(RawData)
names(RawData)
Samples<- read.csv("sampleData4Biotime.csv"); 
dim(Samples)
names(Samples)
Studies<- read.csv("Studies4Biotime.csv"); 
dim(Studies)
names(Studies)
taxa<-read.csv("taxa5.3.csv"); dim(taxa)
head(taxa)

# merge with taxa
merge1<-merge(RawData, taxa, by = "Taxon") #taxa is incorrect

# plots <- unique(RawData$Plot_ID[RawData$Taxon =="BEMBLAMP"])
# Plots$DataSource_ID[Plots$Plot_ID %in% plots]
# 
# 
# # merge with sample data
# merge2<-(merge(merge1, selSamples, by = "Sample_ID"))
# dim(merge2) # all there. 
# length(unique(merge2$DataSource_ID)) #23
# 
# # merge with plot # mind that column 'Datasource ID is in both 
# merge3<- merge(merge2, selPlots , by = c("Plot_ID", "DataSource_ID") )#
# dim(merge3) # all there 
# 
# # merge with studies 
# merge4<- merge(merge3, selStudies, by = "DataSource_ID")
# names(merge4)[order(names(merge4))]
# dim(merge4)
# 
# names(merge4)


## doesn't work so go 1 by 1

### Dataset 1 - 1102 Netherland ground beetles

dt_name <- "Netherlands ground beetles"
dt_ID <- "1102"
Roel_insect_local_folder <- "~/Desktop/Biotime push Roel/"
setwd(paste0(Roel_insect_local_folder,dt_ID," ", dt_name))

Plots <- read.csv(paste0(dt_ID, " Plots.csv"), header = FALSE)
dim(Plots)
names(Plots) <- c(Plots[1,])
Plots <- Plots[-1,]
head(Plots)
dim(Plots)

RawData<- read.csv(paste0(dt_ID, " Data.csv"), header = FALSE)
dim(RawData)
names(RawData) <- c(RawData[1,])
RawData <- RawData[-1,]
head(RawData)
dim(RawData)

Samples<- read.csv(paste0(dt_ID, " Samples.csv"), header = FALSE)
dim(Samples)
names(Samples) <- c(Samples[1,])
Samples <- Samples[-1,]
head(Samples)
dim(Samples)

Studies<- read.csv(paste0(dt_ID, " Study details.csv"), header = FALSE)
dim(Studies)
names(Studies) <- c(Studies[1,])
Studies <- Studies[-1,]
head(Studies)
dim(Studies)


taxa<-read.csv(paste0(dt_ID, " Taxa.csv"), header = FALSE)
dim(taxa)
names(taxa) <- c(taxa[1,])
taxa <- taxa[-1,]
head(taxa)
dim(taxa)

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

summary(dt)
names(dt)

dt$Year <- as.numeric(dt$Year)
dt$Number <- as.numeric(dt$Number)
dt$Biomass <- as.numeric(dt$Biomass)
dt$Latitude <- as.numeric(dt$Latitude)
dt$Longitude <- as.numeric(dt$Longitude)
dt$Elevation <- as.numeric(dt$Elevation)


# are there 2 or more unique years in the dataset?
n_distinct(dt$Year) >= 2
summary(dt)

# only keep relevant column

dt_allraw <- dt #get copy

#get info fo metadata
names(dt)
dt$ExperimentalManipulation
dt$Biomass
dt$Date
dt$Family
dt$SampleArea
sum(!is.na(dt$SampleArea))
dt$SamplingMethod
dt$SamplingMethodDetailed
dt$AggregationOfReplicates
dt$Elevation
dt$SourceGeogrData
dt$Realm
dt$NationState
dt$ClimateZone

table(dt$PlotName)
table(dt$Plot_ID) #Plot_ID is fine

dt<- dt[,c("Number","Biomass","Family", "validTaxon.x","Latitude", "Longitude", "Plot_ID", "Elevation","Year")]
colnames(dt) <- c("Abundance", "Biomass","Family","GenusSpecies","Latitude", "Longitude", "Plot","DepthElevation","Year")
# Abundance and/or biomass, latitude and longitude numeric?

is.numeric(dt$Abundance)
is.factor(dt$Year) | is.numeric(dt$Year)

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
world_map <- map_data('world') # check whether the GPS coordinates match expectations
world <- ggplot(world_map) + 
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  coord_fixed() + 
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())
points <- world + geom_point(data=dt, aes(x=Longitude, y=Latitude), shape=21)
points + coord_fixed(xlim=c(0,10), ylim=c(40,60))
points + coord_fixed(xlim=c(6.4,6.7), ylim=c(52.7,52.9))
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

summary(as.factor(dt$Genus))
summary(as.factor(dt$Species))
sort(unique(dt$Species))

## Prepare for export
dt$Biomass <- rep('', nrow(dt))
dt$DepthElevation <- rep('', nrow(dt))
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

# check wich abundances occur with different frequences
which(table(dt_merged$Abundance)!=table(dt$Abundance)) #18 46 64 (18 + 46 = 64!)
dim(dt_merged[dt_merged$Abundance ==18,])
dim(dt[dt$Abundance ==18,])
table(dt[dt$Abundance ==18,"Species"])
table(dt_merged[dt_merged$Abundance ==18,"Species"])
#lunicollis is the culprit
dt[dt$Abundance==18&dt$Species=="lunicollis",]
dt_merged[dt_merged$Abundance==18&dt_merged$Species=="lunicollis",]
View(dt_merged[dt_merged$Year==1975&dt_merged$Plot==562,])
View(dt[dt$Year==1975&dt$Plot==562,])

### There are 2 recods of Amara lunicollis in 1975 with different abundances. Aggregate?





########


# save the dataset name as an object so we save some typing
dataset_name <- paste0(dt_name," ic",dt_ID)
# put in as many non-blank fields unique to the sampling event
# If plot, depthelevation, month, and day are available, I'd add those in too
dt_merged$SampleDescription <-as.factor(with(dt_merged,
                                             paste(Latitude, Longitude, Plot, Year, sep = '_')
))
length(levels(dt_merged$SampleDescription)) # how many sampling events?
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


#The raw data is ready to be exported!

write.csv(dt_merged, paste0(dataset_name, '_rawdata_RvK_VB.csv'), row.names=F, na='') # replace your initials here

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
centroid[c(2,1)]
write_clip(centroid[c(2,1)]) # copy as lat-long

# transform to mercator to get area in sq km
area <- st_transform(dt_coord, st_crs("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs")) %>%
  st_convex_hull() %>% st_area()  ##get area in sq km
area
write_clip(area)

# Plot the geometries -----------------------------------------------------

require(ggplot2)
world_map <- map_data('world') # check whether the GPS coordinates match expectations
ggplot(world_map) +
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  geom_point(data = dt_merged, aes(x = Longitude, y = Latitude), size = 2, shape = 21, colour = 'blue') +
  geom_point(aes(x = centroid[1], y = centroid[2]), size = 3, colour = 'green') +
  geom_sf(data = dt_coord %>% st_convex_hull(), colour = 'blue', fill = 'transparent') +
  #coord_sf(xlim = c(-115, -110), ylim = c(30,35)) + # make sure these fit your points
  labs(x = NULL, y = NULL) +
  theme_minimal()
