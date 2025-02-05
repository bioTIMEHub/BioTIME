## IC1547 Korea moths


# clear environment
rm(list =ls())

# make sure your working directory is set before running these lines
setwd("~/iCloud Drive (Archive)/Documents/BioTIME/BioTIME push Roel/")


dt_name <- "Korea moths"
dt_ID <- "1547"
setwd(paste0(dt_ID," ", dt_name))

# do not filter as IC so start from raw
data<- read.csv("ERDP-2019-02.2.2-Jirisan_data.csv", header = T)
taxa<- read.csv("ERDP-2019-02.4.2-Jirisan_taxa.txt")

#remove the dates as suggested by prof Choi 
data<- (subset(data, Site != "BS" | Date != "2010-08-10"))
data <-  subset(data, Site != "HY" | Date != "2011-06-07")

#use all the rest at the given resolution
KoreaMoths<- data.frame(
  Datasource_name = "Korea Moths", 
  Plot_ID = data$Site, 
  Plot_name = data$Site, 
  Sample_ID = "443", 
  Year = data$Year,
  Period = "",
  Date = data$Date,
  Taxon = data$Species, 
  Sex = "", 
  Unit = "abundance", 
  Original_number  = data$Individuals, 
  Transformed_number = "", 
  Number = data$Individuals, 
  Error = "")	

write.csv(KoreaMoths, file = "KoreaMoths_all.csv")	
rm(list =ls())

dt_name <- "Korea moths"
dt_ID <- "1547"

Plots <- read.csv("IC1547_plots.csv"); dim(Plots)
names(Plots)
RawData<- read.csv("KoreaMoths_all.csv"); dim(RawData)
names(RawData)
Samples<- read.csv(paste0(dt_ID, " Samples.csv")); dim(Samples)
names(Samples)
Studies<- read.csv(paste0(dt_ID, " Study details.csv")); dim(Studies)
names(Studies)
taxa<-read.csv(paste0(dt_ID, " Taxa.csv")); dim(taxa)
head(taxa)

# merge
merge1<-merge(RawData, taxa, by = "validTaxon")
dim(merge1)

merge2<-(merge(merge1, Samples, by = "Sample_ID"))
dim(merge2) # all there.
length(unique(merge2$DataSource_ID)) #yep

# merge with plot # mind that column 'Datasource ID is in both
merge3<- merge(merge2, Plots , by = c("Plot_name") )#
dim(merge3) # all there

dt <- merge3

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
unique(dt$ExperimentalManipulation)
dt$SamplingMethod
dt$SamplingMethodDetailed
dt$AggregationOfReplicates

dt$Biomass
dt$Date
dt$Family
dt$SampleArea
sum(!is.na(dt$SampleArea))
dt$Elevation # not informative
unique(dt$Elevation)
dt$SourceGeogrData
dt$Realm
dt$NationState
dt$ClimateZone
table(dt$PlotName)
table(dt$Plot_ID) #Plot_ID is fine

dt<- dt[,c("Number","Family", "validTaxon","Latitude", "Longitude", "Plot_ID", "Elevation","Date", "Year")]
colnames(dt) <- c("Abundance","Family","GenusSpecies","Latitude", "Longitude", "Plot","DepthElevation","Date","Year")
# Abundance and/or biomass, latitude and longitude numeric?

is.numeric(dt$Abundance)
is.factor(dt$Year) | is.integer(dt$Year)

# Date should be POSIXct (not applicable in this case) NA, just year
# Taxonomic fields must be characters or factors?
is.factor(dt$Family) | is.character(dt$Family)
is.factor(dt$GenusSpecies) | is.character(dt$GenusSpecies)


### Fixes
# The year column here is numeric so convert it to factor
dt$Year <- as.factor(dt$Year)
Date <- as.matrix(str_split_fixed(dt$Date,"/", 3), ncol=3, nrow= 13403)
tail(Date)
dt$Day <- as.numeric(Date[,1])
dt$Month <- as.numeric(Date[,2])
dt$year <- as.numeric(Date[,3])

length(dt$year == dt$Year)

# and characters for the taxonomic fields
dt$Family <- as.character(dt$Family)
#check again
summary(dt)

# Abundance
# no zeros
dt[dt$Abundance<0,] 
dim(dt[dt$Abundance==0,])
dt<- dt[dt$Abundance>0,]
dim(dt)
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

dt[(is.na(dt$Species)),]
## Prepare for export
dt$Biomass <- rep('', nrow(dt))
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

dim(dt)[1] - dim(dt_merged)[1]

########

# save the dataset name as an object so we save some typing
dataset_name <- paste0(dt_name,"_ic",dt_ID)
# put in as many non-blank fields unique to the sampling event
# If plot, depthelevation, month, and day are available, I'd add those in too
dt_merged$SampleDescription <-as.factor(with(dt_merged,
                                             paste(Plot, Year, Month, Day, sep = '_')
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


## Export and spreadsheet prep

#The raw data is ready to be exported!

write.csv(dt_merged, paste0(dataset_name, '_rawdata_RvK_VB.csv'), row.names=F, na='') # replace your initials here

#Excel to fill out the curation spreadsheet. 

#Central coordinates (`CentralLatitude, CentralLongitude`) and sampling area. 
unique(dt_merged$Longitude)
unique(dt_merged$Latitude)
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

