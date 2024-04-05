### Dataset 3 from Insect Change- 1328 UK hoverflies Jenifer Owen
## curator: Vivi

# clear space
rm(list=ls())

# read data
dt_name <- "UK hoverflies Jenifer Owen"
dt_ID <- "1328"
Roel_insect_local_folder <- "~/Desktop/Biotime push Roel/"
setwd(paste0(Roel_insect_local_folder,dt_ID," ", dt_name))
dt_name <- "UK_hoverflies_Jenifer_Owen"

Plots <- read.csv(paste0(dt_ID, " Plots.csv")); dim(Plots)
names(Plots)

RawData<- read.csv(paste0(dt_ID, " Data.csv")); dim(RawData)
names(RawData)
RawData$Date

# try and include sampling - attribute date to "Data.csv". 
# and keep each sampling abundances separated as they are different sampling events!
RawRawData <- read.csv(paste0(dt_ID, " Raw Data.csv"))
names(RawRawData)
# take the firs day of the week - all years forced to start on Mon 1/1
RawRawData$DayOfYear <- RawRawData$wk*7-6
summary(RawRawData$DayOfYear)
#makes sense - winter not sampled
# assuming all years forced to start on Mon 1/1 again, we can set dates
# setting origin to the first of jan of 2018, which was a Monday.
RawRawData$DateOfYeah <- as.Date(RawRawData$DayOfYear, origin = "2018-01-01")
## from here we will take only day and month, using the year provided as year.
RawRawData$Day<- format(as.Date(RawRawData$DateOfYeah,format="%Y-%m-%d"), format = "%d")
RawRawData$Month<- format(as.Date(RawRawData$DateOfYeah,format="%Y-%m-%d"), format = "%m")
RawRawData$Date <- paste0(RawRawData$yr,"-",RawRawData$Month,"-",RawRawData$Day)

#rebuilt RawData

RawDatab <- data.frame(rep("",nrow(RawRawData)))
RawDatab$Taxon <-  RawRawData$sp
RawDatab$Taxon <- paste0(word(RawDatab$Taxon,1),"_",word(RawDatab$Taxon,2))
RawDatab$Year <- RawRawData$yr
RawDatab$Date <- RawRawData$Date
RawDatab$Sample_ID <- 410 #for merging with Samples.csv
RawDatab$Plot_ID <- 1044 #for merging with Plot.csv
sum(RawRawData$num != RawRawData$males+RawRawData$females)
RawDatab$Number <- RawRawData$num
RawDatab$validTaxon <- RawRawData$sp

RawData <- RawDatab

Samples<- read.csv(paste0(dt_ID, " Samples.csv")); dim(Samples)
names(Samples)
Studies<- read.csv(paste0(dt_ID, " Study details.csv")); dim(Studies)
names(Studies)
taxa<-read.csv(paste0(dt_ID, " Taxa.csv")); dim(taxa)
head(taxa)

# merge
merge1<-merge(RawData, taxa, by = "Taxon")
dim(merge1)

# two are missing. find them 
unique(RawData$Taxon)
unique(merge1$Taxon)
sum(!(RawData$Taxon %in% merge1$Taxon))
RawData$Taxon[which(!(RawData$Taxon %in% merge1$Taxon))] # "Syrphus_torvus"
#add torvus in taxa table.
taxa <- read.csv(paste0(dt_ID, " Taxa VB.csv")); dim(taxa)
merge1<-merge(RawData, taxa, by = "Taxon")
dim(merge1)

# merge sample
merge2<-(merge(merge1, Samples, by = "Sample_ID"))
dim(merge2) # all there.
names(merge2)

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

dt<- dt[,c("Number","Biomass","Family", "validTaxon.x","Latitude", "Longitude", "Plot_ID", "Elevation","Year","Date")]
#split date into day month and year column for consistency with Biotime
dt$Day <- as.numeric(format(as.Date(dt$Date,format="%Y-%m-%d"), format = "%d"))
dt$Month <- as.numeric(format(as.Date(dt$Date,format="%Y-%m-%d"), format = "%m"))
dt$Year <- as.numeric(format(as.Date(dt$Date,format="%Y-%m-%d"), format = "%Y"))
dt<- dt[,c("Number","Biomass","Family", "validTaxon.x","Latitude", "Longitude", "Plot_ID", "Elevation","Year","Day", "Month")]

colnames(dt) <- c("Abundance", "Biomass","Family","GenusSpecies","Latitude", "Longitude", "Plot","DepthElevation","Year","Day", "Month")
# Abundance and/or biomass, latitude and longitude numeric?

is.numeric(dt$Abundance)
summary(dt)
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
points + coord_fixed(xlim=c(-5,10), ylim=c(40,60))
# seems right
detach('package:maps')

                         
### Nomenclature

unique(dt$Family) # check family
unique(dt$GenusSpecies)

dt$Genus <- word(dt$GenusSpecies, 1) # separate genus to its own column
dt$Species <- word(dt$GenusSpecies, start=2) # species to its own column

sum(is.na(dt$Genus))
sum(is.na(dt$Species))
sort(unique(dt$Species))
sort(unique(dt$Species))


## Prepare for export
dt$Biomass <- rep('', nrow(dt))
dt$DepthElevation <- rep('', nrow(dt))
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

dim(dt)[1] - dim(dt_merged)[1] #59 got aggregated

##some checks
table(table(dt_merged$Abundance))
table(table(dt$Abundance))

# patterns are likely retained

# save the dataset name as an object so we save some typing
dataset_name <- paste0("IC",dt_ID,"_",dt_name)
# put in as many non-blank fields unique to the sampling event
# If plot, depthelevation, month, and day are available, I'd add those in too
dt_merged$SampleDescription <-as.factor(with(dt_merged,
                                             paste(Latitude, Longitude, Year, Month, Day, sep = '_')
))
length(levels(dt_merged$SampleDescription)) # 821 sampling events
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
summary(dt_merged)


## Export and spreadsheet prep
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
centroid[c(2,1)] # copy as lat-long
# transform to mercator to get area in sq km
area <- st_transform(dt_coord, st_crs("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs")) %>%
  st_convex_hull() %>% st_area()  ##get area in sq km
write_clip(area)
# well it's 2 plots, no point in doing this...

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

