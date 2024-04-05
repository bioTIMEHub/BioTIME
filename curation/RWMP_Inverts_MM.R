
# Curation Script ---------------------------------------------------------

# Dataset: Regional Watershed Monitoring Program Benthic Macroinvertebrate Data, 2013-2021
# Location:  Canada
# Curator: Mike McWilliam
# Date: 17 / 7 / 2023

# Set up ------------------------------------------------------------------
rm(list=ls()) 
library("maps")
library("ggplot2")
library("reshape2")
library("stringr")

dt <- read.csv("RWMP_inverts_Original/rwmp-benthic-macroinvertebrate-data-2013-2021.csv")
head(dt)

# Primary field check -----------------------------------------------------

dt$Abundance <- as.numeric(dt$Total.Count)
dt$Biomass <- ""

# No negative values, zeroes, or NAs in abundance/biomass fields.
min(dt$Abundance) # check the minimum
dt <- subset(dt, Abundance > 0) # no zeros
min(dt$Abundance)
sum(dt$Abundance=="") # no blanks 

############## YEAR MONTH DAY (int/factors)
# no negative values, 0s or NAs, all are logical
# Year < 2023, month < 12, day < 31
# Date should be POSIXct

dt$Year <- dt$Year  #format(as.Date(dt$Collection.Date, "%Y-%m-%d"), "%Y")
dt$Month <- format(as.Date(dt$Collection.Date, "%Y-%m-%d"), "%m")
dt$Day <- format(as.Date(dt$Collection.Date, "%Y-%m-%d"), "%d")
head(dt)

############## LAT LONG (numeric)
# Latitude -90 to 90 / Longitude -180 to 180,  no blanks, no NAs

dt$Latitude <- as.numeric(dt$Latitude)
dt$Longitude <- as.numeric(dt$Longitude)

# check whether the GPS coordinates match expectations
points <- unique(dt[,c("Latitude", "Longitude")])
mid <- c(points[1,1], points[1,2])
a <- 10
lw_ratio <- 1

world_map <- map_data('world') 
world <- ggplot(world_map) + 
  geom_polygon(aes(x=long, y=lat, group=group)) +
  geom_point(data=points, aes(x=Longitude, y=Latitude), col="red", shape=21)+
coord_cartesian(c(mid[2]-a, mid[2]+a), c(mid[1]-a*lw_ratio, mid[1]+a*lw_ratio))+
  labs(x='Longitude', y='Latitude')+
  theme_bw() + theme(panel.grid=element_blank(), aspect.ratio=lw_ratio)
world

# Taxonomic fields ---------------------------------------------------

# No NAs in taxonomic fields, remove all non-organism records
# misspellings check, but not taxonomic cleaning
# check that genera are genera, not family names (-idae/eae)

head(dt)

dt$Taxon <- dt$BMI.Taxa
unique(dt$Taxon)
broad <- c("(Subclass)|(Class)|(Order)|(Family)|(Subfamily)|(Tribe)|(Suborder)|(Phylum)")
removed <- data.frame(x=unique(dt[grepl(broad, dt$Taxon),"Taxon"]))
dt <- dt[!grepl(broad, dt$Taxon),] # remove broad taxa
length(unique(dt$Taxon))
nrow(removed)

dt$Genus <- word(dt$Taxon, 1)
dt$Species <- word(dt$Taxon, 2)
dt$Family <- ""

# check the species list for misspellings or non-BioTIME conventions
sort(unique(dt$Species))
sort(unique(dt$Genus))

# Secondary fields ---------------------------------------------------

# trawl, plot, transect etc / # elevation or depth 

dt$DepthElevation <- ""

head(dt)
unique(dt$Collection.Method)
unique(dt$Lab.Method)

head(dt)
dt$Plot <- paste(dt$Site.Name, dt$Subsample.Number, sep="_") 
head(dt)

# Prepare raw data --------------------------------------------------------

dt$StudyID <- rep('', dim(dt)[1])

# aggregate abundance records that are same species, plot, and survey day.
dt_merged <- aggregate(Abundance ~ ., data = dt[,c("Family", "Genus", "BMI.Taxa", "Species","Year","Month", "Day", "Collection.Date", "Plot",  "Abundance", "Biomass","Latitude", "Longitude", "DepthElevation", "StudyID")], sum)

dim(dt)[1]-dim(dt_merged)[1] # any change after aggregating 

ggplot(dt_merged[dt_merged$BMI.Taxa %in% unique(dt$BMI.Taxa)[1:20] & dt_merged$Plot %in% unique(dt$Plot)[1:50],], aes(x=as.Date(Collection.Date, "%Y-%m-%d"), y=Abundance, group=Plot))+
geom_line()+
facet_wrap(~BMI.Taxa, scales="free_y")


dataset.name <- 'RWMP_Inverts'
dt_merged$SampleDescription <- as.factor(with(dt_merged, paste(Latitude, Longitude, Year, Month, Day, Plot, sep='_')))
length(levels(dt_merged$SampleDescription)) # 54 samples

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
                         'StudyID')] 
                         
head(dt_merged) # final check :)
str(dt_merged)

# Export final ------------------------------------------------------------

write.csv(dt_merged, paste0(dataset.name, '_rawdata_MM.csv'), row.names=F)

# Convex Hull for centroid ------------------------------------------------

# load libraries
library(sf)
library("clipr")
library("dplyr")

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

centroid 
area