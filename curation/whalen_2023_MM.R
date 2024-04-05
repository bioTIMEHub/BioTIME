
# Curation Script ---------------------------------------------------------

# Dataset: Whalen et al. (2023) Heatwave restructures marine intertidal communities across a stress gradient
# Location:  Calvert Island, BC, Canada
# Curator: Mike McWilliam
# Date: 17-Jul-2023

# Set up ------------------------------------------------------------------
rm(list=ls())
library("maps")
library("ggplot2")
library("reshape2")
library("stringr")

dt <- read.csv("whalen_2023_Original/Martone_Hakai_data_lump_function.csv")
head(dt)

# Primary field check -----------------------------------------------------

dt$Biomass <- as.numeric(dt$Abundance)
dt$Abundance <- ""

# No negative values, zeroes, or NAs in abundance/biomass fields.
min(dt$Biomass) # check the minimum (no zeroes) 
dt <- subset(dt, Biomass > 0)
min(dt$Biomass)
sum(dt$Biomass=="") # no blanks 

############## YEAR MONTH DAY (int/factors)
# no negative values, 0s or NAs, all are logical
# Year < 2023, month < 12, day < 31
# Date should be POSIXct 

meta <- read.csv("whalen_2023_Original/Martone_Hakai_metadata.csv")
dt$Date <- meta$Date[match(dt$UID, meta$UID)]

dt$Year <- format(as.Date(dt$Date, "%Y-%m-%d"), "%Y")
dt$Month <- format(as.Date(dt$Date, "%Y-%m-%d"), "%m")
dt$Day <- format(as.Date(dt$Date, "%Y-%m-%d"), "%d")
head(dt)

############## LAT LONG (numeric)
# Latitude -90 to 90 / Longitude -180 to 180,  no blanks, no NAs

head(meta)
dt$Latitude<- meta$Lat[match(dt$UID, meta$UID)]
dt$Longitude<- meta$Long[match(dt$UID, meta$UID)]

dt[is.na(dt$Latitude),]

# check whether the GPS coordinates match expectations
points <- unique(dt[,c("Latitude", "Longitude")])
mid <- c(points[1,1], points[1,2])
a <- 3
lw_ratio <- 0.75

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

unique(dt$taxon_revised)
unique(dt$kingdom)
unique(dt$non.alga.flag)

# remove substratum ("bare rock", "Wood", "Sand", "Clam shell","Clam hole")
dt <- dt[!dt$kingdom=="Substratum",]

# remove animals - genus level only
dt <- dt[!dt$kingdom=="Animal",]

unique(dt$taxon_revised)

dt$Family <-  ""
dt$Genus <-  word(dt$taxon_revised, 1)
dt$Species <-  word(dt$taxon_revised, 2)

# check the species list for misspellings or non-BioTIME conventions
sort(unique(dt$Species))
sort(unique(dt$Genus))
sort(unique(dt$Family))

# Secondary fields ---------------------------------------------------

# trawl, plot, transect etc / # elevation or depth 

head(meta)

dt$DepthElevation <- "" # possible shore height?

unique(dt$UID)
dt$Transect <- word(dt$UID, 3)

# quadrats are semi-permanent. regularly placed an a permanent transect. 
dt$Site <- meta$Site[match(dt$UID, meta$UID)]
dt$Quadrat <- meta$Quadrat[match(dt$UID, meta$UID)]
dt$Plot <- paste(dt$Site, dt$Transect, dt$Quadrat,sep="_")
head(dt)

# Prepare raw data --------------------------------------------------------

dt$StudyID <- ""

# aggregate abundance records that are same species, plot, and survey day.
dt_merged <- aggregate(Biomass ~ ., data = dt[,c("Family", "Genus", "Species","taxon_revised", "Date", "Year","Month", "Day", "Plot", "Abundance", "Biomass","Latitude", "Longitude", "DepthElevation", "StudyID")], sum)

dim(dt)[1]-dim(dt_merged)[1] # changes after aggregating

taxa_plot <- unique(dt$taxon_revised)[1:8]
plot_plot <-unique(dt_merged$Plot)[1:15]
to_plot <- dt_merged[dt_merged$taxon_revised %in% taxa_plot & dt_merged$Plot %in% plot_plot,]
unique(to_plot$Date)
ggplot(to_plot , aes(x=as.Date(Date), y=Biomass, group=taxon_revised, col=taxon_revised))+
geom_point()+geom_line()+
facet_wrap(~Plot)

dataset.name <- 'whalen_2023'
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
                         
head(dt_merged) # final check 
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