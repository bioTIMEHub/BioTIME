# Curation Script ---------------------------------------------------------

# Dataset:  study 275 Response of an urban remnant reptile community to summer wildfire
# Location: Australia
# (RE) Curator: Viviana Brambilla

# Set up ------------------------------------------------------------------
# load the necessary packages
require(tidyverse)
require(maps)
require(stringr)
require(lubridate)
require(readxl)
require(sf)
require(clipr)

# clear environment
rm(list =ls())

# make sure your working directory is set before running these lines
setwd("~/iCloud Drive (Archive)/Documents/BioTIME/Update datasets/Robert Davies/")
ds <- read_excel("BioTIME 2 Data Kings Park.xlsx", sheet = "Data")
dt <- read_excel("BioTIME 2 Data Kings Park.xlsx", sheet = "Site locations")
# transform to long format
str(ds)
dim(ds)
summary(ds)

str(dt)
dim(dt)

dt <- merge(ds,dt,"Site")
summary(dt)

dt$Year <- as.numeric(substr(dt$Date, 1,4))
dt$Month <- as.numeric(substr(dt$Date, 6,7))
dt$Day <- as.numeric(substr(dt$Date, 9,10))

summary(dt)

#check what it looks like
sort(unique(dt$Genus))
sort(unique(dt$species))


#rename and delete unnecessary columns
dt<-rename(dt, Biomass = SVL)
dt<-rename(dt, Species = species)
dt$SampleDescription <- paste0(dt$Year, "_", dt$Site)
dt$SampleDescription[dt$Month %in% c(1,2)] <- paste0("2019_", dt$Site[dt$Month %in% c(1,2)])
dt$SampleDescription <- as.factor(dt$SampleDescription)
# Abundance and/or biomass, latitude and longitude numeric? Y
# Year, month and day must be integers or factors? Y
# Primary field check 

# ABUNDANCES
# add abundance of 1
dt$Abundance <- 1

# LAT LONG
# no blanks, no NAs
# Latitude constrained from -90 to 90.
# Longitude constrained -180 to 180.
unique(dt$Longitude)
unique(dt$Latitude)

dt[is.na(dt$Latitude),]

world_map <- map_data('world') # check whether the GPS coordinates match expectations
ggplot(world_map) +
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  geom_point(data = dt, aes(x = Longitude, y = Latitude), size = 2, shape = 21, colour = 'blue') +
  #coord_fixed(xlim = c(-52, -47), ylim = c(-25,-20)) +
  labs(x = NULL, y = NULL) +
  theme_minimal()

# yes, correct

# Prepare raw data 
dt$StudyID<- NA
dt$DepthElevation<- NA
dt$Plot <- NA
dt$Family <- NA
# aggregate abundance records that are same species, plot, site, year.
dt_merged <- dt %>% group_by(Biomass,Family,Genus, Species, SampleDescription,
                             Plot,Latitude, Longitude,DepthElevation,
                             Day, Month,Year,StudyID) %>%
  summarise(Abundance = sum(Abundance)) %>% ungroup() %>% arrange(Year, Family, Genus, Species)
dt <- dt %>% group_by(Biomass,Family,Genus, Species,
                      Plot,Latitude, Longitude,DepthElevation,
                      Day, Month,Year,StudyID) %>% arrange(Year, Family, Genus, Species)

dim(dt)[1] - dim(dt_merged)[1] # a lot got pulled!

# put in as many non-blank fields unique to the sampling event
# If plot, depthelevation, month, and day are available, I'd add those in too

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

write.csv(dt_merged, 'study275_modified_continuation_recurate_VB.csv', row.names=F, na='') # replace your initials here
write_clip(dt_merged)
#Excel to fill out the curation spreadsheet. 

# 1. Convert data points into point spatial object
# assumes your rawdata dataframe is called rawdata
dt_coord <- dt_merged %>% select(Longitude, Latitude) %>% drop_na() %>% distinct() %>%
  st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()
# 2. Calculate convex hull, area and centroid
centroid <- dt_coord %>% st_convex_hull() %>% st_centroid() %>% unlist # get centroid
centroid[c(2,1)] # copy as lat-long
# transform to mercator to get area in sq km
area <- st_transform(dt_coord, st_crs("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs")) %>%
  st_convex_hull() %>% st_area()  ##get area in sq km
area



