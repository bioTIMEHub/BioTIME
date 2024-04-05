
setwd("/Users/cerridwenr/Desktop/BioTIME/Completed/Tancredi - Mites")

library(tidyr)
library(dplyr)
library(ggplot2)
library(maps)
library(tidyverse)
library(sf)

# Rename the column so it can be matched with the extra species names dataframe
TomData <- TomData %>% rename(s117 = Juv)

# Gather the data
TomData <- TomData %>% gather(Species, Abundance, s1:s117)

# Remove all zero abundances
TomData <- TomData %>% filter(Abundance > 0)

# Add additional columns
TomData$Day <- NA
TomData$Month <- NA
TomData$Elevation <- NA
TomData$Biomass <- NA

## Assign Latitude and longlitude
TomData$Latitude <- TomData$Site
TomData$Longitude <- TomData$Site

TomData$Latitude[TomData$Latitude == "Young"] <- 57.900000
TomData$Longitude[TomData$Longitude == "Young"] <- 25.000000

TomData$Latitude[TomData$Latitude == "Mid"] <- 57.866667
TomData$Longitude[TomData$Longitude == "Mid"] <- 24.983333

TomData$Latitude[TomData$Latitude == "Old"] <- 57.850000
TomData$Longitude[TomData$Longitude == "Old"] <- 25.000000

# Set the lat and long as numberic
TomData$Latitude <- as.numeric(as.character(TomData$Latitude))
TomData$Longitude <- as.numeric(as.character(TomData$Longitude))


# Read in the species names
names <- read.csv("OribatidsLatviaMites_TancrediCaruso2020_CR_SpeciesNames.csv")

TomData <- left_join(TomData, names)


# Separate the species column into genus and species
TomData <- TomData %>% separate(Binomial, c("Genus", "Species"))

# Make sample description column
TomData <- TomData %>% unite("SampleDescription", 
                           c(Latitude, Longitude, Site, Year), 
                           sep = "_", remove = FALSE)

# Select and rename the needed columns
TomData <- TomData %>% select(Abundance, Biomass, Genus, Species, SampleDescription, 
                            Site, Latitude, Longitude, Elevation, Day, Month, Year)

# Ensure no duplicates
TomData2 <- TomData %>% group_by(Genus, Species, Biomass,
                              Year, Month, Day, 
                              Latitude, Longitude, SampleDescription, 
                              Site, Elevation) %>%
  summarise(Abundance = sum(Abundance))

# Reorder columns
TomData2 <- TomData2 %>% select(Abundance, Biomass, Genus, Species, SampleDescription, 
                            Site, Latitude, Longitude, Elevation, Day, Month, Year)



write.csv(TomData2, "Tancredi_Biotime.csv")


#### DETERMINE THE MIDPOINT & AREA


dt_merged <- TomData2
dt_coord <- dt_merged %>% select(Longitude, Latitude) %>% distinct() %>%
  st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()

# 2. Calculate convex hull, area and centroid
centroid <- dt_coord %>% st_convex_hull() %>% st_centroid() %>% unlist # get centroid
centroid

# transform to mercator to get area in sq km
area <- st_transform(dt_coord, st_crs("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs")) %>%
  st_convex_hull() %>% st_area()  ##get area in sq km
area

## also useful snippet if coordinates are ever in degree minutes seconds

angle2dec <- function(angle) {
  angle <- as.character(angle)
  x <- do.call(rbind, strsplit(angle, split=' '))
  x <- apply(x, 1L, function(y) {
    y <- as.numeric(y)
    y[1] + y[2]/60 + y[3]/3600
  })
  return(x)
}

world_map <- map_data('world') # check whether the GPS coordinates match expectations
ggplot(world_map) +
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  geom_point(data = dt_merged, aes(x = Longitude, y = Latitude), size = 2, shape = 21, colour = 'blue') +
  geom_point(aes(x = centroid[1], y = centroid[2]), size = 3, colour = 'green') +
  geom_sf(data = dt_coord %>% st_convex_hull(), colour = 'blue', fill = 'transparent') +
  coord_sf(xlim = c(20, 30), ylim = c(50,65)) + 
  labs(x = NULL, y = NULL) +
  theme_minimal()




