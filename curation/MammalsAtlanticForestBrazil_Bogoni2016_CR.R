
setwd("/Users/cerridwenr/Desktop/BioTIME/Completed/Bogoni")

list.files()

bogoni <- read.csv("Bogoni et al. (2016) Journal of Mammology - Mammal dataset 2005-2011.csv")

library(tidyr)
library(dplyr)
library(ggplot2)
library(maps)
library(tidyverse)
library(sf)

# Separate the species column into genus and species
bogoni <- bogoni %>% separate(Specie, c("Genus", "Species"))

bogoni$Biomass <- NA

# Drop the NA dates
bogoni <- bogoni %>% drop_na(Date)

# Separate the dates
bogoni <- bogoni %>% separate(Date, c("Day", "Month", "Year"))

# Change abbreviated month to number
bogoni$Month <- match(bogoni$Month, month.abb)

# Make year into 2000s
bogoni$thousands <- 20
bogoni <- bogoni %>% unite("Year", c(thousands, Year), sep = "")


# Make sample description column
bogoni <- bogoni %>% unite("SampleDescription", 
                                     c(y, x, Site, Day, Month, Year, Elevation), 
                                     sep = "_", remove = FALSE)

# Select and rename the needed columns
bogoni <- bogoni %>% select(Abundance = N.Ind, Biomass, Genus, Species, SampleDescription, 
                            Site, Latitude = y, Longitude = x, Elevation, Day, Month, Year)

# Ensure no duplicates
bogoni <- bogoni %>% group_by(Genus, Species, Biomass,
                                    Year, Month, Day, 
                                    Latitude, Longitude, SampleDescription, 
                                    Site, Elevation) %>%
                                    summarise(Abundance = sum(Abundance))

# Reorder columns
bogoni <- bogoni %>% select(Abundance, Biomass, Genus, Species, SampleDescription, 
                            Site, Latitude, Longitude, Elevation, Day, Month, Year)


write.csv(bogoni, "Bogoni et al. (2016) Journal of Mammology.csv")

#### DETERMINE THE MIDPOINT & AREA

dt_merged <- bogoni
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
  coord_sf(xlim = c(-55,-45), ylim = c(-29,-26)) + 
  labs(x = NULL, y = NULL) +
  theme_minimal()

