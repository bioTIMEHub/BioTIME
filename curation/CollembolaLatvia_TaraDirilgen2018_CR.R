
setwd("/Users/cerridwenr/Desktop/BioTIME/Completed/Tancredi - Collembola")

library(tidyr)

tancredi <- read.csv("BioTime_Collembola.csv")


# Gather the data
tancredi <- tancredi %>% gather(Species, Abundance, ALFUS:XBRE)

# Remove all zero abundances
tancredi <- tancredi %>% filter(Abundance > 0)

# Add additional columns
tancredi$Day <- NA
tancredi$Month <- NA
tancredi$Elevation <- NA
tancredi$Biomass <- NA

## Assign Latitude and longlitude
tancredi$Latitude <- tancredi$Site
tancredi$Longitude <- tancredi$Site

tancredi$Latitude[tancredi$Latitude == "Young"] <- 57.900000
tancredi$Longitude[tancredi$Longitude == "Young"] <- 25.000000

tancredi$Latitude[tancredi$Latitude == "Mid"] <- 57.866667
tancredi$Longitude[tancredi$Longitude == "Mid"] <- 24.983333

tancredi$Latitude[tancredi$Latitude == "Old"] <- 57.850000
tancredi$Longitude[tancredi$Longitude == "Old"] <- 25.000000

# Set the lat and long as numberic
tancredi$Latitude <- as.numeric(as.character(tancredi$Latitude))
tancredi$Longitude <- as.numeric(as.character(tancredi$Longitude))


# Read in the species names
names <- read.csv("CollembolaLatvia_TaraDirilgen2018_CR_SpeciesNames.csv")

tancredi <- left_join(tancredi, names)


# Separate the species column into genus and species
tancredi <- tancredi %>% separate(Binomial, c("Genus", "Species"))

# Make sample description column
tancredi <- tancredi %>% unite("SampleDescription", 
                           c(Latitude, Longitude, Site, Year), 
                           sep = "_", remove = FALSE)

# Select and rename the needed columns
tancredi <- tancredi %>% select(Abundance, Biomass, Genus, Species, SampleDescription, 
                            Site, Latitude, Longitude, Elevation, Day, Month, Year)

# Ensure no duplicates
tancredi2 <- tancredi %>% group_by(Genus, Species, Biomass,
                              Year, Month, Day, 
                              Latitude, Longitude, SampleDescription, 
                              Site, Elevation) %>%
  summarise(Abundance = sum(Abundance))

# Reorder columns
tancredi2 <- tancredi2 %>% select(Abundance, Biomass, Genus, Species, SampleDescription, 
                            Site, Latitude, Longitude, Elevation, Day, Month, Year)



write.csv(tancredi2, "Tancredi_Biotime.csv")


#### DETERMINE THE MIDPOINT & AREA

dt_merged <- tancredi2
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

