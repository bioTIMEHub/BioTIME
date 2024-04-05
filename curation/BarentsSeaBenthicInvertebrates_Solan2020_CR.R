
setwd("/Users/cerridwenr/Desktop/BioTIME/Completed/Martin Solan")
abun <- read.csv("abundance.csv")
bio <- read.csv("biomass.csv")


library(tidyr)
library(dplyr)
library(ggplot2)
library(maps)
library(tidyverse)
library(sf)

# Gather all the species abundance and biomass into one column
a <- abun %>% gather(Species, Abundance, 13:169)
b <- bio %>% gather(Species, Biomass, 7:163)

# Join the abundance and biomass dataframes
solan <- left_join(a, b)

# Removed all 0 values and N
solan <- solan %>% select(1:15) %>% filter(Abundance !=0 & Abundance !="N")

# Replace "y" abundances with "NULL"
solan$Abundance[solan$Abundance == "y"] <- "NA"

# Split the column into genus and species
solan <- solan %>% separate(Species, c("Genus", "Species"), extra = "merge", fill = "left")

# Correct the names
solan$Species[solan$Species == "Strelzovia..quadrilobata"] <- "quadrilobata"
solan$Species[solan$Species == "Strelzovia..suecica"] <- "suecica"
solan$Species[solan$Species == "Acmira..catherinae"] <- "catherinae"
solan$Species[solan$Species == "debris"] <- "sp"
solan$Species[solan$Species == "spp."] <- "sp"
solan$Species[solan$Species == "spp"] <- "sp"
solan$Species[solan$Species == "sp."] <- "sp"
solan$Species[solan$Species == "juv"] <- "sp"
solan$Species[solan$Species == "juv."] <- "sp"
solan$Species[solan$Species == "sp..E"] <- "sp"
solan$Species[solan$Species == "sp..A"] <- "sp"
solan$Species[solan$Species == "indet"] <- "sp"
solan$Species[solan$Species == "indet."] <- "sp"
solan$Species[solan$Species == "parva.agg."] <- "parva"
solan$Species[solan$Species == "crenata.agg."] <- "crenata"
solan$Species[solan$Species == "cornuta.agg."] <- "cornuta"


# There are blanks produced from the splitting columns
# Replace the blanks in the Genus column with the Species column values
solan$Genus <- ifelse(is.na(solan$Genus), solan$Species, solan$Genus)

# Now correct the names to sp.
solan$Species[solan$Species == "Pellecepoda"] <- "sp"
solan$Species[solan$Species == "Yoldiidae"] <- "sp"
solan$Species[solan$Species == "Ampeliscidae"] <- "sp"
solan$Species[solan$Species == "Gnathiidae"] <- "sp"
solan$Species[solan$Species == "Ostracoda"] <- "sp"
solan$Species[solan$Species == "Nemertea"] <- "sp"
solan$Species[solan$Species == "Nematoda"] <- "sp"
solan$Species[solan$Species == "Porifera"] <- "sp"
solan$Species[solan$Species == "Tanaidacea"] <- "sp"


solan$Abundance <- as.numeric(solan$Abundance)

# Ensure no duplicates
solan_final <- solan %>% group_by(Genus, Species, 
                           Year, Month, Day, 
                           Latitude, Longitude, 
                           site_num, Depth) %>%
                                        summarise(Abundance = sum(Abundance),
                                                  Biomass = sum(Biomass))

# Make sample decription column
solan_final <- solan_final %>% unite("SampleDescription", 
                              c(Latitude, Longitude, site_num, Day, Month, Year, Depth), 
                              sep = "_", remove = FALSE)

# Reorder columns
solan_final <- solan_final %>% select(Abundance, Biomass, Genus, Species, SampleDescription,
                                      site_num, Latitude,	Longitude, Depth, Day,	Month,	Year)

solan_final <- rename(solan_final, Plot = site_num)

write.csv(solan_final, "solan.csv")


#### DETERMINE THE MIDPOINT & AREA

dt_merged <- solan_final
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
  coord_sf(xlim = c(20,40), ylim = c(60,90)) + 
  labs(x = NULL, y = NULL) +
  theme_minimal()


# End of script ################################################################

