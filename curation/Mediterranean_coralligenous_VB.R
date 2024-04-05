# Curation Script ---------------------------------------------------------

# Dataset:  Climate change transforms the functional identity of Mediterranean coralligenous assemblages
# Location: Mediterranean sea
# Curator: Viviana Brambilla

# Set up ------------------------------------------------------------------
# load the necessary packages
require(tidyverse)
require(maps)
require(stringr)
require(lubridate)
require(readxl)

# clear environment
rm(list =ls())

# set wd and read data
setwd("~/iCloud Drive (Archive)/Documents/BioTIME/Gomes-Gras")
dt <- read.csv("./raw_data/Abundances.csv", header=T, dec = ',', sep = ';')

# short to long format ---------------------------------------------------------

dim(dt) # check dimensions
str(dt) # check structure
summary(dt)

dt$X <- as.factor(dt$X)
dt_long<- data.frame (dt %>% 
                        pivot_longer(cols = Acanthella.acuta:Valonia.macrophysa,
                                     names_to = "taxa", values_to = "abundance")
)
head(dt_long)

# Primary field check -----------------------------------------------------

# ABUNDANCES
# No negative values, zeroes, or NAs in abundance/biomass fields.

# remove
dt<- dt_long[dt_long$abundance>0,]
sum(dt$sbundance=="") # should have no blanks

# Taxonomic field check ---------------------------------------------------

# tx<- unique(dt$taxa)
 # write.csv(cbind(taxa = tx, Family = rep("",length(tx)), 
 #                 Genus = rep("",length(tx)), Species = rep("",length(tx))),"taxa.csv")
taxa <- read.csv("taxa.csv")

# remove dead substrata
dt <- dt[dt$taxa != "Dead.decomposing",]

# add taxonomic field
dt <- merge(dt,taxa)

# get plot, site and year
dt$X <- as.character(dt$X)

dt$Year <- str_split_i(dt$X,"_",1)
dt$Day <- NA
dt$Month <- NA

dt$Site <- paste0(str_split_i(dt$X,"_",2),"_",str_split_i(dt$X,"_",3))
dt$Plot <- str_split_i(dt$X,"_",4)

# add coordinates (provided by author)
dt$Latitude <- NA
dt$Longitude <- NA
dt$DepthElevation <- NA

dt$Latitude[dt$Site == "Pzzu_cor"] <- 42.3798612
dt$Longitude[dt$Site == "Pzzu_cor"] <- 8.5461638
dt$DepthElevation[dt$Site == "Pzzu_cor"] <- -18

dt$Latitude[dt$Site == "Passe_cor"] <- 42.3798612
dt$Longitude[dt$Site == "Passe_cor"] <- 8.5461638
dt$DepthElevation[dt$Site == "Passe_cor"] <- -29

dt$Latitude[dt$Site == "Pzzu_par"] <- 42.3798612
dt$Longitude[dt$Site == "Pzzu_par"] <- 8.5461638
dt$DepthElevation[dt$Site == "Pzzu_par"] <- -18

dt$Latitude[dt$Site == "Pzzinu_par"] <- 42.3798612
dt$Longitude[dt$Site == "Pzzinu_par"] <- 8.55
dt$DepthElevation[dt$Site == "Pzzinu_par"] <- -25

dt$Latitude[dt$Site == "Gabin_par"] <- 42.99125
dt$Longitude[dt$Site == "Gabin_par"] <- 6.4028889
dt$DepthElevation[dt$Site == "Gabin_par"] <- -25

world_map <- map_data('world') # check whether the GPS coordinates match expectations
ggplot(world_map) +
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  geom_point(data = dt, aes(x = Longitude, y = Latitude), size = 2, shape = 21, colour = 'blue') +
  #coord_fixed(xlim = c(-52, -47), ylim = c(-25,-20)) +
  labs(x = NULL, y = NULL) +
  theme_minimal()

world_map <- map_data('world') # check whether the GPS coordinates match expectations
ggplot(world_map) +
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  geom_point(data = dt, aes(x = Longitude, y = Latitude), size = 2, shape = 21, colour = 'blue') +
  coord_fixed(xlim = c(6,9), ylim = c(42, 44)) +
  labs(x = NULL, y = NULL) +
  theme_minimal()

# seems fine

# Prepare raw data --------------------------------------------------------
dt$Abundance <- NA
dt$Biomass <- dt$abundance

# aggregate abundance records that are same species and sampling event.
dt$SampleDescription <- dt$X
dt_merged <- dt %>% group_by(Genus,Species,SampleDescription) %>% 
  summarise(Abundance=sum(Biomass)) %>% ungroup()
nrow(dt) - nrow(dt_merged) # it was already aggregated fine
dt$StudyID <- NA
dt_merged <- dt[c('Abundance',
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
dt_merged$Plot <- NA
View(dt_merged) # final check :)
summary(dt_merged)
str(dt_merged)


# Export final ------------------------------------------------------------

write.csv(dt_merged, 'Mediterranean_coralligenous_rawdata_VB.csv', row.names=F)
library(clipr)
write_clip(dt_merged)


library(sf)
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
write_clip(area) # this is just to check against what the author inputted


