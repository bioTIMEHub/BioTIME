################################################################################
# Study title: Monthly data of mesozooplankton abundance and biomass for a shelf station off A Coruña (NW Spain): 1990-2018
# Curator: AFE
# Date: October 2023
################################################################################

# Sources ======================================================================
# https://doi.org/10.1594/PANGAEA.919080


# Libraries ====================================================================
library(dplyr)
library(tidyverse)
library(ggplot2)
library(maps)
library(stringr)
library(lubridate)
library(stringi)
library(readxl)
library(sf)
library(clipr)
library(pangaear)

rm(list=ls())

setwd("C:/Users/afe1/OneDrive - University of St Andrews/PHD/BioTIMEGithub")
myd <- getwd()


# Read raw data files ==========================================================
dt_file <- pg_data(doi="10.1594/PANGAEA.919080") # Download Data from Pangaea
dt <- dt_file[[1]][["data"]]
spskey <- dt_file[[1]][["metadata"]][["parameters"]]

# Explore Data =================================================================
names(dt)

# NOTE: abundance is the main unit, biomass removed since it is aggregated for all taxa found in each session
dt <- dt[,!names(dt)=="Biom dm [g/m**3]"]
names(dt) <- gsub(" [#/m**3]", "", names(dt), fixed = TRUE) # Almost all taxa
names(dt) <- gsub(" [#/m**2]", "", names(dt), fixed = TRUE) # Tychius picirostris
names(dt)

dt <- gather(dt, key="Taxa", value="Abundance", -1)


# Abundance: ===================================================================
sort(unique(dt$Abundance))
sum(dt$Abundance=="")   # 144
sum(dt$Abundance==" ")  # 0
sum(is.na(dt$Abundance))# 0
sum(dt$Abundance %in% c("#999.00", "#999.0", "#999.000")) # 6746
sum(dt$Abundance %in% c("0.0", "0.00", "0.000"))          # 37500

dt <- dt[!dt$Abundance %in% c("", "#999.00", "#999.0", "#999.000", "0.0", "0.00", "0.000"),]
dt$Abundance <- as.numeric(dt$Abundance)
range(dt$Abundance)


# Temporal: ====================================================================
dt$Day <- as.numeric(str_split_fixed(dt$`Date/Time`, "-",3)[,3])
dt$Month <- as.numeric(str_split_fixed(dt$`Date/Time`, "-",3)[,2])
dt$Year <- as.numeric(str_split_fixed(dt$`Date/Time`, "-",3)[,1])

sort(unique(dt$Day))     # OK
sort(unique(dt$Month))   # OK
sort(unique(dt$Year))    # OK


# Coordinates & Depth: =========================================================
dt$Latitude <- as.numeric(dt_file[[1]]$metadata$events$LATITUDE)
dt$Longitude <- as.numeric(dt_file[[1]]$metadata$events$LONGITUDE)
dt$DepthElevation <- as.numeric(str_split_fixed(dt_file[[1]]$metadata$events$ELEVATION, " ", 2)[,1])  # in m


world_map <- map_data("world") 
world <- ggplot() + coord_fixed() +xlab("") + ylab("") 
world  <- world  + 
  geom_polygon(data=world_map, aes(x=long, y=lat, group=group), 
               colour="gray60", fill="gray60") 
points <- world  + 
  geom_point(data=dt, 
             aes(x=Longitude, y=Latitude, alpha=0.01)) 
points       

#Zoom:
points_zoom <- points +
  ylim(20,50)+
  xlim(-20,20)
points_zoom   # OK


# Taxonomy: ====================================================================
unique(dt$Taxa)
spskey <- lapply(spskey, function(x) {paste(x, collapse = "")})
spskey <- lapply(spskey, function(x) {gsub(",", "", x)})
spskey <- as.data.frame(do.call(rbind, spskey))
spskey$V2 <- str_extract(spskey$V1,  "(?<=\\().+?(?=\\))")    # to match
spskey$V3 <- str_split_fixed(spskey$V1, "\\[", 2) [,1]        # to substitute

dt$Taxa <- gsub("\\(.*","",dt$Taxa)                           # remove all that's after the parentheses
unique(dt$Taxa)
unique(spskey$V2)

dt$Taxa <- trimws(dt$Taxa)                                    # rm trailing spaces
spskey$V2 <- trimws(spskey$V2)

dt$Taxa2 <- spskey$V3[match(dt$Taxa, spskey$V2)]              # match for full names
dt$Taxa2 <- trimws(dt$Taxa2)                                  # rm trailing spaces
dt$Taxa2 <- gsub("eggs", "", dt$Taxa2)                        # to subsequently agg. life stages
dt$Taxa2 <- gsub("larvae", "", dt$Taxa2)                      # to subsequently agg. life stages
dt$Taxa2 <- gsub("zoea", "", dt$Taxa2)                        # to subsequently agg. life stages
dt$Taxa2 <- gsub("nauplii", "", dt$Taxa2)                     # to subsequently agg. life stages
dt$Taxa2 <- gsub("spp.", "sp", dt$Taxa2)                      # standardise to BioTIME
dt$Taxa2 <- gsub("sp.", "sp", dt$Taxa2)                       # standardise to BioTIME

dt$Taxa2 <- trimws(dt$Taxa2)                                  # rm trailing spaces

sort(unique(dt$Taxa2))
sum(is.na(dt$Taxa2))   # 0, OK

dt_fam <- c("Amphipoda", "Annelida", "Appendicularia", "Ascidiacea", "Bivalvia", "Brachyura",
            "Branchiostomatidae", "Bryozoa", "Calanoida", "Cephalopoda", "Chaetognatha", "Cnidaria",
            "Copepoda", "Crustacea", "Ctenophora", "Cumacea", "Cyclopoida", "Decapoda", "Doliolidae",
            "Echinodermata", "Euphausiacea", "Facetotecta", "Foraminifera", "Gastropoda", 
            "Harpacticoida", "Isopoda", "Mysida", "Ostracoda", "Phyllosoma", "Polychaeta", 
            "Pteropoda", "Radiolarians", "Rotifera", "Salpidae", "Siphonophorae", "Teleostei", "Tintinnina") # Classifications above family
sum(dt$Taxa2=="Gelatinous plankton")  # 17 (remove these)
dt <- dt[!dt$Taxa2=="Gelatinous plankton",]

dt$Genus <- str_split_fixed(dt$Taxa2, " ", 2)[,1]
dt$Species <- str_split_fixed(dt$Taxa2, " ", 2)[,2]
dt$Family <- ifelse(dt$Genus %in% dt_fam, dt$Genus, NA)
dt$Genus <- ifelse(dt$Genus %in% dt_fam, NA, dt$Genus)

sort(unique(dt$Family))
sort(unique(dt$Genus))
sort(unique(dt$Species))

dt$Species[dt$Species==""] <- NA

sum(is.na(dt$Family) & is.na(dt$Genus) & is.na(dt$Species))  # 0, OK


# Aggregate ====================================================================

rawdata <- dt %>%
  group_by(Family, Genus, Species, Latitude, Longitude, DepthElevation, Day, Month, Year) %>%
  summarise(Abundance=sum(Abundance)) %>%
  ungroup() # aggregate abundances after taxonomic standardisation

rawdata$Biomass <- rep(NA, nrow(rawdata))
rawdata <- rawdata %>% relocate(c(Abundance, Biomass), .before=Family)
rawdata$SampleDescription <- paste0(rawdata$Day, "_", rawdata$Month, "_", rawdata$Year)
rawdata$Plot <- rep(NA, nrow(rawdata))
rawdata <- rawdata %>% relocate(c(SampleDescription, Plot), .before=Latitude)

rawdata$StudyID <- rep(NA, nrow(rawdata))

str(rawdata)
range(rawdata$Abundance)

rawdata1 <- subset(rawdata, rawdata$Year < 1997)  # series with mesh size 250 μm
rawdata2 <- subset(rawdata, rawdata$Year > 1996)  # series with mesh size 200 μm


path <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIMEGithubMetadatas/MesozooplanktonACoruña_1990-2018_Bode_et_al_2020_AFE"
write.csv(rawdata1, file=paste0(path, "/rawdata1.csv"), row.names = F)  # series 1990-1996
write.csv(rawdata2, file=paste0(path, "/rawdata2.csv"), row.names = F)  # series 1997-2018


# Convex hulls =================================================================
# 1. Convert data points into point spatial object
# assumes your rawdata dataframe is called rawdata
dt_merged <- rawdata
dt_coord <- dt_merged %>% select(Longitude, Latitude) %>% distinct() %>%
  st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()

# 2. Calculate convex hull, area and centroid
centroid <- dt_coord %>% st_convex_hull() %>% st_centroid() %>% unlist # get centroid
write_clip(centroid[c(2,1)]) # copy as lat-long

# transform to mercator to get area in sq km
area <- st_transform(dt_coord, st_crs("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs")) %>%
  st_convex_hull() %>% st_area()  ##get area in sq km
write_clip(area)

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
  coord_sf(xlim = c(-20,20), ylim = c(20,50)) + # make sure these fit your coordinates!
  labs(x = NULL, y = NULL) +
  theme_minimal()


# End of script ################################################################


