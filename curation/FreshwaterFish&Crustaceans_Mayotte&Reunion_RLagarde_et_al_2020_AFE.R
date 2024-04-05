################################################################################
# Study: Structure of small tropical islands freshwater fish and crustacean communities: a niche- or dispersal-based process?
# Curator: AFE
# Date: 16-11-2020 & revised December 2023
################################################################################


# Main sources =================================================================
# https://datadryad.org/stash/dataset/doi:10.5061/dryad.ksn02v72f


# Libraries ====================================================================
library(dplyr)
library(tidyverse)
library(ggplot2)
library(maps)
library(stringr)
library(lubridate)
library(stringi)
library(sf)
library(readxl)
library(sp)


# Read data ====================================================================
rm(list=ls())
path <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIME/BioTIMENewStudiesCuration/FreshwaterFish&Crustaceans_Mayotte&Reunion_RLagarde_et_al_2020_AFE"
dt <- as.data.frame(read_excel(paste0(path, "/DATA_Lagardeetal2020.xlsx"), sheet="DATA"))
sp <- as.data.frame(read_excel(paste0(path, "/DATA_Lagardeetal2020.xlsx"), sheet="Species_names"))
names(dt) <-dt[1,]
dt <-dt[-1,] # 500 observations


# Data structure ===============================================================
dim(dt) #500 x 72
str(dt)


# Select columns of interest ===================================================
names(dt)
names(dt) <- gsub("% ", "", names(dt))
names(dt) <- gsub(" ", "_", names(dt))
dt <- within(dt, rm(cascade, pool, run, shoal, riffle, rapid, Temperature,
                    pH, Conductivity, O2, Distance_from_river_mouth, Slope, N_migration_barriers,
                    barrier_H, Migration_barrier_maximum_height,
                    Herbivorous, Saprophagous, Omnivores, Predators))


# Remove sites monitored only one year (Keep only the time-series data) ========

length(unique(dt$Site)) # 124
sort(unique(dt$Site))

c1 <- dt%>%
  group_by(Site)%>%
  summarise(nYear = n_distinct(Year))

one_year <- unique(c1$Site[c1$nYear==1]) # 63

dt <- dt[!dt$Site %in% one_year,]        # 437 obs

length(unique(dt$Site))                  # 61


# Location =====================================================================
c2 <- dt %>%
  group_by(Site)%>%
  summarise(nLat = n_distinct(Latitude), 
            nLong = n_distinct(Longitude), 
            nRL=n_distinct(river_L),
            nElev=n_distinct(Elevation)) # only a couple of cases of diff elevations in the same site, but otherwise info consistent for the same site


range(dt$Latitude)
range(dt$Longitude)

dt$Latitude <- as.numeric(as.character(dt$Latitude))
dt$Longitude <- as.numeric(as.character(dt$Longitude))

#Split between Mayotte and Reunion:
dtReu <- dt %>%
  filter(Island=="Reunion") # 338
dtMay <- dt %>%
  filter(Island=="Mayotte") # 99

#Reunion (EPSG:2975 RGR92):-----------------------------------------------------
d <- dtReu[, names(dtReu) %in% c("Site", "Longitude", "Latitude")]

coordinates(d) <- c("Longitude", "Latitude")
proj4string(d) <- CRS("+init=epsg:2975") # EPSG:2975 RGR92
CRS.new <- CRS("+init=epsg:4326") # WGS 84
df <-spTransform(d, CRS.new)
df <- data.frame(df)
range(df$coords.x1) # 55.23692 55.75053
range(df$coords.x2) #-21.38528 -20.88778

dtReu$Latitude <- df$coords.x2[match(dtReu$Site, df$Site)]
dtReu$Longitude <- df$coords.x1[match(dtReu$Site, df$Site)]

world_map <- map_data("world") 
world <- ggplot() + coord_fixed() +xlab("") + ylab("") 
world  <- world  + 
  geom_polygon(data=world_map, aes(x=long, y=lat, group=group), 
               colour="gray60", fill="gray60") 
points <- world  + 
  geom_point(data=dtReu, 
             aes(x=Longitude, y=Latitude, alpha=0.01)) 
points

#Zoom (Reunion):
points_zoom <- points +
  ylim(-40,40)+
  xlim(30,80)
points_zoom  # OK


#Mayotte: ----------------------------------------------------------------------
# NOTE: unknown onshore CRS but all those specific for Mayotte yield the same result (4471, 5793, 4290)
# Pick: RGM04 / UTM zone 38S
dM <- dtMay[, names(dtMay) %in% c("Site", "Longitude", "Latitude")]

coordinates(dM) <- c("Longitude", "Latitude")
proj4string(dM) <- CRS("+init=epsg:4471") # EPSG:4471
CRS.new <- CRS("+init=epsg:4326") # WGS 84
dfM <-spTransform(dM, CRS.new)
dfM <- data.frame(dfM)
range(dfM$coords.x1) # 45.10425 45.22016
range(dfM$coords.x2) #-12.96811 -12.72215

dtMay$Latitude <- dfM$coords.x2[match(dtMay$Site, dfM$Site)]
dtMay$Longitude <- dfM$coords.x1[match(dtMay$Site, dfM$Site)]

world_map <- map_data("world") 
world <- ggplot() + coord_fixed() +xlab("") + ylab("") 
world  <- world  + 
  geom_polygon(data=world_map, aes(x=long, y=lat, group=group), 
               colour="gray60", fill="gray60") 
points <- world  + 
  geom_point(data=dtMay, 
             aes(x=Longitude, y=Latitude, alpha=0.01)) 
points

#Zoom (Mayotte):
points_zoom <- points +
  ylim(-20,0)+
  xlim(40,60)
points_zoom  # OK


dt <- as.data.frame(rbind(dtReu, dtMay))


# Time =========================================================================
sum(is.na(dt$Year)) # 0
unique(dt$Year)     # OK


# Gather data ==================================================================
str(dt)

dt[,c(8:53)] <- sapply(dt[,c(8:53)],as.numeric)
sapply(dt[,c(8:53)], class) #Numeric

dt <- gather(dt, key="Species", value="Abundance", -c(1:7))


# Abundances ===================================================================
range(dt$Abundance)
sum(is.na(dt$Abundance))        # 99
dt <- dt[!is.na(dt$Abundance),] # rm
sum(is.null(dt$Abundance))      # 0
sum(dt$Abundance==0)            # 16033
dt <- dt[!dt$Abundance==0,]     # 3970
range(dt$Abundance)             # 6.209712e-02 4.099742e+03


# Sites ========================================================================
sort(unique(dt$Island))         # 2 (Mayotte and Reunion)
dt$Site2 <- paste0(dt$Island, "_", dt$Site)
length(unique(dt$Site2))        # 61
mean(as.numeric(dt$river_L))

# Elevation ====================================================================
dt$Elevation <- as.numeric(as.character(dt$Elevation))
range(dt$Elevation)             # 1 to 680


dt[dt$Site %in% c("MAE1"),] 
dt[dt$Site %in% c("MAJ1"),]


# Taxonomy =====================================================================

length(unique(dt$Species))
length(unique(sp$`Species code`))

identical(sort(unique(dt$Species)), sort(unique(sp$`Species code`))) # FALSE

setdiff(unique(dt$Species), unique(as.character(sp$`Species code`)))
#"AMB" "AMS" (both Ambassis spp)
setdiff(unique(as.character(sp$`Species code`)), unique(dt$Species))
#"AMB & AMS" "PER" 

dup <- sp[4,]
dup$`Species code`<- "AMS"
sp$`Species code`[sp$`Scientific name`=="Ambassis spp."] <- "AMB"

sp <- rbind(sp, dup)

dt$Latin <- sp$`Scientific name`[match(dt$Species, sp$`Species code`)]
dt$Genus <- str_split_fixed(dt$Latin, " ", 4) [,1]
dt$SpeciesII <- str_split_fixed(dt$Latin, " ", 4) [,2]
dt$Family <- rep(NA, nrow(dt))


# Genus: -----------------------------------------------------------------------
sort(unique(dt$Genus))
dt$Latin[dt$Genus=="Carangidaespp."]
dt$Genus[dt$Genus=="Carangidaespp."] <- NA                  # Standardise to BT
dt$SpeciesII[dt$Latin=="Carangidaespp."] <- "sp"            # Standardise to BT
dt$Family[dt$Latin=="Carangidaespp."] <- "Carangidae"       # Standardise to BT


# Species: ---------------------------------------------------------------------

unique(dt$SpeciesII)
dt[dt$Latin=="Ambassis spp.",]
dt$SpeciesII[dt$Species=="AMB"] <- "sp1"
dt$SpeciesII[dt$Species=="AMS"] <- "sp2"
dt$SpeciesII[dt$SpeciesII=="klunzingerii(Pfeffer"] <- "klunzingerii"

dt[dt$SpeciesII=="cf.",]
dt$SpeciesII[dt$SpeciesII=="cf."] <- "cf. aporos"
dt$SpeciesII[dt$SpeciesII=="spp."] <- "sp"


View(unique(dt[,c(8,11,12,13,14)]))  # OK

sum(is.na(dt$Family) & is.na(dt$Genus) & is.na(dt$SpeciesII)) # 0

# Create Rawdata: ==============================================================
range(dt$Abundance)
names(dt)
newdt <- dt %>% group_by(Site2, Family, Genus, SpeciesII, 
                         Latitude, Longitude, Elevation, Year) %>%
  summarise("Abundance"=sum(Abundance)) # gather if needed

newdt$Biomass <- rep(NA, nrow(newdt))
newdt <- newdt %>% relocate(c(Abundance, Biomass), .before=Family)
newdt$Plot <- rep(NA, nrow(newdt))

c3 <- newdt %>% group_by(Site2, Year) %>% summarise(nEle=n_distinct(Elevation)) # no need to add elevation to sample_desc

newdt$SampleDescription <- paste0(newdt$Site2, "_",
                                  newdt$Year)
newdt <- newdt %>% relocate(c(SampleDescription, Plot), .after=SpeciesII)
newdt$Day <- rep(NA, nrow(newdt))
newdt$Month <- rep(NA, nrow(newdt))

newdt <- newdt %>% relocate(c(Day, Month, Year), .after=Elevation)

newdt$StudyID <- rep(NA, nrow(newdt))

newdt <- within(newdt, rm(Site2))
names(newdt)[names(newdt)=="SpeciesII"] <- "Species"
names(newdt)[names(newdt)=="Elevation"] <- "DepthElevation"

rawdata <- as.data.frame(newdt)
str(rawdata)
rawdata$Year <- as.numeric(rawdata$Year)


# Write csv ====================================================================

path <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIMEGithubMetadatas/FreshwaterFish&Crustaceans_Mayotte&Reunion_RLagarde_et_al_2020_AFE"
write.csv(rawdata, file=paste0(path, "/rawdata.csv"), row.names = F)   


# Convex hulls =================================================================
# 1. Convert data points into point spatial object
# assumes your rawdata dataframe is called rawdata
dt_merged <- rawdata
range(dt_merged$Latitude)
range(dt_merged$Longitude)
dt_coord <- dt_merged %>% select(Longitude, Latitude) %>% distinct() %>%
  st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()

# 2. Calculate convex hull, area and centroid
centroid <- dt_coord %>% st_convex_hull() %>% st_centroid() %>% unlist # get centroid
centroid

# transform to mercator to get area in sq km
area <- st_transform(dt_coord, st_crs("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs")) %>%
  st_convex_hull() %>% st_area()  ##get area in sq km
area

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
  coord_sf(xlim = c(45,56), ylim = c(-25,-10)) + # make sure these fit your coordinates!
  labs(x = NULL, y = NULL) +
  theme_minimal() # OK, longitudinal transect


# End of script ################################################################

