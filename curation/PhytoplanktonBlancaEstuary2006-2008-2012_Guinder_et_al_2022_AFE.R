##########################################################################################
# Curation Script: PhytoplanktonBlancaEstuary2006-2008-2012_Guinder_et_al_2022_AFE  (2006-2008, 2012)
# AFE
# August 2023
##########################################################################################


# Libraries ==============================================================================
library(dplyr)
library(tidyverse)
library(ggplot2)
library(maps)
library(stringr)
library(lubridate)
library(stringi)
library(sf)
library(clipr)
library(readxl)


# Main source: ---------------------------------------------------------------------------
# https://doi.org/10.1007/s00227-010-1530-5
# https://doi.org/10.1007/s12237-016-0134-9
# https://doi.org/10.1016/j.scitotenv.2017.08.002

rm(list=ls())
setwd("C:/Users/afe1/OneDrive - University of St Andrews/PHD/BioTIMEGithub")
myd <- getwd()


# Read raw data files ====================================================================
# data converted to csv in Excel for easier load
files_dir <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIME/BioTIMENewStudiesCuration/PhytoplanktonBlancaEstuary2006-2008-2012_Guinder_et_al_2022_AFE"

dtA <- read.csv(paste0(files_dir, "/Abu2006.csv"), h=T)  
dtB <- read.csv(paste0(files_dir, "/Bio2006.csv"), h=T) 
dt12A <- read.csv(paste0(files_dir, "/Abu2012.csv"), h=T)  
dt12B <- read.csv(paste0(files_dir, "/Bio2012.csv"), h=T)  


# Initial formatting =====================================================================
l <- list(dtA, dtB, dt12A, dt12B)
l <- lapply(l, function(x) {names(x)[names(x)=="X"] <- "Species"; x
                            names(x) <- gsub("X", "", names(x)); x})
l <- lapply(l, function(x) {x <- gather(x, key="Date", value="Abundance",-1)})


# Temporal ===============================================================================

l1 <- l[[1]]
l2 <- l[[2]]
l3 <- l[[3]]
l4 <- l[[4]]

l1$Year <- str_split_fixed(l1$Date, "\\.", 3)[,1]
l1$Month <- str_split_fixed(l1$Date, "\\.", 3)[,2]
l1$Day <- str_split_fixed(l1$Date, "\\.", 3)[,3]

l2$Year <- str_split_fixed(l2$Date, "\\.", 3)[,1]
l2$Month <- str_split_fixed(l2$Date, "\\.", 3)[,2]
l2$Day <- str_split_fixed(l2$Date, "\\.", 3)[,3]

l3$Day<- str_split_fixed(l3$Date, "\\.", 3)[,1]
l3$Month<- str_split_fixed(l3$Date, "\\.", 3)[,2]
l3$Year<- str_split_fixed(l3$Date, "\\.", 3)[,3]

l4$Day<- str_split_fixed(l4$Date, "\\.", 3)[,1]
l4$Month<- str_split_fixed(l4$Date, "\\.", 3)[,2]
l4$Year <- "2012"

sort(unique(l4$Day))    # OK for all 4 dts
sort(unique(l4$Month))  # OK for all 4 dts
sort(unique(l4$Year))   # OK for all 4 dts

# Taxonomy ===============================================================================

setdiff(l1$Species, l2$Species)
setdiff(l2$Species, l1$Species)

l1$Species[l1$Species=="Chaetoceros sp. 1 (3-8 \xb5m)"] <- "Chaetoceros sp. 1"

identical(l3$Species, l4$Species) # TRUE

#setdiff(l1$Species, l3$Species)  Keep separate for now
#setdiff(l3$Species, l1$Species)  Keep separate for now

l1$Currency <- "Abundance"
l2$Currency <- "Biomass"

lA <- as.data.frame(rbind(l1, l2))

l3$Currency <- "Abundance"
l4$Currency <- "Biomass"

l12 <- as.data.frame(rbind(l3, l4))

sort(unique(lA$Species))

lA$Species <- gsub("sp. ", "sp", lA$Species)
lA$Species <- gsub("\\.", "", lA$Species)

lA$Species[lA$Species=="Dinoflagellates"] <- "Dinoflagellata"

lA$Species[lA$Species=="Centric diatoms"] <- "Centrales"
lA$Species[lA$Species=="Pennate diatoms"] <- "Pennales"

lA$Species[lA$Species=="Flagellates"] <- "Flagellata"

lA$Genus <- str_split_fixed(lA$Species, " ", 2)[,1]
lA$Family <- ifelse(lA$Genus %in% c("Centrales", "Dinoflagellata", "Flagellata", 
                                    "Pennales"), lA$Genus, NA)
lA$Genus <- ifelse(lA$Genus %in% c("Centrales", "Dinoflagellata", "Flagellata", 
                                    "Pennales"), NA, lA$Genus)
lA$Species <- str_split_fixed(lA$Species, " ", 2)[,2]
lA$Species[lA$Species==""] <- NA

unique(lA$Family)   # OK
unique(lA$Genus)    # Ok
unique(lA$Species)  # Ok
lA$Species[lA$Species=="sp2 "] <- "sp2"


sort(unique(l12$Species))
l12$Species[l12$Species=="Dinoflagellates"] <- "Dinoflagellata"
l12$Species[l12$Species=="Centric diatoms"] <- "Centrales"
l12$Species[l12$Species=="Pennate diatoms"] <- "Pennales"
l12$Species[l12$Species=="Flagellates"] <- "Flagellata"

l12$Species[l12$Species=="Chaetoceros sp. 1 (3-8 \xb5m)"] <- "Chaetoceros sp1"
l12$Species <- gsub("\\.", "", l12$Species)

l12$Species[l12$Species=="thalassiosira hendeyii"] <- "Thalassiosira hendeyii"

l12$Genus <- str_split_fixed(l12$Species, " ", 2)[,1]
sort(unique(l12$Genus))
l12$Family <- ifelse(l12$Genus %in% c("Centrales", "Dinoflagellata", "Flagellata", 
                                    "Pennales"), l12$Genus, NA)
l12$Genus <- ifelse(l12$Genus %in% c("Centrales", "Dinoflagellata", "Flagellata", 
                                   "Pennales"), NA, l12$Genus)
l12$Species <- str_split_fixed(l12$Species, " ", 2)[,2]
l12$Species[l12$Species==""] <- NA

unique(l12$Family)   # OK
unique(l12$Genus)    # Ok
unique(l12$Species)  # Ok


# Locations ==============================================================================
# Assigned general coordinates (ciliates file) from Puerto Cuatreros

lA$Latitude <- -38.751575
lA$Longitude <- -62.379864

l12$Latitude <- -38.751575
l12$Longitude <- -62.379864

world_map <- map_data("world") 
world <- ggplot() + coord_fixed() +xlab("") + ylab("") 
world  <- world  + 
  geom_polygon(data=world_map, aes(x=long, y=lat, group=group), 
               colour="gray60", fill="gray60") 
points <- world  + 
  geom_point(data=lA, 
             aes(x=Longitude, y=Latitude, alpha=0.01)) 
points           # OK

#Zoom:
points_zoom <- points +
  ylim(-70,-30)+
  xlim(-100,-40)
points_zoom      # OK


# Abundance ==============================================================================
lA <- within(lA, rm(Date))
l12 <- within(l12, rm(Date))

dfA <- spread(lA, key="Currency", value="Abundance")  
df12 <- spread(l12, key="Currency", value="Abundance")  

dfA$Abundance <- as.numeric(dfA$Abundance)
dfA$Biomass <-  as.numeric(dfA$Biomass)
df12$Abundance <- as.numeric(df12$Abundance)
df12$Biomass <- as.numeric(df12$Biomass)


# dfA: -----------------------------------------------------------------------------------
sum(is.na(dfA$Abundance))        # 633
sum(dfA$Abundance==0, na.rm=T)   # 1
dfA$Abundance[dfA$Abundance==0] <- NA
sum(is.na(dfA$Biomass))          # 0
sum(dfA$Biomass==0, na.rm=T)     # 634

dfA <- dfA[!(dfA$Biomass==0 & is.na(dfA$Abundance)),]  # 631 remaining obs


range(dfA$Abundance)           # 0.27 5366.67
range(dfA$Biomass)             # 0.0019 328.7911

# df12: ----------------------------------------------------------------------------------
sum(is.na(df12$Abundance))     # 0
sum(df12$Abundance==0)         # 175
sum(is.na(df12$Biomass))       # 0
sum(df12$Biomass==0)           # 175

df12 <- df12[!(df12$Biomass==0 & df12$Abundance==0),]  # 173 remaining obs
range(df12$Abundance)          # 1 1232
range(df12$Biomass)            # 0.0042 138.3970


# SampleDescription & Data aggregation: =================================================
names(dfA) <- str_to_title(names(dfA))
names(df12) <- str_to_title(names(df12))

rawdata0608 <- dfA %>% group_by(Family, Genus, Species, Latitude, Longitude, Day, Month, Year) %>%
  summarise(Abundance=sum(Abundance), Biomass=sum(Biomass)) %>% ungroup() # 631 final observations
rawdata12 <- df12 %>% group_by(Family, Genus, Species, Latitude, Longitude, Day, Month, Year) %>%
  summarise(Abundance=sum(Abundance), Biomass=sum(Biomass)) %>% ungroup() # 173 final observations

names(rawdata0608)
names(rawdata12)

rawdata <- as.data.frame(rbind(rawdata0608, rawdata12))
str(rawdata)

rawdata$Day <- as.integer(rawdata$Day)
rawdata$Month <- as.integer(rawdata$Month)
rawdata$Year <- as.integer(rawdata$Year)

rawdata <- rawdata %>% relocate(c(Abundance, Biomass), .before=Family)
rawdata$SampleDescription <- paste0(rawdata$Day, "_", rawdata$Month, "_", rawdata$Year)
rawdata$Plot <- rep(NA, nrow(rawdata))
rawdata <- rawdata %>% relocate(c(SampleDescription, Plot), .before=Latitude)
rawdata$DepthElevation <- rep(-0.5, nrow(rawdata))
rawdata <- rawdata %>% relocate(c(DepthElevation), .after=Longitude)


rawdata$StudyID <- rep(NA, nrow(rawdata))
rawdata <- rawdata %>% relocate(c(StudyID), .after=Year)

names(rawdata)
rawdata$Abundance <- rawdata$Abundance*1000 # raw values are x 10^3

path <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIMEGithubMetadatas/PhytoplanktonBlancaEstuary2006-2008-2012_Guinder_et_al_2022_AFE"
write.csv(rawdata, file=paste0(path, "/rawdata.csv"), row.names = F)


# Convex hulls ==========================================================================
# 1. Convert data points into point spatial object
# assumes your rawdata dataframe is called rawdata
dt_merged <- rawdata
dt_merged$Latitude <- as.numeric(dt_merged$Latitude)
dt_merged$Longitude <- as.numeric(dt_merged$Longitude)
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
  coord_sf(xlim = c(-80, -40), ylim = c(-50,-30)) + 
  labs(x = NULL, y = NULL) +
  theme_minimal()


# End of script #########################################################################



