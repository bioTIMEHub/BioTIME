################################################################################
# Study title:  BioNet Biological Assessment Database - Fish Survey
# Curator: AFE
# Date: August 2023
################################################################################


# Main sources =================================================================
# https://programs.iowadnr.gov/bionet/
# https://publications.iowa.gov/20274/ (for meta and methods)


# Libraries ====================================================================
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
library(measurements)

rm(list=ls())

setwd("C:/Users/afe1/OneDrive - University of St Andrews/PHD/BioTIMEGithub")
myd <- getwd()


# Read raw data files ==========================================================
files_dir <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIME/BioTIMENewStudiesCuration/BioNet_Biological_Assessment_Database_Fish_RivFishTimeId15_AFE"

sps <- read.csv(paste0(files_dir, "/BioNet-FishSpeciesList.csv"), h=T)
dt <- read_excel(paste0(files_dir, "/Dump-FishSessions.xlsx"), sheet="Dump_FishSessions") # note: warnings ok
habitat <- read_excel(paste0(files_dir, "/AdaEslava-ReachHabitatAnalysis.xlsx"), sheet="Sheet1")


# Explore Data =================================================================

c1 <- dt %>% group_by(Landmark) %>% summarise(nStream=n_distinct(StreamName))       # 1 to 6
c2 <- dt %>% group_by(StreamName) %>% summarise(nLand=n_distinct(Landmark))         # 1 to 15 (unnested)
c3 <- dt %>% group_by(Landmark, StreamName) %>% summarise(nSite=n_distinct(SiteID)) # 1 to 4 
table(c3$nSite) # Mainly 1


c4 <- dt %>% group_by(SiteID) %>% summarise(NLat=n_distinct(LatDD), NLong=n_distinct(LongDD)) # All sites have a unique pair of geographical coordinates


# Explore Methods ==============================================================
table(dt$Method)
c5 <- dt %>% group_by(SiteID) %>% summarise(NMethod=n_distinct(Method))             # Always 1 throughout the series except in very few cases (7 sites)
dt <- dt[!dt$Method=="Fishkill Followup",]                                          # unspecified


# Abundance Data ===============================================================
range(dt$TotalCatch) # 1-1777 (no 0s or NAs)


# Temporal Data ================================================================
dt$Day <- as.integer(str_split_fixed(dt$SampleDate, "-", 3)[,3])
dt$Month <- as.integer(str_split_fixed(dt$SampleDate, "-", 3)[,2])
dt$Year <- as.integer(str_split_fixed(dt$SampleDate, "-", 3)[,1])

sort(unique(dt$Day))
sort(unique(dt$Month))
sort(unique(dt$Year))   

c7 <- dt %>% group_by(SiteID) %>% summarise(nYear=n_distinct(Year))     
c8 <- dt %>% group_by(StreamName) %>% summarise(nYear=n_distinct(Year)) 
c9 <- dt %>% group_by(Landmark) %>% summarise(nYear=n_distinct(Year))   

dt <- dt %>% group_by(SiteID) %>% filter(., length(unique(Year)) > 1) %>% ungroup() # rm sites sampled only 1 year. Remaining 21804 obs


# Locations ====================================================================
sum(is.na(dt$LatDD))  # 0
sum(is.na(dt$LongDD)) # 0


world_map <- map_data("world") 
world <- ggplot() + coord_fixed() +xlab("") + ylab("") 
world  <- world  + 
  geom_polygon(data=world_map, aes(x=long, y=lat, group=group), 
               colour="gray60", fill="gray60") 
points <- world  + 
  geom_point(data=dt, 
             aes(x=LongDD, y=LatDD, alpha=0.01)) 
points       

#Zoom:
points_zoom <- points +
  ylim(30,80)+
  xlim(-120, -70)
points_zoom 


# Taxonomy =====================================================================

sum(is.na(dt$FishLatin))   # 55
sum(is.na(dt$FishSpecies)) # 0

sort(unique(dt$FishSpecies))
sort(unique(dt$FishLatin))
sort(unique(dt$FishSpecies[is.na(dt$FishLatin)]))

"FALSE" %in% dt$FishLatin %in% sps$latin  # FALSE, info for all

dt$Family <- ifelse(is.na(dt$FishLatin), sps$family[match(dt$FishSpecies, sps$species)], sps$family[match(dt$FishLatin, sps$latin)])
sort(unique(dt$Family))
sum(is.na(dt$Family))      # 0


dt$FishLatin <- gsub("spp.", "sp", dt$FishLatin)
dt$Species <- str_split_fixed(dt$FishLatin, " ", 2)[,2]
dt$Genus <- str_split_fixed(dt$FishLatin, " ", 2)[,1]

sum(is.na(dt$Species))        # 0
sum(dt$Species=="")           # 55

sum(is.na(dt$Genus))          # 0
sum(dt$Genus=="", na.rm=T)    # 55

unique(dt$Family[dt$FishSpecies=="Catostomidae - Unidentified"]) # OK
unique(dt$Family[dt$FishSpecies=="Cyprinidae - Unidentified"])   # OK
dt$Species[dt$FishSpecies %in% c("Catostomidae - Unidentified", "Cyprinidae - Unidentified")] <- "sp"

unique(dt$Family[dt$FishSpecies=="Tiger Trout"]) 
unique(dt$Genus[dt$FishSpecies=="Tiger Trout"])  
dt$Species[dt$FishSpecies=="Tiger Trout"] <- "sp"


# Hybrids: ---------------------------------------------------------------------
# NOTE: (when the two species are known, then sp1, sp2, etc, when not, then sp)

unique(dt$Family[dt$FishSpecies %in% c("Hybrid sunfish", "Hybrid: Micropterus Cross")]) 
dt$Species[dt$FishSpecies == "Hybrid sunfish"] <-"sp1"
dt$Species[dt$FishSpecies == "Hybrid: Micropterus Cross"] <-"sp2"

unique(dt$Family[dt$FishSpecies %in% c("Hybrid: Common Shiner x Carmine Shiner", 
                                       "Hybrid: Common Shiner X So. Redbelly Dace", 
                                       "Hybrid: Red Shiner x Spotfin Shiner")])     # "Cyprinidae"
dt$Species[dt$FishSpecies %in% c("Hybrid: Common Shiner x Carmine Shiner")] <-"sp1"
dt$Species[dt$FishSpecies %in% c("Hybrid: Common Shiner X So. Redbelly Dace")] <-"sp2"
dt$Species[dt$FishSpecies %in% c("Hybrid: Red Shiner x Spotfin Shiner")] <-"sp3"

sum(is.na(dt$Species))             # 0
sum(dt$Species=="", na.rm=T)       # 0
sum(is.na(dt$Genus))               # 55


sort(unique(dt$Species))           # OK
sort(unique(dt$Genus))             # OK
sort(unique(dt$Family))            # OK

sum(is.na(dt$Family) & is.na(dt$Genus) & is.na(dt$Species)) # 0


# Methods ======================================================================
table(dt$Method)
c10 <- dt %>% group_by(SiteID) %>% summarise(nYear=n_distinct(Year))         # 2 to 16
c11 <- dt %>% group_by(SiteID, Method) %>% summarise(nYear=n_distinct(Year)) # 1 to 16
# NOTE: the sampling gear used at a given site at any point in time will 
# have been dependent on the physical characteristics of the site (Wadeable or Non-wadeable) and how these have changed over time.
# These changes are often specified in "FishComment". Thus, in this dataset when
# a site has switched from wadeable to non-wadeable status or viceversa, observations are kept in the same series.

sum(c11$nYear==1) # 10
10/339*100        # 2.9%

check <- c11$SiteID[duplicated(c11$SiteID)]
c11[c11$SiteID %in% check,]
length(unique(check))/length(unique(c11$SiteID))*100 # 1.8%

c12 <- dt %>% group_by(SiteID, Method) %>% summarise(nB=n_distinct(EqBoatUsed))       # always 1
c13 <- dt %>% group_by(SiteID, Method) %>% summarise(nBarge=n_distinct(EqBargeUsed))  # sometimes false & true for the same site across time


# Areas ========================================================================
head(habitat, 2L)
unique(habitat$DisplayUnit) # always ft or NA(for ratios)
habitat <- within(habitat, rm(DisplayUnit))
unique(habitat$HabParam)
habitat <- habitat[habitat$HabParam %in% c("Stream Width - Average", "Reach - Total Habitat Reach Length"),]

# dups for site 811.
#View(habitat[rownames(habitat) %in% c(2501, 2502, 2503, 2504),])
#View(habitat[habitat$SiteID==811,])
#View(dt[dt$SiteID==811 & dt$SampleDate=="2017-10-04",]) # date in habitat & dt don't match.

unique(dt$SampleDate[dt$SiteID==811])
unique(habitat$SampleDate[habitat$SiteID==811]) # mismatch of dates...

# rm dups in order to use spread (and date not in dt anyway):
habitat <- habitat[!rownames(habitat)%in% c(2501, 2502, 2503, 2504),]

habitat <- spread(habitat, key="HabParam", value="HabValue")
names(habitat)[names(habitat)=="Reach - Total Habitat Reach Length"] <- "HabitatReachSize"
names(habitat)[names(habitat)=="Stream Width - Average"] <- "HabitatReachAvWidth"
head(dt, 2L)

dt$ConcatSample <- paste0(dt$SiteID, "_",dt$SampleDate)
habitat$ConcatSample <- paste0(habitat$SiteID, "_", habitat$SampleDate)

dt$HabitatReachSize <- habitat$HabitatReachSize[match(dt$ConcatSample, habitat$ConcatSample)]
sum(is.na(dt$HabitatReachSize)) # 2951 obs with NA
dt$HabitatWidth <- habitat$HabitatReachAvWidth[match(dt$ConcatSample, habitat$ConcatSample)]
sum(is.na(dt$HabitatWidth))     # 2951 obs with NA

length(unique(dt$ConcatSample[is.na(dt$HabitatWidth)])) # 162
length(unique(dt$ConcatSample))                         # 1344
162/1344*100                                            # 12% no area
#View(dt[is.na(dt$HabitatReachSize),])
dt <- dt[!is.na(dt$HabitatWidth),]                      # rm since areas can't be calculated

c14 <- dt %>% group_by(SiteID, Day, Month, Year) %>% mutate(diffL=abs(ReachSize-HabitatReachSize))
range(c14$diffL)                                        # 0 867 ft, 264 m

# The length of the reach and the fished length of the reach don't always overlap
# perfectly as indicated by BioNET data manager. This is OK for approx area calculation
# and fished length (distanced sampled) is taken.

mean(dt$ReachSize)     # 941 ft
range(dt$ReachSize)    # no 0s

mean(dt$HabitatWidth)  # 34 ft
range(dt$HabitatWidth) # 2.37 146.30 ft

dt$ReachSize_m <- measurements::conv_unit(dt$ReachSize, "ft", "m")
range(dt$ReachSize_m)  # 66.4464 548.6400 [Distance in m]
dt$Width_m <- measurements::conv_unit(dt$HabitatWidth, "ft", "m")
range(dt$Width_m)      # 0.722376 44.592240

dt$Area_m2 <- dt$Width_m*dt$ReachSize_m
range(dt$Area_m2)      # 105.3948 15283.1149 (0.015 km2)


# After filtering, rm further obs for just 1 year ============================== 
dt <- dt %>% group_by(SiteID) %>% filter(., length(unique(Year)) > 1) %>% ungroup()  # rm sites sampled only 1 year. Remaining 18329 obs
c15 <- dt %>% group_by(SiteID, Method) %>% summarise(nYear=n_distinct(Year))         # 2 to 16 (means few sites monitored with slightly diff methods were removed with filterings above)
unique(dt$Method) # filterings leave only Standard Wadeable sites
mean(dt$Area_m2)  # grain size (3295.351)


# SampleDesc & Aggregation =====================================================
str(dt)
dt$Density_100m2 <- (dt$TotalCatch/dt$Area_m2)*100
range(dt$Density_100m2) # 6.543169e-03 1.623297e+02 (0.007 to 162 ind)

rawdata <- dt %>% group_by(Family, Genus, Species, SiteID, LatDD, LongDD, Day, Month, Year) %>%
  summarise(Abundance=sum(Density_100m2)) %>% ungroup()  # 18329
names(rawdata)[names(rawdata)=="LatDD"] <- "Latitude"
names(rawdata)[names(rawdata)=="LongDD"] <- "Longitude"

rawdata$Biomass <- rep(NA, nrow(rawdata))
rawdata <- rawdata %>% relocate(c(Abundance, Biomass), .before=Family)

rawdata$SampleDescription <- paste0(rawdata$SiteID, "_", rawdata$Day, "_", rawdata$Month, "_", rawdata$Year)
rawdata$Plot <- rep(NA, nrow(rawdata))
rawdata$DepthElevation <- rep(NA, nrow(rawdata))

rawdata <- rawdata %>% relocate(c(SampleDescription, Plot), .after=Species)
rawdata <- rawdata %>% relocate(c(DepthElevation), .after=Longitude)

rawdata$StudyID <- rep(NA, nrow(rawdata))
rawdata <- rawdata %>% relocate(c(StudyID), .after=Year)

rawdata <- within(rawdata, rm(SiteID))

#View(rawdata)
str(rawdata)


# Write csv ====================================================================
path <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIMEGithubMetadatas/BioNet_Biological_Assessment_Database_Fish_RivFishTimeId15_AFE"
write.csv(rawdata, file=paste0(path, "/rawdata.csv"), row.names = F)


# Convex hulls =================================================================
# 1. Convert data points into point spatial object
# assumes your rawdata dataframe is called rawdata
dt_merged <- rawdata
dt_merged$Latitude <- as.numeric(dt_merged$Latitude)
dt_merged$Longitude <- as.numeric(dt_merged$Longitude)
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
  coord_sf(xlim = c(-120,-70), ylim = c(30,80)) + 
  labs(x = NULL, y = NULL) +
  theme_minimal()

# End of script ################################################################
################################################################################

