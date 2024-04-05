################################################################################
# SCRIPT: study ID 435 (new data up to year 2020)
# AFE
# 25/10/2021 & January 2024
################################################################################


# Main source ==================================================================
# https://portal.edirepository.org/nis/mapbrowse?scope=knb-lter-pal&identifier=199&revision=9


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
library(measurements)
library(data.table)


# Read Data ====================================================================

rm(list=ls())

setwd("C:/Users/afe1/OneDrive - University of St Andrews/PHD/BioTIMEGithub")


# Relevant notes: ==============================================================
# Abundance (#/1000 m3) and biomass (i.e., biovolume, mL/1000 m3) 
# were determined for dominant macrozooplankton taxa. 

# Data includes tow location, time, depth, and volume of water filtered by the net. 
# Samples were preserved in 4% buffered formaldehyde. 

# A blank abundance or biovolume value means that the sample was not analyzed for that taxonomic group; 
# a value of -1 means that the taxonomic group was present but not quantified.


# Read data: ===================================================================
path <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIME/BioTIMENewStudiesCuration/0_BioTIMEv1Revision/STUDY_435_AFE"
odt <- read.csv(paste0(path, "/BTv1/rawdata.csv"), h=T)
metdt <- read.csv(paste0(path, "/BTv1/metadata.csv"), h=T) # bt v1

dt <- read.csv(paste0(path, "/ZooplanktonDensity-DataZoo-20220517.csv"), h=T)   # new data accessed 29 January 2024
str(dt)

range(odt$YEAR)
range(dt$Date) # more years of data (up to 2020)


# Methods: =====================================================================
metdt$METHODS
metdt$COMMENTS
metdt$GRAIN_SIZE_TEXT


# Sampling Event Date ==========================================================
names(dt)

dt$Day <- as.integer(str_split_fixed(dt$Date, "-", 3)[,3])
sort(unique(dt$Day))
dt$Month <- as.integer(str_split_fixed(dt$Date, "-", 3)[,2])
sort(unique(dt$Month)) # summer samples
dt$Year <- as.integer(str_split_fixed(dt$Date, "-", 3)[,1])
sort(unique(dt$Year))

sum(is.na(dt$Day) | is.na(dt$Month) | is.na(dt$Year)) # 0


# Sampling Event ===============================================================
length(unique(dt$CruiseTow))  # 432
length(unique(dt$CruiseName)) # 12
length(unique(dt$Event))      # 281
length(unique(dt$Tow))        # 125
length(unique(dt$GridLine))   # 70, line on a pre-defined survey grid in the LTER Palmer study area
length(unique(dt$GridStation))# 64, station on a pre-defined survey grid in the LTER Palmer study area (keep for sampling event)


unique(dt$ZAID)
table(dt$ZAID)

dt <- dt[dt$ZAID=="Full",] # rm not fully analysed

table(dt$NetID)
table(dt$TowType)


c1 <- dt %>% group_by(GridStation) %>% summarise(nLat=n_distinct(LatitudeStart), nLong=n_distinct(LongitudeStart)) # from 1 up to approx 80
c2 <- dt %>% group_by(GridStation) %>% summarise(nLat=n_distinct(LatitudeEnd), nLong=n_distinct(LongitudeEnd))     # from 1 up to approx 80
c3 <- dt %>% group_by(GridLine) %>% summarise(nGrid=n_distinct(GridStation))     # from 1 up to 12
c4 <- dt %>% group_by(GridStation) %>% summarise(nGrid=n_distinct(GridLine))     # from 1 up to 11


c5 <- dt %>% group_by(GridStation) %>% summarise(nYear=n_distinct(Year))     
sum(c5$nYear==1)                                                             # 33
c6 <- dt %>% group_by(GridLine) %>% summarise(nYear=n_distinct(Year))        
sum(c6$nYear==1)                                                             # 47
c7 <- dt %>% group_by(GridLine, GridStation) %>% summarise(nYear=n_distinct(Year)) 
sum(c7$nYear==1)                                                             # 76
c7$GridConcat <- paste0(c7$GridLine, "_", c7$GridStation)
remove <- c7$GridConcat[c7$nYear==1]
dt$GridConcat <- paste0(dt$GridLine, "_", dt$GridStation)
dt <- dt[!dt$GridConcat %in% remove,]
c8 <- dt %>% group_by(GridLine, GridStation) %>% summarise(nYear=n_distinct(Year)) # 2 to 12


# Depth Sample =================================================================
c9 <- dt %>% group_by(GridConcat, Year) %>% distinct(DepthMaximum)
c9B <- dt %>% group_by(GridConcat) %>% summarise(nMax=max(DepthMaximum), nMin=min(DepthMaximum), nDiff=max(DepthMaximum)-min(DepthMaximum)) # only a few with more than 10m

unique(dt$DepthTarget)                                                                 # depth range of sample collection
c10 <- dt %>% group_by(GridConcat) %>% summarise(nDepthT=n_distinct(DepthTarget))      # pretty much always 1
c10B <- dt %>% group_by(GridConcat, DepthTarget) %>% summarise(nYear=n_distinct(Year)) # some equal 1

dt$GridConcat2 <- paste0(dt$GridConcat, "_", dt$DepthTarget)
c10B$GridConcat2 <- paste0(c10B$GridConcat, "_", c10B$DepthTarget)
remove2 <- c10B$GridConcat2[c10B$nYear==1]
dt <- dt[!dt$GridConcat2 %in% remove2,]


# Gather data:==================================================================
names(dt)
lndt <- gather(dt, key="Taxa", value="Currency", -c(1:26, 111:115))
dim(lndt)               # 26964    33

sort(unique(lndt$Taxa))

Andt <- lndt[grep("Num", lndt[["Taxa"]]),] # 13482
Bndt <- lndt[grep("Vol", lndt[["Taxa"]]),] # 13482

Andt$TaxaII <- str_split_fixed(Andt$Taxa, "Num", 2)[,1] # OK
unique(Andt$TaxaII)
Bndt$TaxaII <- str_split_fixed(Bndt$Taxa, "Vol", 2)[,1] # OK
unique(Bndt$TaxaII)
identical(sort(unique(Andt$TaxaII)), sort(unique(Bndt$TaxaII))) # TRUE

Andt$CurrencyII <- rep("abundance", nrow(Andt))
Bndt$CurrencyII <- rep("biomass", nrow(Bndt))


ndt <- as.data.frame(rbind(Andt, Bndt))
ndt <- within(ndt, rm(Taxa))
ndt <- spread(ndt, key="CurrencyII", value="Currency") # 13482


# Abundance & Biomass: =========================================================
sum(is.na(ndt$abundance)) # 94
sum(is.na(ndt$biomass))   # 94
sum(is.na(ndt$abundance) & is.na(ndt$biomass)) # 94
ndt <- ndt[!is.na(ndt$abundance),]


sum(ndt$abundance==-1)    # 71
sum(ndt$biomass==-1)      # 86
sum(ndt$abundance==-1 & ndt$biomass==-1) # 56
ndt <- ndt[!(ndt$abundance==-1 & ndt$biomass==-1),]


sum(ndt$abundance==0) #  7635
sum(ndt$biomass==0)   #  7635
ndt <-ndt[!(ndt$abundance==0 & ndt$biomass==0),]


ndt$abundance[ndt$abundance<0] <- NA  
ndt$biomass[ndt$biomass<0] <- NA        # -1 indicates taxa present, but abundance unreported. Therefore NA.
sum(is.na(ndt$abundance))
sum(is.na(ndt$biomass))
#View(ndt[is.na(ndt$abundance) | is.na(ndt$biomass),])

ndt$abundance <- as.numeric(ndt$abundance)
ndt$biomass <- as.numeric(ndt$biomass)

range(ndt$abundance, na.rm=TRUE) # 0.01 10591.75
range(ndt$biomass, na.rm=TRUE)   # 0.001 4730.655


# Taxonomy: ====================================================================
sort(unique(ndt$TaxaII))


# From metadata:
# Density prefixes: Radio = Radiolaria; Siphon = Siphonophorae; 
# Hydrozoa_o = Hydrozoa (other hydrozoans, excluding Siphonophorae); 
# Periphyl = Periphylla sp. 
# scyphozoans; 
# Scyphozo_o = Scyphozoa (other scyphozoans, excluding Periphylla); 
# Cnidaria_o = Cnidaria (other cnidarians, excluding defined taxa); 
# Ctenoph = Ctenophora; Clio = Clio spp. pteropods; Limacina = Limacina helicina; 
# Clione = Clione spp. pteropods; Spongiob = Spongiobranchaea spp. 
# pteropods; 
# Gastropo_o = Gastropoda (other gastropods, excluding defined taxa); 
# Cephalop = Cephalopoda; Tomopter = Tomopteris spp. 
# polychaetes; 
# Polychae_o = Polychaeta (other polychaetes, excluding Tomopteris); 
# Pseudosa = Pseudosagitta spp. 
# chaetognaths; Chaetogn_o = Sagittoidea (other chaetognaths, excluding Pseudosagitta); 
# Copepoda = Copepoda; Ostracod = Ostracoda; Mysida = Mysida; Gammarid = Gammaridea 
# amphipods; 
# Vibilia = Vibilia spp. amphipods; Cyllopus = Cyllopus spp. amphipods; Primno = Primno macropa; 
# Themisto = Themisto gaudichaudii; Hyperiel = Hyperiella spp. amphipods; Hyperoche = Hyperoche spp. 
# amphipods; Amphipod_o = Amphipoda (other amphipods, excluding defined taxa); 
# Esuperba = Euphausia superba; Ecrystal = Euphausia crystallorophias; 
# Efrigida = Euphausia frigida; Etriacan = Euphausia triacantha; 
# Thysanoe = Thysanoessa macrura; Euphaus_o = Euphausiacea (other euphausiids, excluding defined taxa) (mostly immature stages); 
# Decapoda = Decapoda; Ihlea = Ihlea racovitzai 
# (2013 to present); SalpaAgg = Salpa thompsoni aggregate stage; 
# SalpaSol = Salpa thompsoni solitary stage; Salpidae = unidentified Salpidae; 
# Pantarct = Pleuragramma antarcticum; 
# Teleoste = Teleostei (other fish, excluding P. antarcticum); other = other taxa not listed (taxonomic details available on request) 

ndt$TaxaII <- plyr::revalue(ndt$TaxaII, c("Radio" = "Radiolaria",
                                          "Siphon" = "Siphonophorae",
                                          "Hydrozoa_o" = "Hydrozoa",
                                          "Periphyl" = "Periphylla sp",
                                          "Scyphozo_o" = "Scyphozoa",
                                          "Cnidaria_o" = "Cnidaria",
                                          "Ctenoph" = "Ctenophora",
                                          "Clio" = "Clio spp",
                                          "Limacina" = "Limacina helicina",
                                          "Clione" = "Clione spp",
                                          "Spongiob" = "Spongiobranchaea spp",
                                          "Gastropo_o" = "Gastropoda",
                                          "Cephalop" = "Cephalopoda",
                                          "Tomopter" = "Tomopteris spp",
                                          "Polychae_o" = "Polychaeta",
                                          "Pseudosa" = "Pseudosagitta spp",
                                          "Chaetogn_o" = "Sagittoidea",
                                          "Copepoda" = "Copepoda",
                                          "Ostracod" = "Ostracoda",
                                          "Mysida" = "Mysida",
                                          "Gammarid"= "Gammaridea",
                                          "Vibilia" = "Vibilia spp",
                                          "Cyllopus" = "Cyllopus spp",
                                          "Primno" = "Primno macropa",
                                          "Themisto" = "Themisto gaudichaudii",
                                          "Hyperiel" = "Hyperiella spp",
                                          "Hyperoche" = "Hyperoche spp",
                                          "Amphipod_o" = "Amphipoda",
                                          "Esuperba" = "Euphausia superba",
                                          "Ecrystal" = "Euphausia crystallorophias",
                                          "Efrigida" = "Euphausia frigida",
                                          "Etriacan" = "Euphausia triacantha",
                                          "Thysanoe" = "Thysanoessa macrura",
                                          "Euphaus_o" = "Euphausiacea",
                                          "Decapoda" = "Decapoda",
                                          "Ihlea" = "Ihlea racovitzai",
                                          "SalpaAgg" = "Salpa thompsoni aggregate stage",
                                          "SalpaSol" = "Salpa thompsoni solitary stage",
                                          "Pantarct" = "Pleuragramma antarcticum",
                                          "Teleoste" = "Teleostei"))

sort(unique(ndt$TaxaII))

sum(ndt$TaxaII=="other")             # 38
ndt <- ndt[!ndt$TaxaII == "other",]


vec <- c("Amphipoda", "Cephalopoda", "Cnidaria", "Copepoda", "Ctenophora",
         "Decapoda", "Euphausiacea", "Gammaridea", "Gastropoda", "Hydrozoa", "Mysida",
         "Ostracoda", "Polychaeta", "Salpidae", "Scyphozoa", "Siphonophorae", "Teleostei",
         "Radiolaria", "Sagittoidea")
ndt$Family <- rep(NA, nrow(ndt))
ndt$Genus <- rep(NA, nrow(ndt))
ndt$Species <- rep(NA, nrow(ndt))

ndt$Family <- ifelse(ndt$TaxaII %in% vec, ndt$TaxaII, NA)
ndt$Genus <- ifelse(ndt$TaxaII %in% vec, NA, ndt$TaxaII)


unique(ndt$Family)
sum(ndt$Family %in% vec)            # 2244
2244/5659*100                       # 40%

unique(ndt$Genus)
ndt$Species <- str_split_fixed(ndt$Genus, " ", 2) [,2]
ndt$Genus <- str_split_fixed(ndt$Genus, " ", 2) [,1]

unique(ndt$Genus)
unique(ndt$Species)

ndt$Species <- plyr::revalue(ndt$Species, c("spp"="sp",
                                          "thompsoni aggregate stage"="thompsoni",
                                          "thompsoni solitary stage"="thompsoni"))
sort(unique(ndt$Species))

ndt$Genus[ndt$Genus==""] <- NA
ndt$Species[ndt$Species==""] <- NA

sum(is.na(ndt$Family) & is.na(ndt$Genus) & is.na(ndt$Species)) # 0


# Coordinates: =================================================================

range(ndt$LatitudeStart)
range(ndt$LongitudeStart)

range(ndt$LatitudeEnd, na.rm=T)
range(ndt$LongitudeEnd, na.rm=T)

world_map <- map_data("world") 
world <- ggplot() + coord_fixed() +xlab("") + ylab("") 
world  <- world  + 
  geom_polygon(data=world_map, aes(x=long, y=lat, group=group), 
               colour="gray60", fill="gray60") 
points <- world  + 
  geom_point(data=ndt, 
             aes(x=LongitudeStart, y=LatitudeStart, alpha=0.01)) 
points 

world_map <- map_data("world") 
world <- ggplot() + coord_fixed() +xlab("") + ylab("") 
world  <- world  + 
  geom_polygon(data=world_map, aes(x=long, y=lat, group=group), 
               colour="gray60", fill="gray60") 
points <- world  + 
  geom_point(data=ndt, 
             aes(x=LongitudeEnd, y=LatitudeEnd, alpha=0.01)) 
points 


c11 <- ndt %>% group_by(GridConcat2) %>% summarise(nLat=n_distinct(LatitudeStart), nLong=n_distinct(LongitudeStart)) 
c12 <- ndt %>% group_by(Tow, Year) %>% summarise(nDay=n_distinct(Day))      # always 1
c12B <- ndt %>% group_by(Event, Year) %>% summarise(nDay=n_distinct(Day))   # always 1
c12C <- ndt %>% group_by(Event, Year) %>% summarise(nCru=n_distinct(CruiseTow))   # always 1
c13 <- ndt %>% group_by(GridConcat2, Year, Month, Day) %>% summarise(nTow=n_distinct(Tow), nEv= n_distinct(Event))   # a few times 2 or 3

c14 <- ndt %>% group_by(GridConcat2) %>% summarise(nCN=n_distinct(CruiseName)) # different cruises sampling the series
c14B <- ndt %>% group_by(CruiseTow) %>% summarise(nY=n_distinct(Year))         # always 1
c14C <- ndt %>% group_by(GridConcat2, Day, Month, Year) %>% summarise(nCTow = n_distinct(CruiseTow))   
c14D <- ndt %>% group_by(GridConcat2, Day, Month, Year) %>% distinct(CruiseTow)   


#ndt <- ndt %>% group_by(GridConcat2, Day, Month, Year) %>% mutate(IndexWithinGroup2 = dense_rank(CruiseTow))   # n_distinct tow as sample number (sample 1, sample 2, sample 3)
#c14F <- c14E %>% group_by(GridConcat2, Day, Month, Year) %>% distinct(IndexWithinGroup2)  


length(unique(ndt$CruiseName))
length(unique(ndt$Event))
length(unique(ndt$Tow))
length(unique(ndt$CruiseTow))


# RawData ======================================================================
c15 <- ndt %>% group_by(CruiseTow) %>% summarise(nDepth=n_distinct(DepthMaximum))
c16 <- ndt %>% group_by(CruiseTow) %>% summarise(nLat=n_distinct(LatitudeStart))
c17 <- ndt %>% group_by(CruiseTow) %>% summarise(nLong=n_distinct(LongitudeStart))
rawData <- ndt %>% group_by(Family, Genus, Species, GridConcat2, CruiseTow, LatitudeStart, LongitudeStart, DepthMaximum, Day, Month, Year) %>%
  summarise(Abundance=sum(abundance), Biomass=sum(biomass))
sum(is.na(rawData$Abundance)) # 14
range(rawData$Abundance, na.rm=T)
sum(is.na(rawData$Biomass))   # 30
range(rawData$Biomass, na.rm=T)

rawData <- rawData %>% relocate(c(Abundance, Biomass), .before=Family)
rawData$SampleDescription <- paste0(rawData$GridConcat2, "_", rawData$CruiseTow, "_",
                                    rawData$Day, "_", rawData$Month, "_",
                                    rawData$Year)
rawData$Plot <- rep(NA, nrow(rawData))
rawData <- rawData %>% relocate(c(SampleDescription, Plot), .after=Species)


names(rawData) <- plyr::revalue(names(rawData), c("LatitudeStart"="Latitude",
                                                  "LongitudeStart"="Longitude",
                                                  "DepthMaximum"="DepthElevation"))
rawData$StudyID <- rep(435, nrow(rawData))

rawData <- within(rawData, rm(GridConcat2, CruiseTow))

rawData <- as.data.frame(rawData)
range(rawData$Latitude)
range(rawData$Longitude)
range(rawData$DepthElevation, na.rm=T)


# Write csv ====================================================================
path <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIMEGithubMetadatas/Study_435_AFE"
write.csv(rawData, file=paste0(path, "/rawData.csv"), row.names = F)


# convex hulls =================================================================
# 1. Convert data points into point spatial object
dt_merged <- rawData
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
  coord_sf(xlim = c(-80,-60), ylim = c(-80,-50)) + 
  labs(x = NULL, y = NULL) +
  theme_minimal()

# End of script ################################################################







