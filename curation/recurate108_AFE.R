################################################################################
# Revision of study 108
# AFE Nov 2024
################################################################################


# Main sources =================================================================
# https://data.aad.gov.au/metadata/ASAC_2208_seabirds


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
require(taxize)




# Inspect: =====================================================================

rm(list=ls())

setwd("C:/Users/afe1/OneDrive - University of St Andrews/PHD/BioTIMEGithub")
myd <- getwd()



# Read raw data files ==========================================================
files_dir <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIME/BioTIMENewStudiesCuration/RecurateOct2024/id108"

dt <- read.csv(paste0(files_dir, "/ASAC_2208_seabirds.csv"), h=T)
sort(unique(dt$observation_code))
sort(unique(dt$observation_code[dt$species_type=="Bird"]))
table(dt$observation_code[dt$species_type=="Bird"])

voy <- read.csv(paste0(files_dir, "/voyage_id to set_code mappings.csv"), h=T)




# Removal of observations lacking crucial metadata:=============================
dt <- dt[!dt$observation_code %in% c("null observation - no birds seen", "Suspect observation"),]
dt <- dt[dt$identification_status %in% c("Confirmed"),] # rm unconfirmed obs


table(dt$species_type)           # predominantly birds
unique(dt$species_type)
unique(dt$taxon_id[dt$species_type==""]) # NA
unique(dt$species[dt$species_type==""])  # ""
unique(dt$genus[dt$species_type==""])    # ""
dt <- dt[!dt$species_type=="",]          # unidentified cols

#c1 <- dt %>%
#  group_by(species_type) %>% summarise(n=n_distinct(observation_date_year))

#c2 <- dt %>%
#  group_by(species_type, latitude, longitude) %>% summarise(n=n_distinct(observation_date_year)) # mostly 1, OK

dt <- dt[!is.na(dt$latitude),]   # rm obs with no coords
dt <- dt[!is.na(dt$longitude),]


sum(is.na(dt$taxon_id))          # obs with no species id
sum(is.na(dt$species))           # 0
sum(dt$species=="")              # 45
sum(dt$genus=="")                # 45
dt <- dt[!is.na(dt$taxon_id),]   # rm


sum(is.na(dt$observation_code))
sum(dt$observation_code=="")     # 1466
unique(dt$observation_code)
dt <- dt[!dt$observation_code=="",] # rm records with no observation code (monitoring type) 


dt$SpeciesII <- paste0(dt$genus, " ", dt$species)
length(sort(unique(dt$SpeciesII)))   # 119


sum(is.na(dt$observation_date_year)) # 38
dt$observation_date[is.na(dt$observation_date_year)] 
sort(unique(dt$observation_date_year))

dt <- dt[!is.na(dt$observation_date_year),]  # 152041


sum(is.na(dt$observation_date_day))   # 0
sum(is.na(dt$observation_date_month)) # 0



# Select Columns to retain for BioTIME: ========================================

names(dt)
tokeep <- c("wov_data_id", 
  "observation_date_year", "observation_date_month", "observation_date_day",
  "latitude", "longitude", 
  "depth", 
  "species_type", "taxon_id",
  "distance",
  "species_count",
  "voyage_id",
  "notes",
  "data_notes",
  "genus", "species", "SpeciesII") # to retain for now
dt <- dt[,names(dt) %in% tokeep]




# Check coordinates: ===========================================================

# world_map <- map_data("world") 
# world <- ggplot() + coord_fixed() +xlab("") + ylab("") 
# world  <- world  + 
#   geom_polygon(data=world_map, aes(x=long, y=lat, group=group), 
#                colour="gray60", fill="gray60") 
# points <- world  + 
#   geom_point(data=dt, 
#              aes(x=longitude, y=latitude, alpha=0.01)) 
# points    # locs look fine for mammals 
#  

#View(dt[dt$longitude<0,])
sort(unique(dt$voyage_id[dt$longitude<0])) # 89
sort(unique(dt$voyage_id[dt$longitude>0])) # 89
#View(dt[dt$longitude>0 & dt$voyage_id==89,]) # coming from the other side


c4 <- dt[dt$longitude<0,]

unique(c4$observation_date_year[c4$longitude > -10])                      # 1992
unique(c4$observation_date_year[c4$longitude > -20])                      # 1992
unique(c4$observation_date_year[c4$longitude > -30 & c4$longitude < -20]) # 1992
unique(c4$observation_date_year[c4$longitude > -40 & c4$longitude < -30]) # 1992
unique(c4$observation_date_year[c4$longitude > -50 & c4$longitude < -40]) # 1992
unique(c4$observation_date_year[c4$longitude > -55 & c4$longitude < -50]) # 1991 - 1992 (diff from -60/-55, -55/-50)
unique(c4$observation_date_year[c4$longitude > -55 & c4$longitude < -50]) # 1991 - 1992 (diff from -60/-55, -55/-50)
unique(c4$observation_date_year[c4$longitude > -65 & c4$longitude < -60]) # 1991 - 1992 ...
unique(c4$observation_date_year[c4$longitude > -70 & c4$longitude < -65]) # 1991
unique(c4$observation_date_year[c4$longitude > -80 & c4$longitude < -70]) # 1991
unique(c4$observation_date_year[c4$longitude > -90 & c4$longitude < -80]) # 1991
unique(c4$observation_date_year[c4$longitude > -100 & c4$longitude < -90]) # 1991
unique(c4$observation_date_year[c4$longitude > -110 & c4$longitude < -100]) # 1991
unique(c4$observation_date_year[c4$longitude > -150 & c4$longitude < -100]) # 1991
unique(c4$observation_date_year[c4$longitude > -180 & c4$longitude < -100]) # 1991


dt <- dt[!dt$voyage_id==89,]



# world_map <- map_data("world") 
# world <- ggplot() + coord_fixed() +xlab("") + ylab("") 
# world  <- world  + 
#   geom_polygon(data=world_map, aes(x=long, y=lat, group=group), 
#                colour="gray60", fill="gray60") 
# points <- world  + 
#   geom_point(data=dt, 
#              aes(x=longitude, y=latitude, alpha=0.01)) 
# points  



# Check year/month/day: ========================================================
range(dt$observation_date_year)
sort(unique(dt$observation_date_year))

range(dt$observation_date_month)
sort(unique(dt$observation_date_month))

range(dt$observation_date_day)
sort(unique(dt$observation_date_day))



# Depths: ======================================================================
sum(is.na(dt$depth))      # 52763
range(dt$depth, na.rm=T)  # 0 6000
range(dt$depth[dt$species_type=="Bird"], na.rm=T)
unique(dt$species_type[dt$depth > 0])
dt$depth <- dt$depth*(-1)


# Counts: ======================================================================
sum(is.na(dt$species_count))      # 51
sum(dt$species_count==0, na.rm=T) # 187

dt <- dt[!is.na(dt$species_count),]
dt <- dt[!dt$species_count==0,]

table(dt$distance) 

unique(dt$observation_date_year[dt$distance==""]) # across many different years (29169)


# Taxonomy: ====================================================================
# NOTE: taxonomy has been curated taking into account the species IDs assigned to
# the taxa in the v1 dataset version.

sps1 <- read.csv("C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIME/BioTIMENewStudiesCuration/RecurateOct2024/sps1.csv") # sps
sps2 <- read.csv("C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIME/BioTIMENewStudiesCuration/RecurateOct2024/sps2.csv") # spsSpare
dt_108_bt <- read.csv(paste0(files_dir, "/recurate108.csv"), h=T) # previous file in BT
sort(unique(dt_108_bt$class)) # Aves & Mammalia


# Mammals: ---------------------------------------------------------------------
sum(is.na(dt$species_type)) # 0
length(unique(dt$taxon_id[dt$species_type %in% c("Seal", "Whale")]))         # 19
length(unique(dt$observation_code[dt$species_type %in% c("Seal", "Whale")])) # 1

dtmam <- dt[dt$species_type %in% c("Seal", "Whale"),]                        # subset for easier checks
sort(unique(dtmam$SpeciesII))
dtmam$SpeciesII <- gsub("\\.", "", dtmam$SpeciesII)

# Match with BT:

matchmam <- data.frame(matrix(ncol=4, nrow=19))
names(matchmam) <- c("ogName", "valid_name", "newID", "spsID")
matchmam$ogName <- sort(unique(dtmam$SpeciesII))
matchmam$valid_name <- sps2$valid_name[match(matchmam$ogName, sps2$origSpecies)] # all matched
matchmam$newID <- sps2$newID[match(matchmam$ogName, sps2$origSpecies)] 


# Bird: ------------------------------------------------------------------------
length(unique(dt$taxon_id[dt$species_type %in% c("Bird")]))         # 96
dtb <- dt[dt$species_type %in% c("Bird"),]                          # subset for easier checks
sort(unique(dtb$SpeciesII))                                         # 93

dtb$SpeciesII <- gsub("\\.", "", dtb$SpeciesII)
dtb$SpeciesII <- gsub("spp", "sp", dtb$SpeciesII)

length(sort(unique(dtb$SpeciesII)))

# Match with BT:

matchb <- data.frame(matrix(ncol=4, nrow=93))
names(matchb) <- c("ogName", "valid_name", "newID", "spsID")
matchb$ogName <- sort(unique(dtb$SpeciesII))
matchb$valid_name <- sps2$valid_name[match(matchb$ogName, sps2$origSpecies)] # all matched
matchb$newID <- sps2$newID[match(matchb$ogName, sps2$origSpecies)] # all matched



matchb$ogName[is.na(matchb$valid_name)]
ids <- c(sort(unique(matchmam$valid_name)), sort(unique(matchb$valid_name)))
setdiff(sort(unique(dt_108_bt$valid_name)), ids)


res <- classification(matchb$ogName[is.na(matchb$valid_name)], db="gbif", rows=1)

# "Puffinus sp indet" to "Puffinus sp9" [checked]
# "Pterodroma cookilaria sp" is "Pterodroma sp" [checked]
# "Pterodroma white-headed sp" # Pterodroma lessonii [checked]
# "Pterodroma sp indet" is Pterodroma sp9 [checked]


matchb$valid_name[matchb$ogName=="Puffinus sp indet"] <- "Puffinus sp9"
matchb$valid_name[matchb$ogName=="Pterodroma cookilaria sp"] <- "Pterodroma sp"
matchb$valid_name[matchb$ogName=="Pterodroma white-headed sp"] <- "Pterodroma lessonii"
matchb$valid_name[matchb$ogName=="Pterodroma sp indet"] <- "Pterodroma sp9"
matchb$newID <- sps2$newID[match(matchb$valid_name, sps2$valid_name)] 


res <- classification(matchb$ogName[is.na(matchb$valid_name)], db="gbif", rows=1) # ALL ACCEPTED

#dtb[dtb$SpeciesII=="Diomedea royal albatross sp" & dtb$observation_date_year==2002 & dtb$observation_date_month==1 & dtb$observation_date_day==17,]
#View(dt_108_bt[dt_108_bt$YEAR==2002 & dt_108_bt$MONTH==1 & dt_108_bt$DAY==1,]) # not there...


matching <- as.data.frame(rbind(matchmam, matchb))
matching$newID[matching$newID==42842] <- NA
matching$spsID <- sps2$ID_SPECIES[match(matching$newID, sps2$newID)]


matching$spsID[matching$ogName=="Diomedea royal albatross sp"] <- "Diomedea sp" 
matching$spsID[matching$ogName=="Eudyptes chrysolophus"] <- "Eudyptes chrysolophus"
matching$spsID[matching$ogName=="Eudyptes pachyrhynchus"] <- "Eudyptes pachyrhynchus"
matching$spsID[matching$ogName=="Eudyptes robustus"] <- "Eudyptes robustus"
matching$spsID[matching$ogName=="Eudyptes sclateri"] <- "Eudyptes sclateri"
matching$spsID[matching$ogName=="Eudyptula minor"] <- "Eudyptula minor"         # all these are new


dt <- as.data.frame(rbind(dtmam, dtb))
dt$newID <- rep(NA, nrow(dt))
dt$ID_SPECIES <- rep(NA, nrow(dt))

dt$newID <- matching$newID[match(dt$SpeciesII, matching$ogName)]
unique(dt$SpeciesII[is.na(dt$newID)])

dt$ID_SPECIES <- matching$spsID[match(dt$SpeciesII, matching$ogName)]
unique(dt$SpeciesII[is.na(dt$ID_SPECIES)]) # 0, OK


# OTHERS =======================================================================
sort(unique(dt$notes)) # :)
sort(unique(dt$data_notes)) 
length(unique(dt$voyage_id)) # 108


# SAMPLE_DESC: =================================================================
names(dt)
dim(dt)
length(dt$wov_data_id)
names(dt) <- gsub("observation_date_", "", names(dt))
names(dt) <- toupper(names(dt))
names(dt)[names(dt) == "SPECIES_COUNT"] <- "ABUNDANCE"
dt$BIOMASS <- rep(NA, nrow(dt))

range(dt$VOYAGE_ID)
sort(unique(dt$VOYAGE_ID))
dt$SAMPLE_DESC <- paste0(dt$YEAR, "_", dt$MONTH, "_", dt$DAY, "_", dt$VOYAGE_ID, "_",
                         dt$LATITUDE, "_", dt$LONGITUDE)
dt$PLOT <- rep(NA, nrow(dt))
dt$STUDY_ID <- rep(108, nrow(dt))

names(dt)[names(dt)=="NEWID"] <- "newID"


dt <- select(dt, ABUNDANCE, BIOMASS, ID_SPECIES, SAMPLE_DESC, PLOT, LATITUDE, LONGITUDE, DEPTH, DAY, MONTH, YEAR, STUDY_ID, newID)
# 0.02 94.80
rawData <- dt %>% group_by(BIOMASS, ID_SPECIES, SAMPLE_DESC, PLOT, LATITUDE, LONGITUDE, DEPTH, DAY, MONTH, YEAR, STUDY_ID, newID) %>%
  summarise(ABUNDANCE=sum(ABUNDANCE))
rawData <- rawData %>% relocate(ABUNDANCE, .before=BIOMASS)
rawData <- as.data.frame(rawData)
range(rawData$ABUNDANCE)
str(rawData)

# WRITE. CSV: ==================================================================
path <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIME/BioTIMENewStudiesCuration/RecurateOct2024/id108"
write.csv(rawData, file=paste0(path, "/108New.csv"), row.names = F)


# End of Script ################################################################



