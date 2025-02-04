################################################################################
# Curation Script: Freshwater fish surveys (NFPD) (RivFishTIME id 27)
# Curator: AFE
# Date: February 2024
################################################################################

# Data downloaded from ==========================================================
# https://environment.data.gov.uk/ecology/explorer/downloads/


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
library(sp)

rm(list=ls())

setwd("C:/Users/afe1/OneDrive - University of St Andrews/PHD/BioTIMEGithub")



# NOTE: ========================================================================
# Meeting with DEFRA Fish Monitoring Specialist March 2024:
# Use fished area (important for part width sampling) for computing densities.
# Standardise n of runs across the series. In Catch Depletion, take minimum n of runs.
# Should be OK to pool records monitored with different electro fishing techniques:
# but keep other method techniques separate. 



# Read data: ===================================================================
mypath <- getwd() 
rawdatapath <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIME/BioTIMENewStudiesCuration/U.K._Environmental_Agency_RivFishId27_AFE"

dt <- read.csv(paste0(rawdatapath, "/FW_Fish_Counts.csv"), h=T)                 # 341925
names(dt)
site <- read.csv(paste0(rawdatapath, "/FW_Fish_Sites.csv"), h=T)                



# Explore & retrieve subset meeting BT criteria ================================
unique(dt$DATA_OWNER)        # "Environment Agency"
unique(dt$IS_THIRD_PARTY)    # "No"
unique(dt$SURVEY_STATUS)     # "Completed"
unique(dt$ZERO_CATCH)        # "No"  "Yes"
sum(dt$ZERO_CATCH=="Yes")    # 2343 (keep for now)


unique(dt$COUNTRY)
unique(dt$LOCATION_NAME)
unique(dt$REGION)
unique(dt$AREA)


range(dt$EVENT_DATE_YEAR)      # 1973-2023
length(unique(dt$SITE_ID))     # 16492
sum(is.na(dt$SITE_ID))         # 0   
sum(dt$SITE_ID=="")            # 0
length(unique(dt$SITE_NAME))   # 16079
sum(is.na(dt$SITE_NAME))       # 0   
sum(dt$SITE_NAME=="")          # 0


c1 <- dt %>% group_by(SITE_ID) %>% summarise(nID=n_distinct(SITE_NAME))   # always 1
c2 <- dt %>% group_by(SITE_NAME) %>% summarise(nID=n_distinct(SITE_ID))   # not always 1 (continue with SITE_ID)


# Remove species-selective surveys ---------------------------------------------
unique(dt$IS_SPECIES_SELECTIVE)
#View(dt[dt$IS_SPECIES_SELECTIVE=="Yes",])
sum(dt$IS_SPECIES_SELECTIVE=="Yes")                                       # 13243
dt <- dt[!dt$IS_SPECIES_SELECTIVE == "Yes", ]                             # rm species selective surveys


# remove sites monitored only in 1 year ----------------------------------------
c3 <- dt %>% group_by(SITE_ID) %>% summarise(n1Year=n_distinct(EVENT_DATE_YEAR))
sum(c3$n1Year==1)                                                         # 6556 sites monitored only 1 year
monitoreonly1year <- unique(c3$SITE_ID[c3$n1Year==1])
dt <- dt[!dt$SITE_ID %in% monitoreonly1year, ]                            # rm sites monitored only 1 year

c4 <- dt %>% group_by(SITE_ID) %>% summarise(nSurv=n_distinct(SURVEY_ID)) # 2 - 579
c5 <- dt %>% group_by(SITE_ID, EVENT_DATE_YEAR) %>% summarise(nSurv=n_distinct(SURVEY_ID)) # 1 - 83


# Remove zero catch ------------------------------------------------------------
sum(is.na(dt$SPECIES_ID))          # 630
range(dt$SPECIES_ID, na.rm=T)      # 1 - 1198, all with ids
#View(dt[is.na(dt$SPECIES_ID),])
sum(is.na(dt$SPECIES_NAME))        # 0
sum(dt$SPECIES_NAME=="")           # 630
unique(dt$ZERO_CATCH[is.na(dt$SPECIES_ID)])                # these are zero catch surveys
dt <- dt[!(is.na(dt$SPECIES_ID) & dt$SPECIES_NAME==""),]   # rm records with no species identified

sum(dt$ZERO_CATCH=="Yes")          # 0



# Methods: =====================================================================
unique(dt$SURVEY_METHOD)
# NOTE: DC = Direct Current, PDC = Pulsed Direct Current, AC= Alternating Current

table(dt$SURVEY_METHOD)

unique(dt$SURVEY_STRATEGY[dt$SURVEY_METHOD=="NOT RECORDED"]) # "", "CATCH DEPLETION SAMPLE", "SINGLE CATCH SAMPLE"
sum(dt$SURVEY_METHOD=="NOT RECORDED")                        # 159
dt <- dt[!dt$SURVEY_METHOD=="NOT RECORDED",]                 # rm records with no reported catching method

sum(dt$SURVEY_METHOD=="POLLUTION")                           # 6
unique(dt$SURVEY_STRATEGY[dt$SURVEY_METHOD=="POLLUTION"])    # ""
dt <- dt[!dt$SURVEY_METHOD=="POLLUTION",]                    # rm few records with sampling method == pollution

dt <- dt[!dt$SURVEY_METHOD=="Beam Trawl Netting 1.5m",]      # rm, only 1 record, 1 survey

unique(dt$SURVEY_ID[dt$SURVEY_METHOD=="DIP NETTING"])
#View(dt[dt$SURVEY_ID %in% c("54350", "147409"),])           
dt <- dt[!dt$SURVEY_METHOD=="DIP NETTING",]                  # rm since only for a few surveys not repeated though time

unique(dt$SURVEY_ID[dt$SURVEY_METHOD=="DRAINING"])
#View(dt[dt$SURVEY_ID %in% c("23664", "55056"),])            
dt <- dt[!dt$SURVEY_METHOD=="DRAINING",]                     # rm since only for a few surveys not repeated though time

unique(dt$SURVEY_ID[dt$SURVEY_METHOD=="INTAKE SCREEN SAMPLING"])
#View(dt[dt$SURVEY_ID %in% c("112892", "112915"),])         
dt <- dt[!dt$SURVEY_METHOD=="INTAKE SCREEN SAMPLING",]       # rm, only two surveys in the same site and the same year

unique(dt$SURVEY_ID[dt$SURVEY_METHOD=="POISON"])
#View(dt[dt$SURVEY_ID %in% c("24024", "112940"),])         
dt <- dt[!dt$SURVEY_METHOD=="POISON",]                       # rm since only for a few surveys not repeated though time

      

kick <- unique(dt$SURVEY_ID[dt$SURVEY_METHOD=="KICK NETTING"])
#View(dt[dt$SURVEY_ID %in% kick,])                           # it seems some sites are monitored entirely with kick nets across years, keep for now

fyke <- unique(dt$SURVEY_ID[dt$SURVEY_METHOD=="FYKE NETTING"])
#View(dt[dt$SURVEY_ID %in% fyke,])                           # it seems some sites are monitored entirely with fyke nets across years, keep for now

gill <- unique(dt$SURVEY_ID[dt$SURVEY_METHOD=="GILL NETTING"])
#View(dt[dt$SURVEY_ID %in% gill,])                           # idem kick & fyke, there are gill net sites, keep for now

hook <- unique(dt$SURVEY_ID[dt$SURVEY_METHOD=="HOOK AND LINE FISHING"])
#View(dt[dt$SURVEY_ID %in% hook,])                           # idem kick & fyke, there are gill net sites, keep for now

portable <- unique(dt$SURVEY_ID[dt$SURVEY_METHOD=="PORTABLE TRAP FISHING"])
#View(dt[dt$SURVEY_ID %in% portable,])                       # idem kick & fyke, there are gill net sites, keep for now

trapping <- unique(dt$SURVEY_ID[dt$SURVEY_METHOD=="TRAPPING"])
#View(dt[dt$SURVEY_ID %in% trapping,])                       # idem kick & fyke, there are gill net sites, keep for now


dt$SURVEY_METHOD2 <- dt$SURVEY_METHOD
dt$SURVEY_METHOD2 <- ifelse(dt$SURVEY_METHOD %in% c("ELECTRIC FISHING", "DC ELECTRIC FISHING", "PDC ELECTRIC FISHING",
                                                    "AC ELECTRIC FISHING"), "ELECTRICFISHING", dt$SURVEY_METHOD2)
sort(unique(dt$SURVEY_METHOD2))


# Strategy: --------------------------------------------------------------------
unique(dt$SURVEY_STRATEGY)
sum(dt$SURVEY_STRATEGY=="NOT RECORDED")          # 171
dt <- dt[!dt$SURVEY_STRATEGY=="NOT RECORDED",]
#View(dt[dt$SURVEY_STRATEGY=="NOT RECORDED",])   # methods & runs specified


# Runs: ------------------------------------------------------------------------
range(dt$NO_OF_RUNS)                             # 1 to 6
c6 <- dt %>% group_by(SURVEY_ID) %>% summarise(nMethod=n_distinct(SURVEY_METHOD), nStrategy=n_distinct(SURVEY_STRATEGY),
                                               nRun=n_distinct(NO_OF_RUNS))     
# c6: always 1 for the three method specifications. I.e. no multi-method surveys


# Check consistent methods over time -------------------------------------------
length(unique(dt$SITE_ID)) # 9659 total n locations sampled at least 2 years


c7 <- dt %>% group_by(SITE_ID, SURVEY_METHOD2, SURVEY_STRATEGY) %>% summarise(nYear=n_distinct(EVENT_DATE_YEAR))
c7$ConcatRm <- paste0(c7$SITE_ID, "_", c7$SURVEY_METHOD2, "_", c7$SURVEY_STRATEGY)
dt$ConcatRm <- paste0(dt$SITE_ID, "_", dt$SURVEY_METHOD2, "_", dt$SURVEY_STRATEGY)
sum(c7$nYear==1)                          # 8177
length(unique(c7$ConcatRm[c7$nYear==1]))  
toRm <- unique(c7$ConcatRm[c7$nYear==1])
length(unique(dt$SITE_ID))                # total 9659 sites
dt <- dt[!dt$ConcatRm %in% toRm, ]        # rm surveys with a specific methodology used only 1 year
length(unique(dt$SITE_ID))                # new total 8989 sites 


# additional checks methods ----------------------------------------------------
c8 <- dt %>% group_by(SITE_ID, SURVEY_METHOD2, SURVEY_STRATEGY) %>% summarise(nYear=n_distinct(EVENT_DATE_YEAR)) # 2-29
c8B <- dt %>% group_by(SITE_ID, SURVEY_METHOD2, SURVEY_STRATEGY) %>% summarise(nM=n_distinct(SURVEY_METHOD))     
length(unique(c8B$SITE_ID[c8B$nM > 1]))                            # 4620
length(unique(c8B$SITE_ID[c8B$SURVEY_METHOD2=="ELECTRICFISHING"])) # 8221


# Standardise runs -------------------------------------------------------------
c9 <- dt %>% group_by(SITE_ID, SURVEY_METHOD2, SURVEY_STRATEGY) %>% summarise(nR=n_distinct(NO_OF_RUNS)) # 1-4
table(dt$NO_OF_RUNS)

unique(c9$SURVEY_METHOD2[c9$nR>1])   # Electro, seine netting, wrap seine & netting
unique(c9$SURVEY_STRATEGY[c9$nR>1])  # Catch Depletion & Catch Depletion Part-Width

range(dt$NO_OF_RUNS[dt$SURVEY_STRATEGY=="CATCH DEPLETION SAMPLE"])  # 2-6
range(dt$NO_OF_RUNS[!dt$SURVEY_STRATEGY=="CATCH DEPLETION SAMPLE"]) # 1-4
range(dt$NO_OF_RUNS[dt$SURVEY_STRATEGY=="SINGLE CATCH SAMPLE"])     # always 1


dt <- dt %>% group_by(SITE_ID, SURVEY_METHOD2, SURVEY_STRATEGY) %>% mutate(minS=min(NO_OF_RUNS)) %>% ungroup()
range(dt$minS) # 1-5
table(dt$minS)

#View(dt[dt$minS==5,]) # Fyke
#View(dt[dt$minS==4,]) # Electrofishing in 2 specific sites.


dt$ALL_RUNS_STANDARD <- rep(NA, nrow(dt))
dt$ALL_RUNS_STANDARD <- ifelse(dt$minS==1, dt$RUN1, dt$ALL_RUNS_STANDARD)
dt$ALL_RUNS_STANDARD <- ifelse(dt$minS==2, dt$RUN1+dt$RUN2, dt$ALL_RUNS_STANDARD)
dt$ALL_RUNS_STANDARD <- ifelse(dt$minS==3, dt$RUN1+dt$RUN2+dt$RUN3, dt$ALL_RUNS_STANDARD)
dt$ALL_RUNS_STANDARD <- ifelse(dt$minS==4, dt$RUN1+dt$RUN2+dt$RUN3+dt$RUN4, dt$ALL_RUNS_STANDARD)
dt$ALL_RUNS_STANDARD <- ifelse(dt$minS==5, dt$RUN1+dt$RUN2+dt$RUN3+dt$RUN4+dt$RUN5, dt$ALL_RUNS_STANDARD)
# checked a few visually, looks OK.

sum(dt$ALL_RUNS_STANDARD==0, na.rm=T) # 1108 (these are those lost by removing/evening n of runs)
sum(dt$ALL_RUNS==0, na.rm=T)          # 0

sum(is.na(dt$ALL_RUNS_STANDARD))      # 47428
sum(is.na(dt$ALL_RUNS))               # 47428



# Abundance ====================================================================
range(dt$ALL_RUNS_STANDARD, na.rm=T)    # 0 41822
unique(dt$OBSERVED_ABUNDANCE[dt$ALL_RUNS_STANDARD==0])     # "" / NA
unique(dt$OBSERVED_ABUNDANCE[is.na(dt$ALL_RUNS_STANDARD)]) # multiple categories

47428/281914*100                       # 17% obs

unique(dt$OBSERVED_ABUNDANCE)                               # multiple
unique(dt$ALL_RUNS_STANDARD[dt$OBSERVED_ABUNDANCE!=""])     # always NA

dt$OBSERVED_ABUNDANCE[dt$OBSERVED_ABUNDANCE==""] <- NA
dt$OBSERVED_ABUNDANCE[dt$OBSERVED_ABUNDANCE=="Present [Best Run]"] <- 1
dt$OBSERVED_ABUNDANCE[dt$OBSERVED_ABUNDANCE=="Present [Survey]"] <- 1
dt$OBSERVED_ABUNDANCE[dt$OBSERVED_ABUNDANCE=="1 to 9 [Best Run]"] <- 5
dt$OBSERVED_ABUNDANCE[dt$OBSERVED_ABUNDANCE=="1 to 9 [Survey]"] <- 5
dt$OBSERVED_ABUNDANCE[dt$OBSERVED_ABUNDANCE=="10 to 99 [Best Run]"] <- 55
dt$OBSERVED_ABUNDANCE[dt$OBSERVED_ABUNDANCE=="10 to 99 [Survey]" ] <- 55
dt$OBSERVED_ABUNDANCE[dt$OBSERVED_ABUNDANCE=="100 to 999 [Best Run]"] <- 550
dt$OBSERVED_ABUNDANCE[dt$OBSERVED_ABUNDANCE=="100 to 999 [Survey]"] <- 550
dt$OBSERVED_ABUNDANCE[dt$OBSERVED_ABUNDANCE=="1000 to 9999 [Best Run]"] <- 5500
dt$OBSERVED_ABUNDANCE[dt$OBSERVED_ABUNDANCE=="1000 to 9999 [Survey]"] <- 5500
dt$OBSERVED_ABUNDANCE[dt$OBSERVED_ABUNDANCE=="10000+ [Best Run]"] <- 10000
dt$OBSERVED_ABUNDANCE[dt$OBSERVED_ABUNDANCE=="10000+ [Survey]"] <- 10000

dt$OBSERVED_ABUNDANCE <- as.integer(dt$OBSERVED_ABUNDANCE)
unique(dt$OBSERVED_ABUNDANCE)


dt$TOTAL_ABUNDANCE <- ifelse(is.na(dt$ALL_RUNS_STANDARD), dt$OBSERVED_ABUNDANCE, dt$ALL_RUNS_STANDARD)
range(dt$TOTAL_ABUNDANCE)       # 0 - 41822
dt <- dt[dt$TOTAL_ABUNDANCE>0,] # (rested 1108)



# Taxonomy: ====================================================================
length(unique(dt$SPECIES_NAME)) # 112
length(unique(dt$LATIN_NAME))   # 80
sum(is.na(dt$LATIN_NAME))       # 0

unique(dt$SPECIES_NAME)         # common names
unique(dt$LATIN_NAME)
table(dt$LATIN_NAME)

# ""
# "Rutilus rutilus x Abramis brama"                   
# "Alburnus alburnus x Rutilus rutilus"               
# "Rutilus rutilus x Scardinius erythrophthalmus"     
# "Rutilus rutilus x Leuciscus cephalus"              
# "Rutilus rutilus x Abramis bjoerkna"                
# "Leuciscus leuciscus x Alburnus alburnus"           
# "Abramis brama x Abramis bjoerkna"                  
# "Scardinius erythrophthalmus x Abramis brama"
# "Carassius auratus x Cyprinus carpio"
# "Leuciscus cephalus x Leuciscus leuciscus"
# "Rutilus rutilus x Leuciscus leuciscus"
# "Cyprinus carpio x Carassius carassius"
# "Leuciscus cephalus x Alburnus alburnus"
# "Carassius auratus x Carassius carassius"
# "Leuciscus leuciscus x Alburnus alburnus"
# "Abramis brama x Abramis bjoerkna"
# NOTE: Hybrids of Cyprinidae (sp1, sp2, sp3, ... spn)

# "Salmo trutta x Salvelinus fontinalis"
# "Salmo trutta x Salmo salar"
# "Salmo trutta x Oncorhynchus mykiss"
# NOTE: Hybrids of Salmonidae (sp1, sp2, sp3, ... spn)


sum(dt$LATIN_NAME=="")                           # 213
length(unique(dt$SURVEY_ID[dt$LATIN_NAME==""]))  # 213
#View(dt[dt$LATIN_NAME=="",])                   

dt <- dt[!dt$LATIN_NAME=="",]                    # rm, these are surveys for which fish taxa aren't IDd


cyprinid_hybrids <- c("Rutilus rutilus x Abramis brama", "Alburnus alburnus x Rutilus rutilus", 
                      "Rutilus rutilus x Scardinius erythrophthalmus", "Rutilus rutilus x Leuciscus cephalus",
                      "Rutilus rutilus x Abramis bjoerkna", "Abramis brama x Abramis bjoerkna",
                      "Scardinius erythrophthalmus x Abramis brama", "Carassius auratus x Cyprinus carpio",
                      "Leuciscus cephalus x Leuciscus leuciscus", "Rutilus rutilus x Leuciscus leuciscus",
                      "Cyprinus carpio x Carassius carassius", "Leuciscus cephalus x Alburnus alburnus",
                      "Carassius auratus x Carassius carassius",
                      "Leuciscus leuciscus x Alburnus alburnus")

salmonid_hybrids <- c("Salmo trutta x Salvelinus fontinalis", "Salmo trutta x Salmo salar",
                      "Salmo trutta x Oncorhynchus mykiss")



value_mappingCyp <- data.frame("Original"= unique(cyprinid_hybrids),
                            "New"= setNames(paste0("Cyprinidae sp", seq_along(cyprinid_hybrids)), cyprinid_hybrids))
value_mappingSal <- data.frame("Original"= unique(salmonid_hybrids),
                               "New"= setNames(paste0("Salmonidae sp", seq_along(salmonid_hybrids)), salmonid_hybrids))
value_mapping <- as.data.frame(rbind(value_mappingCyp, value_mappingSal))

dt$LATIN_NAME2 <- dt$LATIN_NAME
dt$LATIN_NAME2 <- ifelse(dt$LATIN_NAME %in% value_mapping$Original, value_mapping$New[match(dt$LATIN_NAME, value_mapping$Original)], dt$LATIN_NAME2)

sort(unique(dt$LATIN_NAME2))

dt$LATIN_NAME2[dt$LATIN_NAME2=="Gobius"] <- "Gobius sp"
dt$LATIN_NAME2[dt$LATIN_NAME2=="Lampetra"] <- "Lampetra sp"

dt$Family <- rep(NA, nrow(dt))
families <- unique(dt$LATIN_NAME2[grep("idae", dt$LATIN_NAME2)])
dt$Family <- ifelse(dt$LATIN_NAME2 %in% families, str_split_fixed(dt$LATIN_NAME2, " ", 2)[,1], dt$Family)
unique(dt$Family)
dt$Genus <- str_split_fixed(dt$LATIN_NAME2, " ", 2)[,1]
dt$Genus <- ifelse(!is.na(dt$Family), NA, dt$Genus)
unique(dt$Genus)
sort(unique(dt$Genus))
dt$Species <- str_split_fixed(dt$LATIN_NAME2, " ", 2)[,2]
dt$Species[dt$Species==""] <-NA
unique(dt$Species)
sort(unique(dt$Species))

sum(is.na(dt$Family) & is.na(dt$Genus) & is.na(dt$Species)) # 0


# Checks taxonomy: -------------------------------------------------------------
unique(dt$LATIN_NAME[dt$LATIN_NAME2=="Cyprinidae sp1"])
unique(dt$LATIN_NAME[dt$LATIN_NAME2=="Cyprinidae sp5"])
unique(dt$LATIN_NAME[dt$LATIN_NAME2=="Cyprinidae sp14"])
unique(dt$LATIN_NAME[dt$LATIN_NAME2=="Salmonidae sp2"])



# Coordinates ==================================================================
# NGR: British National Grid (BNG), EPSG:27700
# easting = long, northing = lat
range(dt$SURVEY_RANKED_EASTING)
range(dt$SURVEY_RANKED_NORTHING)

c9 <- dt %>% group_by(SURVEY_RANKED_EASTING, SURVEY_RANKED_NORTHING) %>% summarise(NSite=n_distinct(SITE_ID))                    # some sites have the same coordinates but it's infrequent
c9B<- dt %>% group_by(SITE_ID) %>% summarise(NLat=n_distinct(SURVEY_RANKED_NORTHING), NLong=n_distinct(SURVEY_RANKED_EASTING))   # multiple vals

site9 <- site %>% group_by(SITE_RANKED_EASTING, SITE_RANKED_NORTHING) %>% summarise(NSite=n_distinct(SITE_ID))                   # some sites have the same coordinates but it's infrequent
site9B<- site %>% group_by(SITE_ID) %>% summarise(NLat=n_distinct(SITE_RANKED_NORTHING), NLong=n_distinct(SITE_RANKED_EASTING))  # same throughout the series, only 1 pair per site

dt$SURVEY_RANKED_EASTING <- site$SITE_RANKED_EASTING[match(dt$SITE_ID, site$SITE_ID)]
dt$SURVEY_RANKED_NORTHING <- site$SITE_RANKED_NORTHING[match(dt$SITE_ID, site$SITE_ID)]
# substituted since some typos detected in dt values (e.g., sites falling at sea)

d <- dt[, names(dt) %in% c("SITE_ID", "SURVEY_RANKED_EASTING", "SURVEY_RANKED_NORTHING")]

coordinates(d) <- c("SURVEY_RANKED_EASTING", "SURVEY_RANKED_NORTHING")
proj4string(d) <- CRS("+init=epsg:27700") 

# Warning message:
# In CPL_crs_from_input(x) :
#  GDAL Message 1: +init=epsg:XXXX syntax is deprecated. It might return a CRS with a non-EPSG compliant axis order.

CRS.new <- CRS("+init=epsg:4326") # WGS 84
df <-spTransform(d, CRS.new)
df <- data.frame(df)
range(df$coords.x1) 
range(df$coords.x2) 

dt$Longitude <- df$coords.x1[match(dt$SITE_ID, df$SITE_ID)]
dt$Latitude <- df$coords.x2[match(dt$SITE_ID, df$SITE_ID)]

range(dt$Longitude)
range(dt$Latitude)

# Correction coordinates site id 15492:
unique(dt$SURVEY_RANKED_EASTING[dt$SITE_ID %in% c(15492)])
unique(dt$SURVEY_RANKED_NORTHING[dt$SITE_ID %in% c(15492)]) # idem in site
unique(dt$SITE_NAME[dt$SITE_ID == 15492])                   # "Tipton St John (Dip)*"
dt$Latitude[dt$SITE_ID == 15492] <- 50.72224566977441
dt$Longitude[dt$SITE_ID == 15492] <- -3.290619262138365     # coords Tipton St John Google Maps


# Correction coordinates site id 22009:
unique(dt$SURVEY_RANKED_EASTING[dt$SITE_ID %in% c(22009)])
unique(dt$SURVEY_RANKED_NORTHING[dt$SITE_ID %in% c(22009)]) # idem in site
unique(dt$SITE_NAME[dt$SITE_ID == 22009])                   # Maidenwood
dt$Latitude[dt$SITE_ID == 22009] <- 50.848447
dt$Longitude[dt$SITE_ID == 22009] <- -4.511990              # coords Maidenwood Google Maps


# map --------------------------------------------------------------------------
dt_merged <- dt
dt_coord <- dt_merged %>% select(Longitude, Latitude) %>% distinct() %>%
  st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()
centroid <- dt_coord %>% st_convex_hull() %>% st_centroid() %>% unlist # get centroid
area <- st_transform(dt_coord, st_crs("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs")) %>%
  st_convex_hull() %>% st_area()  ##get area in sq km
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
  coord_sf(ylim=c(48,60), xlim=c(-7,2)) + 
  labs(x = NULL, y = NULL) +
  theme_minimal()



# Sampling Event Date  =========================================================
dt$Day <- as.integer(str_split_fixed(dt$EVENT_DATE, "/", 3)[,1])
sort(unique(dt$Day))

dt$Month <- as.integer(str_split_fixed(dt$EVENT_DATE, "/", 3)[,2])
sort(unique(dt$Month))

dt$Year <- as.integer(str_split_fixed(dt$EVENT_DATE, "/", 3)[,3])
sort(unique(dt$Year))

identical(as.integer(dt$EVENT_DATE_YEAR), dt$Year)                    # TRUE

c10 <- dt %>% group_by(SURVEY_ID) %>% summarise(nDay=n_distinct(Day)) # a survey always takes place over one unique day



# Sample Area & Fished Area ====================================================
range(dt$SURVEY_AREA, na.rm=T)           # in m2
range(dt$FISHED_AREA, na.rm=T)           # in m2
TRUE %in% grepl("\\.", as.character(dt$SURVEY_AREA))


# Small areas: -----------------------------------------------------------------
sum(is.na(dt$SURVEY_AREA))      # 266
sum(is.na(dt$FISHED_AREA))      # 287

sum(dt$SURVEY_AREA==dt$FISHED_AREA, na.rm=T) # 275327
sum(dt$SURVEY_AREA!=dt$FISHED_AREA, na.rm=T) # 6087
unique(dt$SURVEY_STRATEGY[(dt$SURVEY_AREA==dt$FISHED_AREA)])
unique(dt$SURVEY_STRATEGY[(dt$SURVEY_AREA!=dt$FISHED_AREA)])

#View(dt[is.na(dt$FISHED_AREA),])
dt$AREAm2 <- dt$FISHED_AREA
dt$AREAm2 <- ifelse(is.na(dt$FISHED_AREA) & dt$SURVEY_AREA >0, dt$SURVEY_AREA, dt$AREAm2)
sum(dt$SURVEY_AREA!=dt$AREAm2, na.rm=T)      # 6087
sum(is.na(dt$AREAm2))                        # 266


sum(dt$AREAm2==0, na.rm=T) # 4824
sum(dt$AREAm2==1, na.rm=T) # 1408
sum(dt$AREAm2 < 10 & dt$AREAm2 > 1, na.rm=T) # 8
sum(dt$AREAm2 < 20 & dt$AREAm2 > 9, na.rm=T) # 122


unique(dt$SURVEY_METHOD[is.na(dt$AREAm2)])         
unique(dt$SURVEY_METHOD[dt$AREAm2==0])   
unique(dt$SURVEY_METHOD[dt$AREAm2==1])   


unique(dt$SURVEY_STRATEGY[is.na(dt$SURVEY_AREA)])  
unique(dt$SURVEY_STRATEGY[dt$SURVEY_AREA==0])  
unique(dt$SURVEY_STRATEGY[dt$SURVEY_AREA==1])     


dt <- dt[!is.na(dt$AREAm2),]  
dt <- dt[!(dt$AREAm2==0 & dt$SURVEY_METHOD2 == "ELECTRICFISHING"),]
dt <- dt[!(dt$AREAm2==1 & dt$SURVEY_METHOD2 == "ELECTRICFISHING"),]


range(dt$AREAm2[dt$SURVEY_METHOD2=="FIXED TRAP FISHING"]) # 0-1, counts idem density, keep
range(dt$AREAm2[dt$SURVEY_METHOD2=="TRAPPING"])           # 1 1, counts idem density, keep
range(dt$AREAm2[dt$SURVEY_METHOD2=="SEINE NETTING"])      # 0 - >10000
sum(dt$AREAm2==0 & dt$SURVEY_METHOD2=="SEINE NETTING")    # 631, rm
sum(dt$AREAm2==1 & dt$SURVEY_METHOD2=="SEINE NETTING")    # 9, rm
dt <- dt[!(dt$AREAm2==0 & dt$SURVEY_METHOD2 == "SEINE NETTING"),]
dt <- dt[!(dt$AREAm2==1 & dt$SURVEY_METHOD2 == "SEINE NETTING"),]

# NOTE: when methods are "PDC ELECTRIC FISHING", "ELECTRIC FISHING", "SEINE NETTING"
# rm surveys with no info on area (NA or 0) because density cannot be computed
# rm surveys with area equal to 1 (this is likely a typo)
# keep records for trapping and fixed trapping

unique(dt$SURVEY_METHOD2[dt$AREAm2==0])   
dt$AREAm2[dt$AREAm2==0] <- 1
unique(dt$SURVEY_METHOD[dt$AREAm2==1]) 


# Large areas: -----------------------------------------------------------------
range(dt$AREAm2)
class(dt$AREAm2)
c11 <- dt %>% group_by(SITE_ID, SURVEY_METHOD2, SURVEY_STRATEGY) %>% 
  summarise(nMin=min(AREAm2), nMax=max(AREAm2),
            nDiff=max(AREAm2) - min(AREAm2)) 
c12 <- dt %>% group_by(SURVEY_ID) %>% 
  distinct(AREAm2)
#View(dt[dt$SURVEY_AREA==9997992,])  # OK
#View(dt[dt$SURVEY_AREA==384300,])   # OK
#View(dt[dt$SURVEY_AREA >384300,])   # mostly 999 etc in length, width, etc (rm)
unique(dt$SURVEY_STRATEGY[dt$AREAm2 > 384300]) # "CATCH PUE/T SAMPLE"
dt <- dt[!dt$AREAm2 > 384300, ]      # NOTE: rm, since these values are unlikely to be true areas



# Remove sites after processing methods ========================================
c13 <- dt %>% group_by(SITE_ID, SURVEY_METHOD2, SURVEY_STRATEGY) %>% summarise(nYear=n_distinct(EVENT_DATE_YEAR))
c13$ConcatRm <- paste0(c13$SITE_ID, "_", c13$SURVEY_METHOD2, "_", c13$SURVEY_STRATEGY)

sum(c13$nYear==1)                          # 75
length(unique(c13$ConcatRm[c13$nYear==1])) # 75
toRm2 <- unique(c13$ConcatRm[c13$nYear==1])

dt <- dt[!dt$ConcatRm %in% toRm2, ]        # rm surveys with a specific methodology used only 1 year, keep 254379

c14 <- dt %>% group_by(SITE_ID, SURVEY_METHOD2, SURVEY_STRATEGY) %>% summarise(nR=n_distinct(minS)) # always 1



# Additional Step before upload in BioTIME =====================================
# All data records with survey strategy CPUE cannot be included in BioTIME
# since time of sampling is necessary to standardise these records, but 
# this info is not available in the DEFRA Database (check availability in the
# future if the dataset is updated with new records)
unique(dt$SURVEY_STRATEGY)
dt <- dt[!dt$SURVEY_STRATEGY=="CATCH PUE/T SAMPLE",] # 260918


# Final checks =================================================================
unique(dt$SURVEY_METHOD2)
c15 <- dt %>% group_by(SITE_ID, SURVEY_ID) %>% summarise(nArea=n_distinct(AREAm2)) # always 1
c16 <- dt %>% group_by(SITE_ID, SURVEY_METHOD2, SURVEY_STRATEGY) %>% 
  summarise(nMin=min(AREAm2), nMax=max(AREAm2),
            nDiff=max(AREAm2) - min(AREAm2))                                       # 278100m2, 0.28km2

c17 <- dt %>% group_by(SITE_ID, SURVEY_METHOD2, SURVEY_STRATEGY, 
                       Day, Month, Year) %>% summarise(n=n_distinct(SURVEY_ID))
range(c17$n)                                                                       # always 1 (so SampleDesc can be Site_Day_Month_Year)



# Rawdata ======================================================================
rawData <- dt %>% group_by(Family, Genus, Species, Latitude, Longitude, Day, Month, Year,
                           SITE_ID, SURVEY_ID, SURVEY_METHOD2, SURVEY_STRATEGY, AREAm2) %>%
  summarise(Abundance=sum(TOTAL_ABUNDANCE)) # some observations pooled (259333)
range(rawData$AREAm2)
rawData$Abundance <- (rawData$Abundance/rawData$AREAm2)*100
range(rawData$Abundance)

names(rawData)
rawData$Biomass <- rep(NA, nrow(rawData))
rawData <- rawData %>% relocate(c(Abundance, Biomass), .before = Family)

rawData$Plot <- rep(NA, nrow(rawData))
rawData$SampleDescription <- paste0(rawData$SITE_ID, "_", rawData$Day, "_",
                                    rawData$Month, "_", rawData$Year)
rawData <- rawData %>% relocate(c(SampleDescription, Plot), .before = Latitude)
rawData$DepthElevation <- rep(NA, nrow(rawData))
rawData <- rawData %>% relocate(c(DepthElevation), .before = Day)
rawData$StudyID <- rep(NA, nrow(rawData))
rawData <- rawData %>% relocate(c(StudyID), .after = Year)
names(rawData)
rawData <-as.data.frame(rawData)
str(rawData)



# Split ========================================================================
unique(paste0(rawData$SURVEY_METHOD2, "_", rawData$SURVEY_STRATEGY))
unique(rawData$SURVEY_METHOD2)
unique(rawData$SURVEY_STRATEGY)
rawData$SURVEY_STRATEGY <- str_replace_all(rawData$SURVEY_STRATEGY, "[[:punct:]]", " ")
rawData$SURVEY_STRATEGY <- str_replace_all(rawData$SURVEY_STRATEGY, "  ", " ")
rawData$SURVEY_METHOD2 <- str_replace_all(rawData$SURVEY_METHOD2, " ", "_")
rawData$SURVEY_STRATEGY <- str_replace_all(rawData$SURVEY_STRATEGY, " ", "_")

rawData$ConcatMethods <- paste0(rawData$SURVEY_METHOD2, "_", rawData$SURVEY_STRATEGY)
lr <- split(rawData, f=rawData$ConcatMethods)


# Grain size:-------------------------------------------------------------------
meanArealr <- lapply(lr, function(x) {mean(x$AREAm2)})

lr <- lapply(lr, function(x){within(x, rm(SITE_ID, SURVEY_ID, SURVEY_METHOD2,
                                          SURVEY_STRATEGY, AREAm2, ConcatMethods))})


# Save =========================================================================
path <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIMEGithubMetadatas/U.K._Environmental_Agency_RivFishId27_AFE/RawData"
for(i in names(lr)){
  write.csv(lr[[i]], paste0(path, "/", i,".csv"), row.names=F)
}


# convex hulls =================================================================
# 1. Convert data points into point spatial object
dt_merged <- rawData
dt_coord <- dt_merged %>% select(Longitude, Latitude) %>% distinct() %>%
  st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()

coordslr <- lapply(lr, function(x){dt_coord <- x %>% select(Longitude, Latitude) %>% distinct() %>%
  st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()})


# 2. Calculate convex hull, area and centroid
centroid <- dt_coord %>% st_convex_hull() %>% st_centroid() %>% unlist # get centroid
centroid
centroidlr <- lapply(coordslr, function(x){x %>% st_convex_hull() %>% st_centroid() %>% unlist}) 


# transform to mercator to get area in sq km
area <- st_transform(dt_coord, st_crs("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs")) %>%
  st_convex_hull() %>% st_area()  ##get area in sq km
area 
arealr <- lapply(coordslr, function(x){st_transform(x, st_crs("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs")) %>%
    st_convex_hull() %>% st_area()}) 


# End of script ################################################################
################################################################################


