################################################################################
# Curation Script: Freshwater fish surveys (NFPD) (RivFishTIME id 27)
# Curator: AFE
# Date: February 2024
################################################################################

# Data dowloaded from ==========================================================
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

# 1) more conservative: idem gear, idem strategy, idem n runs
dt$ConcatMethods1 <- paste0(dt$SURVEY_METHOD, "_", dt$SURVEY_STRATEGY, "_", dt$NO_OF_RUNS)
c7_1 <- dt %>% group_by(SITE_ID) %>% summarise(n=n_distinct(ConcatMethods1))
plot(table(c7_1$n)) # most sites have 1 or 2 consistent methodologies

c7_1B <- dt %>% group_by(SITE_ID, ConcatMethods1) %>% summarise(nYear=n_distinct(EVENT_DATE_YEAR))
plot(table(c7_1B$nYear))
length(unique(c7_1B$SITE_ID[c7_1B$nYear==1]))      # 5865
length(unique(c7_1B$SITE_ID[c7_1B$nYear>1]))       # 8093


# 2) idem gear, idem strategy
dt$ConcatMethods2 <- paste0(dt$SURVEY_METHOD, "_", dt$SURVEY_STRATEGY)
c7_2 <- dt %>% group_by(SITE_ID) %>% summarise(n=n_distinct(ConcatMethods2))
plot(table(c7_2$n))
c7_2B <- dt %>% group_by(SITE_ID, ConcatMethods2) %>% summarise(nYear=n_distinct(EVENT_DATE_YEAR))
plot(table(c7_2B$nYear))
length(unique(c7_2B$SITE_ID[c7_2B$nYear==1]))      # 5264
length(unique(c7_2B$SITE_ID[c7_2B$nYear>1]))       # 8226


# 3) only idem strategy
c7_3 <- dt %>% group_by(SITE_ID) %>% summarise(n=n_distinct(SURVEY_STRATEGY))
plot(table(c7_3$n))
c7_3B <- dt %>% group_by(SITE_ID, SURVEY_STRATEGY) %>% summarise(nYear=n_distinct(EVENT_DATE_YEAR))
plot(table(c7_3B$nYear))
length(unique(c7_3B$SITE_ID[c7_3B$nYear==1]))      # 2346
length(unique(c7_3B$SITE_ID[c7_3B$nYear>1]))       # 9042


# 4) only idem gear
c7_4 <- dt %>% group_by(SITE_ID) %>% summarise(n=n_distinct(SURVEY_METHOD))
plot(table(c7_4$n))
c7_4B <- dt %>% group_by(SITE_ID, SURVEY_METHOD) %>% summarise(nYear=n_distinct(EVENT_DATE_YEAR))
plot(table(c7_4B$nYear))
length(unique(c7_4B$SITE_ID[c7_4B$nYear==1]))      # 3724
length(unique(c7_4B$SITE_ID[c7_4B$nYear>1]))       # 8676


# NOTE: whether fish catchability is comparable between gears & strategies
# is uncertain. Thus I keep the more conservative filtering (idem gear, idem 
# strategy). Densities are standardised x 1 run below.


c7 <- dt %>% group_by(SITE_ID, SURVEY_METHOD, SURVEY_STRATEGY) %>% summarise(nYear=n_distinct(EVENT_DATE_YEAR))
c7$ConcatRm <- paste0(c7$SITE_ID, "_", c7$SURVEY_METHOD, "_", c7$SURVEY_STRATEGY)
dt$ConcatRm <- paste0(dt$SITE_ID, "_", dt$SURVEY_METHOD, "_", dt$SURVEY_STRATEGY)
sum(c7$nYear==1)                          # 8177
length(unique(c7$ConcatRm[c7$nYear==1]))  
toRm <- unique(c7$ConcatRm[c7$nYear==1])
length(unique(dt$SITE_ID))                # total 9659 sites
dt <- dt[!dt$ConcatRm %in% toRm, ]        # rm surveys with a specific methodology used only 1 year
length(unique(dt$SITE_ID))                # new total 8226 sites 


# final checks methods ---------------------------------------------------------
c8 <- dt %>% group_by(SITE_ID, SURVEY_METHOD, SURVEY_STRATEGY) %>% summarise(nYear=n_distinct(EVENT_DATE_YEAR)) # 2-28


# Abundance ====================================================================
range(dt$ALL_RUNS, na.rm=T)    # 1 41822
sum(is.na(dt$ALL_RUNS))        # 42697
#View(dt[is.na(dt$ALL_RUNS),]) # observed, not sampled (range of possible abundances)

length(unique(dt$SURVEY_ID[is.na(dt$ALL_RUNS)]))   
length(unique(dt$SURVEY_ID[!is.na(dt$ALL_RUNS)]))  
17240/(17240+45444)*100                            # 27.5%              

unique(dt$OBSERVED_ABUNDANCE)
unique(dt$ALL_RUNS[dt$OBSERVED_ABUNDANCE!=""])     # NA

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

dt$TOTAL_ABUNDANCE <- ifelse(is.na(dt$ALL_RUNS), dt$OBSERVED_ABUNDANCE, dt$ALL_RUNS)
range(dt$TOTAL_ABUNDANCE)       # 1 - 41822


# Taxonomy: ====================================================================
length(unique(dt$SPECIES_NAME)) # 111
length(unique(dt$LATIN_NAME))   # 79
sum(is.na(dt$LATIN_NAME))       # 0

unique(dt$SPECIES_NAME)         # common names
unique(dt$LATIN_NAME)

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


sum(dt$LATIN_NAME=="")                           # 207
length(unique(dt$SURVEY_ID[dt$LATIN_NAME==""]))  # 207
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

c9 <- dt %>% group_by(SURVEY_RANKED_EASTING, SURVEY_RANKED_NORTHING) %>% summarise(NSite=n_distinct(SITE_ID))                   # some sites have the same coordinates but it's infrequent
c9B<- dt %>% group_by(SITE_ID) %>% summarise(NLat=n_distinct(SURVEY_RANKED_NORTHING), NLong=n_distinct(SURVEY_RANKED_EASTING))  # multiple vals

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
unique(dt$SITE_NAME[dt$SITE_ID == 15492])
dt$Latitude[dt$SITE_ID == 15492] <- 50.72224566977441
dt$Longitude[dt$SITE_ID == 15492] <- -3.290619262138365     # coords Tipton St John Google Maps


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

c11 <- dt %>% group_by(SURVEY_ID) %>% summarise(nDay=n_distinct(Day)) # a survey always takes place over one unique day


# Sample Area & Fished Area ====================================================
range(dt$SURVEY_AREA)           # in m2
TRUE %in% grepl("\\.", as.character(dt$SURVEY_AREA))

# Small areas: -----------------------------------------------------------------
sum(is.na(dt$SURVEY_AREA))      # 266
sum(dt$SURVEY_AREA==0, na.rm=T) # 4478
sum(dt$SURVEY_AREA==1, na.rm=T) # 1392
sum(dt$SURVEY_AREA < 10 & dt$SURVEY_AREA > 1, na.rm=T) # 8
sum(dt$SURVEY_AREA < 20 & dt$SURVEY_AREA > 9, na.rm=T) # 108


unique(dt$SURVEY_METHOD[is.na(dt$SURVEY_AREA)])         
unique(dt$SURVEY_METHOD[dt$SURVEY_AREA==0])   
unique(dt$SURVEY_METHOD[dt$SURVEY_AREA==1])   


unique(dt$SURVEY_STRATEGY[is.na(dt$SURVEY_AREA)])  
unique(dt$SURVEY_STRATEGY[dt$SURVEY_AREA==0])  
unique(dt$SURVEY_STRATEGY[dt$SURVEY_AREA==1])     


# View(dt[is.na(dt$SURVEY_AREA),])                                                            
range(dt$FISHED_AREA[dt$SURVEY_AREA==0 & dt$SURVEY_METHOD=="PDC ELECTRIC FISHING"], na.rm=T)  # 0                    
range(dt$FISHED_WIDTH[dt$SURVEY_AREA==0 & dt$SURVEY_METHOD=="PDC ELECTRIC FISHING"], na.rm=T) # 0 to approx. 60
range(dt$FISHED_AREA[dt$SURVEY_AREA==0 & dt$SURVEY_METHOD=="ELECTRIC FISHING"], na.rm=T)      # 0                    
range(dt$FISHED_WIDTH[dt$SURVEY_AREA==0 & dt$SURVEY_METHOD=="ELECTRIC FISHING"], na.rm=T)     # 0 to 24
#View(dt[dt$FISHED_AREA==1 & dt$SURVEY_METHOD %in% c("ELECTRIC FISHING", "PDC ELECTRIC FISHING"),])
sum(dt$SURVEY_AREA==0 & dt$SURVEY_METHOD %in% c("PDC ELECTRIC FISHING", "ELECTRIC FISHING"), na.rm=T) # 3683
sum(dt$SURVEY_AREA==1 & dt$SURVEY_METHOD %in% c("PDC ELECTRIC FISHING", "ELECTRIC FISHING"), na.rm=T) # 174

range(dt$SURVEY_AREA[dt$SURVEY_ID %in% unique(dt$SURVEY_ID[dt$SURVEY_AREA==0 & dt$SURVEY_METHOD=="PDC ELECTRIC FISHING"])])
range(dt$SURVEY_AREA[dt$SURVEY_ID %in% unique(dt$SURVEY_ID[dt$SURVEY_AREA==0 & dt$SURVEY_METHOD=="ELECTRIC FISHING"])])
range(dt$SURVEY_AREA[dt$SURVEY_ID %in% unique(dt$SURVEY_ID[dt$SURVEY_AREA==1 & dt$SURVEY_METHOD=="PDC ELECTRIC FISHING"])])
range(dt$SURVEY_AREA[dt$SURVEY_ID %in% unique(dt$SURVEY_ID[dt$SURVEY_AREA==1 & dt$SURVEY_METHOD=="ELECTRIC FISHING"])])      
 

dt <- dt[!is.na(dt$SURVEY_AREA),]  
dt <- dt[!(dt$SURVEY_AREA==0 & dt$SURVEY_METHOD %in% c("PDC ELECTRIC FISHING", "ELECTRIC FISHING")),]
#dt <- dt[!(dt$SURVEY_AREA==1 & dt$SURVEY_METHOD %in% c("PDC ELECTRIC FISHING", "ELECTRIC FISHING")),] 


range(dt$SURVEY_AREA[dt$SURVEY_METHOD=="FIXED TRAP FISHING"]) # 0-1, counts idem density, keep
range(dt$SURVEY_AREA[dt$SURVEY_METHOD=="TRAPPING"])           # 1 1, counts idem density, keep
range(dt$SURVEY_AREA[dt$SURVEY_METHOD=="SEINE NETTING"])      # 0 - >10000
sum(dt$SURVEY_AREA==0 & dt$SURVEY_METHOD=="SEINE NETTING")    # 631, rm
sum(dt$SURVEY_AREA==1 & dt$SURVEY_METHOD=="SEINE NETTING")    # 9, rm
range(dt$FISHED_AREA[dt$SURVEY_AREA==0 & dt$SURVEY_METHOD=="SEINE NETTING"], na.rm=T)  # 0                  
range(dt$FISHED_AREA[dt$SURVEY_AREA==1 & dt$SURVEY_METHOD=="SEINE NETTING"], na.rm=T)  # 1                  
dt <- dt[!(dt$SURVEY_AREA==0 & dt$SURVEY_METHOD == "SEINE NETTING"),]
#dt <- dt[!(dt$SURVEY_AREA==1 & dt$SURVEY_METHOD == "SEINE NETTING"),]

# NOTE: when methods are "PDC ELECTRIC FISHING", "ELECTRIC FISHING", "SEINE NETTING"
# rm surveys with no info on area (NA or 0) because density cannot be computed
# rm surveys with area equal to 1 (this is likely a typo)
# keep records for trapping and fixed trapping

unique(dt$SURVEY_METHOD[dt$SURVEY_AREA==0])   
dt$SURVEY_AREA[dt$SURVEY_AREA==0] <- 1
unique(dt$SURVEY_METHOD[dt$SURVEY_AREA==1]) 


# Large areas: -----------------------------------------------------------------
range(dt$SURVEY_AREA)
class(dt$SURVEY_AREA)
c12 <- dt %>% group_by(SITE_ID, SURVEY_METHOD, SURVEY_STRATEGY) %>% 
  summarise(nMin=min(SURVEY_AREA), nMax=max(SURVEY_AREA),
            nDiff=max(SURVEY_AREA) - min(SURVEY_AREA)) 
c13 <- dt %>% group_by(SURVEY_ID) %>% 
  distinct(SURVEY_AREA)
#View(dt[dt$SURVEY_AREA==9997992,])  # OK
#View(dt[dt$SURVEY_AREA==384300,])   # OK
#View(dt[dt$SURVEY_AREA >384300,])   # mostly 999 etc in length, width, etc (rm)

dt <- dt[!dt$SURVEY_AREA > 384300, ] # NOTE: rm, since these values are unlikely to be true areas


# Remove sites after processing methods ========================================
c14 <- dt %>% group_by(SITE_ID, SURVEY_METHOD, SURVEY_STRATEGY, NO_OF_RUNS) %>% summarise(nYear=n_distinct(EVENT_DATE_YEAR))
c14$ConcatRm <- paste0(c14$SITE_ID, "_", c14$SURVEY_METHOD, "_", c14$SURVEY_STRATEGY, "_", c14$NO_OF_RUNS)

sum(c14$nYear==1)                          # 1807
length(unique(c14$ConcatRm[c14$nYear==1])) # 1807
toRm2 <- unique(c14$ConcatRm[c14$nYear==1])

dt <- dt[!dt$ConcatRm %in% toRm2, ]        # rm surveys with a specific methodology used only 1 year, keep 254379


# Aggregate & compute densities ================================================
unique(dt$SURVEY_METHOD)
c15 <- dt %>% group_by(SITE_ID, SURVEY_ID) %>% summarise(nArea=n_distinct(SURVEY_AREA)) # always 1
c16 <- dt %>% group_by(SITE_ID, SURVEY_METHOD, SURVEY_STRATEGY) %>% 
  summarise(nMin=min(SURVEY_AREA), nMax=max(SURVEY_AREA),
            nDiff=max(SURVEY_AREA) - min(SURVEY_AREA))                                  # 278100m2, 0.28km2

c17 <- dt %>% group_by(SITE_ID, SURVEY_METHOD, SURVEY_STRATEGY, 
                       Day, Month, Year) %>% summarise(n=n_distinct(SURVEY_ID))
range(c17$n)                                                                            # always 1 (so SampleDesc can be Site_Day_Month_Year)


# Rawdata ======================================================================
rawData <- dt %>% group_by(Family, Genus, Species, Latitude, Longitude, Day, Month, Year,
                           SITE_ID, SURVEY_ID, SURVEY_METHOD, SURVEY_STRATEGY, NO_OF_RUNS, SURVEY_AREA) %>%
  summarise(Abundance=sum(TOTAL_ABUNDANCE)) # some observations pooled (252811)
range(rawData$SURVEY_AREA)
rawData$Abundance <- (rawData$Abundance/rawData$SURVEY_AREA)*100
rawData$Abundance <- rawData$Abundance/rawData$NO_OF_RUNS
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
unique(paste0(rawData$SURVEY_METHOD, "_", rawData$SURVEY_STRATEGY))
unique(rawData$SURVEY_METHOD)
unique(rawData$SURVEY_STRATEGY)
rawData$SURVEY_STRATEGY <- str_replace_all(rawData$SURVEY_STRATEGY, "[[:punct:]]", " ")
rawData$SURVEY_STRATEGY <- str_replace_all(rawData$SURVEY_STRATEGY, "  ", " ")
rawData$SURVEY_METHOD <- str_replace_all(rawData$SURVEY_METHOD, " ", "_")
rawData$SURVEY_STRATEGY <- str_replace_all(rawData$SURVEY_STRATEGY, " ", "_")

rawData$ConcatMethods <- paste0(rawData$SURVEY_METHOD, "_", rawData$SURVEY_STRATEGY)
lr <- split(rawData, f=rawData$ConcatMethods)
sum_lr <- lapply(lr, function(x) {x %>% group_by(SITE_ID) %>% summarise(n=n_distinct(NO_OF_RUNS))})

# Grain size:
meanArealr <- lapply(lr, function(x) {mean(x$SURVEY_AREA)})

lr <- lapply(lr, function(x){within(x, rm(SITE_ID, SURVEY_ID, SURVEY_METHOD,
                                          SURVEY_STRATEGY, NO_OF_RUNS, SURVEY_AREA, ConcatMethods))})


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

