################################################################################
# Revision of study 327
# AFE Nov 2024
################################################################################


# Main sources =================================================================
# https://figshare.com/collections/Long-term_monitoring_of_mammals_in_the_face_of_biotic_and_abiotic_influences_at_a_semiarid_site_in_north-central_Chile/3305493


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
path <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIME/BioTIMENewStudiesCuration/RecurateOct2024/id327"
dt <- read.table(paste0(path, "/Fray_Jorge_Small_Mammals_198903200512.txt"), h=T)
str(dt)


# Sampling Event Date: =========================================================
dt$Year <- as.integer(substr(dt$MO, start = 1, stop = 4))
range(dt$Year)
dt$Month <- as.integer(substr(dt$MO, start = 5, stop = 6))
range(dt$Month)
length(unique(dt$MO)) # 202

c1 <- dt %>% group_by(GR) %>% summarise(n=n_distinct(MO)) # 67-202


# Abundance & Biomass: =========================================================
range(dt$WT)
dt$WT[dt$WT=="."] <- NA # no data
dt$WT <- as.numeric(dt$WT)
sum(is.na(dt$WT))      # 3109
sum(dt$WT==0, na.rm=T) # 0
range(dt$WT, na.rm=T)  # 800 grams
sort(unique(dt$WT))

unique(dt$SP[dt$WT>300]) 
table(dt$SP[dt$WT>300])

unique(dt$SP[dt$WT>200]) 
table(dt$SP[dt$WT>200])

# NOTE AFE: not confident on which might be typos, 
# so all weights kept as they are.

dt$Abundance <- rep(1, nrow(dt))


# Sample descriptors: ==========================================================
sort(unique(dt$TRT))
dt <- dt[dt$TRT %in% c("-D+P", "+D+P"),]
sort(unique(dt$Year[dt$TRT=="-D+P"]))
sort(unique(dt$Year[dt$TRT=="+D+P"]))

sort(unique(dt$GR))  # OK
sort(unique(dt$ROW)) # 6, 9 & 43 could be typos, 0s are fine, just means unknown row
sort(unique(dt$COL)) # 6, 9 & 43 could be typos, 0s are fine, just means unknown col


c1 <- dt %>% group_by(MO, TRT) %>% summarise(n=n_distinct(GR))
table(c1$n) # given the data structure, we can assume that less grids mean 
# that certain grids yielded no captures

subdt <- dt[,names(dt) %in% c("MO", "TRT", "GR", "ROW")]
c2B <- subdt %>%
  group_by(MO, TRT, GR) %>%
  mutate(nR = n_distinct(ROW)) %>%  # Create a new column with count of distinct ROW
  distinct(MO, TRT, GR, ROW, .keep_all = TRUE) 
subdt2 <- dt[,names(dt) %in% c("MO", "TRT", "GR", "COL")]
c2B <- subdt2 %>%
  group_by(MO, TRT, GR) %>%
  mutate(nR = n_distinct(COL)) %>%  # Create a new column with count of distinct ROW
  distinct(MO, TRT, GR, COL, .keep_all = TRUE) 

# In most cases, when row or col have a number that is not 1-5, it's because the
# trap is unknown (i.e., 0). There's a few typos, clearly, but these are ok since 
# grid will be the finest grain kept in SAMPLE_DESC.


# Recaptures: ==================================================================
c1 <- dt %>% group_by(MO, TRT, GR, AnimalID, SP) %>% summarise(n=n()) # recaptures!
#op1 <- dt %>% distinct(MO, TRT, GR, AnimalID, SP, keep_all=T) # rm recaptures 
#dim(op1)

dt1 <- dt %>% group_by(Abundance, Year, Month, TRT, GR, AnimalID, SP) %>% summarise(WeightMean=mean(WT, na.rm=T))
dim(dt1)
sum(is.na(dt1$WeightMean))
c2 <- dt1 %>% group_by(Abundance, Year, Month, TRT, GR, AnimalID, SP) %>% summarise(n=n()) # the prev step gets rid of dups


# Location: ====================================================================
dt1$Latitude <- -30.6333   # as in meta of main source
dt1$Longitude <- -71.6667

#world_map <- map_data("world") 
#world <- ggplot() + coord_fixed() +xlab("") + ylab("") 
#world  <- world  + 
#  geom_polygon(data=world_map, aes(x=long, y=lat, group=group), 
#               colour="gray60", fill="gray60") 
#points <- world  + 
#  geom_point(data=dt1, 
#             aes(x=Longitude, y=Latitude, alpha=0.01)) 
#points    # OK


# Taxonomy: ====================================================================
v1 <- read_excel("C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIME/BioTIMENewStudiesCuration/RecurateOct2024/id327/Study_327_v1.xlsx", sheet="RawData") # load v1
names(v1)
sort(unique(dt$SP))
sort(unique(dt1$SP))
table(dt1$SP)
#View(dt1[dt1$SP=="UN",])
sort(unique(v1$newID2))

dt1$SP2 <- rep(NA, nrow(dt1))
dt1$SP2[dt1$SP=="AB"] <- "Abrocoma bennettii"
dt1$SP2[dt1$SP=="AL"] <- "Abrothrix longipilis"
dt1$SP2[dt1$SP=="AO"] <- "Abrothrix olivaceus"
dt1$SP2[dt1$SP=="CH"] <- "Chelemys megalonyx"
dt1$SP2[dt1$SP=="LU"] <- "Octodon lunatus"

check <- gnr_resolve(c("Thylamys [Marmosa] elegans", "Marmosa elegans", "Thylamys elegans"))

dt1$SP2[dt1$SP=="ME"] <- "Thylamys sp"  # Updated in database, this is actually T.elegans
dt1$SP2[dt1$SP=="OD"] <- "Octodon degus"
dt1$SP2[dt1$SP=="OL"] <- "Oligoryzomys longicaudatus"
dt1$SP2[dt1$SP=="PD"] <- "Phyllotis darwini"
dt1$SP2[dt1$SP=="SC"] <- "Spalacopus cyanus"
dt1$SP2[dt1$SP=="UN"] <- ""

dt1 <- dt1[!dt1$SP=="UN",] # can't be sure of whther they are typos, so considered "unknown" and removed

sort(unique(dt1$SP2))

sort(unique(dt1$SP2[dt1$TRT=="-D+P"]))
table(dt1$SP2[dt1$TRT=="-D+P"])

dt1 <- dt1[!(dt1$SP=="OD" & dt1$TRT=="-D+P"),] # rm OD from "-D+P"
# NOTE Methods: Any degus captured in -D plots during monthly censuses are removed and released approximately 1 km from the experimental grid complex.
77864 - 5094


# Format for BT: ===============================================================
# NOTE: data kept unaggregated

raw <- dt1
names(raw)
names(raw) <- toupper(names(raw))

raw$SAMPLE_DESC <- paste0(raw$YEAR, "_", raw$MONTH, "_", raw$TRT, "_", raw$GR) # Year_Month_Treatment_Grid
names(raw)[names(raw)=="WEIGHTMEAN"] <- "BIOMASS"
raw$PLOT <- raw$GR
raw$DEPTH <- NA
raw$DAY <- NA
raw$STUDY_ID <- rep(327, nrow(raw))
raw$newID <- v1$newID[match(raw$SP2, v1$newID2)]
raw$ID_SPECIES <- v1$ID_SPECIES[match(raw$SP2, v1$newID2)]

sum(is.na(raw$newID))
sum(is.na(raw$ID_SPECIES))

raw <- raw %>% ungroup() %>% select(ABUNDANCE, BIOMASS, ID_SPECIES, SAMPLE_DESC, PLOT, LATITUDE, LONGITUDE, DEPTH, DAY, MONTH, YEAR, STUDY_ID, newID)
head(raw)
str(raw)


path <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIME/BioTIMENewStudiesCuration/RecurateOct2024/id327/NEW_327_v2"
write.csv(raw, file=paste0(path, "/rawData.csv"), row.names = F)
