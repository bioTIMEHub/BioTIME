#######################################################################
# Curation Script: Ammend Study 234 (PART II, Ammend)
# Curator: AFE
# Date: April 2023
#######################################################################

# Libraries:-----------------------------------------------------------
library(dplyr)
library(tidyverse)
library(ggplot2)
library(maps)
library(stringr)
library(lubridate)
library(sp)
library(rgeos)
library(sf)
library(clipr)

rm(list=ls())

#Read data (BT & raw): -------------------------------------------------
mypath <- getwd()     
mypath

# Biotime v1:-----------------------------------------------------------
btv1 <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIME/BioTIMENewStudiesCuration/0_BioTIMEv1Revision"

raw234 <- read.csv2(file = paste0(btv1, "/STUDY_234_AFE/raw234.csv"), h=T)
m234 <- read.csv2(file = paste0(btv1, "/STUDY_234_AFE/methods234.csv"), h=T)
sps <- read.csv2(file = paste0(btv1, "/sps.csv"), h=T)

# RawData downloaded 2023:----------------------------------------------
load("C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIME/BioTIMENewStudiesCuration/0_BioTIMEv1Revision/STUDY_234_AFE/RawData_retrieved_2023/wt6veg.RData")

########################################################################
# Inspect data:---------------------------------------------------------
dt <- wt6veg
sort(unique(dt$Plot))       # 208, OK
sort(unique(dt$Zone))  
# 1 to 5, vegetation zone, which is determined by elevation and type of wood.
# Each vegetation zone has a different number of plots.

sort(unique(dt$Species))    # 21
sum(is.na(dt$Species))      # 0
sort(unique(dt$SppNum))     # 21
sum(is.na(dt$SppNum))       # 13422, so keep "Species" for ID.


# sequence number (unique number for each tree; multiple stems of same tree have same sequence)
sort(unique(dt$Seq))        # 14584, OK this allows to pool data for the same individual
sum(is.na(dt$Seq))          # 0

# tag number (not relevant until 2002 when W6 trees were tagged)
sort(unique(dt$Tag))        # 11639
sum(is.na(dt$Tag))          # 73099, from before 2002

# diameter at breast height, in cm
sort(unique(dt$Dbh))
range(dt$Dbh) 

range(dt$Dbh[dt$year==1977])
sum(dt$Dbh[dt$year==1977]==10) # 97
sum(dt$Dbh[dt$year==1982]==10) # 84
sum(dt$Dbh[dt$year==1987]==10) # 86

# vigor
sort(unique(dt$Vigor))      # seems these might vary per year, for each year we'll remove the obs of the dead trees, but keep the sick ones 8and ofc the healthy ones)

# Above & below ground biomass are estimated (units are kg)
range(dt$AbvBmss)              # 0.00 9385.04
range(dt$BlwBmss)              # 0.00 1248.27
range(dt$AbvBmss + dt$BlwBmss) # 0.00 10439.13


# Two to ten
# presence (1) or absence (0) of measured trees in 
# the 2 - 10 cm diameter class for that plot (in some years, not all plots were measured)
sort(unique(dt$TwotoTen))   # 1 0
sum(is.na(dt$TwotoTen))     # no NAs

# 10Area (Area of plot on which trees >10 cm dbh were measured (m-2))
sort(unique(dt$v_10Area))
sort(unique(dt$year[dt$v_10Area=="100"])) # 1965, OK (different sampling effort)

# TwotoTenArea (Area of plot on which trees 2-10 cm dbh were measured (m-2))
sort(unique(dt$TwotoTenArea)) # OK, these are variable as indicated in methods.

# ElevB (New category of elevation zone used for 2002 and later collections)
# High (>710 m), Mid (630 - 710 m), Low (<630 m)
sort(unique(dt$ElevB))
sort(unique(dt$year[dt$ElevB %in% c("H", "M", "L")])) # 2002 2007 2012 2017, OK, as indicated.
sum(is.na(dt$ElevB))


# AnalysisCode (Dbh interpolated from later data; not applicable for this data set at this time)
sort(unique(dt$AnalysisCode)) #  I

# bbd (Beech bark disease (bbd) code)
sort(unique(dt$bbd)) # 0 1 2 3, from disease not present to severely affecting the tree.

########################################################################
# Curation: ------------------------------------------------------------

# STEP1: remove year 1965:----------------------------------------------

sum(dt$year==1965)  # 3990 observations
dt <- dt[!dt$year==1965,]
sort(unique(dt$year)) # OK

# STEP2: remove dead trees:---------------------------------------------

str(dt$Vigor)
sort(unique(dt$Vigor[dt$year==1977])) # 0 1 2 3 4 5 (rm 4 & 5)
sort(unique(dt$Vigor[dt$year==1982])) # 0 1 2 3 4 5 (rm 4 & 5)
sort(unique(dt$Vigor[dt$year==1987])) # 0 1 2 3 4 5 (rm 4 & 5)
sort(unique(dt$Vigor[dt$year==1992])) # 0 3 4 5 (rm 4 & 5)
sort(unique(dt$Vigor[dt$year==1997])) # 0 3 4 5 (rm 4 & 5)
sort(unique(dt$Vigor[dt$year==2002])) # 0 3 4 5 (rm 4 & 5)
sort(unique(dt$Vigor[dt$year==2007])) # 0 3 4 5 6 (rm 4 5 6)
sort(unique(dt$Vigor[dt$year==2012])) # 0 3 4 5 6 (rm 4 5 6)
sort(unique(dt$Vigor[dt$year==2017])) # 0 3 4 5 (rm 4 & 5 )

sum(dt$Vigor %in% c("4", "5", "6")) # 18065
dt <- dt[!dt$Vigor %in% c("4", "5", "6"),]
sort(unique(dt$Vigor)) # 0 1 2 3, OK


# STEP3: Convert abbreviations to species names:------------------------

sort(unique(dt$Species)) # Some codes in online guide (dataset entry descriptors not present in dataset)
dt$Species <- as.character(dt$Species)
dt$Genus_species <- dt$Species

sort(unique(dt$year[dt$Species=="ACSA3"])) # 2012
sum(dt$Species=="ACSA3") # 1512
sort(unique(dt$Species[dt$Tag==1278]))
sort(unique(dt$Species[dt$Tag==5454]))

sort(unique(dt$year[dt$Species=="PRPE2"])) # 2012
sum(dt$Species=="PRPE2") # 2
sort(unique(dt$Species[dt$Tag==10063]))
sort(unique(dt$Species[dt$Tag==10069]))

sort(unique(dt$year[dt$Species=="PRSE2"])) # 2012
sum(dt$Species=="PRSE2") # 2
sort(unique(dt$Species[dt$Tag==2174]))
sort(unique(dt$Species[dt$Tag==2366]))


dt$Genus_species <- plyr::revalue(dt$Genus_species, c("ABBA"="Abies balsamea",
                                                      "ACPE"="Acer pensylvanicum",
                                                      "ACRU"="Acer rubrum",
                                                      "ACSA"="Acer saccharum",
                                                      "ACSA3"="Acer saccharum",      # Typo identified by tracking tags above
                                                      "ACSP"="Acer spicatum",
                                                      "AMSP"="Amelanchier sp",
                                                      "BEAL"="Betula alleghaniensis",
                                                      "BECO"="Betula cordifolia",
                                                      "BEPA"="Betula papyrifera",
                                                      "FAGR"="Fagus grandifolia",
                                                      "FRAM"="Fraxinus americana",
                                                      "PIRU"="Picea rubens",
                                                      "PRPE"="Prunus pensylvanica",
                                                      "PRPE2"="Prunus pensylvanica", # Typo identified by tracking tags above
                                                      "PRSE"="Prunus serotina",
                                                      "PRSE2"="Prunus serotina",     # Typo identified by tracking tags above
                                                      "SOAM"="Sorbus americana",
                                                      "TSCA"="Tsuga canadensis"))
sort(unique(dt$Genus_species))

sum(dt$Species=="UNKN") # 3
dt <- dt[!dt$Species=="UNKN",]

dt$genus <- str_split_fixed(dt$Genus_species, " ", 2)[,1]
dt$species <- str_split_fixed(dt$Genus_species, " ", 2)[,2]
dt$family <- rep(NA, nrow(dt))
sort(unique(dt$genus))
dt$family <- ifelse(dt$genus %in% c("Abies"), "Pinaceae", dt$family)
dt$family <- ifelse(dt$genus %in% c("Acer"), "Sapindaceae", dt$family)
dt$family <- ifelse(dt$genus %in% c("Amelanchier"), "Rosaceae", dt$family)
dt$family <- ifelse(dt$genus %in% c("Betula"), "Betulaceae", dt$family)
dt$family <- ifelse(dt$genus %in% c("Fagus"), "Fagaceae", dt$family)
dt$family <- ifelse(dt$genus %in% c("Fraxinus"), "Oleaceae", dt$family)
dt$family <- ifelse(dt$genus %in% c("Picea"), "Pinaceae", dt$family)
dt$family <- ifelse(dt$genus %in% c("Prunus"), "Rosaceae", dt$family)
dt$family <- ifelse(dt$genus %in% c("Sorbus"), "Rosaceae", dt$family)
dt$family <- ifelse(dt$genus %in% c("Tsuga"), "Pinaceae", dt$family)
sort(unique(dt$family))

# STEP 4: Check for duplicates in dataframe:----------------------------
# Check that each sequence each year has only one species name:
check1 <- dt %>%
  group_by(Seq, year) %>%
  summarise(countSps=n_distinct(Genus_species), countPlot=n_distinct(Plot))
range(check1$countSps)  #always 1, OK
range(check1$countPlot) #always 1, OK

# Check how many observations are there for one same individual at a given time event:
check2 <- dt %>%
  group_by(Seq, year) %>%
  summarise(no_rows = length(Seq))
range(check2$no_rows) # always 1, OKa given sequence only appears once every year
check3 <- dt %>% group_by(year) %>% filter(duplicated(Seq)) # 0, OK

# Check with tag (individual stems tagged):
dt_check4 <- dt[!is.na(dt$Tag),]
check4 <- dt_check4 %>%
  group_by(Tag, year, Plot) %>%
  summarise(no_rows = length(Tag))
range(check4$no_rows)  # 1 or 2 (only 1s if Seq is used in grouping instead of tag)
sum(check4$no_rows==2) # 19 cases
check4 <- subset(check4, check4$no_rows==2)

check5 <- dt[(dt$Tag %in% check4$Tag) & (dt$year %in% check4$year) & (dt$Plot %in% check4$Plot),]
check5b <- check5[,!names(check5) %in% c("Seq", "SppNum")]
check5b <- check5b %>% group_by(Tag, year, Plot) %>% hablar::find_duplicates()


# NOTE: Having looked at the data, there's 8 obs that could be duplicates 
# (same tag, same diameter, same plot, same species 
# & those prior to 2017 have only one tag record in subsequent years). However,
# it's a bit difficult to be certain, considering there are a few other observations 
# where the number of identical tags for a species in a given plot varies between years. 
# Overall, this is a small number and to be conservative, these 8 obs are not removed

# Check for additional similar records ignoring the identifiers:
check6 <- dt[duplicated(dt[-c(4,5)]),]  
5471/88752*100 
# 6.16 (relatively small proportion, and it is entirely possible that multiple trees in a plot have
# the same measurements, so these observation should be all kept)

# STEP 5: Add central coordinates:--------------------------------------
sort(unique(raw234$LATITUDE))
sort(unique(raw234$LONGITUDE))

# the centroid of the bounding coordinates is used.
points_lat <- c(43.957001, 43.949928)
points_long <- c(-71.743462, -71.735649)
points <- SpatialPoints(cbind(points_lat, points_long))
convhull<-gConvexHull(points)
centroid <-gCentroid(convhull)    
centroid

dt$latitude <- rep(43.95346, nrow(dt))
dt$longitude <- rep(-71.73956, nrow(dt)) # idem to BT v1

world_map <- map_data("world") 
world <- ggplot() + coord_fixed() +xlab("") + ylab("") 
world  <- world  + 
  geom_polygon(data=world_map, aes(x=long, y=lat, group=group), 
               colour="gray60", fill="gray60") 
points <- world  + 
  geom_point(data=dt, 
             aes(x=longitude, y=latitude, alpha=0.01)) 
points

points_zoom <- points +
  ylim(20,70)+
  xlim(-90,-50)
points_zoom    # correct placement


# STEP 6: Split into three time series x sample effort:-----------------

# Subset 1:-------------------------------------------------------------
# Forest Inventory of a Northern Hardwood Forest: Watershed 6, 
# Hubbard Brook Experimental Forest. Trees >=2 cm dbh and <=10cm dbh (1982-1987)

dt1 <- dt[dt$year %in% c(1982, 1987),]
sum(dt1$TwotoTenArea==625, na.rm=T) 
sort(unique(dt1$TwotoTenArea))
range(dt1$Dbh[dt1$TwotoTenArea==625], na.rm=T)   # 2.0 9.9, OK
range(dt1$Dbh[is.na(dt1$TwotoTenArea)], na.rm=T) # 10.0 84.2, OK

dt1 <- subset(dt1, dt1$TwotoTenArea==625)        # 3550

length(unique(dt1$Plot[dt1$year==1982])) # 32
length(unique(dt1$Plot[dt1$year==1987])) # 35

setdiff(unique(dt1$Plot[dt1$year==1982]), unique(dt1$Plot[dt1$year==1987])) # 0
setdiff(unique(dt1$Plot[dt1$year==1987]), unique(dt1$Plot[dt1$year==1982])) # "22" "32" "61"


dt1final <- dt1[!dt1$Plot %in% c("22","32","61"),]    # 3339 observations


# Subsets 2 & 3: -------------------------------------------------------
dt <- anti_join(dt, dt1)                              # 85202
range(dt$TwotoTenArea, na.rm=T)                       # 51.9 88.2

dt2 <- dt[is.na(dt$TwotoTenArea),]
range(dt2$Dbh)
sum(dt2$Dbh<10)                                       # only 2 obs
#View(dt2[dt2$Dbh<10,])                               # kept, very close to 10 cm so that's probably why they were still included
sort(unique(dt2$year))                                # this subsetting way is valid for data up until 2012

dt3 <- anti_join(dt, dt2)
range(dt3$Dbh)
sum(dt3$Dbh>=10)                                      # 8016
sort(unique(dt3$year[dt3$Dbh>=10]))                   # 2017
check7 <- subset(wt6veg, wt6veg$year==2017)
sum(is.na(check7$TwotoTenArea))                       # 0
unique(check7$TwotoTen)                               # 1 
# This means, in 2017 the entry of data was slightly different, and for all
# observations, small or large trees, the area of the two to ten subplot is given

dt2 <- rbind(dt2, dt3[dt3$Dbh>=10,])                  # 65978
sum(dt2$Dbh<10)                                       # 2 (see above), OK

# Confirm features:
dt2 %>% group_by(year) %>% summarise(n_distinct(Plot))          # Always 208, good
dt2 %>% group_by(year) %>% summarise(n_distinct(Genus_species)) # nothing off here, good
dt2 %>% group_by(year) %>% summarise(n_distinct(Zone))          # always 5, good

dt2final <- dt2                                                 # 65978 observations


# Subset 3: ------------------------------------------------------------
# This dataset starts in 1992, Correct

dt3 <- anti_join(dt, dt2)
range(dt3$Dbh)                                        # 2.0 9.9, OK
range(dt3$TwotoTenArea)                               # 51.9 88.2
sum(is.na(dt3$TwotoTenArea))                          # 0

dt3 %>% group_by(year) %>% summarise(n_distinct(Plot))          # Always 208, good
dt3 %>% group_by(year) %>% summarise(n_distinct(Genus_species)) # nothing off here, good
dt3 %>% group_by(year) %>% summarise(n_distinct(Zone))          # always 5, good
check8 <- dt3 %>% group_by(year, Plot) %>% summarise(n_distinct(TwotoTenArea))
range(check8$`n_distinct(TwotoTenArea)`)
sum(check8$`n_distinct(TwotoTenArea)`==2)                       # Only 3 instances, otherwise always 1 (good)

dt3final <- dt3                                                 # 19224 observations

##########################################################################
# STEP 7: Aggreagate no of individuals & biomasses for the whole watershed:

# Subset 1: --------------------------------------------------------------
sort(unique(dt1final$TwotoTenArea)) # Always 625 m2
length(unique(dt1final$Seq))        # 3045
check9 <- dt1final %>% group_by(year, Plot, Seq) %>% summarise(n_distinct(Seq))  
check9b <- dt1final %>% group_by(year, Plot, Seq) %>% summarise(length(Seq))  
range(check9$`n_distinct(Seq)`)     # 1, OK

dt1final$Abundance <- rep(1, nrow(dt1final))
dt1agg <- aggregate(cbind(Abundance, AbvBmss) ~ year + Genus_species, data = dt1final, FUN = sum, na.rm = TRUE)
dt1agg$density <- dt1agg$Abundance/(32*625) # Individuals * m2(total area)

# Quick manual sanity checks relative to the original file:

# wt6veg$year==1982 & wt6veg$Species=="ABBA" & wt6veg$Dbh < 10 & wt6veg$Vigor %in% c("0","1","2","3") (IDEM)
# wt6veg$year==1987 & wt6veg$Species=="ABBA" & wt6veg$Dbh < 10 & wt6veg$Vigor %in% c("0","1","2","3") & ! wt6veg$Plot %in% c("22","32","61") (IDEM)
# subset(wt6veg, wt6veg$year==1987 & wt6veg$Species=="ACSP" & wt6veg$Dbh < 10 & wt6veg$Vigor %in% c("0","1","2","3") & ! wt6veg$Plot %in% c("22","32","61")) (IDEM)
# subset(wt6veg, wt6veg$year==1982 & wt6veg$Species=="ACSP" & wt6veg$Dbh < 10 & wt6veg$Vigor %in% c("0","1","2","3") & ! wt6veg$Plot %in% c("22","32","61")) (IDEM)
#test_agg1 <- subset(wt6veg, wt6veg$year==1982 & wt6veg$Species=="ACSP" & wt6veg$Dbh < 10 & wt6veg$Vigor %in% c("0","1","2","3") & ! wt6veg$Plot %in% c("22","32","61"))
#sum(test_agg1$AbvBmss) # modify above for any other specific obsrevation check


# Subset 2: --------------------------------------------------------------
sort(unique(dt2final$v_10Area))      # Always 625 m2
check10 <- dt2final %>% group_by(year, Plot, Seq) %>% summarise(n_distinct(Seq)) 
check10b <- dt2final %>% group_by(year, Plot, Seq) %>% summarise(length(Seq))  
range(check10$`n_distinct(Seq)`)     # 1, OK

dt2final$Abundance <- rep(1, nrow(dt2final))
dt2finalA <- subset(dt2final, dt2final$year < 2002)
range(dt2finalA$year)

dt2finalB <- subset(dt2final, dt2final$year >= 2002)
range(dt2finalB$year)

dt2agg <- aggregate(cbind(Abundance, AbvBmss) ~ year + Genus_species, data = dt2final, FUN = sum, na.rm = TRUE)
dt2aggA <- aggregate(cbind(Abundance, AbvBmss) ~ year + Genus_species, data = dt2finalA, FUN = sum, na.rm = TRUE)
dt2aggB <- aggregate(cbind(Abundance, AbvBmss) ~ year + Genus_species + Plot, data = dt2finalB, FUN = sum, na.rm = TRUE)
length(unique(dt2aggB$Plot)) # 208, OK

dt2agg$density <- dt2agg$Abundance/(208*625) # Individuals * m2 (total area 208 plots of 625m2 each)

# Quick manual sanity checks relative to the original file:

# subset(wt6veg, wt6veg$year==2012 & wt6veg$Species=="ABBA" & wt6veg$Dbh >= 10 & wt6veg$Vigor %in% c("0","1","2","3")) (IDEM)
# subset(wt6veg, wt6veg$year==2007 & wt6veg$Species=="ABBA" & wt6veg$Dbh >= 10 & wt6veg$Vigor %in% c("0","1","2","3")) (IDEM)
# subset(wt6veg, wt6veg$year==2012 & wt6veg$Species=="BEPA" & wt6veg$Dbh >= 10 & wt6veg$Vigor %in% c("0","1","2","3")) (IDEM)
# subset(wt6veg, wt6veg$year==2007 & wt6veg$Species=="BEPA" & wt6veg$Dbh >= 10 & wt6veg$Vigor %in% c("0","1","2","3")) (FINE, 31.44 less bc that's one of the two cases when a tree <10 is kept and it's biomass is 31.44)
# subset(wt6veg, wt6veg$year==2002 & wt6veg$Species=="FAGR" & wt6veg$Dbh >= 10 & wt6veg$Vigor %in% c("0","1","2","3")) (IDEM)
#test_agg2 <- subset(wt6veg, wt6veg$year==2017 & wt6veg$Species=="FAGR" & wt6veg$Dbh >= 10 & wt6veg$Vigor %in% c("0","1","2","3"))
#sum(test_agg2$AbvBmss)


# Subset 3: --------------------------------------------------------------
sort(unique(dt3final$v_10Area))      # Always 625 m2
range(dt3final$TwotoTenArea)         # 51.9 88.2 m2
check11 <- dt3final %>% group_by(year, Plot, Seq) %>% summarise(n_distinct(Seq))  
range(check11$`n_distinct(Seq)`)     # 1, OK

dt3final %>% group_by(year) %>% summarise(n=n_distinct(Plot)) # good
check12 <- dt3final %>% group_by(year, Plot) %>% summarise(n=n_distinct(TwotoTenArea))
check13 <- dt3final %>% group_by(year, Plot) %>% distinct(TwotoTenArea)
sum(check13$TwotoTenArea[check13$year==1992])
sum(check13$TwotoTenArea[check13$year==1997])
sum(check13$TwotoTenArea[check13$year==2002])
sum(check13$TwotoTenArea[check13$year==2007])
sum(check13$TwotoTenArea[check13$year==2012])
sum(check13$TwotoTenArea[check13$year==2017])


# Aggregate the nº of individuals per year, then calculate density per year with each of the total m2 values.
dt3final$Abundance <- rep(1, nrow(dt3final))
dt3agg <- aggregate(cbind(Abundance, AbvBmss) ~ year + Genus_species, data = dt3final, FUN = sum, na.rm = TRUE)
dt3agg$totalA <- rep(NA, nrow(dt3agg))

dt3agg$totalA <- ifelse(dt3agg$year==1992, sum(check13$TwotoTenArea[check13$year==1992]), dt3agg$totalA)
dt3agg$totalA <- ifelse(dt3agg$year==1997, sum(check13$TwotoTenArea[check13$year==1997]), dt3agg$totalA)
dt3agg$totalA <- ifelse(dt3agg$year==2002, sum(check13$TwotoTenArea[check13$year==2002]), dt3agg$totalA)
dt3agg$totalA <- ifelse(dt3agg$year==2007, sum(check13$TwotoTenArea[check13$year==2007]), dt3agg$totalA)
dt3agg$totalA <- ifelse(dt3agg$year==2012, sum(check13$TwotoTenArea[check13$year==2012]), dt3agg$totalA)
dt3agg$totalA <- ifelse(dt3agg$year==2017, sum(check13$TwotoTenArea[check13$year==2017]), dt3agg$totalA)

dt3agg$densityA <- dt3agg$Abundance/dt3agg$totalA # total densities are very similar tho
dt3agg$densityB <- dt3agg$AbvBmss/dt3agg$totalA   # total densities are very similar tho

# Quick manual sanity checks relative to the original file (for nº of ind):
# subset(wt6veg, wt6veg$year==2002 & wt6veg$Species=="FAGR" & wt6veg$Dbh < 10 & wt6veg$Vigor %in% c("0","1","2","3"))  (IDEM)
# subset(wt6veg, wt6veg$year==2012 & wt6veg$Species=="ABBA" & wt6veg$Dbh < 10 & wt6veg$Vigor %in% c("0","1","2","3"))
# subset(wt6veg, wt6veg$year==2012 & wt6veg$Species=="PIRU" & wt6veg$Dbh < 10 & wt6veg$Vigor %in% c("0","1","2","3"))
#test_agg3 <- subset(wt6veg, wt6veg$year==2017 & wt6veg$Species=="ACPE" & wt6veg$Dbh < 10 & wt6veg$Vigor %in% c("0","1","2","3"))
#sum(test_agg3$AbvBmss)

##########################################################################
# STEP 8: Create rawdata

meta_path <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIMEGithubMetadatas/Hubbard_Brook_Experimental_Forest_AFE"


# Subset 1: “Forest Inventory of a Northern Hardwood Forest: Watershed 6, 
# Hubbard Brook Experimental Forest. Trees >=2 cm dbh and <=10cm dbh (1982-1987)”

head(dt1agg)
rawdata_dt1 <- dt1agg
names(rawdata_dt1) <- c("Year", "Genus_species", "Abundance", "Biomass", "Density")

rawdata_dt1$Family <- dt1final$family[match(rawdata_dt1$Genus_species, dt1final$Genus_species)]
rawdata_dt1$Genus <- str_split_fixed(rawdata_dt1$Genus_species, " ", 2)[,1]
rawdata_dt1$Species <- str_split_fixed(rawdata_dt1$Genus_species, " ", 2)[,2]
rawdata_dt1$Plot <- rep("Northern Hardwood Forest: Watershed 6", nrow(rawdata_dt1))
rawdata_dt1$Latitude <- unique(dt1final$latitude)
rawdata_dt1$Longitude <- unique(dt1final$longitude)
rawdata_dt1$DepthElevation <- rep(NA, nrow(rawdata_dt1))

rawdata_dt1$Day <- rep(NA, nrow(rawdata_dt1))
rawdata_dt1$Month <- rep(NA, nrow(rawdata_dt1))
rawdata_dt1$SampleDescription <- rawdata_dt1$Year
rawdata_dt1$StudyID <- rep(NA, nrow(rawdata_dt1))

head(rawdata_dt1)

rawdata_dt1 <- within(rawdata_dt1, rm(Density))
rawdata_dt1 <- rawdata_dt1 %>% relocate(SampleDescription, .after=Species)
rawdata_dt1 <- rawdata_dt1 %>% relocate(Year, .after=Month)
rawdata_dt1 <- within(rawdata_dt1, rm(Genus_species))

write.csv(rawdata_dt1, file=paste0(meta_path, "/rawdata_dt1.csv"), row.names = F)


 
# Subset 2: “Forest Inventory of a Northern Hardwood Forest: Watershed 6, 
# Hubbard Brook Experimental Forest. Trees >=10 cm dbh (1977-2017)”

head(dt2agg)
rawdata_dt2 <- dt2agg
names(rawdata_dt2) <- c("Year", "Genus_species", "Abundance", "Biomass", "Density")

rawdata_dt2$Family <- dt2final$family[match(rawdata_dt2$Genus_species, dt2final$Genus_species)]
rawdata_dt2$Genus <- str_split_fixed(rawdata_dt2$Genus_species, " ", 2)[,1]
rawdata_dt2$Species <- str_split_fixed(rawdata_dt2$Genus_species, " ", 2)[,2]
rawdata_dt2$Plot <- rep("Northern Hardwood Forest: Watershed 6", nrow(rawdata_dt2))
rawdata_dt2$Latitude <- unique(dt2final$latitude)
rawdata_dt2$Longitude <- unique(dt2final$longitude)
rawdata_dt2$DepthElevation <- rep(NA, nrow(rawdata_dt2))

rawdata_dt2$Day <- rep(NA, nrow(rawdata_dt2))
rawdata_dt2$Month <- rep(NA, nrow(rawdata_dt2))
rawdata_dt2$SampleDescription <- rawdata_dt2$Year
rawdata_dt2$StudyID <- rep(NA, nrow(rawdata_dt2))

head(rawdata_dt2)

rawdata_dt2 <- within(rawdata_dt2, rm(Density))
rawdata_dt2 <- rawdata_dt2 %>% relocate(SampleDescription, .after=Species)
rawdata_dt2 <- rawdata_dt2 %>% relocate(Year, .after=Month)
rawdata_dt2 <- within(rawdata_dt2, rm(Genus_species))

write.csv(rawdata_dt2, file=paste0(meta_path, "/rawdata_dt2.csv"), row.names = F)



# Subset 3: “Forest Inventory of a Northern Hardwood Forest: Watershed 6, 
# Hubbard Brook Experimental Forest. Trees >=2 cm dbh and <=10cm dbh (1992-2017)”

head(dt3agg)
rawdata_dt3 <- dt3agg
names(rawdata_dt3) <- c("Year", "Genus_species", "Abundance", "Biomass", "totalArea", "DensityA", "DensityB")

rawdata_dt3$Family <- dt3final$family[match(rawdata_dt3$Genus_species, dt3final$Genus_species)]
rawdata_dt3$Genus <- str_split_fixed(rawdata_dt3$Genus_species, " ", 2)[,1]
rawdata_dt3$Species <- str_split_fixed(rawdata_dt3$Genus_species, " ", 2)[,2]
rawdata_dt3$Plot <- rep("Northern Hardwood Forest: Watershed 6", nrow(rawdata_dt3))
rawdata_dt3$Latitude <- unique(dt3final$latitude)
rawdata_dt3$Longitude <- unique(dt3final$longitude)
rawdata_dt3$DepthElevation <- rep(NA, nrow(rawdata_dt3))

rawdata_dt3$Day <- rep(NA, nrow(rawdata_dt3))
rawdata_dt3$Month <- rep(NA, nrow(rawdata_dt3))


rawdata_dt3$SampleDescription <- rawdata_dt3$Year
rawdata_dt3$StudyID <- rep(NA, nrow(rawdata_dt3))
head(rawdata_dt3)

rawdata_dt3 <- within(rawdata_dt3, rm(Abundance, Biomass, totalArea)) # we keep the densities for this dataset
rawdata_dt3 <- rawdata_dt3 %>% relocate(SampleDescription, .after=Species)
rawdata_dt3 <- rawdata_dt3 %>% relocate(Year, .after=Month)
names(rawdata_dt3) <- c("Genus_species", "Abundance", "Biomass", "Family", 
                        "Genus", "Species", "SampleDescription", "Plot", "Latitude", 
                        "Longitude", "DepthElevation", "Day", "Month", "Year", "StudyID")
head(rawdata_dt3)
rawdata_dt3 <- within(rawdata_dt3, rm(Genus_species))

write.csv(rawdata_dt3, file=paste0(meta_path, "/rawdata_dt3.csv"), row.names = F)


# Assess differences with STUDY 234:--------------------------------------
sort(unique(raw234$YEAR)) # 1982 1987 1992 1997 2002

# e.g. year 1982:

# Biomass:
sum(rawdata_dt2$Biomass[rawdata_dt2$Year==1982])
sum(rawdata_dt1$Biomass[rawdata_dt1$Year==1982])
sum(wt6veg$AbvBmss[wt6veg$year==1982 & wt6veg$Vigor %in% c("4", "5", "6")]) 
2444617 + 11878.18 + 124018.4 # (IDEM BELOW, OK)
sum(raw234$BIOMASS[raw234$YEAR==1982])
sum(wt6veg$AbvBmss[wt6veg$year==1982]) 

# Abundance:
sum(rawdata_dt2$Abundance[rawdata_dt2$Year==1982])
sum(rawdata_dt1$Abundance[rawdata_dt1$Year==1982])
dim(wt6veg[wt6veg$year==1982 & wt6veg$Vigor %in% c("4", "5", "6"),]) 
7732 + 1329 + 1194 # (IDEM BELOW, OK)
sum(raw234$ABUNDANCE[raw234$YEAR==1982])
dim(wt6veg[wt6veg$year==1982,])


# e.g. year 1997:
# Biomass:
sum(rawdata_dt2$Biomass[rawdata_dt2$Year==1997])
sum(dt3agg$AbvBmss[dt3agg$year==1997])
sum(wt6veg$AbvBmss[wt6veg$year==1997 & wt6veg$Vigor %in% c("4", "5", "6")]) 
sum(wt6veg$AbvBmss[wt6veg$year==1997 & wt6veg$Vigor %in% c("0", "1", "2", "3") & wt6veg$Species=="UNKN"]) 
2425506 + 13157.32 + 172500.9 + 692.03 # (IDEM BELOW, OK)
sum(raw234$BIOMASS[raw234$YEAR==1997])
sum(wt6veg$AbvBmss[wt6veg$year==1997]) 

# Abundance:
sum(rawdata_dt2$Abundance[rawdata_dt2$Year==1997])
sum(dt3agg$Abundance[dt3agg$year==1997])
dim(wt6veg[wt6veg$year==1997 & wt6veg$Vigor %in% c("4", "5", "6"),]) 
dim(wt6veg[wt6veg$year==1997 & wt6veg$Vigor %in% c("0", "1", "2", "3") & wt6veg$Species=="UNKN",]) 
6964 + 2912 + 1542 + 3 # (IDEM BELOW, OK)
sum(raw234$ABUNDANCE[raw234$YEAR==1997])
dim(wt6veg[wt6veg$year==1997,])


# Convex hulls ====================================================
# 1. Convert data points into point spatial object
# assumes your rawdata dataframe is called rawdata
dt_merged <- rawdata_dt3 # (or dt1 or dt2) All three datasets have the same coordinates
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
  coord_sf(xlim = c(-80,-60), ylim = c(40,55)) + 
  labs(x = NULL, y = NULL) +
  theme_minimal()


# End of script ###################################################
