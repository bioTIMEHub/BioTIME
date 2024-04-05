################################################################################
# Curation Script: Ammend Study 308
# Curator: AFE
# Date: April 2023 & January 2024
################################################################################


# Libraries:--------------------------------------------------------------------
library(dplyr)
library(tidyverse)
library(ggplot2)
library(maps)
library(stringr)
library(lubridate)

rm(list=ls())

# Source: ----------------------------------------------------------------------
# https://doi.org/10.6073/pasta/086fda03bd91ce9c2331e3a6fdd9bcd1


################################################################################
# Package ID: knb-lter-vcr.67.21 Cataloging System:https://pasta.edirepository.org.
# Data set title: Long Term Mammal Data from Powdermill Biological Station 1979-1999.
# Data set creator:  Joseph Merritt -  
# Metadata Provider:    - Virginia Coast Reserve Long-Term Ecological Research Project 
# Contact:  Joseph Merritt -    - jmerritt@illinois.edu
# Contact:    - Information manager - Virginia Coast Reserve Long-Term Ecological Research Project   - jhp7e@virginia.edu
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-vcr/67/21/82cf3f3ce41a598251f85e4b0da6caf8" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F 
               ,skip=22
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "ID",     
                 "NEW",     
                 "SPECIES",     
                 "PERIOD",     
                 "TIME",     
                 "DATE",     
                 "QUADR",     
                 "SEX",     
                 "NO",     
                 "WEIGHT",     
                 "OT",     
                 "SC",     
                 "IN",     
                 "PR",     
                 "OP",     
                 "LG"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt1$ID)!="factor") dt1$ID<- as.factor(dt1$ID)
if (class(dt1$NEW)!="factor") dt1$NEW<- as.factor(dt1$NEW)
if (class(dt1$SPECIES)!="factor") dt1$SPECIES<- as.factor(dt1$SPECIES)
if (class(dt1$PERIOD)=="factor") dt1$PERIOD <-as.numeric(levels(dt1$PERIOD))[as.integer(dt1$PERIOD) ]               
if (class(dt1$PERIOD)=="character") dt1$PERIOD <-as.numeric(dt1$PERIOD)
if (class(dt1$TIME)!="factor") dt1$TIME<- as.factor(dt1$TIME)                                   
# attempting to convert dt1$DATE dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"YMD"
tmp1DATE<-as.Date(dt1$DATE,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1DATE) == length(tmp1DATE[!is.na(tmp1DATE)])){dt1$DATE <- tmp1DATE } else {print("Date conversion failed for dt1$DATE. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp1DATE) 
if (class(dt1$QUADR)!="factor") dt1$QUADR<- as.factor(dt1$QUADR)
if (class(dt1$SEX)!="factor") dt1$SEX<- as.factor(dt1$SEX)
if (class(dt1$NO)!="factor") dt1$NO<- as.factor(dt1$NO)
if (class(dt1$WEIGHT)=="factor") dt1$WEIGHT <-as.numeric(levels(dt1$WEIGHT))[as.integer(dt1$WEIGHT) ]               
if (class(dt1$WEIGHT)=="character") dt1$WEIGHT <-as.numeric(dt1$WEIGHT)
if (class(dt1$OT)!="factor") dt1$OT<- as.factor(dt1$OT)
if (class(dt1$SC)!="factor") dt1$SC<- as.factor(dt1$SC)
if (class(dt1$IN)!="factor") dt1$IN<- as.factor(dt1$IN)
if (class(dt1$PR)!="factor") dt1$PR<- as.factor(dt1$PR)
if (class(dt1$OP)!="factor") dt1$OP<- as.factor(dt1$OP)
if (class(dt1$LG)!="factor") dt1$LG<- as.factor(dt1$LG)

# Convert Missing Values to NA for non-dates



# Here is the structure of the input data frame:
str(dt1)                            
attach(dt1)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(ID)
summary(NEW)
summary(SPECIES)
summary(PERIOD)
summary(TIME)
summary(DATE)
summary(QUADR)
summary(SEX)
summary(NO)
summary(WEIGHT)
summary(OT)
summary(SC)
summary(IN)
summary(PR)
summary(OP)
summary(LG) 
# Get more details on character variables

summary(as.factor(dt1$ID)) 
summary(as.factor(dt1$NEW)) 
summary(as.factor(dt1$SPECIES)) 
summary(as.factor(dt1$TIME)) 
summary(as.factor(dt1$QUADR)) 
summary(as.factor(dt1$SEX)) 
summary(as.factor(dt1$NO)) 
summary(as.factor(dt1$OT)) 
summary(as.factor(dt1$SC)) 
summary(as.factor(dt1$IN)) 
summary(as.factor(dt1$PR)) 
summary(as.factor(dt1$OP)) 
summary(as.factor(dt1$LG))
detach(dt1)  

################################################################################


#Read data (BT ): --------------------------------------------------------------
setwd("C:/Users/afe1/OneDrive - University of St Andrews/PHD/BioTIMEGithub")
btv1 <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIME/BioTIMENewStudiesCuration/0_BioTIMEv1Revision"
raw308 <- read.csv2(file = paste0(btv1, "/STUDY_308_AFE/raw308.csv"), h=T)
m308 <- read.csv2(file = paste0(btv1, "/STUDY_308_AFE/methods308.csv"), h=T)
sps <- read.csv2(file = paste0(btv1, "/sps.csv"), h=T)


################################################################################
# Curation:---------------------------------------------------------------------
dt <- dt1
str(dt)

# STEP 1: Sampling Event Date --------------------------------------------------
dt$YEAR <- as.integer(substr(dt$DATE, 1, 4))
sort(unique(dt$YEAR)) 

dt$MONTH <- as.integer(substr(dt$DATE, 5, 6))
sort(unique(dt$MONTH))

dt$DAY <- as.integer(substr(dt$DATE, 7, 8))
sort(unique(dt$DAY))

sum(is.na(dt$YEAR))
sum(is.na(dt$MONTH))
sum(is.na(dt$DAY))
dt[is.na(dt$YEAR),]

dt <- dt[!is.na(dt$YEAR),]

c1 <- dt %>% group_by(PERIOD) %>% distinct(YEAR, MONTH, DAY) # chronological
c1[c1$PERIOD==335,]                     # 335, 1995  01    26
#View(c1[c1$YEAR==1995 & c1$MONTH==1,]) # typo

dt$PERIOD[dt$YEAR==1995 & dt$MONTH==01 & dt$DAY==26] <- 235   # fix typo
unique(dt$PERIOD[dt$YEAR==1995 & dt$MONTH==01 & dt$DAY==26])

# Are there periods that span two years (end / beginning year)?
c2 <- dt %>% group_by(PERIOD) %>% summarise(n_distinct(YEAR)) # a few cases

sort(unique(dt$YEAR[dt$PERIOD==199])) # "1991" "1992"
#View(dt[dt$PERIOD==199,]) # 1991 is a typo, correct
dt$YEAR[dt$PERIOD==199 & dt$YEAR==1991] <- 1992

sort(unique(dt$YEAR[dt$PERIOD==248])) # "1995" "1996"
#View(dt[dt$PERIOD==248,]) # 1995 is a typo, correct
dt$YEAR[dt$PERIOD==248 & dt$YEAR==1995] <- 1996

sort(unique(dt$YEAR[dt$PERIOD==250])) # "1995" "1996"
#View(dt[dt$PERIOD==250,]) # 1995 is a typo, correct
dt$YEAR[dt$PERIOD==250 & dt$YEAR==1995] <- 1996

sort(unique(dt$YEAR[dt$PERIOD==258])) # "1996" "1997"
#View(dt[dt$PERIOD==258,]) # 1996 is a typo, correct
dt$YEAR[dt$PERIOD==258 & dt$YEAR==1996] <- 1997

sort(unique(dt$YEAR[dt$PERIOD==272])) # "1997" "1998"
#View(dt[dt$PERIOD==272,]) # 1998 is a typo, correct
dt$YEAR[dt$PERIOD==272 & dt$YEAR==1998] <- 1997

sort(unique(dt$YEAR[dt$PERIOD==30])) # "1980" "1981"
#View(dt[dt$PERIOD==30,]) # 1980 is a typo, correct
dt$YEAR[dt$PERIOD==30 & dt$YEAR==1980] <- 1981

sort(unique(dt$YEAR[dt$PERIOD==31])) # "1981" "1987"
#View(dt[dt$PERIOD==31,]) # 1987 is a typo, correct
dt$YEAR[dt$PERIOD==31 & dt$YEAR==1987] <- 1981

c3 <- dt %>% group_by(PERIOD) %>% summarise(n_distinct(YEAR)) 
c4 <- dt %>% group_by(PERIOD) %>% summarise(n_distinct(MONTH)) # happens in 21 cases
#dt[dt$PERIOD==0,]          # perhaps a typo
#dt[dt$PERIOD==9,]
#dt[dt$PERIOD==25,]
#dt[dt$PERIOD==26,]
#dt[dt$PERIOD==36,]
#View(dt[dt$PERIOD==42,])
#View(dt[dt$PERIOD==62,])
#View(dt[dt$PERIOD==97,])   # perhaps a typo
#View(dt[dt$PERIOD==62,])
#View(dt[dt$PERIOD==141,])  
#View(dt[dt$PERIOD==142,])  
#View(dt[dt$PERIOD==144,])  
#View(dt[dt$PERIOD==147,]) 
#View(dt[dt$PERIOD==178,]) 
#View(dt[dt$PERIOD==207,]) 
#View(dt[dt$PERIOD==213,]) 
#View(dt[dt$PERIOD==235,]) 
#View(dt[dt$PERIOD==238,]) 
#View(dt[dt$PERIOD==242,]) 
#View(dt[dt$PERIOD==260,]) 
#View(dt[dt$PERIOD==272,])  # perhaps a typo
#View(dt[dt$PERIOD==284,])  # perhaps a typo



# STEP 2: remove cases when observations are not identified:--------------------
dt$ID <- as.character(dt$ID)
dt$SPECIES <- as.character(dt$SPECIES)

sum(is.na(dt$SPECIES))          # 0
sort(unique(dt$SPECIES))
sum(dt$SPECIES %in% c("", "?")) # 47

sort(unique(dt$ID[dt$SPECIES %in% c("", "?")]))
sum(dt$SPECIES %in% c("", "?") & dt$ID=="")
dt <- dt[! (dt$SPECIES %in% c("", "?") & dt$ID==""),]


# STEP 3: remove recaptures of animals in the same period-----------------------

# Look at "NEW":
sum(is.na(dt1$NEW))                    # 2946 NAs
sort(unique(dt$PERIOD[is.na(dt$NEW)])) # the first 19 periods don't contain this info
unique(dt$DATE[dt$PERIOD==0])          # March April in the 1980, odd might be a typo?
unique(dt$DATE[dt$PERIOD==1])          # Sept 1979
unique(dt$DATE[dt$PERIOD==19])         # July 1980

#View(dt[dt$YEAR==1980,])              # period 0 before period 1979?
dt <- dt[!dt$PERIOD ==0,]              # rm period 0


# Check what info is in NEW:
egID285 <- subset(dt, dt$ID==2485) 
# first time an individual is measured
# but it cannnot be used as an identifier of dups within period

# Look at "ID":
sum(dt$ID=="")        # 625
sum(dt$ID=="NONE")    # 415
sum(dt$ID=="?")       # 11
sum(is.na(dt$ID))     # 0
sum(is.null(dt$ID))   # 0
nasII <- sort(unique(dt$PERIOD[dt$ID %in% c("NONE", "", "?")]))
nasII # Occur across periods.

sort(unique(dt$ID))
dt$ID <- str_replace_all(dt$ID, "[[:punct:]]", "")
dt$ID[dt$ID=="NONE"] <- ""
dt$ID[dt$ID=="?"] <- ""
dt$ID[dt$ID=="`"] <- ""
dt$ID[dt$ID=="`702"] <- "702"
dt$ID <- trimws(dt$ID)
sort(unique(dt$ID))
length(unique(dt$ID)) # 3689


################################################################################
# STEP 4, FLAG & DEAL WITH DUPS (I):--------------------------------------------
sort(unique(dt$SPECIES))
dt_NA_SP <- subset(dt, dt$SPECIES %in% c("?", "")) # 41 obs

# CASE WHEN SPECIES ARE UNAVAILABLE:--------------------------------------------
sort(unique(dt_NA_SP$YEAR))   # 1979 1980 (all in the first two years)
sort(unique(dt_NA_SP$PERIOD)) # 1 to 18 (all in the first two years)

# ID 1 & period 2:
dt[dt$ID=="1" & dt$PERIOD<=2,] # likely a duplicate
dt$SPECIES[dt$ID=="1" & dt$SPECIES=="" & dt$PERIOD==2] <- "DV"

# ID "102" in periods 16 & 18:
dt[dt$ID=="102" & dt$PERIOD<=18 & dt$SEX=="F",] # likely duplicate, similar weight as in period 17
dt$SPECIES[dt$ID=="102" & dt$SPECIES=="" & dt$PERIOD %in% c(16,18)] <- "PL"

# ID "12" in period 13:
dt[dt$ID=="12" & dt$PERIOD <=200,] # cannot determine, likley a typo in ID, remove
dt <- dt[!(dt$ID=="12" & dt$PERIOD==13),]

# ID "120" in period 18:
dt[dt$ID=="120" & dt$PERIOD <=18 & dt$SEX=="M",] # unknown species,much earlier perios than posterior obs, remove
dt[dt$ID=="120" ,] 
dt <- dt[!(dt$ID=="120" & dt$PERIOD==18),]

# ID "125" in period 13:
dt[dt$ID=="125" & dt$PERIOD <=100,] # could be PM, but the weight is very different, remove
dt <- dt[!(dt$ID=="125" & dt$PERIOD==13 & dt$SEX=="M"),]

# ID "139" in period 3:
dt[dt$ID=="139" & dt$PERIOD <=30,] # same sex, almost same weight, same quadrant even and only a few days of difference. Re-classified to "PL"
dt$SPECIES[dt$ID=="139" & dt$PERIOD==3 & dt$SEX=="M"] <- "PL"

# ID "140" in period 3:
dt[dt$ID=="140" & dt$PERIOD <=100,] # can't know the species. Remove
dt <- dt[!(dt$ID=="140" & dt$PERIOD==3),]

# ID "149" in period 4:
dt[dt$ID=="149" & dt$PERIOD <=4,] # very likely a recapture of PM, assign "PM"
dt$SPECIES[dt$ID=="149" & dt$PERIOD==4 & dt$SEX=="M" & dt$WEIGHT==17] <- "PM"

# ID "153" in period 4:
dt[dt$ID=="153" & dt$PERIOD <=10,] # very likely a recapture of TS, assign "TS"
dt$SPECIES[dt$ID=="153" & dt$PERIOD==4 & dt$SEX=="M" & dt$WEIGHT==69.5] <- "TS"

# ID "161" in period 7:
#View(dt[dt$ID=="161" & dt$PERIOD <=10,]) # likely the same female as in the 13 of the same month
dt$SPECIES[dt$ID=="161" & dt$PERIOD==7 & dt$SEX=="F" & dt$WEIGHT==13] <- "PL"

# ID "163" in periods 4 & 5:
dt[dt$ID=="163" & dt$PERIOD <=10,] # very likely the same "PL" female.
dt$SPECIES[dt$ID=="163" & dt$PERIOD %in% c(4,5) & dt$SEX=="F" & dt$WEIGHT %in% c(0,10.5)] <- "PL"

# ID "165" in period 4:
dt[dt$ID=="165" & dt$PERIOD <=50,] # cannot determine, remove
dt <- dt[!(dt$ID=="165" & dt$PERIOD==4),]

# ID "171" in period 4:
dt[dt$ID=="171" & dt$PERIOD <=10,] # cannot determine, remove
dt <- dt[!(dt$ID=="171" & dt$PERIOD==4),]

# ID "177" in period 4:
dt[dt$ID=="177" & dt$PERIOD <=10,] # assumed to be a dup of the same individual, the same day
dt$SPECIES[dt$ID=="177" & dt$PERIOD %in% c(4) & dt$SEX=="F" & dt$WEIGHT %in% c(12.5)] <- "PL"

# ID "179" in period 4:
dt[dt$ID=="179" & dt$PERIOD <=100,] # this might be a juvenile capture, seen a few months after (assigned PL)
dt$SPECIES[dt$ID=="179" & dt$PERIOD %in% c(4)] <- "PL"

# ID "180" in periods 13, 14, 15 & 17:
dt[dt$ID=="180" & dt$PERIOD <=20,] # enigmatic, very small female unclassified (remove)
dt <- dt[!(dt$ID=="180" & dt$PERIOD %in% c(13,14,15,17) & dt$SEX=="F"),]

# ID "185" in periods 13, 14 & 16:
dt[dt$ID=="185" & dt$PERIOD <=50,] # these are three extremely large ind, unidentified (remove)
dt <- dt[!(dt$ID=="185" & dt$PERIOD %in% c(13,14,16)),]

# ID 194 in period 5:
dt[dt$ID=="194" & dt$PERIOD <=20,] # assumed to be the first capture of an ind of PL
dt$SPECIES[dt$ID=="194" & dt$PERIOD %in% c(5) & dt$WEIGHT %in% c(12.0)] <- "PL"

# ID 195 in period 14:
dt[dt$ID=="195" & dt$PERIOD <=200,] # can't determine, removed
dt <- dt[!(dt$ID=="195" & dt$PERIOD %in% c(14)),]

# 29 in period 1:
dt[dt$ID=="29" & dt$PERIOD <=5,] # assumed to be "PM"
dt$SPECIES[dt$ID=="29" & dt$PERIOD %in% c(1) & dt$WEIGHT %in% c(21.0)] <- "PM"

# 51 in period 1:
dt[dt$ID=="51" & dt$PERIOD <=5,] # could be "PM" or "PL", removed
dt <- dt[!(dt$ID=="51" & dt$PERIOD %in% c(1)),]

# 62 in period 1 & 2:
dt[dt$ID=="62" & dt$PERIOD <=5,] # likely the same ind of PM, only identified later on
dt$SPECIES[dt$ID=="62" & dt$PERIOD %in% c(1, 2)] <- "PM"

# 63 in period 1 & 2:
dt[dt$ID=="63" & dt$PERIOD <=10,] # will assume female of PM
dt$SPECIES[dt$ID=="63" & dt$PERIOD %in% c(1)] <- "PM"

# 7 in period 10:
dt[dt$ID=="7" & dt$PERIOD <=200,] # cannot determine
dt <- dt[!(dt$ID=="7" & dt$PERIOD %in% c(10)),]

# 76FL1 in period 2:
dt[dt$ID=="76FL1" & dt$PERIOD <=200,] # cannot determine
dt <- dt[!(dt$ID=="76FL1" & dt$PERIOD %in% c(2)),]

# 77 in period 7:
dt[dt$ID=="77" & dt$PERIOD <=10,] # very likely PL
dt$SPECIES[dt$ID=="77" & dt$PERIOD %in% c(7) & dt$WEIGHT==19] <- "PL"

# 8 in period 10:
dt[dt$ID=="8" & dt$PERIOD <=200,] # cannot determine, extremely large ind
dt <- dt[!(dt$ID=="8" & dt$PERIOD %in% c(10)),] 

# FR3 in period 17:
dt[dt$ID=="FR3" & dt$PERIOD <=20,] # very likely CG
dt$SPECIES[dt$ID=="FR3" & dt$PERIOD %in% c(17)] <- "CG"

sum(dt$SPECIES=="") # 0, OK
sort(unique(dt$SPECIES)) # OK


# STEP 5: check weight outliers:------------------------------------------------
sum(dt$WEIGHT>500)    # 4 obs, these are likely mistakes...
sum(dt$WEIGHT>400)    # 7 obs
sum(dt$WEIGHT>300)    # 12 obs
sum(dt$WEIGHT>200)    # 26 obs
sum(dt$WEIGHT>100)    # 611 obs 

# View(dt[dt$WEIGHT>300,])

# ID 124:
# Large individuals of both NF & SC are observed in multiple periods. NO REASON TO REMOVE

# ID 189 & ID 159 of OT (one of the unidentified species). NO REASON TO REMOVE.

# Un-IDd PM of 835g. UPDATED.
# Likely is a mistake, that's the rat ID in that period (see 835 in periods 38-42)
dt$ID[dt$SPECIES=="PM" & dt$WEIGHT==835.0] <- 835
dt$WEIGHT[dt$SPECIES=="PM" & dt$WEIGHT==835.0] <- 0  # OK

# 972 Glaucomys volans in period 62 could be a mistake too, reports of the same rat in similar periods
# indicate weights of 50-60g
# Maximum weights reported online are nowhere near 500g. ASSUMED TYPO AND UPDATED.
dt$WEIGHT[dt$ID=="972" & dt$SPECIES=="GV" & dt$WEIGHT==535.0] <- 0

# FR11RR only one observation with that ID, so it could have been a typo. 
# No other observation of BB in the series comes close. 
# Moreover, the max size reported in the internet is much less than 50g. ASSUMED TYPO AND UPDATED.
dt$WEIGHT[dt$ID=="FR11RR" & dt$SPECIES=="BB" & dt$WEIGHT==311.5] <- 0

# LR1 Clethrionomys gapperi in period 86 could be a mistake in the entry,
# since the same rat is observed in the same period weighting around 30g (normal)
# Online size is also reported below 50g. ASSUMED TYPO AND UPDATED.
dt$WEIGHT[dt$ID=="LR1" & dt$SPECIES=="CG" & dt$WEIGHT==340] <- 0

# Unidentified SH in period 287:
# Can't be according to the max sizes only, this species should be very small.
dt$WEIGHT[dt$ID=="" & dt$SPECIES=="SH" & dt$WEIGHT==301.5] <- 0 # 0 since no ID available to compare



# STEP 6, FLAG & DEAL WITH DUPS (II):-------------------------------------------
# CASES WHEN ID & SPECIES ARE AVAILABLE OR ONLY SPS:----------------------------
# have a look at different identifier columns

sort(unique(dt$SEX)) # F M O
sum(dt$SEX=="")      # 173
sum(dt$SEX=="O")     # 2562, unclear what it indicates
sort(unique(dt$NO[dt$SEX=="O"])) # FALSE & TRUE

sort(unique(droplevels(dt$NEW[dt$ID==""])))  # FALSE & TRUE
sum(dt$NEW[dt$ID==""]=="TRUE", na.rm= T)     # 51
sum(dt$NEW[dt$ID==""]=="FALSE", na.rm = T)   # 945
sort(unique(dt$QUADR[dt$ID==""]))            # different cases...
sort(unique(dt$PERIOD[dt$ID==""]))           # across periods

dt$ID2 <- paste0(dt$ID,"_", dt$SPECIES,"_", dt$PERIOD)

dt <- dt %>% mutate(FLAG =ifelse(duplicated(ID2) | duplicated(ID2, fromLast = TRUE), 1, 0)) # flag all dups
dt$FLAG <- ifelse(dt$ID=="", 0, dt$FLAG) # obs with ID="" cannot be checked for dups reliably (untagged or had lost tag?)

# Remove dups with weight 0 except when none is weighted:
dtA <- dt %>% group_by(ID2) %>% filter(sum(WEIGHT)==0 & FLAG==1) # 78, subset of duplicates with weights always 0
dtB <- anti_join(dt, dtA) 


dtA <- dtA %>% distinct(ID2, .keep_all = T)               # 34, keep only one distinct obs (no weights for any obs in a period)
dtB_1 <- subset(dtB, dtB$FLAG==0)                         # 12411. non-duplicated observations
dtB_2 <- subset(dtB, dtB$FLAG==1)                         # 28787. obs with dups & at least one reported weight per period
range(dtB_2$WEIGHT)                                       # 0-413

dtB_2 <- dtB_2 %>%
  group_by(ID2) %>%
  mutate(meanWEIGHT = mean(WEIGHT[WEIGHT != 0]))%>%          # mean across values larger than 0
  distinct(ID2, .keep_all = T)                               # rm dups & keep mean weight

dtA$meanWEIGHT <- dtA$WEIGHT
dtB_1$meanWEIGHT <- dtB_1$WEIGHT
dtnew <- rbind(dtA, dtB_1, dtB_2) # 22915 obs
range(dtnew$meanWEIGHT)           # 0 to 630
sum(dtnew$meanWEIGHT==0)          # 1256 


# STEP 6: Final aggregation by period:------------------------------------------

# NOTE: the total size of the sampling location is 1 ha (i.e. one football field)
# Quadrants are only the way in which mammals are sampled within the ha, so 
# they shouldn't be added in sample desc. QUADR== position on grid.

dtnew$ABUNDANCE <- rep(1, nrow(dtnew))
sum(dtnew$meanWEIGHT==0)                        # 1256
1256/22915*100                                  # 6% of the data
sort(unique(dtnew$PERIOD[dtnew$meanWEIGHT==0])) # occur in many periods.


# STEP 7: Assign species names:-------------------------------------------------
sort(unique(dtnew$SPECIES))
dtnew$SPECIES <- as.character(dtnew$SPECIES)
dtnew$SPECIES[dtnew$SPECIES=="BB"] <- "Blarina brevicauda"
dtnew$SPECIES[dtnew$SPECIES=="CG"] <- "Clethrionomys gapperi"
dtnew$SPECIES[dtnew$SPECIES=="DV"] <- "Didelphis virginiana"
dtnew$SPECIES[dtnew$SPECIES=="GV"] <- "Glaucomys volans"

dtnew$SPECIES[dtnew$SPECIES=="MF"] <- "Mustela frenata"
dtnew$SPECIES[dtnew$SPECIES=="MM"] <- "Marmota monax"
dtnew$SPECIES[dtnew$SPECIES=="NI"] <- "Neozapus insignis"
dtnew$SPECIES[dtnew$SPECIES=="NM"] <- "Neotoma magister"

dtnew$SPECIES[dtnew$SPECIES=="PL"] <- "Peromyscus leucopus"
dtnew$SPECIES[dtnew$SPECIES=="PM"] <- "Peromyscus maniculatus"
dtnew$SPECIES[dtnew$SPECIES=="SC"] <- "Sorex cinereus"
dtnew$SPECIES[dtnew$SPECIES=="SD"] <- "Sorex dispar"

dtnew$SPECIES[dtnew$SPECIES=="SF"] <- "Sorex fumeus"
dtnew$SPECIES[dtnew$SPECIES=="SH"] <- "Sorex hoyi"
dtnew$SPECIES[dtnew$SPECIES=="TS"] <- "Tamias striatus"

sort(unique(dtnew$SPECIES))

sum(dtnew$SPECIES %in% c("MP", "NF", "OT", "ZH"))                # 74 (cannot find the guide for these)
dtnew <- dtnew[!dtnew$SPECIES %in% c("MP", "NF", "OT", "ZH"),]   # rm


# STEP 8: Coordinates:----------------------------------------------------------

dtnew$LATITUDE <- rep(40.1707412093728, nrow(dtnew))
dtnew$LONGITUDE <- rep(-79.2601776123047, nrow(dtnew))

world_map <- map_data("world") 
world <- ggplot() + coord_fixed() +xlab("") + ylab("") 
world  <- world  + 
  geom_polygon(data=world_map, aes(x=long, y=lat, group=group), 
               colour="gray60", fill="gray60") 
points <- world  + 
  geom_point(data=dtnew, 
             aes(x=LONGITUDE, y=LATITUDE, alpha=0.01)) 
points # OK


# STEP 9: aggregate the data: --------------------------------------------------
sum(dtnew$meanWEIGHT==0)           # 1223
dtnew$meanWEIGHT[dtnew$meanWEIGHT==0] <- NA
range(dtnew$meanWEIGHT, na.rm=T)   # 1 413

c5 <- dtnew %>% group_by(PERIOD, SPECIES) %>% summarize(count_na = sum(is.na(meanWEIGHT)))
sum(c5$count_na >0)                # 651

c6 <- dtnew %>% group_by(PERIOD, YEAR) %>% summarize(nMonth=n_distinct(MONTH))
c7 <- dtnew %>% group_by(PERIOD, YEAR) %>% distinct(MONTH)
dtnew <- dtnew %>% group_by(PERIOD, YEAR) %>% mutate(Month = first(MONTH))
c7B <- dtnew %>% group_by(PERIOD, YEAR) %>% distinct(Month)


dtnew2 <- dtnew %>% group_by(SPECIES, PERIOD, LATITUDE, LONGITUDE, Month, YEAR) %>%
  summarise(Abundance=sum(ABUNDANCE), Biomass=sum(meanWEIGHT))     # 2017
sum(is.na(dtnew2$Biomass))         # 651, OK

names(dtnew2)
names(dtnew2) <- c("Species","Period","Latitude","Longitude", "Month", "Year", "Abundance","Biomass")

dtnew2$Family <- rep(NA, nrow(dtnew2))
dtnew2$Genus <- str_split_fixed(dtnew2$Species, " ", 2) [,1]
unique(dtnew2$Genus)
dtnew2$Species <- str_split_fixed(dtnew2$Species, " ", 2) [,2]
unique(dtnew2$Species)

dtnew2 <- dtnew2 %>% relocate(c(Abundance, Biomass, Family, Genus), .before=Species)

dtnew2$SampleDescription <- dtnew2$Period
dtnew2$Plot <- rep("Powdermill Biological Station", nrow(dtnew2))
dtnew2 <- dtnew2 %>% relocate(c(SampleDescription, Plot), .after=Species)

dtnew2$DepthElevation <- rep(450, nrow(dtnew2)) # from metadata, in m
dtnew2$Day <- rep(NA, nrow(dtnew2))

dtnew2 <- dtnew2 %>% relocate(c(DepthElevation, Day), .after=Longitude)
dtnew2 <-within(dtnew2, rm(Period))
dtnew2$StudyID <- rep(302, nrow(dtnew2))
dtnew2 <- as.data.frame(dtnew2)
str(dtnew2)

# Write csv ====================================================================
rawData <- dtnew2
path <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIMEGithubMetadatas/Study_308_AFE"
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
  coord_sf(xlim = c(-80,-60), ylim = c(30,50)) + 
  labs(x = NULL, y = NULL) +
  theme_minimal()

# End of script ################################################################
