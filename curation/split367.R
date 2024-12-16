################################################################################
# Revision of study 367
# AFE Nov 2024
################################################################################


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
library(data.table)

# Read Data: -------------------------------------------------------------------

dt <- read.csv("C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIME/BioTIMENewStudiesCuration/RecurateOct2024/SecurityCopies/Study_367.csv")


# Revise SampleDesc: -----------------------------------------------------------

dt$Site <- str_split_fixed(dt$SAMPLE_DESC, "_", 2)[,1]
sort(unique(dt$Site))     # 8 Sites, OK
sort(unique(dt$LATITUDE)) # Lots, OK

dt$Area <- str_split_fixed(dt$SAMPLE_DESC, "_", 6)[,5]
sort(unique(dt$Area))     
table(dt$Area)

dt$Point<- str_split_fixed(dt$SAMPLE_DESC, "_", 7)[,6]
sort(unique(dt$Point)) # 8 Sites, OK
table(dt$Point)

dt$SqFrame<- str_split_fixed(dt$SAMPLE_DESC, "_", 8)[,7]
sort(unique(dt$SqFrame))


c1 <- dt %>% group_by(Site, Area, Point) %>% summarise(n=n_distinct(SqFrame)) # Always 5, Great!

c2 <- dt %>% group_by(Site, Area, Point, SqFrame) %>% summarise(nLat=n_distinct(LATITUDE),
                                                                nLong=n_distinct(LONGITUDE))

c3 <- dt %>% group_by(DAY, MONTH, YEAR, Site, Area, Point, SqFrame) %>% summarise(nLat=n_distinct(LATITUDE),
                                                                nLong=n_distinct(LONGITUDE)) # Always 1, but coords may vary by year (OK)


# NEW DESC: --------------------------------------------------------------------
dt$Fauna <- str_split_fixed(dt$SAMPLE_DESC, "_", 10)[,10]
names(dt)[names(dt)=="SAMPLE_DESC"] <- "SAMPLE_DESC_Old"
dt$SAMPLE_DESC <- paste0(dt$YEAR, "_", dt$MONTH, "_", dt$DAY, "_",
                         dt$Site, "_", dt$Area, "_", dt$Point, "_", dt$SqFrame)
length(unique(dt$SAMPLE_DESC_Old))
length(unique(paste0(dt$SAMPLE_DESC, "_", dt$Fauna))) # idem, OK


dt <- within(dt, rm(Site, Area, Point, SqFrame, SAMPLE_DESC_Old))
names(dt)

dt <- dt %>% relocate(SAMPLE_DESC, .before=PLOT)
dt$PLOT <- rep(NA, nrow(dt))
str(dt)


range(dt$ABUNDANCE)
range(dt$BIOMASS, na.rm=T)



# SPLIT DATA:------------------------------------------------------------------- 

sort(unique(dt$Fauna)) # ok

id367 <- dt[dt$Fauna == "epifauna",]
id917 <- dt[dt$Fauna == "infauna",]

id367 <- within(id367, rm(Fauna))
id917 <- within(id917, rm(Fauna))


# NOTE further updates of cent Lat & Long, etc done directly in the DataBase



# WRITE.csv: -------------------------------------------------------------------
path <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIME/BioTIMENewStudiesCuration/RecurateOct2024/id367"
write.csv(id367, file=paste0(path, "/367Split_new367.csv"), row.names = F)
write.csv(id917, file=paste0(path, "/367Split_new917.csv"), row.names = F)


# End of Script ################################################################







