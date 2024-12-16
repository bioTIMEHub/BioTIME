################################################################################
# Revision of study 501
# AFE Oct 2024
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

dt <- read.csv("C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIME/BioTIMENewStudiesCuration/RecurateOct2024/SecurityCopies/Study_501.csv")


# Edit & Split: ----------------------------------------------------------------
dim(dt)

range(dt$ABUNDANCE)
sum(dt$ABUNDANCE==0) # 6
#View(dt[dt$ABUNDANCE==0,])
range(dt$BIOMASS, na.rm=T)

dt <- dt[!dt$ABUNDANCE==0,] # rm six records of 0 abundance (AFE confirmed these were blanks in rawdata sent by author)

range(dt$DEPTH)
dt$DEPTH <- dt$DEPTH*(-1)   # change depth to negative
range(dt$DEPTH)


sort(unique(dt$PLOT))
dt$SAMPLE_DESC[1]
dt$Station <- str_split_fixed(dt$SAMPLE_DESC, "_", 6)[,6]
identical(dt$Station, dt$PLOT) # TRUE, OK

dt$PLOT <- rep(NA, nrow(dt))   # rm because it's not a fixed plot


# modify SAMPLE_DESC:
dt$dataset <- ifelse(dt$Station %like% "long", "long", "short") # to split later
table(dt$dataset)


dt$Station <- gsub("long", "", dt$Station)
dt$Station <- gsub("short", "", dt$Station)
sort(unique(dt$Station))

c1 <- dt %>% group_by(DAY, MONTH, YEAR, Station, dataset) %>% summarise(n=n_distinct(DEPTH))
range(c1$n) # Always 1, Depth does not need to be in SAMPLE_DESC


dt$SAMPLE_DESC <- paste0(dt$LATITUDE, "_", dt$LONGITUDE, "_",
                         dt$YEAR, "_", dt$MONTH, "_", dt$DAY, "_",
                         dt$Station)
dt <- within(dt, rm(Station))

dt1 <- subset(dt, dt$dataset=="long")  # LONG
dt2 <- subset(dt, dt$dataset=="short") # SHORT

dt1 <- within(dt1, rm(dataset))
dt2 <- within(dt2, rm(dataset))

dt2$STUDY_ID <- rep(916, nrow(dt2)) # new available ID in BioTIME


path <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIME/BioTIMENewStudiesCuration/RecurateOct2024/id501"
write.csv(dt1, file=paste0(path, "/501Split_Long.csv"), row.names = F)
write.csv(dt2, file=paste0(path, "/501Split_Short_new916.csv"), row.names = F)


# End of Script ################################################################


