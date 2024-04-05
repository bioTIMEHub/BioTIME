################################################################################
# Study title: Update Study 521
# Curator: AFE
# Date: February 2023
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
library(rgdal)


# main source: https://zslpublications.onlinelibrary.wiley.com/doi/10.1111/j.1469-1795.2008.00194.x


# Read Data (formmated already, only a few fixes needed):
files_dir <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIME/BioTIMENewStudiesCuration/0_BioTIMEv1Revision/STUDY_531_AFE"

dt <- read.csv(paste0(files_dir, "/study521_observations.csv"), h=T)

sort(unique(dt$PLOT))

c1 <- dt %>% group_by(PLOT) %>% summarise(nY=n_distinct(YEAR))
toRm <- unique(c1$PLOT[c1$nY==1])
dt <- dt[!dt$PLOT %in% toRm, ]
dt$PLOT <- NA # these are not permanent plots

sort(unique(dt$family))
sort(unique(dt$valid_name))
dt$SPECIES <- str_split_fixed(dt$valid_name, " ", 2)[,2]
sort(unique(dt$SPECIES))

names(dt)
names(dt)[names(dt)=="family"] <- "FAMILY"
str(dt)
dt <- dt %>% relocate(c(FAMILY, GENUS, SPECIES), .after=BIOMASS)
dt <- within(dt, rm(valid_name, newID))


range(dt$ABUNDANCE)

# Save:-------------------------------------------------------------------------
pathfinal <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIMEGithubMetadatas/STUDY_531_AFE"
write.csv(dt, file=paste0(pathfinal, "/dt.csv"), row.names = F)  

