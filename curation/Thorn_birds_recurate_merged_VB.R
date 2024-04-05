### Biotime v1 fix of Thorn birds
## Curator: VB
## datasets split in amend but remerged using treatment info
## Ago 2023

library(clipr)
library(readxl)
library(tidyverse)


rm(list=ls())

setwd("~/Documents/BioTIME/amend")
dt_ul <- read_excel("BioTIMETemplate_birds_unlogged.xlsx", sheet = "rawData") 
dt_l <- read_excel("BioTIMETemplate_birds_logged.xlsx", sheet = "rawData") 
length(unique(dt_l$Plot))
length(unique(dt_l$Latitude))
length(unique(dt_l$Longitude))
length(unique(dt_ul$Plot))
length(unique(dt_ul$Latitude))
length(unique(dt_ul$Longitude))

unique(dt_l$Plot) %in% unique(dt_ul$Plot)
dt_l$treat <- "logged"
dt_ul$treat <- "unlogged"

dt <- rbind (dt_l,dt_ul)

dt$SampleDescription<- paste(dt$treat,dt$Plot,dt$Year, sep = "_")

dt$Plot <- NA
dt <- dt[,1:14]
summary(dt)

write.csv(dt,"Thorn_birds_recurate_merged_VB.csv")
write_clip(dt)
