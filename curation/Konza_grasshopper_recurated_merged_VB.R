### Biotime v1 fix of Konza grasshoppers
## Curator: VB
## datasets split in amend but remerged using treatment info
## Ago 2023

library(clipr)
library(readxl)
library(tidyverse)


rm(list=ls())
setwd("~/Documents/BioTIME/amend")

dt_1 <- read_excel("Konza grasshoppers CGR022 n20b.xlsx", sheet = "rawData") 
dt_2 <- read_excel("Konza grasshoppers CGR022 n20a.xlsx", sheet = "rawData") 
dt_3 <- read_excel("Konza grasshoppers CGR022 n04d.xlsx", sheet = "rawData") 
dt_4 <- read_excel("Konza grasshoppers CGR022 n04a.xlsx", sheet = "rawData") 
dt_5 <- read_excel("Konza grasshoppers CGR022 n01b.xlsx", sheet = "rawData") 
dt_6 <- read_excel("Konza grasshoppers CGR022 n01a.xlsx", sheet = "rawData") 
dt_7 <- read_excel("Konza grasshoppers CGR022 020b.xlsx", sheet = "rawData") 
dt_8 <- read_excel("Konza grasshoppers CGR022 004f.xlsx", sheet = "rawData") 
dt_9 <- read_excel("Konza grasshoppers CGR022 002d.xlsx", sheet = "rawData") 
dt_10 <- read_excel("Konza grasshoppers CGR022 002c.xlsx", sheet = "rawData") 
dt_11 <- read_excel("Konza grasshoppers CGR022 001d.xlsx", sheet = "rawData") 
dt_12 <- read_excel("Konza grasshoppers CGR022 0sub.xlsx", sheet = "rawData") 
dt_13 <- read_excel("Konza grasshoppers CGR022 0spb.xlsx", sheet = "rawData")
dt_14 <- read_excel("Konza grasshoppers CGR022 004b.xlsx", sheet = "rawData") 

dt <- rbind (dt_1,dt_2,dt_3,dt_4,dt_5,dt_6,dt_7,dt_8,dt_9,dt_10,dt_11,dt_12,dt_13,dt_14)

dt$SampleDescription<- paste(dt$Plot,dt$Year, dt$Month, dt$Day, sep = "_")

dt$Plot <- NA
summary(dt)

write.csv(dt,"Konza_grasshopper_recurated_merged_VB.csv")
write_clip(dt)
