# Dataset: CERN_Sichuan_Gonggar_Mountain_trees
# Location: Gonggar Mountain, Sichuan province, China
# Curator: Wubing Xu
# Date: 02-07-2023

rm(list = ls())

setwd("C:/Dropbox/iDiv/Data/BTDataCuration_2023/wubing/CERN_Sichuan_Gonggar_Mountain_trees_WBX")

require(tidyverse)
require(readxl)
require(sp)
library(plantlist) 

# read original data of 2 plots in size 30*40m
dat_plot1 <- read_excel('CERN_Sichuan_Gonggar_Mountain_trees_original_WBX/实体数据/贡嘎山生物数据_乔木层植物种组成.xlsx', sheet= 2, skip = 1)
colnames(dat_plot1) <- c("Year", "species_cn", "Abundance", "DBH", "Height")

dat_plot2 <- read_excel('CERN_Sichuan_Gonggar_Mountain_trees_original_WBX/实体数据/贡嘎山生物数据_乔木层植物种组成.xlsx', sheet= 4, skip = 1)
colnames(dat_plot2) <- c("Year", "species_cn", "Abundance", "DBH", "Height")
# note: there is another plot, but its size is 50*50m and thus was not included here 

# combine data
dat <- bind_rows(dat_plot1, dat_plot2) %>%
  mutate(Plot = c(rep(1, nrow(dat_plot1)), rep(2, nrow(dat_plot2))))


## match Chinese names to scientific names 
# species list
species_list <- read_excel('CERN_Sichuan_Gonggar_Mountain_trees_original_WBX/实体数据/贡嘎山生物数据_动植物名录.xlsx', 
                     sheet= 1, range = "A78:B155")
colnames(species_list) <- c("species_cn", "species")

# match Chinese and scientific species names
dat <- left_join(dat, species_list)

# all species has a scientific name 
dat %>% filter(is.na(species))

# split genus and species and check these names
dat$Genus <- word(dat$species, 1)
dat$Species <- word(dat$species, 2)
sort(unique(dat$Genus))


# add coordinates
# coordinates of plot 1
char2dms("101d59'54\"E") %>% as.numeric()
char2dms("29d34'34\"N") %>% as.numeric()

# coordinates of plot 2
char2dms("101d59'42\"E") %>% as.numeric()
char2dms("29d34'33\"N") %>% as.numeric()

dat <- dat %>%
  mutate(Latitude = ifelse(Plot == 1, 29.57611, 29.57583),
         Longitude = ifelse(Plot == 1, 101.9983, 101.995))


## prepare the output file
# sum abundance for each sampling event; no abundance was summed
dat_out <- dat %>% 
  # add sample description
  mutate(SampleDescription = paste(Year, Plot, sep = "_")) %>%
  select(- c(species_cn, DBH, Height, species)) %>%
  # sum abundance for each sampling event
  group_by(Year, Plot, Genus, Species, Latitude, Longitude, SampleDescription) %>%
  summarise(Abundance = sum(Abundance, na.rm = TRUE)) %>%
  ungroup()
  
# add required columns for BioTIME
dat_out <- dat_out %>%
  mutate(Family  = "",
         Biomass = "",
         DepthElevation = "",
         Day = "",
         Month  = "",
         StudyID = "")

# rename and relocate columns
dat_out <- dat_out %>%
  relocate(Abundance,	Biomass, Family, Genus, Species, SampleDescription, Plot, Latitude, Longitude,
           DepthElevation, Day,	Month, Year, StudyID)


# output the clean raw data
write_excel_csv(dat_out, file = "CERN_Sichuan_Gonggar_Mountain_trees_WBX_rawdata.csv")
