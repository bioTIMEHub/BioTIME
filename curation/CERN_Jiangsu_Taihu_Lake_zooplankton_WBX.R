# Dataset: CERN_Jiangsu_Taihu_Lake_zooplankton
# Location: Taihu lake, Jiangsu province, China
# Curator: Wubing Xu
# Date: 05-07-2023

rm(list = ls())

setwd("C:/Dropbox/iDiv/Data/BTDataCuration_2023/wubing/CERN_Jiangsu_Taihu_Lake_zooplankton_WBX")

require(tidyverse)
require(readxl)
require(sp)
library(plantlist) 


# read original data
# there are eight sites and each sites was surveyed monthly through 1997 to 2002.
# to simply data reading, the original data were combined into a new excel
dat <- read_excel('CERN_Jiangsu_Taihu_Lake_zooplankton_original_WBX/Combined_data_from_original_tables_manually.xlsx', sheet= 1)

# reshape the data structure
dat <- dat %>% 
  pivot_longer(cols= 2:13,
               names_to='Month',
               values_to='Biomass')

dat <- dat %>% 
  filter(!is.na(Biomass)) %>%
  mutate(Month = sub(pattern = "月", replacement = "", Month),
         Month = as.numeric(Month))


## match Chinese names to scientific names 
# species list
species_list <- read_excel('CERN_Jiangsu_Taihu_Lake_zooplankton_original_WBX/实体数据/THL_SWSJ_FUYOUDONGWU0003.xlsx', 
                           sheet= 1, skip = 3)

# split Chinese species names and scientific names
species_list <- tibble(species_list, species_cn = "", species = "")
for(i in 1:nrow(species_list)){
  x <- unlist(strsplit(species_list$动物名称拉丁文[i], split = " "))
  species_list$species_cn[i] <-  x[1]
  species_list$species[i] <-  paste(x[x != ""][-1], collapse = " ")
}
species_list[, 1] <- NULL

# match Chinese and scientific species names
dat <- left_join(dat, species_list)

# some names were not matched. Add scientific names manually
species_nomatched <- dat %>% filter(is.na(species)) %>% distinct(species_cn)

write_excel_csv(species_nomatched, file = "CERN_Jiangsu_Taihu_Lake_zooplankton_findSpeciesName.csv")
species_nomatched <- read_excel("CERN_Jiangsu_Taihu_Lake_zooplankton_findSpeciesName_manual.xlsx", sheet=1)

dat <- dat %>% 
  left_join(species_nomatched) %>%
  mutate(species = ifelse(is.na(species), species_manual, species)) %>%
  select(- species_manual)


# split genus and species and check these names
dat$Genus <- word(dat$species, 1)
dat$Species <- word(dat$species, 2)
sort(unique(dat$Genus))


# add coordinates
site_coordinates <- tibble(Site = c("THL00", "THL01", "THL03", "THL4",  "THL05", "THL06", "THL07", "THL08"), 
                           Latitude = c(31.53968, 31.5131, 31.47633, 31.4360, 31.41117, 31.50383, 31.33833, 31.24816), 
                           Longitude = c(120.21944, 120.19067, 120.19433, 120.18796, 120.18733, 120.13117, 120.18017, 120.17062))

dat <- dat %>% left_join(site_coordinates)


## prepare the output file
# sum abundance for each sampling event
dat_out <- dat %>% 
  # add sample description
  mutate(SampleDescription = paste(Year, Month, Site, sep = "_")) %>%
  select(- c(species_cn, taxon, species)) %>%
  # sum abundance for each sampling event
  group_by(Year, Month, Site, Genus, Species, Latitude, Longitude, SampleDescription) %>%
  summarise(Biomass = sum(Biomass, na.rm = TRUE)) %>%
  ungroup()
  
# add required columns for BioTIME
dat_out <- dat_out %>%
  mutate(Family  = "",
         Abundance = "",
         DepthElevation = "",
         Day = "",
         StudyID = "")

# rename and relocate columns
dat_out <- dat_out %>%
  relocate(Abundance,	Biomass, Family, Genus, Species, SampleDescription, Plot = Site, Latitude, Longitude,
           DepthElevation, Day,	Month, Year, StudyID)


# output the clean raw data
write_excel_csv(dat_out, file = "CERN_Jiangsu_Taihu_Lake_zooplankton_WBX_rawdata.csv")
