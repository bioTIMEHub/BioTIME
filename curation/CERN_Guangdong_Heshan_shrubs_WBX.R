# Dataset: CERN_Guangdong_Heshan_shrubs
# Location: Heshan, Guangdong province, China
# Curator: Wubing Xu
# Date: 29-06-2023

rm(list = ls())

setwd("C:/Dropbox/iDiv/Data/BTDataCuration_2023/wubing/CERN Guangdong Heshan_shrubs_WBX")

require(tidyverse)
require(readxl)
require(sp)

# read original data in two plot. Plot 1 located in native forests, while plot 2 located in coniferous forests
dat_plot1 <- read_excel('CERN Guangdong Heshan_shrubs_original_WBX/实体数据/鹤山生物数据_灌木层植物种组成.xlsx', sheet=2, range = "A2:C56")
dat_plot2 <- read_excel('CERN Guangdong Heshan_shrubs_original_WBX/实体数据/鹤山生物数据_灌木层植物种组成.xlsx', sheet=3, range = "A2:C48")

# combine data and rename columns
dat <- bind_rows(dat_plot1, dat_plot2) %>%
  mutate(Plot = c(rep("site1", nrow(dat_plot1)), rep("site2", nrow(dat_plot2))))

colnames(dat) <- c("Year", "species_cn", "Abundance", "Plot")


## match Chinese names to scientific names 
# species list
species <- read_excel('CERN Guangdong Heshan_shrubs_original_WBX/实体数据/鹤山生物数据_动植物名录.xlsx', sheet=3)
colnames(species) <- c("species_cn", "species")
species <- species[!is.na(species$species_cn), ]

# match Chinese and scientific species names
dat <- left_join(dat, species)

# some names were not matched. Add scientific names manually
species_nomatched <- dat %>% filter(is.na(species)) %>% distinct(species_cn)
species_manual <- data.frame(species_cn = species_nomatched$species_cn,
                             species_manual = c("Alangium chinense", "Cardiospermum halicacabum", "Gardenia jasminoides", 
                                         "Litsea glutinosa", "Glochidion puberum", "Urena lobata", 
                                         "Toxicodendron succedaneum", "Litsea glutinosa", "Toxicodendron vernicifluum", 
                                         "Ligustrum sinense", "Bridelia tomentosa", "Camphora officinarum"))

dat <- left_join(dat, species_manual) %>%
  mutate(species = ifelse(is.na(species), species_manual, species)) %>%
  select(- species_manual)


# split genus and species and check these names
dat$Genus <- word(dat$species, 1)
dat$Species <- word(dat$species, 2)
sort(unique(dat$Genus))


# add coordinates
# coordinates of site 1
char2dms("112d54'2.5\"E") %>% as.numeric()
char2dms("22d40'45\"N") %>% as.numeric()

# coordinates of site 2
char2dms("112d54'1.5\"E") %>% as.numeric()
char2dms("22d40'52.5\"N") %>% as.numeric()

dat <- dat %>%
  mutate(Latitude = ifelse(Plot == "site1", 22.67917, 22.68125),
         Longitude = ifelse(Plot == "site1", 112.9007, 112.9004))


## prepare the output file
# sum abundance for each sampling event; no abundance were summed
dat_out <- dat %>% 
  # add sample description
  mutate(SampleDescription = paste(Year, Plot, sep = "_")) %>%
  select(- c(species_cn, species)) %>%
  # sum abundance for each sampling event
  group_by(Year, Plot, Genus, Species, Latitude, Longitude, SampleDescription) %>%
  summarise(Abundance = sum(Abundance)) %>%
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
write_excel_csv(dat_out, file = "CERN_Guangdong_Heshan_shrubs_WBX_rawdata.csv")
