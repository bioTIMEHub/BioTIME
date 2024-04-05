# Dataset: CERN_Heilongjiang_Sanjiang_brids
# Location: Sanjiang, Heilongjiang province, China
# Curator: Wubing Xu
# Date: 29-06-2023

rm(list = ls())

setwd("C:/Dropbox/iDiv/Data/BTDataCuration_2023/wubing/CERN_Heilongjiang_Sanjiang_birds_WBX")

require(tidyverse)
require(readxl)


# read original data
dat <- read_excel('CERN_Heilongjiang_Sanjiang_birds_original_WBX/实体数据/2-SWSJ_NIAOLEIZL.xls', sheet=1, skip = 4 )
colnames(dat) <- c("Year", "Month", "Day", "species_cn", "bird_type", "Abundance")

# as the original data only provide Chinese species names, the scientific names is going to be found manualy
species <- data.frame(species_cn = unique(dat$species_cn), species = "NA")
write_excel_csv(species, file = "CERN_Heilongjiang_Sanjiang_birds_findSpeciesName.csv")
species <- read_excel('CERN_Heilongjiang_Sanjiang_birds_findSpeciesName_manual.xlsx', sheet=1)

# match scientific names to the assemblage data
dat <- dat %>% left_join(species)

# split genus and species and check these naems
dat$Genus <- word(dat$species, 1)
dat$Species <- word(dat$species, start=2, end=-1)

sort(unique(dat$Genus))


# add coordinates
# coordinates of plot 1
char2dms("133d31'E") %>% as.numeric()
char2dms("47d35'N") %>% as.numeric()

dat <- dat %>%
  mutate(Latitude = 47.58333,
         Longitude = 133.5167)


## prepare the output file
# sum abundance for each sampling event
dat_out <- dat %>% 
  # add sample description
  mutate(SampleDescription = paste(Year, Month, Day, sep = "_")) %>%
  select(- c(species_cn, species, bird_type)) %>%
  # sum abundance for each sampling event
  group_by(Year, Month, Day, Genus, Species, Latitude, Longitude, SampleDescription) %>%
  summarise(Abundance = sum(Abundance)) %>%
  ungroup()
  
# add required columns for BioTIME
dat_out <- dat_out %>%
  mutate(Family  = "",
         Biomass = "",
         Plot = "",
         DepthElevation = "",
         StudyID = "")

# rename and relocate columns
dat_out <- dat_out %>%
  relocate(Abundance,	Biomass, Family, Genus, Species, SampleDescription, Plot, Latitude, Longitude,
           DepthElevation, Day,	Month, Year, StudyID)


# output the clean raw data
write_excel_csv(dat_out, file = "CERN_Heilongjiang_Sanjiang_birds_WBX_rawdata.csv")
