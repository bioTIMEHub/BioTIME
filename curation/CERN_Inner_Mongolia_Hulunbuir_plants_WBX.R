# Dataset: CERN_Inner_Mongolia_Hulunbuir_plants
# Location: Hulunbuir, Inner Mongolia province, China
# Curator: Wubing Xu
# Date: 02-07-2023

rm(list = ls())

setwd("C:/Dropbox/iDiv/Data/BTDataCuration_2023/wubing/CERN_Inner_Mongolia_Hulunbuir_plants_WBX")

require(tidyverse)
require(readxl)
require(sp)
library(plantlist) 

# read original data of the year 2006
dat_2006 <- read_excel('CERN_Inner_Mongolia_Hulunbuir_plants_original_WBX/实体数据/HLG_SWSJ_植物群落种类组成、群落特征及植物名录.xls', 
                        sheet=" 2006年辅助观测场群落种类组成", skip = 1)
dat_2006[, 4:5] <- NULL
colnames(dat_2006) <- c("Plot", "date", "species_cn", "Abundance", "Biomass_fresh", "Biomass_dry")
dat_2006$Abundance <- as.numeric(dat_2006$Abundance)
dat_2006$Biomass_fresh <- as.numeric(dat_2006$Biomass_fresh)
dat_2006$Biomass_dry <- as.numeric(dat_2006$Biomass_dry) # 54 of 573 rows are NA because the biomass is very low and was not measured
dat_2006 <- dat_2006 %>% filter(!species_cn %in% c("杂类草", "枯落物"))

# read original data of the year 2007
dat_2007 <- read_excel('CERN_Inner_Mongolia_Hulunbuir_plants_original_WBX/实体数据/HLG_SWSJ_植物群落种类组成、群落特征及植物名录.xls', 
                       sheet="2007年主要观测场群落种类组成", skip = 1)
dat_2007[, 4:5] <- NULL
colnames(dat_2007) <- c("Plot", "date", "species_cn", "Abundance", "Biomass_fresh", "Biomass_dry")
dat_2007$Abundance <- as.numeric(dat_2007$Abundance)
dat_2007$Biomass_fresh <- as.numeric(dat_2007$Biomass_fresh)
dat_2007$Biomass_dry <- as.numeric(dat_2007$Biomass_dry) # 226 of 1603 rows are NA because the biomass is very low and was not measured
dat_2007<- dat_2007 %>% filter(!species_cn %in% c("杂类草", "枯落物"))
dat_2007 <- dat_2007 %>% filter(Plot %in% 1:5) # as years 2006 and 2008 only have data in plot 1:5, keep these plot data for year 2007


# read original data of the year 2008
dat_2008 <- read_excel('CERN_Inner_Mongolia_Hulunbuir_plants_original_WBX/实体数据/HLG_SWSJ_植物群落种类组成、群落特征及植物名录.xls', 
                       sheet="2008年主要观测场群落种类组成", skip = 1)
dat_2008[, 4:5] <- NULL
colnames(dat_2008) <- c("Plot", "date", "species_cn", "Abundance", "Biomass_fresh", "Biomass_dry")
dat_2008$Biomass_fresh <- as.numeric(dat_2008$Biomass_fresh)
dat_2008$Biomass_dry <- as.numeric(dat_2008$Biomass_dry) # 76 of 964 rows are NA because the biomass is very low and was not measured
dat_2008<- dat_2008 %>% filter(!species_cn %in% c("杂类草", "枯落物", "立枯"))

# combine data
dat <- bind_rows(dat_2006, dat_2007, dat_2008) %>%
  mutate(Year = c(rep(2006, nrow(dat_2006)), rep(2007, nrow(dat_2007)), rep(2008, nrow(dat_2008))))


# as some species have very low biomass and no measurement of dry biomass, fill them based on fresh biomass and using the minimum measurement value
# the ratio of dry to fresh biomass: the mean value is 0.5
summary(dat$Biomass_dry/dat$Biomass_fresh)
# how many rows are NA in biomass
dat %>% filter(is.na(Biomass_dry)) # 279 of 2598 rows
dat %>% filter(!is.na(Biomass_fresh) & is.na(Biomass_dry)) # 94 of 2598 rows
# fill dry biomass based on fresh biomass
dat <- dat %>% mutate(Biomass_dry = ifelse(is.na(Biomass_dry), 0.5*Biomass_fresh, Biomass_dry))
# for other rows, fill them using the minimum measurement value (0.02 g)
dat <- dat %>% mutate(Biomass_dry = ifelse(is.na(Biomass_dry), min(dat$Biomass_dry, na.rm = TRUE), Biomass_dry)) # 0.02g

# 81 rows of abundance are NA
dat %>% filter(is.na(Abundance)) 


## columns of Day and Month
dat <- dat %>% 
  mutate(Month = as.numeric(substring(date , 2, 2)),
         Day = as.numeric(substring(date , 4, 5)))


## match Chinese names to scientific names 
# species list
species_list <- read_excel('CERN_Inner_Mongolia_Hulunbuir_plants_original_WBX/实体数据/HLG_SWSJ_植物群落种类组成、群落特征及植物名录.xls', 
                     sheet="植物名录", skip = 1)
colnames(species_list) <- c("family_cn", "genus_cn", "species_cn", "species")

# species names in assemblage data
species <- tibble(species_cn = unique(dat$species_cn))
species <- left_join(species, species_list)

# some names were not matched. Add scientific names manually
species_nomatched <- species %>% filter(is.na(species))
species_CTPL <- plantlist::CTPL(species_nomatched$species_cn)
species_nomatched$family_cn <- species_CTPL$FAMILY_CN
species_nomatched$genus_cn <- species_CTPL$GENUS_CN
species_nomatched$species <- species_CTPL$SPECIES_FULL

write_excel_csv(species_nomatched, file = "CERN_Inner_Mongolia_Hulunbuir_plants_findSpeciesName.csv")
species_nomatched <- read_excel("CERN_Inner_Mongolia_Hulunbuir_plants_findSpeciesName_manual.xlsx", sheet=1)

species <- species %>%
  left_join(species_nomatched %>% select(species_cn, species_manual = species)) %>%
  mutate(species = ifelse(is.na(species), species_manual, species)) %>%
  select(- species_manual)

# add scientific names to the assemblage data
dat <- dat %>% 
  left_join(species %>% select(species_cn, species)) %>% 
  filter(!is.na(species)) # remove names that wer not species, which were not manually added 


# split genus and species and check these names
dat$Genus <- word(dat$species, 1)
dat$Species <- word(dat$species, 2)
sort(unique(dat$Genus))


# add coordinates. 
# Original document have no precise coordinates. The coordinates was rough, based on locality description
dat <- dat %>%
  mutate(Latitude = 49.423,
         Longitude = 119.950)


## prepare the output file
# sum abundance for each sampling event;
dat_out <- dat %>% 
  # add sample description
  mutate(SampleDescription = paste(Year, Month, Day, Plot, sep = "_")) %>%
  select(- c(date, species_cn, species, Biomass_fresh)) %>%
  # sum abundance for each sampling event
  group_by(Year, Month, Day, Plot, Genus, Species, Latitude, Longitude, SampleDescription) %>%
  summarise(Abundance = sum(Abundance, na.rm = TRUE),
            Biomass = sum(Biomass_dry)) %>%
  ungroup()
  
# add required columns for BioTIME
dat_out <- dat_out %>%
  mutate(Family  = "",
         DepthElevation = "",
         StudyID = "")

# because the precise location of plots may be different, delete the plot column information 
dat_out$Plot <- ""

# rename and relocate columns
dat_out <- dat_out %>%
  relocate(Abundance,	Biomass, Family, Genus, Species, SampleDescription, Plot, Latitude, Longitude,
           DepthElevation, Day,	Month, Year, StudyID)


# output the clean raw data
write_excel_csv(dat_out, file = "CERN_Inner_Mongolia_Hulunbuir_plants_WBX_rawdata.csv")
