# Dataset: CERN_Heilongjiang_Sanjiang_plants
# Location: Sanjiang, Heilongjiang province, China
# Curator: Wubing Xu
# Date: 29-06-2023

rm(list = ls())

setwd("C:/Dropbox/iDiv/Data/BTDataCuration_2023/wubing/CERN_Heilongjiang_Sanjiang_plants_WBX")

require(tidyverse)
require(readxl)
require(sp)

# the package to find scientific names from Chinese names for plants
# library(devtools)
# install_github("helixcn/plantlist", build_vignettes = TRUE)
library(plantlist) 

# read original plot1 data
dt_plot1 <- read_excel('CERN_Heilongjiang_Sanjiang_plants_original_WBX/实体数据/1-SWSJ_ZHONGLEIZC.xls', sheet=1, range = "A6:F122" )

dt_plot1 <- dt_plot1 %>% 
  select(c(1:3, 6)) %>%
  set_names(c("year", "species_cn", "N", "coverage")) %>% 
  filter(!is.na(year)) %>%
  # the plot data was collected from Perennial Water regions
  mutate(plot = "PerennialWater")

# read original plot1 data
dt_plot2 <- read_excel('CERN_Heilongjiang_Sanjiang_plants_original_WBX/实体数据/1-SWSJ_ZHONGLEIZC.xls', sheet=1, range = "A128:F224" )

dt_plot2 <- dt_plot2 %>% 
  select(c(1:3, 6)) %>%
  set_names(c("year", "species_cn", "N", "coverage")) %>% 
  filter(!is.na(year)) %>%
  # the plot data was collected from seasonal Water regions
  mutate(plot = "SeasonalWater")

# combine plot 1 and 2 data
dt <- bind_rows(dt_plot1, dt_plot2)


## find scientific name based on Chinese species name
species_cn <- unique(dt$species_cn)
species <- plantlist::CTPL(species_cn)
species <- species %>% 
  as_tibble() %>%
  select(YOUR_SEARCH, SPECIES)

# many Chinese names was failed to find scientific names.
# output these names and find scientific names manually 
species_nomatch <- species %>% filter(is.na(SPECIES))
write_excel_csv(species_nomatch, file = "CERN_Heilongjiang_Sanjiang_plants_findSpeciesName.csv")
species_manual <- read_csv("CERN_Heilongjiang_Sanjiang_plants_findSpeciesName_manual.csv")

# combine scientifc names
species <- left_join(species, species_manual %>% rename("SPECIES_new" = "SPECIES")) %>%
  mutate(SPECIES = ifelse(is.na(SPECIES), SPECIES_new, SPECIES)) %>%
  select(- SPECIES_new) %>% 
  rename("species_cn" = "YOUR_SEARCH", "species" = "SPECIES")


# matcch scientific names to the assemblage data
dt <- dt %>% left_join(species)

# split genus and species and check these naems
dt$Genus <- word(dt$species, 1)
dt$Species <- word(dt$species, start=2, end=-1)

sort(unique(dt$Genus))


# add coordinates
# coordinates of plot 1
char2dms("133d30'6.84\"E") %>% as.numeric()
char2dms("47d35'18.48\"N") %>% as.numeric()

# coordinates of plot 2
char2dms("133d30'6.3\"E") %>% as.numeric()
char2dms("47d35'19.26\"N") %>% as.numeric()

dt <- dt %>%
  mutate(Latitude = ifelse(plot == "PerennialWater", 47.58847, 47.58868),
         Longitude = ifelse(plot == "PerennialWater", 133.5019, 133.5017))


## prepare the output file
# sum abundance and biomass for each sampling event
dt_out <- dt %>% 
  # add sample description
  mutate(SampleDescription = paste(year, plot, sep = "_")) %>%
  select(- c(species_cn, species)) %>%
  # sum abundance and biomass (coverage here) for each sampling event
  group_by(year, Genus, Species, plot, Latitude, Longitude, SampleDescription) %>%
  summarise(Abundance = sum(N),
            Biomass = sum(coverage, na.rm = T))
  
# add required columns for BioTIME
dt_out <- dt_out %>%
  mutate(Family  = "",
         DepthElevation = "",
         Day = "",
         Month = "",
         StudyID = "")

# rename and relocate columns
dt_out <- dt_out %>%
  relocate(Abundance,	Biomass, Family, Genus, Species, SampleDescription, Plot = plot, Latitude, Longitude,
           DepthElevation, Day,	Month, Year = year, StudyID)


# output the clean raw data
write_excel_csv(dt_out, file = "CERN_Heilongjiang_Sanjiang_plants_WBX_rawdata.csv")
