# Dataset: CERN_Yunnan_Xishuangbanna_trees
# Location: Xishuangbanna, Yunnan province, China
# Curator: Wubing Xu
# Date: 03-07-2023

rm(list = ls())

setwd("C:/Dropbox/iDiv/Data/BTDataCuration_2023/wubing/CERN_Yunnan_Xishuangbanna_trees_WBX")

require(tidyverse)
require(readxl)
require(sp)
library(plantlist) 

## six plot data were compiled here. The area of plot 1 is 100*100 meters, and the area of plot 2 is 50*100 meters, 
# while the area of plots 3-6 is 50*50 meters. To keep a consistent sampling effort, individual-based rarefaction was used to 
# standardize data, that is 1/4 the number of individuals was randomly selected for plot 1, 
# and 1/2 the number of individuals was randomly selected for plot 2.

# plot 1
dat_plot1 <- read_excel('CERN_Yunnan_Xishuangbanna_trees_original_WBX/实体数据/版纳站生物数据_乔木层植物种组成.xlsx', 
                        sheet= "热带季节雨林综合观测场", range = "A2:C2281")
colnames(dat_plot1) <- c("Year", "species_cn", "Abundance")

dat_plot1 <- dat_plot1 %>% filter(!is.na(Abundance))

dat_plot1_subset <- NULL
for(i in unique(dat_plot1$Year)){
  subset_year_i <- dat_plot1 %>% filter(Year == i)
  subset_indiv <- rep(subset_year_i$species_cn, subset_year_i$Abundance)
  set.seed(1)
  subset_indiv_rare <- sample(subset_indiv, round(length(subset_indiv)/4))
  dat_plot1_subset_year_i <- tibble(Year = i, species_cn = subset_indiv_rare) %>% 
    group_by(Year, species_cn) %>% 
    summarise(Abundance = n()) %>%
    ungroup()
  dat_plot1_subset <- bind_rows(dat_plot1_subset, dat_plot1_subset_year_i)
}

# plot 2
dat_plot2 <- read_excel('CERN_Yunnan_Xishuangbanna_trees_original_WBX/实体数据/版纳站生物数据_乔木层植物种组成.xlsx', 
                        sheet= "西双版纳热带次生林辅助观测场", range = "A2:C350")
colnames(dat_plot2) <- c("Year", "species_cn", "Abundance")

dat_plot2_subset <- NULL
for(i in unique(dat_plot2$Year)){
  subset_year_i <- dat_plot2 %>% filter(Year == i)
  subset_indiv <- rep(subset_year_i$species_cn, subset_year_i$Abundance)
  subset_indiv_rare <- sample(subset_indiv, round(length(subset_indiv)/2))
  dat_plot2_subset_year_i <- tibble(Year = i, species_cn = subset_indiv_rare) %>% 
    group_by(Year, species_cn) %>% 
    summarise(Abundance = n()) %>%
    ungroup()
  dat_plot2_subset <- bind_rows(dat_plot2_subset, dat_plot2_subset_year_i)
}

# plot 3
dat_plot3 <- read_excel('CERN_Yunnan_Xishuangbanna_trees_original_WBX/实体数据/版纳站生物数据_乔木层植物种组成.xlsx', 
                        sheet= "西双版纳窄序崖豆树热带次生林站区调查点", range = "A2:C278")
colnames(dat_plot3) <- c("Year", "species_cn", "Abundance")

# plot 4
dat_plot4 <- read_excel('CERN_Yunnan_Xishuangbanna_trees_original_WBX/实体数据/版纳站生物数据_乔木层植物种组成.xlsx', 
                        sheet= "西双版纳曼安热带次生林站区调查点", range = "A2:C461")
colnames(dat_plot4) <- c("Year", "species_cn", "Abundance")

# plot 5
dat_plot5 <- read_excel('CERN_Yunnan_Xishuangbanna_trees_original_WBX/实体数据/版纳站生物数据_乔木层植物种组成.xlsx', 
                        sheet= "西双版纳石灰山季雨林站区调查点", range = "A2:C208")
colnames(dat_plot5) <- c("Year", "species_cn", "Abundance")

# plot 6
dat_plot6 <- read_excel('CERN_Yunnan_Xishuangbanna_trees_original_WBX/实体数据/版纳站生物数据_乔木层植物种组成.xlsx', 
                        sheet= "西双版纳次生常绿阔叶林站区调查点", range = "A2:C321")
colnames(dat_plot6) <- c("Year", "species_cn", "Abundance")


# combine data
dat <- bind_rows(dat_plot1_subset, dat_plot2_subset, dat_plot3, dat_plot4, dat_plot5, dat_plot6) %>%
  mutate(Plot = c(rep(1, nrow(dat_plot1_subset)), rep(2, nrow(dat_plot2_subset)), rep(3, nrow(dat_plot3)),
                  rep(4, nrow(dat_plot4)), rep(5, nrow(dat_plot5)), rep(6, nrow(dat_plot6))))

# remove blank space in the Chinese species namees
dat <- dat %>% 
  mutate(species_cn = gsub(" ", "", species_cn))

## match Chinese names to scientific names 
# species list
species_list <- read_excel('CERN_Yunnan_Xishuangbanna_trees_original_WBX/实体数据/版纳站生物数据_植物名录.xlsx', 
                     sheet= 1, range = "A2:B723")
colnames(species_list) <- c("species_cn", "species")

# remove blank space in the Chinese species names
species_list <- species_list %>% 
  mutate(species_cn = gsub(pattern = " ", replacement = "", species_cn))

# remove duplicated species
species_list <- species_list %>% distinct()
# the species "梭果玉蕊" have two rows, with the same scientific names but one have blank space after the species names.
species_list[duplicated(species_list$species_cn), ]
species_list %>% filter(species_cn == "梭果玉蕊")
# keep one name for "梭果玉蕊" (the one without blank space)
species_list <- species_list %>% filter(!duplicated(species_list$species_cn))


# match Chinese and scientific species names
dat <- left_join(dat, species_list)

# some names were not matched. Add scientific names manually
species_nomatched <- dat %>% filter(is.na(species)) %>% distinct(species_cn)
species_CTPL <- plantlist::CTPL(species_nomatched$species_cn)
species_nomatched$family_cn <- species_CTPL$FAMILY_CN
species_nomatched$genus_cn <- species_CTPL$GENUS_CN
species_nomatched$species <- species_CTPL$SPECIES_FULL

write_excel_csv(species_nomatched, file = "CERN_Yunnan_Xishuangbanna_trees_findSpeciesName.csv")
species_nomatched <- read_excel("CERN_Yunnan_Xishuangbanna_trees_findSpeciesName_manual.xlsx", sheet=1)

dat <- dat %>% 
  left_join(species_nomatched %>% select(species_cn, species_manual = species)) %>%
  mutate(species = ifelse(is.na(species), species_manual, species)) %>%
  select(- species_manual)


# split genus and species and check these names
dat$Genus <- word(dat$species, 1)
dat$Species <- word(dat$species, 2)
sort(unique(dat$Genus))


# add coordinates
# coordinates of plot 1
x1 <- char2dms("21d57'40\"N") %>% as.numeric()
y1 <- char2dms("101d12'1\"E") %>% as.numeric()

# coordinates of plot 2
x2 <- char2dms("21d55'08\"N") %>% as.numeric()
y2 <- char2dms("101d16'23\"E") %>% as.numeric()

# coordinates of plot 3
x3 <- char2dms("21d55'16\"N") %>% as.numeric()
y3 <- char2dms("101d16'11\"E") %>% as.numeric()

# coordinates of plot 4
x4 <- char2dms("21d55'49\"N") %>% as.numeric()
y4 <- char2dms("101d16'23\"E") %>% as.numeric()

# coordinates of plot 5
x5 <- char2dms("21d54'42\"N") %>% as.numeric()
y5 <- char2dms("101d16'59\"E") %>% as.numeric()

# coordinates of plot 6
x6 <- char2dms("21d57'54\"N") %>% as.numeric()
y6 <- char2dms("101d12'14\"E") %>% as.numeric()

plot_coordinates <- tibble(Plot = 1:6, Latitude = c(x1, x2, x3, x4, x5, x6), Longitude = c(y1, y2, y3, y4, y5, y6))

dat <- dat %>% left_join(plot_coordinates)


## prepare the output file
# sum abundance for each sampling event
dat_out <- dat %>% 
  # add sample description
  mutate(SampleDescription = paste(Year, Plot, sep = "_")) %>%
  select(- c(species_cn, species)) %>%
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
write_excel_csv(dat_out, file = "CERN_Yunnan_Xishuangbanna_trees_WBX_rawdata.csv")
