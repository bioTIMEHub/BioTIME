## ---------------------------
##
## Script name: InnerMongolia_Ordos_XK.R
##
## Purpose of script: Data Curation for BioTIME Database
## CERN Inner Mongolia Ordos plant monitoring data, 2004-2006
##
## Author: Xuejia Ke
##
## Date Created: 2023-06-27
##
## Copyright (c) Xuejia Ke, 2023
## Email: xk5@st-andrews.ac.uk
##
## ---------------------------
##
## Notes:
## The species name list file InnerMongolia_Ordos_namelist_XK.xls is semi-manually curated
##
## ---------------------------

## set working directory

setwd("~/myphd/BioTIME/InnerMongolia_Ordos-20230627T161756Z-001/InnerMongolia_Ordos_XK")    # Xuejia's working directory (PC)

## ---------------------------

## load up the packages we will need:  (uncomment as required)

require(dplyr)
require(ggplot2)
require(stringr)
require(readxl)
require(maps)
require(tidyr)

## load functions
## Function if coordinates are ever in degree minutes seconds

angle2dec <- function(angle) {
  angle <- as.character(angle)
  x <- do.call(rbind, strsplit(angle, split=' '))
  x <- apply(x, 1L, function(y) {
    y <- as.numeric(y)
    y[1] + y[2]/60 + y[3]/3600
  })
  return(x)
}

## ---------------------------read files-----------------------------
## read files for mapping
name_map <- read_excel('./RawData/ESD_SWSJ.xls', sheet=1, col_names=T, na='', skip=1)

## rename the columns
colnames(name_map) <- c("Species_cn", "Taxon")

## split taxon into species & genus name
name_map <- name_map %>% separate(Taxon, c("Genus", "Species"), " ")

## check whether genus name is in species
str_which(name_map$Species, 'idae$|eae$') # None
## check the species list for misspellings or non-BioTIME taxonomic convention names
## Do visual checks
sort(unique(name_map$Species))
sort(unique(name_map$Genus))


## put 苔藓 to Family
## add family
name_map$Family <- rep("", nrow(name_map))
name_map$Family[name_map$Species_cn == "苔藓"] <- "Bryophytes"
name_map$Species[name_map$Species_cn == "苔藓"] <- NA
name_map$Genus[name_map$Species_cn == "苔藓"] <- NA
  
## put 地衣 as Lichen to Family
name_map$Family[name_map$Species_cn == "地衣"] <- "Lichen"
name_map$Species[name_map$Species_cn == "地衣"] <- NA
name_map$Genus[name_map$Species_cn == "地衣"] <- NA


## load data file





