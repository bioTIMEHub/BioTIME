## ---------------------------
##
## Script name: Taihu_phytoplankton_XK.R
##
## Purpose of script: Data Curation for BioTIME Database
## CERN Jiangsu Taihu Lake phytoplankton community composition, 1991-2008
##
## Author: Xuejia Ke
##
## Date Created: 2023-06-26
##
## Copyright (c) Xuejia Ke, 2023
## Email: xk5@st-andrews.ac.uk
##
## ---------------------------
##
## Notes:
## Manually reformat file ./RawData/THL_SWSJ_FUYOUZHIWU0003.xlsx, split it into THL00, THL01, THL03 three files based on sites
## Manually reformat file ./RawData/THL_SWSJ_FUYOUZHIWU0406.xlsx, split it into THL04, THL05, THL06 three files based on sites
## Manually reformat file ./RawData/THL_SWSJ_FUYOUZHIWU0708.xlsx, split it into THL07, THL08 two files based on sites
## 
## 5 phylums: "Cyanophyta", "Chlorophyta", "Bacillariophyta", "Euglenophyta",  "Chrysophyta" go to family.
## 2 orders: "Oscillatoriales", "Euglenales" go to family, delete 续表 as there is no English name for it.
## replace sp. with sp1 and spp. with sp, sample description.
##   
## ---------------------------

## set working directory for Mac and PC

setwd("~/myphd/BioTIME/Taihu_phyto-20230626T162008Z-001/Taihu_phyto/CuratedData")    # Xuejia's working directory (PC)

## ---------------------------

## load up the packages we will need:  (uncomment as required)

require(dplyr)
require(ggplot2)
require(stringr)
require(readxl)
require(maps)
require(tidyr)
library(sf)
library(clipr)

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
name_map <- read_excel('./Taihu_phytoplankton_namelist_XK.xlsx', sheet=1, col_names=T, na='')

## read data files
THL00 <- read_excel('./THL_SWSJ_FUYOUZHIWU0003/THL00.xlsx', sheet=1, col_names=T, na='')
THL01 <- read_excel('./THL_SWSJ_FUYOUZHIWU0003/THL01.xlsx', sheet=1, col_names=T, na='')
THL03 <- read_excel('./THL_SWSJ_FUYOUZHIWU0003/THL03.xlsx', sheet=1, col_names=T, na='')
THL04 <- read_excel('./THL_SWSJ_FUYOUZHIWU0406/THL04.xlsx', sheet=1, col_names=T, na='')
THL05 <- read_excel('./THL_SWSJ_FUYOUZHIWU0406/THL05.xlsx', sheet=1, col_names=T, na='')
THL06 <- read_excel('./THL_SWSJ_FUYOUZHIWU0406/THL06.xlsx', sheet=1, col_names=T, na='')
THL07 <- read_excel('./THL_SWSJ_FUYOUZHIWU0708/THL07.xlsx', sheet=1, col_names=T, na='')
THL08 <- read_excel('./THL_SWSJ_FUYOUZHIWU0708/THL08.xlsx', sheet=1, col_names=T, na='')

## add site name
THL00$site <- rep('THL00', nrow(THL00))
THL01$site <- rep('THL01', nrow(THL01))
THL03$site <- rep('THL03', nrow(THL03))
THL04$site <- rep('THL04', nrow(THL04))
THL05$site <- rep('THL05', nrow(THL05))
THL06$site <- rep('THL06', nrow(THL06))
THL07$site <- rep('THL07', nrow(THL07))
THL08$site <- rep('THL08', nrow(THL08))


## bind them
THL <- rbind(THL00,THL01,THL03,THL04,THL05,THL06,THL07,THL08)

## rename the first column
colnames(THL)[1] <- "Species_Cn"
THL_mapped <- left_join(THL, name_map, by="Species_Cn", multiple="all")

## ---------------------------Taxonomic fields---------------------------
## check names that are not mapped successfully

#Cn_list <- unique(THL_mapped_not[,1]) %>% as.data.frame()
## Manually add them & their EN names to the file name_map.xlsx

THL_mapped %>% filter(is.na(Species) && 
                        is.na(Genus) && 
                        is.na(Family) && 
                        is.na(Order) && 
                        is.na(Phylum)) # none after refined mapping


## Convert months into longer format
months <- colnames(THL_mapped)[2:13]
THL_mapped <- THL_mapped %>% tidyr::pivot_longer(cols=months,
                                                 names_to='Month',
                                                 values_to='Biomass')


## Add variables to data
THL_mapped$Latitude <- as.numeric(rep('', nrow(THL_mapped)))
THL_mapped$Longitude <- as.numeric(rep('', nrow(THL_mapped)))
THL_mapped$Plot <- rep('', nrow(THL_mapped))
THL_mapped$DepthElevation <- rep('', nrow(THL_mapped))
THL_mapped$Day <- rep('', nrow(THL_mapped))
THL_mapped$StudyID <- rep('', nrow(THL_mapped))
THL_mapped$Abundance <- rep('', nrow(THL_mapped))


## Taxa check
## --------------------- check Family ---------------------
# check that genera are genera, not family names (-idae/eae)
# this returns the record index number if there are any
str_which(THL_mapped$Species, 'idae$|eae$') # None
# check the species list for misspellings or non-BioTIME taxonomic convention names
# Do visual checks
sort(unique(THL_mapped$Species)) # check species, np
sort(unique(THL_mapped$Genus)) # check genus, np
sort(unique(THL_mapped$Family)) # check family, np



## ---------------------------coordinates---------------------------
## Assign coordinates to different sites
THL_mapped$Longitude[THL_mapped$site == "THL00"] <- 120.21944
THL_mapped$Latitude[THL_mapped$site == "THL00"] <- 31.53968
THL_mapped$Longitude[THL_mapped$site == "THL01"] <- 120.19067
THL_mapped$Latitude[THL_mapped$site == "THL01"] <- 31.5131
THL_mapped$Longitude[THL_mapped$site == "THL03"] <- 120.19433
THL_mapped$Latitude[THL_mapped$site == "THL03"] <- 31.47633
THL_mapped$Longitude[THL_mapped$site == "THL04"] <- 120.18796
THL_mapped$Latitude[THL_mapped$site == "THL04"] <- 31.4360
THL_mapped$Longitude[THL_mapped$site == "THL05"] <- 120.18733
THL_mapped$Latitude[THL_mapped$site == "THL05"] <- 31.41117
THL_mapped$Longitude[THL_mapped$site == "THL06"] <- 120.13117
THL_mapped$Latitude[THL_mapped$site == "THL06"] <- 31.50383
THL_mapped$Longitude[THL_mapped$site == "THL07"] <- 120.18017
THL_mapped$Latitude[THL_mapped$site == "THL07"] <- 31.33833
THL_mapped$Longitude[THL_mapped$site == "THL08"] <- 120.17062
THL_mapped$Latitude[THL_mapped$site == "THL08"] <- 31.24816


world_map <- map_data('world') # check whether the GPS coordinates match expectations
world <- ggplot(world_map) + 
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  coord_fixed() + 
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())
points <- world + geom_point(data=THL_mapped %>% distinct(Latitude, Longitude, Year), 
                             aes(x=Longitude, y=Latitude, fill = Year), shape=21)

points + coord_fixed(xlim=c(100,150), ylim=c(0,40))

## ---------------------------Depth Elevation---------------------------
## not provided


##  ---------------------------add Plot ---------------------------
## none



##  ---------------------------format check ---------------------------
str(THL_mapped)

##  convert Cn months to En
THL_mapped$Month[THL_mapped$Month=="1月"] <- 1
THL_mapped$Month[THL_mapped$Month=="2月"] <- 2
THL_mapped$Month[THL_mapped$Month=="3月"] <- 3
THL_mapped$Month[THL_mapped$Month=="4月"] <- 4
THL_mapped$Month[THL_mapped$Month=="5月"] <- 5
THL_mapped$Month[THL_mapped$Month=="6月"] <- 6
THL_mapped$Month[THL_mapped$Month=="7月"] <- 7
THL_mapped$Month[THL_mapped$Month=="8月"] <- 8
THL_mapped$Month[THL_mapped$Month=="9月"] <- 9
THL_mapped$Month[THL_mapped$Month=="10月"] <- 10
THL_mapped$Month[THL_mapped$Month=="11月"] <- 11
THL_mapped$Month[THL_mapped$Month=="12月"] <- 12

unique(THL_mapped$Month) ## check

# --------------------------- aggregate biomass records that are same species, plot, and survey day.
THL_aggre <- THL_mapped %>% group_by(Abundance, Family, Genus, Species, Plot, Latitude, Longitude, DepthElevation, Day, Month, Year, StudyID, site) %>% 
  summarise(Biomass=sum(Biomass)) %>% ungroup() %>% arrange(Year, Family, Genus, Species)

dim(THL_aggre)[1]-dim(THL_mapped)[1] # check changes

##  ---------------------------Remove records with no AB ---------------------------
## covert 0 Biomass to NA
THL_aggre$Biomass[THL_aggre$Biomass == 0] <- NA
## covert null Abundance to NA
THL_aggre$Abundance <- NA
## remove records with no Abundance and Biomass
THL_aggre <- THL_aggre[!with(THL_aggre,is.na(Abundance)& is.na(Biomass)),]


##  ---------------------------add SampleDescription ---------------------------
THL_aggre$SampleDescription <- as.factor(with(THL_aggre, paste(THL_aggre$site, THL_aggre$Month, THL_aggre$Year, sep='_')))

##  ---------------------------final check ---------------------------
str(THL_aggre)

##  ---------------------------final format ---------------------------
THL_aggre <- THL_aggre[c('Abundance',
                         'Biomass',
                         'Family','Genus',
                         'Species',
                         'SampleDescription',
                         'Plot',
                         'Latitude',
                         'Longitude',
                         'DepthElevation',
                         'Day',
                         'Month',
                         'Year',
                         'StudyID')] %>% arrange(Year, Family, Genus, Species)

str(THL_aggre) # final check!

## Export
write.csv(THL_aggre,file="./Taihu_phytoplankton_XK.csv")


# 1. Convert data points into point spatial object
# assumes your rawdata dataframe is called rawdata
dt_coord <- THL_aggre %>% select(Longitude, Latitude) %>% distinct() %>%
  st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()

# 2. Calculate convex hull, area and centroid
centroid <- dt_coord %>% st_convex_hull() %>% st_centroid() %>% unlist # get centroid
write_clip(centroid[c(2,1)]) # copy as lat-long
write_clip(THL_aggre) # copy
