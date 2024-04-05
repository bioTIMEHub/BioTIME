## ---------------------------
##
## Script name: Guangdong_Dinghu_P_herb_XK.R
##
## Purpose of script: Data Curation for BioTIME Database
## CERN Guangdong Dinghu Mountain plant community composition from 2003 to 2008 - herb
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
## Notes: Data from site DHFZQ01 & DHFZQ02 is removed as they have no data in 2005.
## This data set only contain observations in 2004 and 2005.
## 
##   
## ---------------------------

## set working directory

setwd("~/myphd/BioTIME/Guangdong_Dinghu_P-20230627T173335Z-001/Guangdong_Dinghu_P_XK/Guangdong_Dinghu_P_herb_XK")   # Xuejia's working directory (PC)

## ---------------------------

## ---------------------------

## load up the packages we will need:  (uncomment as required)

require(dplyr)
require(ggplot2)
require(stringr)
require(readxl)
require(maps)
require(tidyr)
require(sf)
require(clipr)

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
## copy file ./RawData/鼎湖山生物数据-灌木层植物种组成.xlsx and rename it as Guangdong_Dinghu_P_namelist_XK.xlsx
name_map <- read_excel('./Guangdong_Dinghu_P_namelist_XK.xlsx', sheet=1, col_names=T, na='', skip=1)
## Rename columns
colnames(name_map) <- c("Species_Cn","Taxon","Other_name","Family_Cn")
## Split Genus and Species
name_map <- name_map %>% separate(Taxon, c("Genus", "Species"), " ")

## read data files
## copy file ./RawData/鼎湖山生物数据-草本层植物种组成.xlsx and rename it as Guangdong_Dinghu_P_herb_XK_RawData.xlsx
herb <- read_excel('./Guangdong_Dinghu_P_herb_XK_RawData.xlsx', sheet=3, col_names=T, na='', skip=1)
## rename columns
colnames(herb) <- c("Year","Site","Species_Cn","Abundance","Average_Height","Biomass")

## map their names
herb_mapped <- left_join(herb, name_map, by="Species_Cn", multiple="all")

## Add variables to data
herb_mapped$Latitude <- as.numeric(rep('', nrow(herb_mapped)))
herb_mapped$Longitude <- as.numeric(rep('', nrow(herb_mapped)))
herb_mapped$Plot <- rep('', nrow(herb_mapped))
herb_mapped$DepthElevation <- rep('', nrow(herb_mapped))
herb_mapped$Day <- rep('', nrow(herb_mapped))
herb_mapped$StudyID <- rep('', nrow(herb_mapped))
herb_mapped$Family <- rep('', nrow(herb_mapped))
herb_mapped$Month <- rep('', nrow(herb_mapped))

## ---------------------------coordinates---------------------------
## assign sites
herb_mapped[1:11,"Site"] <- "DHFZQ01"
herb_mapped[12:28,"Site"] <- "DHFFZ02"
herb_mapped[29:61,"Site"] <- "DHFZH01"
herb_mapped[62:79,"Site"] <- "DHFFZ01"
herb_mapped[80:108,"Site"] <- "DHFZQ02"

herb_mapped[109:127,"Site"] <- "DHFFZ02"
herb_mapped[128:147,"Site"] <- "DHFZH01"
herb_mapped[148:170,"Site"] <- "DHFFZ01"

## assign coordinates
DHFZQ01 <- data.frame(Longitude = c(angle2dec("112 32 29.18"),angle2dec("112 32 30.91"),
                        angle2dec("112 32 30.65"),angle2dec("112 32 31.61")),
                      Latitude = c(angle2dec("23 10 2.44"),angle2dec("23 10 3.68"),
                        angle2dec("23 10 2.04"),angle2dec("23 10 2.84")))

DHFZQ01_coord <- DHFZQ01 %>% select(Longitude, Latitude) %>% distinct() %>%
  st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()

## Calculate convex hull, area and centroid
centroid <- DHFZQ01_coord %>% st_convex_hull() %>% st_centroid() %>% unlist # get centroid
write_clip(centroid[c(2,1)]) # copy as lat-long

herb_mapped$Longitude[herb_mapped$Site == "DHFZQ01"] <- 112.541814398244 
herb_mapped$Latitude[herb_mapped$Site == "DHFZQ01"] <- 23.167435262244

## assign coordinates
DHFFZ02 <- data.frame(Longitude = c(angle2dec("112 32 54.66"),angle2dec("112 32 55.54"),
                                    angle2dec("112 32 50.85"),angle2dec("112 32 52.70")),
                      Latitude = c(angle2dec("23 10 25.49"),angle2dec("23 10 12.63"),
                                   angle2dec("23 10 24.41"),angle2dec("23 10 21.37")))

DHFFZ02_coord <- DHFFZ02 %>% select(Longitude, Latitude) %>% distinct() %>%
  st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()

## Calculate convex hull, area and centroid
centroid <- DHFFZ02_coord %>% st_convex_hull() %>% st_centroid() %>% unlist # get centroid
write_clip(centroid[c(2,1)]) # copy as lat-long

herb_mapped$Longitude[herb_mapped$Site == "DHFFZ02"] <- 112.548245374589 
herb_mapped$Latitude[herb_mapped$Site == "DHFFZ02"] <- 23.1724564824651

## assign coordinates
DHFZH01 <- data.frame(Longitude = c(angle2dec("112 32 22.64"),angle2dec("112 32 21.12"),
                                    angle2dec("112 32 23.64"),angle2dec("112 32 20.48")),
                      Latitude = c(angle2dec("23 10 9.90"),angle2dec("23 10 10.65"),
                                   angle2dec("23 10 11.27"),angle2dec("23 10 13.00")))

DHFZH01_coord <- DHFZH01 %>% select(Longitude, Latitude) %>% distinct() %>%
  st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()

## Calculate convex hull, area and centroid
centroid <- DHFZH01_coord %>% st_convex_hull() %>% st_centroid() %>% unlist # get centroid
write_clip(centroid[c(2,1)]) # copy as lat-long

herb_mapped$Longitude[herb_mapped$Site == "DHFZH01"] <- 112.539435975954 
herb_mapped$Latitude[herb_mapped$Site == "DHFZH01"] <- 23.1698111592563


## assign coordinates
DHFFZ01 <- data.frame(Longitude = c(angle2dec("112 33 21.40"),angle2dec("112 33 22.99"),
                                    angle2dec("112 33 22.64"),angle2dec("112 33 26.24")),
                      Latitude = c(angle2dec("23 9 58.91"),angle2dec("23 10 1.36"),
                                   angle2dec("23 9 55.43"),angle2dec("23 9 58.32")))

DHFFZ01_coord <- DHFFZ01 %>% select(Longitude, Latitude) %>% distinct() %>%
  st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()

## Calculate convex hull, area and centroid
centroid <- DHFFZ01_coord %>% st_convex_hull() %>% st_centroid() %>% unlist # get centroid
write_clip(centroid[c(2,1)]) # copy as lat-long

herb_mapped$Longitude[herb_mapped$Site == "DHFFZ01"] <- 112.556521752549 
herb_mapped$Latitude[herb_mapped$Site == "DHFFZ01"] <- 23.1662300870867

herb_mapped$Longitude[herb_mapped$Site == "DHFZQ02"] <- 112.5
herb_mapped$Latitude[herb_mapped$Site == "DHFZQ02"] <- 23.18


## assign Depth Elevation, take the mean
herb_mapped$DepthElevation[herb_mapped$Site == "DHFZQ01"] <- 250  
herb_mapped$DepthElevation[herb_mapped$Site == "DHFFZ02"] <- 150  
herb_mapped$DepthElevation[herb_mapped$Site == "DHFZH01"] <- 290  
herb_mapped$DepthElevation[herb_mapped$Site == "DHFFZ01"] <- 100 
herb_mapped$DepthElevation[herb_mapped$Site == "DHFZQ02"] <- 600


world_map <- map_data('world') # check whether the GPS coordinates match expectations
world <- ggplot(world_map) + 
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  coord_fixed() + 
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())
points <- world + geom_point(data=herb_mapped %>% distinct(Latitude, Longitude, Year), 
                             aes(x=Longitude, y=Latitude, fill = Year), shape=21)

points + coord_fixed(xlim=c(100,150), ylim=c(0,40))
  

## ---------------------------Taxonomic fields---------------------------
## check that genera are genera, not family names (-idae/eae)
str_which(herb_mapped$Species, 'idae$|eae$') # none
# check the species list for misspellings or non-BioTIME taxonomic convention names
sort(unique(herb_mapped$Species)) # check species
herb_mapped$Species[herb_mapped$Species == "chinensis(Champ.ex"] <- "chinensis"

sort(unique(herb_mapped$Genus)) # check genus
sort(unique(herb_mapped$Family_Cn)) # check family

## check names that are not mapped successfully
check <- herb_mapped %>% filter(is.na(Species))
unique(check$Species_Cn)

## assign unknown names
herb_mapped$Species[herb_mapped$Species_Cn == "厚壳桂"] <- "chinensis"
herb_mapped$Genus[herb_mapped$Species_Cn == "厚壳桂"] <- "Cryptocarya"
herb_mapped$Species[herb_mapped$Species_Cn == "厚壳桂　"] <- "chinensis"
herb_mapped$Genus[herb_mapped$Species_Cn == "厚壳桂　"] <- "Cryptocarya"
herb_mapped$Species[herb_mapped$Species_Cn == "乌蔹梅"] <- "japonica"
herb_mapped$Genus[herb_mapped$Species_Cn == "乌蔹梅"] <- "Causonis"
herb_mapped$Species[herb_mapped$Species_Cn == "花葶苔草"] <- "scaposa"
herb_mapped$Genus[herb_mapped$Species_Cn == "花葶苔草"] <- "Carex"
herb_mapped$Species[herb_mapped$Species_Cn == "隐穗苔草"] <- "cryptostachys"
herb_mapped$Genus[herb_mapped$Species_Cn == "隐穗苔草"] <- "Carex"

herb_mapped[38,"Genus"] <- "Cryptocarya"
herb_mapped[38,"Species"] <- "chinensis"

check <- herb_mapped %>% filter(is.na(Species))
# None

# --------------------------- aggregate biomass records that are same species, plot, and survey day.
herb_aggre <- herb_mapped %>% group_by(Abundance, Family, Genus, Species, Plot, Latitude, Longitude, 
                                       DepthElevation, Day, Month, Year, StudyID, Site) %>% 
  summarise(Biomass=sum(Biomass)) %>% ungroup() %>% arrange(Year, Family, Genus, Species)

dim(herb_aggre)[1]-dim(herb_mapped)[1] # check changes

##  --------------------------- aggregate Abundance records that are same species, plot, and survey day.-----
herb_aggre <- herb_aggre %>% group_by(Biomass, Family, Genus, Species, Plot, Latitude, Longitude, 
                                       DepthElevation, Day, Month, Year, StudyID, Site) %>% 
  summarise(Abundance=sum(Abundance)) %>% ungroup() %>% arrange(Year, Family, Genus, Species)

dim(herb_aggre)[1]-dim(herb_mapped)[1] # check changes

##  ---------------------------Remove records with no AB ---------------------------
## covert 0 Biomass to NA
herb_aggre$Biomass[herb_aggre$Biomass == 0] <- NA
## covert 0 Abundance to NA
herb_aggre$Abundance[herb_aggre$Abundance == 0] <- NA
## remove records with no Abundance and Biomass
herb_aggre <- herb_aggre[!with(herb_aggre,is.na(Abundance)& is.na(Biomass)),]

## check
herb_aggre %>% filter(is.na(Species)) # none

##  ---------------------------add SampleDescription ---------------------------
herb_aggre$SampleDescription <- as.factor(with(herb_aggre, paste(herb_aggre$Site, herb_aggre$Year, sep='_')))
unique(herb_aggre$SampleDescription)

##  ---------------------------add Plot ---------------------------
## none

## check
table(herb_aggre$Site,herb_aggre$Year)

## remove data from site DHFZQ01 & DHFZQ02 as no data is in 2005
herb_aggre <- herb_aggre %>% filter(Site != "DHFZQ01") %>% 
  filter(Site != "DHFZQ02")

## check again
table(herb_aggre$Site,herb_aggre$Year) ## np

##  ---------------------------final check ---------------------------
str(herb_aggre)

##  ---------------------------final format ---------------------------
herb_aggre <- herb_aggre[c('Abundance',
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


str(herb_aggre) # final check!

## Export
write.csv(herb_aggre,file="./Guangdong_Dinghu_P_herb_XK.csv")


dt_coord <- herb_aggre %>% select(Longitude, Latitude) %>% distinct() %>%
  st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()

# 2. Calculate convex hull, area and centroid
centroid <- dt_coord %>% st_convex_hull() %>% st_centroid() %>% unlist # get centroid
write_clip(centroid[c(2,1)]) # copy as lat-long

# transform to mercator to get area in sq km
area <- st_transform(dt_coord, st_crs("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs")) %>%
  st_convex_hull() %>% st_area()  ##get area in sq km
write_clip(area)


write_clip(herb_aggre) # copy
