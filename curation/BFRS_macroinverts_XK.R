## ---------------------------
##
## Script name: BFRS_macroinverts_XK.R
##
## Purpose of script: Data Curation for BioTIME Database
## RivFish ID 38 Vincent Resh Blodgett Forest macroinvertes
##
## Author: Xuejia Ke
##
## Date Created: 2023-06-30
##
## Copyright (c) Xuejia Ke, 2023
## Email: xk5@st-andrews.ac.uk
##
## ---------------------------
##
## Notes: Methods info of site Pilot Creek (P1 & P2) is missing from the literature. Info of the rest sites is used.
## 
##   
## ---------------------------

## set working directory

setwd("~/myphd/BioTIME/BFRS_macroinverts_XK")   # Xuejia's working directory (PC)

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
## read files
name_map <- read_excel('./BFRS_macroinverts_XK_RawData.xls', sheet=2, col_names=T, na='')
abundance_raw <- read_excel('./BFRS_macroinverts_XK_RawData.xls', sheet=1, col_names=T, na='')

## reformat the data frame
abundance_raw <- abundance_raw %>% pivot_longer(Acarin:Zapada,names_to = "Taxon", values_to = "Abundance")

## Rename the column name
colnames(name_map)[colnames(name_map) == "code"] ="Taxon"
## map their names
all_mapped <- left_join(abundance_raw, name_map, by="Taxon", multiple="all")

## check
unique(all_mapped$stream)

## extract date
all_mapped$Year <- format(all_mapped$date, format="%Y")
all_mapped$Month <- format(all_mapped$date, format="%m")
all_mapped$Day <- format(all_mapped$date, format="%d")


## Add variables to data
all_mapped$Latitude <- as.numeric(rep('', nrow(all_mapped)))
all_mapped$Longitude <- as.numeric(rep('', nrow(all_mapped)))
all_mapped$Plot <- rep('', nrow(all_mapped))
all_mapped$DepthElevation <- rep('', nrow(all_mapped))
all_mapped$StudyID <- rep('', nrow(all_mapped))
all_mapped$Biomass <- as.numeric(rep('', nrow(all_mapped)))


## ---------------------------coordinates---------------------------
## assign coordinates
## Bacon Creek
all_mapped$Longitude[all_mapped$stream == "B1"] <- angle2dec("120 39 25")
all_mapped$Latitude[all_mapped$stream == "B1"] <- angle2dec("38 54 44")

all_mapped$Longitude[all_mapped$stream == "B2"] <- angle2dec("120 39 25")
all_mapped$Latitude[all_mapped$stream == "B2"] <- angle2dec("38 54 44")

## Dark Canyon Creek
all_mapped$Longitude[all_mapped$stream == "D1"] <- angle2dec("120 38 48")
all_mapped$Latitude[all_mapped$stream == "D1"] <- angle2dec("38 54 21")

## Deep Canyon Creek
all_mapped$Longitude[all_mapped$stream == "D2"] <- angle2dec("120 40 14")
all_mapped$Latitude[all_mapped$stream == "D2"] <- angle2dec("38 55 13")

## Gaddis Creek
all_mapped$Longitude[all_mapped$stream == "G1"] <- angle2dec("120 38 31")
all_mapped$Latitude[all_mapped$stream == "G1"] <- angle2dec("38 52 12")

all_mapped$Longitude[all_mapped$stream == "G2"] <- angle2dec("120 38 31")
all_mapped$Latitude[all_mapped$stream == "G2"] <- angle2dec("38 52 12")

## Mutton Creek
all_mapped$Longitude[all_mapped$stream == "M1"] <- angle2dec("120 38 39")
all_mapped$Latitude[all_mapped$stream == "M1"] <- angle2dec("38 54 17")


## -----------------Find pilot study-----------------




world_map <- map_data('world') # check whether the GPS coordinates match expectations
world <- ggplot(world_map) + 
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  coord_fixed() + 
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())
points <- world + geom_point(data=all_mapped %>% distinct(Latitude, Longitude, Year), 
                             aes(x=Longitude, y=Latitude, fill = Year), shape=21)

points + coord_fixed(xlim=c(100,150), ylim=c(0,50))




## ---------------------------Depth Elevation---------------------------
## assign Depth Elevation
all_mapped$DepthElevation[all_mapped$stream == "B1"] <- 1380
all_mapped$DepthElevation[all_mapped$stream == "B2"] <- 1380
all_mapped$DepthElevation[all_mapped$stream == "D1"] <- 1452
all_mapped$DepthElevation[all_mapped$stream == "D2"] <- 1280
all_mapped$DepthElevation[all_mapped$stream == "G1"] <- 1243
all_mapped$DepthElevation[all_mapped$stream == "G2"] <- 1243
all_mapped$DepthElevation[all_mapped$stream == "M1"] <- 1310

## -----------------Find pilot study-----------------




## ---------------------------Taxonomic fields---------------------------
## check
unique(all_mapped$Genus)
unique(all_mapped$Family)

## convert unknown to NA
index <- str_which(all_mapped$Genus, 'unknown')
all_mapped[index,"Genus"] <- NA

index <- str_which(all_mapped$Family, 'unknown')
all_mapped[index,"Genus"] <- NA

all_mapped$Family[all_mapped$Family == "unknown #1"] <- NA

## get species from Genus
all_mapped$Species <- word(all_mapped$Genus, start=2)
all_mapped$Genus <- word(all_mapped$Genus, start=1)

all_mapped$Species[all_mapped$Species == "sp."] <- "sp1"
all_mapped$Species[all_mapped$Species == "sp"] <- "sp1"


## check again
unique(all_mapped$Family)
unique(all_mapped$Genus)
unique(all_mapped$Species)
## look good


## --------------------- check Family ---------------------
## check names that are not mapped successfully
check <- all_mapped %>% filter(is.na(Family))

unique(check$Order)

## put Order into family
index <- which(is.na(all_mapped$Family))
unique(all_mapped[index,"Order"]) ## check

all_mapped[index,"Family"] <- all_mapped[index,"Order"]

## again check names that are not mapped successfully
check <- all_mapped %>% filter(is.na(Family))

unique(check$Order) ## none
unique(check$Class) ## none


## Final check
check <- all_mapped %>% filter(is.na(Family))
unique(check$Order) ## none
unique(check$Class) ## none

sum(is.na(all_mapped$Family)) ## 0
## Done


## ------ sanity check -----
unique(all_mapped$Class)
check <- all_mapped %>% filter(Class == "Non-Insect")
unique(check$Family)

## check that genera are genera, not family names (-idae/eae)
str_which(all_mapped$Species, 'idae$|eae$') ## none


## rename column
colnames(all_mapped)[colnames(all_mapped) == "stream"] = "Site"

# --------------------------- aggregate biomass & abundance records that are same species, plot, and survey day.
## aggregate abundance
all_aggre <- all_mapped %>% group_by(Biomass,Family, Genus, Species, Plot, Latitude, Longitude, 
                                     DepthElevation, Day, Month, Year, StudyID, Site) %>% 
  summarise(Abundance=sum(Abundance)) %>% ungroup() %>% arrange(Year, Family, Genus, Species)

dim(all_aggre)[1]-dim(all_mapped)[1] # check changes

##  ---------------------------Remove records with no AB ---------------------------
## covert 0 Biomass to NA
all_aggre$Biomass[all_aggre$Biomass == 0] <- NA
## covert 0 Abundance to NA
all_aggre$Abundance[all_aggre$Abundance == 0] <- NA
## remove records with no Abundance and Biomass
all_aggre <- all_aggre[!with(all_aggre,is.na(Abundance)& is.na(Biomass)),]

## check
all_aggre %>% filter(is.na(Family)) # none

##  ---------------------------add SampleDescription ---------------------------
all_aggre$SampleDescription <- as.factor(with(all_aggre, paste(all_aggre$Site, all_aggre$Year, sep='_')))
unique(all_aggre$SampleDescription)

##  ---------------------------add Plot ---------------------------
## none

##  ---------------------------final check ---------------------------
str(all_aggre)


##  ---------------------------final format ---------------------------
all_aggre <- all_aggre[c('Abundance',
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

str(all_aggre) # final check!

## Export
write.csv(all_aggre,file="./BFRS_macroinverts_XK.csv")

## for calculation only
all_aggre_cal <- all_aggre %>% filter(!is.na(Latitude))

dt_coord <- all_aggre_cal %>% select(Longitude, Latitude) %>% distinct() %>%
  st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()

# 2. Calculate convex hull, area and centroid
centroid <- dt_coord %>% st_convex_hull() %>% st_centroid() %>% unlist # get centroid
write_clip(centroid[c(2,1)]) # copy as lat-long

# transform to mercator to get area in sq km
area <- st_transform(dt_coord, st_crs("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs")) %>%
  st_convex_hull() %>% st_area()  ##get area in sq km
write_clip(area)


write_clip(all_aggre) # copy



