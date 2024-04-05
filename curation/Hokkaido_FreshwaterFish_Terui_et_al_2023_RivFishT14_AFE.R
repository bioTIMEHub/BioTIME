##################################################################
# Study title: Metapopulation stability in branching river networks
# Curator: AFE
# Date: 26/06/2023
##################################################################

# Main sources ===================================================
#https://www.pnas.org/doi/abs/10.1073/pnas.1800060115
#https://www.pnas.org/doi/abs/10.1073/pnas.2218044120

# Libraries ======================================================
library(dplyr)
library(tidyverse)
library(ggplot2)
library(maps)
library(stringr)
library(lubridate)
library(stringi)
library(sf)
library(clipr)

rm(list=ls())

setwd("C:/Users/afe1/OneDrive - University of St Andrews/PHD/BioTIMEGithub")
myd <- getwd()

# Read raw data files =============================================
files_dir <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIME/BioTIMENewStudiesCuration/Hokkaido_FreshwaterFish_Terui_et_al_2023_RivFishT14_AFE"
dt <- read.csv(paste0(files_dir, "/data_hkd_prtwsd_fmt.csv"), h=T)
coords <- read.csv(paste0(files_dir, "/site-coordinate_hogosuimen_terui-org-2019.csv"), h=T)

# Checks with sources =============================================
str(dt)
str(coords)
dt <- hablar::retype(dt)


# General checks (area) ===========================================
range(dt$area) # 12.1 859.0
dtcheck1 <- dt %>% group_by(river, site) %>% summarise(minA=min(area), maxA=max(area))


# Abundance & biomasses ===========================================
dtcheck2 <- dt %>% group_by(river, site, year) %>% summarise(n=n_distinct(pass))         # OK, always 2
dtcheck3 <- dt %>% group_by(river, site) %>% summarise(n=n_distinct(year))               # OK, not always 2

range(dt$abundance)          # OK
sum(dt$abundance==0)         # 68594

dt <- dt[!dt$abundance==0,]  # 6206

range(dt$area)               # 12.1 859.0 (Standardization by area needed)
sort(unique(dt$area_unit))   # OK, always m2

# Temporal data ===================================================
sort(unique(dt$year))  # OK
dt$river_site <- paste0(dt$river, "_", dt$site)
dtcheck2 <- dt %>% group_by(river_site) %>% summarise(n=n_distinct(year))
remove1y <- unique(dtcheck2$river_site[dtcheck2$n==1]) # Remove 12 sites
dt <- dt[!dt$river_site %in% remove1y,] # 6122


# Location data ===================================================
sort(unique(dt$pass))  # OK
sort(unique(dt$site))  # OK, 8 codes
sort(unique(dt$river)) # OK, 32 watersheds

length(unique(dt$river_site))     # 117
sort(unique(dt$river_site))       # unique spatial sample id

coords$river_site <- paste0(coords$river, "_", coords$site)
length(unique(coords$river_site)) # 129

dt$latitude <- coords$latitude[match(dt$river_site, coords$river_site)]
dt$longitude <- coords$longitude[match(dt$river_site, coords$river_site)]
sum(is.na(dt$latitude) | is.na(dt$longitude))       # 0, OK

world_map <- map_data("world") 
world <- ggplot() + coord_fixed() +xlab("") + ylab("") 
world  <- world  + 
  geom_polygon(data=world_map, aes(x=long, y=lat, group=group), 
               colour="gray60", fill="gray60") 
points <- world  + 
  geom_point(data=dt, 
             aes(x=longitude, y=latitude, alpha=0.01)) 
points       

#Zoom:
points_zoom <- points +
  ylim(25,50)+
  xlim(115,150)
points_zoom 


# Taxonomy ========================================================

sort(unique(dt$latin))
dt$latin <- gsub("_spp", "_sp", dt$latin)                  # spp to sp
dt$latin <- gsub("_complex", "_agg.", dt$latin)            # complex to agg.
dt$latin[dt$latin=="Lethenteron_sp"] <- "Lethenteron_sp1"  # one species
sort(unique(dt$genus))                                     # OK


dt$species <- str_split_fixed(dt$latin,"_", 3)[,2]         # remove classifications under species
dt$species[dt$species=="castaneus"] <- "castaneus agg."    # make sure the complex is kept
sort(unique(dt$species))

sort(unique(dt$family))                                    # OK


# Sample_desc & rawdata agg =======================================
dtcheck4 <- dt %>% group_by(river_site, year) %>% summarise(n=n_distinct(pass))         # not always 2 because some passes yielded 0 abundance
dtcheck5 <- dt %>% group_by(river_site, year) %>% summarise(n=n_distinct(area))         # ok, always 1

dt$coords <- paste0(dt$latitude, "_", dt$longitude)
dtcheck6 <- dt %>% group_by(river_site) %>% summarise(n=n_distinct(coords))             # ok, always 1
length(unique(dt$coords))       # 117
length(unique(dt$river_site))   # 117

dt$sample_desc <- paste0(dt$year, "_", 
                         dt$latitude, "_", 
                         dt$longitude, "_",
                         dt$river_site)
dt$sample_desc_sp_area <- paste0(dt$family, "_", dt$genus, "_", dt$species, "_", dt$area, "_", dt$sample_desc)

dtraw <- aggregate(dt$abundance,
                   by = list(dt$sample_desc_sp_area),
                   FUN = sum) # 3729 (aggregated nº individuals per site, summed abundances of first and second passes)

"TRUE" %in% duplicated(dtraw) # FALSE

dtraw$area <- as.numeric(str_split_fixed(dtraw$Group.1, "_", 9) [,4])
dtraw$Abundance <- (dtraw$x/dtraw$area)*100 # nº individuals x 100m2

dtraw$Biomass <- rep(NA, nrow(dtraw))
dtraw$Family <- str_split_fixed(dtraw$Group.1, "_", 9) [,1]
dtraw$Genus <- str_split_fixed(dtraw$Group.1, "_", 9) [,2]
dtraw$Species <- str_split_fixed(dtraw$Group.1, "_", 9) [,3]

dtraw$Plot <- rep(NA, nrow(dtraw))
dtraw$Latitude <- as.numeric(str_split_fixed(dtraw$Group.1, "_", 9) [,6])
sum(is.na(dtraw$Latitude))
dtraw$Longitude <- as.numeric(str_split_fixed(dtraw$Group.1, "_", 9) [,7])
sum(is.na(dtraw$Longitude))
dtraw$DepthElevation <- rep(NA, nrow(dtraw))

dtraw$Day <- rep(NA, nrow(dtraw))
dtraw$Month <- rep(NA, nrow(dtraw))
dtraw$Year <- str_split_fixed(dtraw$Group.1, "_", 9) [,5]

dtraw$SampleDescription <- paste0(str_split_fixed(dtraw$Group.1, "_", 6) [,5], "_", str_split_fixed(dtraw$Group.1, "_", 8) [,8])

dtraw <- dtraw %>% relocate(SampleDescription, .before=Plot)
dtraw <- dtraw %>% relocate(Abundance, .before=Biomass)
dtraw$StudyID <- rep(NA, nrow(dtraw))
dtraw <- within(dtraw, rm(Group.1, x, area))

str(dtraw)
dim(dtraw)
range(dtraw$Abundance)

rawdata <- dtraw

path <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIMEGithubMetadatas/Hokkaido_FreshwaterFish_Terui_et_al_2023_RivFishT14_AFE"
write.csv(rawdata, file=paste0(path, "/rawdata.csv"), row.names = F)


# Convex hulls ====================================================
# 1. Convert data points into point spatial object
# assumes your rawdata dataframe is called rawdata
dt_merged <- rawdata
dt_coord <- dt_merged %>% select(Longitude, Latitude) %>% distinct() %>%
  st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()

# 2. Calculate convex hull, area and centroid
centroid <- dt_coord %>% st_convex_hull() %>% st_centroid() %>% unlist # get centroid
write_clip(centroid[c(2,1)]) # copy as lat-long

# transform to mercator to get area in sq km
area <- st_transform(dt_coord, st_crs("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs")) %>%
  st_convex_hull() %>% st_area()  ##get area in sq km
write_clip(area)

## also useful snippet if coordinates are ever in degree minutes seconds

angle2dec <- function(angle) {
  angle <- as.character(angle)
  x <- do.call(rbind, strsplit(angle, split=' '))
  x <- apply(x, 1L, function(y) {
    y <- as.numeric(y)
    y[1] + y[2]/60 + y[3]/3600
  })
  return(x)
}

world_map <- map_data('world') # check whether the GPS coordinates match expectations
ggplot(world_map) +
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  geom_point(data = dt_merged, aes(x = Longitude, y = Latitude), size = 2, shape = 21, colour = 'blue') +
  geom_point(aes(x = centroid[1], y = centroid[2]), size = 3, colour = 'green') +
  geom_sf(data = dt_coord %>% st_convex_hull(), colour = 'blue', fill = 'transparent') +
  coord_sf(xlim = c(130, 150), ylim = c(40,46)) + # make sure these fit your coordinates!
  labs(x = NULL, y = NULL) +
  theme_minimal()


# End of script ###################################################