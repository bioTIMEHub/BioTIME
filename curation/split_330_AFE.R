################################################################################
# Splitting 330
# AFE
# March 2024
################################################################################



# Libraries ====================================================================
library(dplyr)
library(tidyverse)
library(ggplot2)
library(maps)
library(stringr)
library(lubridate)
library(stringi)
library(data.table)
library(sf)


rm(list=ls())


myd <- getwd()



# Read data: ===================================================================
mypath <- getwd() 
rawdatapath <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIME/BioTIMENewStudiesCuration/0_BioTIMEv1Revision/STUDY_330_AFE"
dt <- read.csv(paste0(rawdatapath, "/study330_observations.csv"), h=T)

dt$projectID <-str_split_fixed(dt$SAMPLE_DESC, "_", 7)[,7]
sort(unique(dt$projectID))
length(unique(dt$projectID)) # 38
Rm_ids <- c(2, 3, 5, 6, 8, 11, 12, 16, 24, 25, 29, 30, 31, 479, 603, 605, 607, 609, 615, 18)
dt <- dt[!dt$projectID %in% Rm_ids,]
c1 <- dt %>% group_by(projectID) %>% summarise(nY=n_distinct(YEAR)) # 2 to 13, OK


# Check climate ================================================================
dt_merged <- dt[dt$projectID==618,] # for eact study
dt_coord <- dt_merged %>% select(LONGITUDE, LATITUDE) %>% distinct() %>%
  st_as_sf(., coords = c("LONGITUDE", "LATITUDE"), crs = 4326) %>% st_union()
centroid <- dt_coord %>% st_convex_hull() %>% st_centroid() %>% unlist # get centroid
area <- st_transform(dt_coord, st_crs("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs")) %>%
  st_convex_hull() %>% st_area()  ##get area in sq km
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
  geom_point(data = dt_merged, aes(x = LONGITUDE, y = LATITUDE), size = 2, shape = 21, colour = 'blue') +
  geom_point(aes(x = centroid[1], y = centroid[2]), size = 3, colour = 'green') +
  geom_sf(data = dt_coord %>% st_convex_hull(), colour = 'blue', fill = 'transparent') +
  coord_sf(ylim=c(-50,-5), xlim=c(100,170)) + 
  labs(x = NULL, y = NULL) +
  theme_minimal()


# Split data: ==================================================================
dt_s <- split(dt, f=dt$projectID)
names(dt_s) <- paste0("ProjID", names(dt_s))
dt_s <- lapply(dt_s, function(x) {within(x, rm(projectID))})


# Save =========================================================================
path <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIMEGithubMetadatas/STUDY_330_AFE/RawData"
for(i in names(dt_s)){
  write.csv(dt_s[[i]], paste0(path, "/", i,".csv"), row.names=F)
}

coordslr <- lapply(dt_s, function(x){dt_coord <- x %>% select(LONGITUDE, LATITUDE) %>% distinct() %>%
  st_as_sf(., coords = c('LONGITUDE', 'LATITUDE'), crs = 4326) %>% st_union()})
centroidlr <- lapply(coordslr, function(x){x %>% st_convex_hull() %>% st_centroid() %>% unlist}) 
arealr <- lapply(coordslr, function(x){st_transform(x, st_crs("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs")) %>%
    st_convex_hull() %>% st_area()}) 


# End of script ################################################################
################################################################################

