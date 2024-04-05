################################################################################
# Curation Script: DataAnuransSaoPaulo_GomesSantos_Vasconcelos_2009_AFE (Adults & Tadpoles)
# Curator: AFE
# Date: 2020 & February 2024
################################################################################

# Main sources =================================================================
#http://dx.doi.org/10.1080/00222930802702498
#https://link.springer.com/article/10.1007%2Fs10750-011-0762-9
#http://www.intechweb.org/
#https://doi.org/10.2994/057.006.0201


# Libraries ====================================================================
library(dplyr)
library(tidyverse)
library(ggplot2)
library(maps)
library(stringr)
library(lubridate)
library(stringi)
library(sf)
library(readxl)
library(measurements)
library(data.table)
library(sp)

rm(list=ls())


# Read Data ====================================================================
rawdata <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIME/BioTIMENewStudiesCuration/DataAnuransSaoPaulo_GomesSantos_Vasconcelos_2009_AFE"

# TGS:
tgs1 <- read_excel(paste0(rawdata, "/data_anurans_Sao Paulo_TGS.xlsx"), sheet="permanent stream 1")
tgs2 <- read_excel(paste0(rawdata, "/data_anurans_Sao Paulo_TGS.xlsx"), sheet="permanent stream 2")
tgs3 <- read_excel(paste0(rawdata, "/data_anurans_Sao Paulo_TGS.xlsx"), sheet="temporary pond 1")
tgs4 <- read_excel(paste0(rawdata, "/data_anurans_Sao Paulo_TGS.xlsx"), sheet="temporary pond 2")
tgs5 <- read_excel(paste0(rawdata, "/data_anurans_Sao Paulo_TGS.xlsx"), sheet="permanent dam 1")
tgs6 <- read_excel(paste0(rawdata, "/data_anurans_Sao Paulo_TGS.xlsx"), sheet="permanent dam 2")

# TSV:
tsv1 <- read_excel(paste0(rawdata, "/data_anurans_Sao Paulo_TSV.xlsx"), sheet="stream")
tsv2 <- read_excel(paste0(rawdata, "/data_anurans_Sao Paulo_TSV.xlsx"), sheet="permanent_swamp")
tsv3 <- read_excel(paste0(rawdata, "/data_anurans_Sao Paulo_TSV.xlsx"), sheet="temporary_pond1")
tsv4 <- read_excel(paste0(rawdata, "/data_anurans_Sao Paulo_TSV.xlsx"), sheet="temporary_pond2")
tsv5 <- read_excel(paste0(rawdata, "/data_anurans_Sao Paulo_TSV.xlsx"), sheet="semi-permanent_pond")
tsv6 <- read_excel(paste0(rawdata, "/data_anurans_Sao Paulo_TSV.xlsx"), sheet="permanent_pond")



# Initial Formatting ===========================================================
l1A <- list("Permanent stream 1"=tgs1[c(8:32),], 
            "Permanent stream 2"=tgs2[c(8:32),],
            "Temporary pond 1"=tgs3[c(8:32),],
            "Temporary pond 2"=tgs4[c(8:32),],
            "Permanent dam 1"=tgs5[c(8:32),],
            "Permanent dam 2"=tgs6[c(8:32),])
l1A <- lapply(l1A, function(x){x[,-ncol(x)]})                                   # rm totals
l1A <- lapply(l1A, function(x) {names(x) <- paste0(x[1,],"_", x[2,]);x})        # date as name
l1A <- lapply(l1A, function(x) {x[-c(1:2),]})                                   # rm month, year
l1A <- lapply(l1A, function(x) {gather(x, key="Date", value="Abundance", -1)})  # long format
l1A <- lapply(l1A, function(x) {names(x) <- c("Species", "Date", "Abundance");x})
l1A <- Map(cbind, l1A, "Site" = names(l1A))                                     # merge

l1T <- list("Permanent stream 1"=tgs1[c(36:51),], 
            "Permanent stream 2"=tgs2[c(36:51),],
            "Temporary pond 1"=tgs3[c(36:51),],
            "Temporary pond 2"=tgs4[c(36:51),],
            "Permanent dam 1"=tgs5[c(36:51),])
l1T <- lapply(l1T, function(x){x[,-ncol(x)]})  # rm totals
l1T <- lapply(l1T, function(x) {names(x) <- paste0(x[1,],"_", x[2,]);x})
l1T <- lapply(l1T, function(x) {x[-c(1:2),]})
l1T <- lapply(l1T, function(x) {gather(x, key="Date", value="Abundance", -1)})
l1T <- lapply(l1T, function(x) {names(x) <- c("Species", "Date", "Abundance");x})
l1T <- Map(cbind, l1T, "Site" = names(l1T))

l2A <- list("Stream"=tsv1[c(9:37),], 
            "Permanent swamp"=tsv2[c(8:36),], 
            "Temporary pond 3"=tsv3[c(9:37),], 
            "Temporary pond 4"=tsv4[c(9:37),], 
            "Semi-permanent pond"=tsv5[c(9:37),], 
            "Permanent pond"=tsv6[c(9:37),])   # no totals col
l2A <- lapply(l2A, function(x) {names(x) <- paste0(x[1,],"_", x[2,]);x})
l2A <- lapply(l2A, function(x) {x[-c(1:2),]})
l2A <- lapply(l2A, function(x) {gather(x, key="Date", value="Abundance", -1)})
l2A <- lapply(l2A, function(x) {names(x) <- c("Species", "Date", "Abundance");x})
l2A <- Map(cbind, l2A, "Site" = names(l2A))

l2T <- list("Stream"=tsv1[c(40:43),], 
            "Permanent swamp"=tsv2[c(39:49),], 
            "Temporary pond 3"=tsv3[c(40:47),], 
            "Temporary pond 4"=tsv4[c(40:46),], 
            "Semi-permanent pond"=tsv5[c(40:53),], 
            "Permanent pond"=tsv6[c(40:48),])
l2T <- lapply(l2T, function(x) {x[, colSums(is.na(x)) < nrow(x)]})
l2T <- lapply(l2T, function(x) {names(x) <- paste0(x[1,],"_", x[2,]);x})
l2T <- lapply(l2T, function(x) {x[-c(1:2),]})
l2T <- lapply(l2T, function(x) {gather(x, key="Date", value="Abundance", -1)})
l2T <- lapply(l2T, function(x) {names(x) <- c("Species", "Date", "Abundance");x})
l2T <- Map(cbind, l2T, "Site" = names(l2T))


ad <- as.data.frame(rbind(do.call(rbind, l1A), do.call(rbind, l2A)))            # adults
ad$Dataset <- rep("Adults", nrow(ad))
tad <- as.data.frame(rbind(do.call(rbind, l1T), do.call(rbind, l2T)))           # tadpoles
tad$Dataset <- rep("Tadpoles", nrow(tad))

dt <- as.data.frame(rbind(ad, tad))


# Fill in Meta Key: ============================================================
meta <- data.frame(matrix(ncol=4, nrow=12))
names(meta) <- c("Site", "Latitude", "Longitude", "Elevation")
meta$Site <- unique(ad$Site)


#Permanent stream 1:
conv_unit("-22 28 30.8", "deg_min_sec", "dec_deg")
conv_unit("-52 20 30.9", "deg_min_sec", "dec_deg")
#Lat: 22?28'30.8"S : -22.47522222222222
#Long:52?20'30.9"W : -52.34191666666667
#Elevation: 299

meta$Latitude <- ifelse(meta$Site=="Permanent stream 1", -22.47522222222222, meta$Latitude)
meta$Longitude <- ifelse(meta$Site=="Permanent stream 1", -52.34191666666667, meta$Longitude)
meta$Elevation <- ifelse(meta$Site=="Permanent stream 1", 299, meta$Elevation)


#Permanent stream 2:
conv_unit("-22 36 16.2", "deg_min_sec", "dec_deg")
conv_unit("-52 18 00.8", "deg_min_sec", "dec_deg")
#Lat: 22?36'16.2"S : -22.6045
#long: 52?18'00.8": -52.30022222222222
#Elevation: 299

meta$Latitude <- ifelse(meta$Site=="Permanent stream 2", -22.6045, meta$Latitude)
meta$Longitude <- ifelse(meta$Site=="Permanent stream 2", -52.30022222222222, meta$Longitude)
meta$Elevation <- ifelse(meta$Site=="Permanent stream 2", 299, meta$Elevation)


#Temporary pond 1:
conv_unit("-22 37 10.5", "deg_min_sec", "dec_deg")
conv_unit("-52 09 55.8", "deg_min_sec", "dec_deg")
#Lat: 22?37'10.5"S: -22.619583333333335
#long: 52?09'55.8"W: -52.1655
#Elevation: 263

meta$Latitude <- ifelse(meta$Site=="Temporary pond 1", -22.619583333333335, meta$Latitude)
meta$Longitude <- ifelse(meta$Site=="Temporary pond 1", -52.1655, meta$Longitude)
meta$Elevation <- ifelse(meta$Site=="Temporary pond 1", 263, meta$Elevation)


#Temporary pond 2:
conv_unit("-22 37 07.8", "deg_min_sec", "dec_deg")
conv_unit("-52 10 01.9", "deg_min_sec", "dec_deg")
#Lat: 22?37'07.8"S: -22.618833333333335
#long: 52?10'01.9"W: -52.16719444444444
#Elevation: 259

meta$Latitude <- ifelse(meta$Site=="Temporary pond 2", -22.618833333333335, meta$Latitude)
meta$Longitude <- ifelse(meta$Site=="Temporary pond 2", -52.16719444444444, meta$Longitude)
meta$Elevation <- ifelse(meta$Site=="Temporary pond 2", 259, meta$Elevation)


#Permanent dam 1:
conv_unit("-22 27 03.7", "deg_min_sec", "dec_deg")
conv_unit("-52 20 43.3", "deg_min_sec", "dec_deg")
#Lat: 22?27'03.7"S: -22.451027777777778
#long: 52?20'43.3"W: -52.34536111111111
#Elevation: 261

meta$Latitude <- ifelse(meta$Site=="Permanent dam 1", -22.451027777777778, meta$Latitude)
meta$Longitude <- ifelse(meta$Site=="Permanent dam 1", -52.34536111111111, meta$Longitude)
meta$Elevation  <- ifelse(meta$Site=="Permanent dam 1", 261, meta$Elevation)


#Permanent dam 2:
conv_unit("-22 37 00.4", "deg_min_sec", "dec_deg")
conv_unit("-52 10 09.5", "deg_min_sec", "dec_deg")
#Lat: 22?37'00.4"S: -22.616777777777777
#long: 52?10'09.5"W: -52.16930555555555
#Elevation: 264

meta$Latitude <- ifelse(meta$Site=="Permanent dam 2", -22.616777777777777, meta$Latitude)
meta$Longitude <- ifelse(meta$Site=="Permanent dam 2", -52.16930555555555, meta$Longitude)
meta$Elevation <- ifelse(meta$Site=="Permanent dam 2", 264, meta$Elevation)


#Stream:
conv_unit("-22 36 16.3", "deg_min_sec", "dec_deg")
conv_unit("-52 18 04.2", "deg_min_sec", "dec_deg")
#lat	22?36'16.3"S	
#long	52?18'04.2"W	
#altitude (m)	~300m	

meta$Latitude <- ifelse(meta$Site=="Stream", -22.604528, meta$Latitude)
meta$Longitude <- ifelse(meta$Site=="Stream", -52.301167, meta$Longitude)
meta$Elevation <- ifelse(meta$Site=="Stream", 300, meta$Elevation)


#Permanent swamp:
conv_unit("-22 37 01.0", "deg_min_sec", "dec_deg")
conv_unit("-52 10 08.8", "deg_min_sec", "dec_deg")
#lat	22?37'01.0"S
#long	52?10'08.8"W
#altitude (m)	~300m

meta$Latitude <- ifelse(meta$Site=="Permanent swamp", -22.616944, meta$Latitude)
meta$Longitude <- ifelse(meta$Site=="Permanent swamp", -52.169111, meta$Longitude)
meta$Elevation <- ifelse(meta$Site=="Permanent swamp", 300, meta$Elevation)


#Temporary pond 3:
conv_unit("-22 37 02.2", "deg_min_sec", "dec_deg")
conv_unit("-52 10 01.4", "deg_min_sec", "dec_deg")
#lat	22?37'02.2"S
#long	52?10'01.4"W
#altitude (m)	~300m

meta$Latitude <- ifelse(meta$Site=="Temporary pond 3", -22.617278, meta$Latitude)
meta$Longitude <- ifelse(meta$Site=="Temporary pond 3", -52.167056, meta$Longitude)
meta$Elevation <- ifelse(meta$Site=="Temporary pond 3", 300, meta$Elevation)


#Temporary pond 4:
conv_unit("-22 37 06.8", "deg_min_sec", "dec_deg")
conv_unit("-52 10 05.9", "deg_min_sec", "dec_deg")
#lat	22?37'06.8"S
#long	52?10'05.9"W
#altitude (m)	~300m

meta$Latitude <- ifelse(meta$Site=="Temporary pond 4", -22.618556, meta$Latitude)
meta$Longitude <- ifelse(meta$Site=="Temporary pond 4", -52.168306, meta$Longitude)
meta$Elevation <- ifelse(meta$Site=="Temporary pond 4", 300, meta$Elevation)


#Semi-permanent pond:
conv_unit("-22 32 43.7", "deg_min_sec", "dec_deg")
conv_unit("-52 14 02.9", "deg_min_sec", "dec_deg")
#lat	22?32'43.7"S
#long	52?14'02.9"W
#altitude (m)	300~m

meta$Latitude <- ifelse(meta$Site=="Semi-permanent pond", -22.545472, meta$Latitude)
meta$Longitude <- ifelse(meta$Site=="Semi-permanent pond", -52.234139, meta$Longitude)
meta$Elevation <- ifelse(meta$Site=="Semi-permanent pond", 300, meta$Elevation)


#Permanent pond:
conv_unit("-22 22 10.2", "deg_min_sec", "dec_deg")
conv_unit("-52 19 43.0", "deg_min_sec", "dec_deg")
#lat	22?22'10.2"S
#long	52?19'43.0"W
#altitude (m)	~300m

meta$Latitude <- ifelse(meta$Site=="Permanent pond", -22.3695, meta$Latitude)
meta$Longitude <- ifelse(meta$Site=="Permanent pond", -52.328611, meta$Longitude)
meta$Elevation <- ifelse(meta$Site=="Permanent pond", 300, meta$Elevation)
#View(meta)


# Format for BioTIME ===========================================================

# Abundance: -------------------------------------------------------------------
dt$Abundance <- as.numeric(dt$Abundance)
sum(is.na(dt$Abundance))      # 1
dt[is.na(dt$Abundance),]      # NA in raw files too (checked)

sum(dt$Abundance==0, na.rm=T) # 6438

dt <- dt[!is.na(dt$Abundance),]
dt <- dt[!dt$Abundance==0,]
range(dt$Abundance)           # 1 857


# Sampling Event Date: ---------------------------------------------------------
sort(unique(dt$Date))

dt$Year <- as.integer(str_split_fixed(dt$Date, "_", 2)[,1])
sort(unique(dt$Year))


dt$Month <- str_split_fixed(dt$Date, "_", 2)[,2]
sort(unique(dt$Month))
dt$Month <- plyr::revalue(dt$Month, c("Nov"=11, "Jun"=6, "Jul"=7, "Aug"=8,
                                      "Oct"=10, "Dec"=12, "Jan"=1, "Feb"=2,
                                      "Mar"=3, "Apr"=4, "Set"=9, "May"=5))
dt$Month <- as.integer(dt$Month)


# Sampling Event Meta: ---------------------------------------------------------

dt$Latitude <- meta$Latitude[match(dt$Site, meta$Site)]
sum(is.na(dt$Latitude))
dt$Longitude <- meta$Longitude[match(dt$Site, meta$Site)]
sum(is.na(dt$Longitude))
dt$DepthElevation <- meta$Elevation[match(dt$Site, meta$Site)]
range(dt$DepthElevation)

world_map <- map_data("world") 
world <- ggplot() + coord_fixed() +xlab("") + ylab("") 
world  <- world  + 
  geom_polygon(data=world_map, aes(x=long, y=lat, group=group), 
               colour="gray60", fill="gray60") 
points <- world  + 
  geom_point(data=dt, 
             aes(x=Longitude, y=Latitude, alpha=0.01)) 
points


# Taxonomy: --------------------------------------------------------------------
sort(unique(dt$Species))                                                        # adults
dt$Species <- gsub("_", " ", dt$Species)
dt$Genus <- str_split_fixed(dt$Species, " ", 2)[,1]
dt$Species <- str_split_fixed(dt$Species, " ", 2)[,2]
sort(unique(dt$Genus))
dt$Genus[dt$Genus=="Elaschistocleis"] <- "Elachistocleis"                       # correct typo
sort(unique(dt$Species))
dt$Species <- sub("^\\s+", "", dt$Species)                                      # rm extra spaces


# RawData: ---------------------------------------------------------------------
dtA <- subset(dt, dt$Dataset=="Adults")
dtT <- subset(dt, dt$Dataset=="Tadpoles")

dtA  <- dtA %>% group_by(Genus, Species, Site, Latitude, Longitude, DepthElevation, Month, Year) %>%
  summarise(Abundance=sum(Abundance))
dtT  <- dtT %>% group_by(Genus, Species, Site, Latitude, Longitude, DepthElevation, Month, Year) %>%
  summarise(Abundance=sum(Abundance))

dtA$Biomass <- rep(NA, nrow(dtA))
dtA$Family <- rep(NA, nrow(dtA))
dtA$SampleDescription <- paste0(dtA$Site, "_", dtA$Month, "_", dtA$Year)
dtA$Plot <- rep(NA, nrow(dtA)) 
dtA$Day <- rep(NA, nrow(dtA))
dtA$StudyID <- rep(NA, nrow(dtA))
length(unique(dtA$Site))
dtA <- within(dtA, rm(Site))
dtA <- dtA %>% relocate(c(Abundance, Biomass, Family), .before=Genus)
dtA <- dtA %>% relocate(c(SampleDescription, Plot), .before=Latitude)
dtA <- dtA %>% relocate(c(Day), .before=Month)
dtA <- as.data.frame(dtA)
str(dtA)

dtT$Biomass <- rep(NA, nrow(dtT))
dtT$Family <- rep(NA, nrow(dtT))
dtT$SampleDescription <- paste0(dtT$Site, "_", dtT$Month, "_", dtT$Year)
dtT$Plot <- rep(NA, nrow(dtT)) 
dtT$Day <- rep(NA, nrow(dtT))
dtT$StudyID <- rep(NA, nrow(dtT))
length(unique(dtT$Site))
dtT <- within(dtT, rm(Site))
dtT <- dtT %>% relocate(c(Abundance, Biomass, Family), .before=Genus)
dtT <- dtT %>% relocate(c(SampleDescription, Plot), .before=Latitude)
dtT <- dtT %>% relocate(c(Day), .before=Month)
dtT <- as.data.frame(dtT)
str(dtT)


# Save: ------------------------------------------------------------------------
path <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIMEGithubMetadatas/DataAnuransSaoPaulo_GomesSantos_Vasconcelos_2009_AFE"
write.csv(dtA, file=paste0(path, "/dtA.csv"), row.names = F)
write.csv(dtT, file=paste0(path, "/dtT.csv"), row.names = F)


# Convex Hulls: ================================================================
# 1. Convert data points into point spatial object
# assumes your rawdata dataframe is called rawdata
dt_merged <- dtT # checked idem info for dtA & dtT
dt_coord <- dt_merged %>% select(Longitude, Latitude) %>% distinct() %>%
  st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()

# 2. Calculate convex hull, area and centroid
centroid <- dt_coord %>% st_convex_hull() %>% st_centroid() %>% unlist # get centroid
centroid

# transform to mercator to get area in sq km
area <- st_transform(dt_coord, st_crs("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs")) %>%
  st_convex_hull() %>% st_area()  ##get area in sq km
area

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
  coord_sf(xlim = c(-53,-52), ylim = c(-23,-22)) + # make sure these fit your coordinates!
  labs(x = NULL, y = NULL) +
  theme_minimal()


# End of script ################################################################