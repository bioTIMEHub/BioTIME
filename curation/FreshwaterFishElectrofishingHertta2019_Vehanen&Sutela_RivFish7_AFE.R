##################################################################
# Study title: Finnish electrofishing register Hertta (2019)
# Curator: AFE
# Date: 27/06/2023
##################################################################

# Main sources ===================================================
# https://wwwp2.ymparisto.fi/koekalastus_sahko/yhteinen/Login.aspx?ReturnUrl=%2fkoekalastus_sahko

# Libraries ======================================================
library(dplyr)
library(tidyverse)
library(ggplot2)
library(maps)
library(stringr)
library(lubridate)
library(stringi)
library(readxl)
library(data.table)
library(sf)
library(clipr)
library(rgdal)

rm(list=ls())

setwd("C:/Users/Usuario/Documents/PHD/BioTIMEGithub")
myd <- getwd()


# Read raw data files =============================================

files_dir <- "C:/Users/Usuario/Documents/PHD/BIOTIME/BioTIMENewStudiesCuration/FreshwaterFishElectrofishingHertta2019_Vehanen&Sutela_RivFish7_AFE"
dtk <- read.csv2(paste0(files_dir, "/kokodata.csv"), h=T) # Prior steps: created a copy of original data. on this copy, "data type" for date changed to "short date" in Excel. On this copy, csv created. Reason: to be able to read the columns properly in R
dt <- read_excel(paste0(files_dir, "/RiverFishTimeFinland2022DATA.xlsx"), sheet = "Report results")
sps <- read_excel(paste0(files_dir, "/Key_to_Finnish_fish_names.xlsx"), sheet = "Taul1")
sps2 <- read_excel(paste0(files_dir, "/Key_to_Finnish_fish_names.xlsx"), sheet = "Taul2")


# Inspect data ====================================================

# 1) kokodata -----------------------------------------------------

names(dtk)
TRUE %in% duplicated(dtk) # FALSE, no dups

dtk <- dtk[,-c(156:171)] # rm columns referring to river features such as substrate composition & presence of organic matter
dtk <- within(dtk, rm(Kunta, Kalastusalue, Vesistöalue, Vesienhoitoalue, Pintavesityyppi, 
                      Uoma, Järvi, Ympäristötyyppi, Ympäristöpaine, Etäisyys.merestä, 
                      Koekalastaja, Koekalastajan.organisaatio, Käytetty.jännite, Pulssin.frekvenssi,
                      Virran.voimakkuus,Veden.lämpötila, Veden.sähkönjohtavuus, Veden.näkösyvyys, 
                      Keskimääräinen.virtausnopeus.koealalla, Sää, Veden.suhteellinen.korkeus,
                      Koealan.kalastettavuus, Vesisammalet, Putkilokasvit, Puut.ja.pensaat,
                      Muu.kasvillisuus, Ympäristöhavaintojen.lisätieto, Sähkökalastuskerran.alkuaika,
                      Sähkökalastuskerran.loppuaika)) # rm additional columns not needed for BioTIME


dtk <- dtk[! (names(dtk) %like% c("..g") | names(dtk) %like% c(".g"))] # rm biomass data
dtk <- gather(dtk, key="Species", value="Abundance", -c(1:20))

names(dtk) <- plyr::revalue(names(dtk), c("Vesimuodostuma"="River_code_and_name",
                                          "Seurantapaikka"="River_site",
                                          "Sähkökalastusalan.nimi"="Sampling_site",
                                          "Koordinaatit..YK..itä"="Longitude",
                                          "Koordinaatit..YK..pohj"="Latitude",
                                          "Jokiuoman.leveys"="Width_river_bed",
                                          "Sähkökalastusalan.lisätieto"="Notes",
                                          "Pyyntipäivämäärä"="Date",
                                          "Hanke"="Project",
                                          "Hanketyyppi"="Project_type",
                                          "Syvyysluokka"="Depth_class",
                                          "Sähkökalastuskerran.kesto"="Duration_electro",
                                          "Koealan.pituus"="Length",
                                          "Koealan.leveys"="Width",
                                          "Koealan.pinta.ala"="Surface_area",
                                          "Kalastettu.koko.uoman.leveydeltä"="Fished_over_bed_width",  # seems a yes or no category
                                          "Sulkuverkot"="Barrier_nets",   # yes and no
                                          "Tiedot.tarkistettu"="Information_checked",
                                          "Pyynnin.lisätieto"="Request_more_information",
                                          "Laitteen.malli"="Device_model")) # revalue Finnish variable names
names(dtk)
length(unique(dtk$River_code_and_name)) # 275
length(unique(dtk$Sampling_site))       # 915

# 2) RiverFishTimeFinland2022DATA ---------------------------------
"TRUE" %in% duplicated(dt) #FALSE, no dups

names(dt) <- gsub(" ", "_", names(dt))
length(unique(dt$River_code_and_name)) # 15
length(unique(dt$Sampling_site))       # 150
str(dt)


# Abundance & biomasses ===========================================

# 1) kokodata -----------------------------------------------------

sum(is.na(dtk$Abundance)) # 250489
sum(dtk$Abundance==0)     # NA
sum(dtk$Abundance=="")    # NA (also double-space)

dtk <- dtk[!is.na(dtk$Abundance),] # rm abundances=NA, 21991 obs
range(dtk$Abundance)               # 1 to 620, OK

# 2) RiverFishTimeFinland2022DATA ---------------------------------

sum(is.na(dt$`Fish_density_(Catch_/100m2,_comma_as_a_decimal_separator)`)) # 0
sum(dt$`Fish_density_(Catch_/100m2,_comma_as_a_decimal_separator)`==0)     # 2
dt <- dt[!dt$`Fish_density_(Catch_/100m2,_comma_as_a_decimal_separator)`==0,]
range(dt$`Fish_density_(Catch_/100m2,_comma_as_a_decimal_separator)`)      # OK

# Temporal data ===================================================

# 1) kokodata -----------------------------------------------------

dtk$Date <- as.character(dtk$Date)
dtk$year <- str_split_fixed(dtk$Date, "/", 3)[,3]
dtk$month <- str_split_fixed(dtk$Date, "/", 3)[,2]
dtk$day <- str_split_fixed(dtk$Date, "/", 3)[,1]

sort(unique(dtk$year))   # all values OK
sort(unique(dtk$month))  # all values OK
sort(unique(dtk$day))    # all values OK

# 2) RiverFishTimeFinland2022DATA ---------------------------------

dt$Date <- as.character(dt$Date)
dt$year <- str_split_fixed(dt$Date, "\\.", 3)[,3]
dt$month <- str_split_fixed(dt$Date, "\\.", 3)[,2]
dt$day <- str_split_fixed(dt$Date, "\\.", 3)[,1]

sort(unique(dt$year))   # all values OK
sort(unique(dt$month))  # all values OK
sort(unique(dt$day))    # all values OK

# Location data ===================================================

# 1) kokodata -----------------------------------------------------
dtkcheck1 <- dtk %>% group_by(year) %>% summarise(nR=n_distinct(River_code_and_name),         # 1 to 144
                                                 nS=n_distinct(Sampling_site))               # 4 to 421 (not all sites & rivers sampled every year)

dtcoords <- data.frame(lon=dtk$Longitude, lat=dtk$Latitude, 
                              lon_fin=dtk$Longitude, lat_fin=dtk$Latitude)
dtcoordsT <- dtcoords[,c(1:2)]
coordinates(dtcoordsT) <- c("lon", "lat")
proj4string(dtcoordsT) <- CRS("+init=epsg:2393") # projection is KKJ / Finland Uniform Coordinate System
CRS.new <- CRS("+init=epsg:4326") # WGS 84
df1 <-spTransform(dtcoordsT, CRS.new)
df1 <- data.frame(df1, dtcoords[,c(3:4)])
sum(is.na(df1$lon))  # 0
sum(is.na(df1$lat))  # 0

dtk$LatitudeII <- df1$lat[match(as.character(dtk$Latitude), as.character(df1$lat_fin))]
dtk$LongitudeII <- df1$lon[match(as.character(dtk$Longitude), as.character(df1$lon_fin))]

dtlocscheck <- dtk %>% distinct(LatitudeII, LongitudeII)
world_map <- map_data("world") 
world <- ggplot() + coord_fixed() +xlab("") + ylab("") 
world  <- world  + 
  geom_polygon(data=world_map, aes(x=long, y=lat, group=group), 
               colour="gray60", fill="gray60") 
points <- world  + 
  geom_point(data=dtlocscheck, 
             aes(x=LongitudeII, y=LatitudeII, alpha=0.01)) 
points       # seems OK

#Zoom:
points_zoom <- points +
  ylim(40,80)+
  xlim(10,50)
points_zoom  # OK

dtk$LatitudeII <- round(dtk$LatitudeII, 2)
dtk$LongitudeII <- round(dtk$LongitudeII, 2)


# 2) RiverFishTimeFinland2022DATA ---------------------------------

dt$Latitude <- as.numeric(str_split_fixed(dt$`Coordinates(EUREF-FIN_/_WGS84)`, "-", 2)[,1])
dt$Longitude <- as.numeric(str_split_fixed(dt$`Coordinates(EUREF-FIN_/_WGS84)`, "-", 2)[,2])

sum(is.na(dt$Latitude) | is.na(dt$Longitude))  # 0

world_map <- map_data("world") 
world <- ggplot() + coord_fixed() +xlab("") + ylab("") 
world  <- world  + 
  geom_polygon(data=world_map, aes(x=long, y=lat, group=group), 
               colour="gray60", fill="gray60") 
points <- world  + 
  geom_point(data=dt, 
             aes(x=Longitude, y=Latitude, alpha=0.01)) 
points       # OK

#Zoom:
points_zoom <- points +
  ylim(40,80)+
  xlim(10,50)
points_zoom  # OK

dt$Latitude <- round(dt$Latitude, 2)
dt$Longitude <- round(dt$Longitude, 2)

# Taxonomy ========================================================

# 1) kokodata -----------------------------------------------------

sort(unique(dtk$Species))
sps_all <- rbind(sps, sps2)   # Guide common Finnish names to latin names

# NOTE: ei.määritetty means unspecified (likely refers to age because this index is only given for Lohi and Taimen)

dtk$Species <- ifelse(dtk$Species %like% "Taimen", "Taimen", dtk$Species) # remove age class ids for Salmo trutta
dtk$Species <- ifelse(dtk$Species %like% "Lohi", "Lohi", dtk$Species)     # remove age class ids for Salmo salar

sort(unique(sps_all$`Finnish Name`))
setdiff(sort(unique(dtk$Species)), sort(unique(sps_all$`Finnish Name`)))
# setdiff(sort(unique(sps_all$`Finnish Name`)), sort(unique(dtk$Species)))

# Kirjolohi is rainbow trout (Oncorhynchus mykiss)
# Kolmipiikki is Gasterosteus aculeatus
# Muut.lajit is "other species"
# Nahkiainen.sp..toukka. is larvae of Nahkiainen
# Turpa is Squalius cephalus

dtk$Taxa <- ifelse(dtk$Species %in% sps_all$`Finnish Name`, sps_all$`Scientific Name`[match(dtk$Species, sps_all$`Finnish Name`)], dtk$Species)
dtk$Taxa <- plyr::revalue(dtk$Taxa, c("Coregonus lavaretus or Coregonus albula"="Coregonus sp",
                                      "Kirjolohi"="Oncorhynchus mykiss",
                                      "Kolmipiikki"="Gasterosteus aculeatus",
                                      "Nahkiainen.sp..toukka."="Lampetra sp",
                                      "Turpa"="Squalius cephalus"))
sum(dtk$Taxa=="Muut.lajit")           # 63 out of 21991
dtk <- dtk[!dtk$Taxa=="Muut.lajit",]  # 21928

dtk$Genus <- str_split_fixed(dtk$Taxa, " ", 2)[,1]
dtk$Species <- str_split_fixed(dtk$Taxa, " ", 2)[,2]

sort(unique(dtk$Genus))
sort(unique(dtk$Species))


# 2) RiverFishTimeFinland2022DATA ---------------------------------

sort(unique(dt$Species))  # OK
dt$Genus <- str_split_fixed(dt$Species, " ", 2)[,1]
dt$Species <- str_split_fixed(dt$Species, " ", 2)[,2]

sort(unique(dt$Genus))
sort(unique(dt$Species))


# Additional checks (kokodata) ====================================

sort(unique(dtk$Project))
sort(unique(dtk$Project_type))
sort(unique(dtk$Depth_class))
sort(unique(dtk$Duration_electro)) # 1 to 120

dtk[names(dtk) %in% c("Length", "Width", "Surface_area")] <- sapply(dtk[names(dtk) %in% c("Length", "Width", "Surface_area")],as.numeric)


range(dtk$Surface_area)      # 1 to 1440
dtkcheck2 <- dtk %>% group_by(Sampling_site) %>% summarise(minT=min(Surface_area), maxT=max(Surface_area), diff=maxT-minT) 
range(dtkcheck2$diff)        # Quite variable, so data needs standardization by sampling area.

dtkcheck3 <- dtk %>% group_by(Sampling_site, year) %>% summarise(n=n_distinct(Project_type)) # NOTE FROM AUTHORS: sampling effort highly comparable across monitoring programmes
dtkcheck4 <- dtk %>% group_by(Sampling_site, year) %>% summarise(nMonth=n_distinct(month), nDay=n_distinct(day))
#View(dtkcheck4[dtkcheck4$nMonth==1 & dtkcheck4$nDay==2,])

mean(dtk$Surface_area)


# Sample_desc & rawdata agg =======================================

dtk[names(dtk) %in% c("year", "month", "day")] <- sapply(dtk[names(dtk) %in% c("year", "month", "day")], as.numeric)
dt[names(dt) %in% c("year", "month", "day")] <- sapply(dt[names(dt) %in% c("year", "month", "day")], as.numeric)

str(dtk)
str(dt)


dtkcheck5 <- dtk %>% group_by(Sampling_site, year) %>% summarise(nLat=n_distinct(Latitude), nLong=n_distinct(Longitude))

length(unique(dtkcheck4$Sampling_site[dtkcheck4$nMonth > 1 | dtkcheck4$nDay > 1])) # 100
length(unique(dtkcheck4$Sampling_site))                                            # 915
# Sites sometimes monitored more than once a year

# 1) kokodata -----------------------------------------------------

dtk$Density <- (dtk$Abundance/dtk$Surface_area)*100  # compute density x obs for kokodata
range(dtk$Density) # 6.944444e-02 4.600000e+03

# Aggregate densities:
dtkraw <- dtk %>% 
  group_by(Species, Genus, LatitudeII, LongitudeII, Sampling_site, day, month, year) %>%
  summarise(DensitySum=sum(Density)) %>% ungroup() # 19394
  
range(dtkraw$DensitySum)

dtkraw$DensitySum <- round(dtkraw$DensitySum, 2) # round to 2 decimal points

names(dtkraw)[names(dtkraw)=="LatitudeII"] <- "Latitude"
names(dtkraw)[names(dtkraw)=="LongitudeII"] <- "Longitude"

# 2) RiverFishTimeFinland2022DATA ---------------------------------

# Aggregate densities:
names(dt)[names(dt)=="Fish_density_(Catch_/100m2,_comma_as_a_decimal_separator)"] <- "Density"
dtraw <- dt %>% 
  group_by(Species, Genus, Latitude, Longitude, Sampling_site, day, month, year) %>%
  summarise(DensitySum=sum(Density)) %>% ungroup() # 2267


range(dtraw$DensitySum)


# Check overlap between files =====================================
dtkraw$dup_concat <- paste0(dtkraw$Genus, "_", dtkraw$Species, "_",
                            dtkraw$Latitude, "_", dtkraw$Longitude, "_",
                            dtkraw$Sampling_site, "_",
                            dtkraw$day, "_", dtkraw$month, "_", dtkraw$year)
dtraw$dup_concat <- paste0(dtraw$Genus, "_", dtraw$Species, "_",
                           dtraw$Latitude, "_", dtraw$Longitude, "_",
                           dtraw$Sampling_site, "_",
                           dtraw$day, "_", dtraw$month, "_", dtraw$year)


dup <- subset(dtkraw, dtkraw$dup_concat %in% dtraw$dup_concat)            # 865 dups
sort(unique(dup$year))                                                    # 2010 to 2015
rivFishnon_dup <- subset(dtraw, !dtraw$dup_concat %in% dtkraw$dup_concat) # 1402 (rm dups)
sort(unique(rivFishnon_dup$year))                                         # 2010 to 2022
rivFishnon_dup <- rivFishnon_dup[!rivFishnon_dup$year < 2015,]            # (1254 kept) Remove records from 2010-2014, even if not duplicated, because density calculations are inequivalent to those in kokodata


# Merge files excluding duplicates=================================
names(rivFishnon_dup)
names(dtkraw)

dtnew <- rbind(dtkraw, rivFishnon_dup)
dtnewcheck1 <- dtnew %>% group_by(Sampling_site) %>% summarise(nYear=n_distinct(year))
dtnew <- dtnew %>%
  group_by(Sampling_site) %>%
  filter(., length(unique(year)) > 1) # rm sites with only one year of sampled data (20302)

# Structure rawdata ===============================================
rawdata <- dtnew

rawdatacheck1 <- rawdata %>% group_by(Sampling_site) %>% summarise(nLat=n_distinct(Latitude), nLong=n_distinct(Longitude)) # ok, always 1

rawdata <- within(rawdata, rm(dup_concat))
names(rawdata)[names(rawdata)=="DensitySum"] <- "Abundance"
rawdata$Biomass <- rep(NA, nrow(rawdata))

rawdata <- rawdata %>% relocate(Biomass, .after=Abundance)

rawdata$Family <- rep(NA, nrow(rawdata))
rawdata <- rawdata %>% relocate(c(Genus, Species), .after=Family)
rawdata$SampleDescription <- paste0(rawdata$Sampling_site, "_",
                                    rawdata$day, "_", rawdata$month, "_", rawdata$year) # sampling site & coordinates provide the same info
rawdata$Plot <- rep(NA, nrow(rawdata))
rawdata <- rawdata %>% relocate(c(Latitude, Longitude), .after=Plot)
rawdata$DepthElevation <- rep(NA, nrow(rawdata))
rawdata <- rawdata %>% relocate(c(day, month, year), .after=DepthElevation)
names(rawdata) <- plyr::revalue(names(rawdata), c("day"="Day",
                                                  "month"="Month",
                                                  "year"="Year"))
rawdata$StudyID <- rep(NA, nrow(rawdata))
rawdata <- within(rawdata, rm(Sampling_site))

str(rawdata)
dim(rawdata)
range(rawdata$Abundance)


path <- "C:/Users/Usuario/Documents/PHD/BIOTIMEGithubMetadatas/FreshwaterFishElectrofishingHertta2019_Vehanen&Sutela_RivFish7_AFE"
write.csv2(rawdata, file=paste0(path, "/rawdata.csv"), row.names = F)


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
  coord_sf(xlim = c(0,50), ylim = c(50,70)) + # make sure these fit your coordinates!
  labs(x = NULL, y = NULL) +
  theme_minimal()


# End of script ###################################################




