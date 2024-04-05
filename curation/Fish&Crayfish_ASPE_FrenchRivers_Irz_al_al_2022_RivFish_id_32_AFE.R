################################################################################
# Study title: A long-term monitoring database on fish and crayfish species in French rivers
# Curator: AFE
# Date: 03/07/2023
################################################################################

# Main sources =================================================================
# https://zenodo.org/record/8099409
# https://rpubs.com/kamoke/715102

# Libraries ====================================================================
library(dplyr)
library(tidyverse)
library(ggplot2)
library(maps)
library(stringr)
library(lubridate)
library(stringi)
library(sf)
library(clipr)
library(rgdal)
library(aspe) # ASPE Data Package

rm(list=ls())

setwd("C:/Users/afe1/OneDrive - University of St Andrews/PHD/BioTIMEGithub")
myd <- getwd()


# Read raw data files ==========================================================
# Focus on 6 main tables described in main source:
files_dir <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIME/BioTIMENewStudiesCuration/Fish&Crayfish_ASPE_FrenchRivers_Irz_al_al_2022_RivFish_id_32_AFE"
load(paste0(files_dir,"/tables_sauf_mei.RData"))     # All tables
load(paste0(files_dir,"/mei.RData"))                 # Weights (individual)

list_tables <- c("ambiance", "groupe_points", "lot_poissons",
                 "operation", "operation_description_peche",
                 "point_prelevement", "prelevement_elementaire", "passage",
                 "ref_espece", "ref_localisation_ambiance", "ref_moyen_prospection",
                 "ref_objectif", "ref_protocole", "ref_sexe", "ref_type_projection", "ref_espece",
                 "ref_type_abondance", "ref_type_groupe_points", "ref_type_longeur", "ref_type_lot",
                 "ref_type_prelevement_elementaire", "station", "region", "departement", "commune",
                 "myd", "files_dir", "crayfishes", "crayfish_lop", "crayfish_codes", "aelb_ids")

rm(list=ls()[! ls() %in% list_tables])              # rm unnecessary tables 


# Merge core data tables =======================================================
dt <- mef_creer_passerelle() %>%
  mef_ajouter_ope_date() %>% 
  mef_ajouter_lots() %>% 
  mef_ajouter_esp()                                               # 5545203, core data


identical(sort(dt$lop_effectif), sort(lot_poissons$lop_effectif)) # T (same info)


# Add methods ==================================================================
ref_protocole
ref_type_prelevement_elementaire
ref_moyen_prospection

dt <- dt %>% 
  aspe::mef_ajouter_type_protocole() %>%
  aspe::mef_ajouter_type_prelevement() %>%
  aspe::mef_ajouter_moyen_prospection() %>%
  aspe::mef_ajouter_passage()

sort(unique(dt$pro_libelle))
sort(unique(dt$tpe_libelle))
sort(unique(dt$mop_libelle))
sort(unique(dt$pas_numero))
table(dt$mop_libelle)

sum(is.na(dt$pro_libelle))      # 0
sum(is.na(dt$tpe_libelle))      # 0
sum(is.na(dt$mop_libelle))      # 7092
sum(is.na(dt$pas_numero))       # 1891037

sort(unique(dt$pro_libelle[is.na(dt$mop_libelle)]))
sort(unique(dt$tpe_libelle[is.na(dt$mop_libelle)]))      # NAs occurr across programmes

dt$mop_libelle[is.na(dt$mop_libelle)] <- "Non renseigné" # Substitute by "undocumented"
sum(dt$mop_libelle=="Non renseigné")                     # 185402
dt <- dt[!dt$mop_libelle=="Non renseigné",]              # 5359801


# Add locations & elevation ====================================================
names(departement)[names(departement)=="dep_code_insee"] <- "dept"
dt <- dt %>%
  left_join(y = point_prelevement %>% 
              select(pop_id, pop_typ_id, pop_coordonnees_x, pop_coordonnees_y, pop_altitude)) %>% # Add coordinates, projection & altitude
  aspe::mef_ajouter_dept() %>%
  left_join(departement %>%
              select(dept, dep_reg_code_insee))                                                   # Department info to link with Region

dt$Region <- region$reg_libelle[match(dt$dep_reg_code_insee, region$reg_code_insee)]              # Add Region (for checks)


# Filter records by methods & duration =========================================
dt <- dt %>%
  filter(., pro_libelle %in% c("Pêche complète à un ou plusieurs passages", "Pêche par ambiances", 
                               "Pêche partielle par points (grand milieu)", "Pêche partielle sur berge")) %>%     # Remove species-specific surveys
  group_by(pop_id, pro_libelle, tpe_libelle, mop_libelle) %>% filter(., length(unique(annee)) > 1) %>% ungroup()  # Remove sampling points sampled only 1 year
# 4463045

dtcheck1 <- dt %>% group_by(pop_id, pro_libelle, tpe_libelle, mop_libelle) %>% summarise(n=n_distinct(annee))     # 2 to 38


# Sampling Event Date ==========================================================

dt$year <- str_split_fixed(dt$ope_date, "-", 2) [,1]
dt$month <- str_split_fixed(dt$ope_date, "-", 3) [,2]
dt$day <- str_split_fixed(str_split_fixed(dt$ope_date, "-", 3) [,3], " ", 2)[,1]
dt$time <- str_split_fixed(str_split_fixed(dt$ope_date, "-", 3) [,3], " ", 2)[,2]

sort(unique(dt$year))    # 1977 to 2022
sort(unique(dt$month))   # "01" "02" "03" "04" "05" "06" "07" "08" "09" "10" "11" "12"
sort(unique(dt$day))     # 01 to 31
sort(unique(dt$time))    # Diurnal & nocturnal (all sampling in mainland France)

dt$time2 <- as.numeric(str_split_fixed(dt$time, ":", 3)[,1])  # To assess quantity of diurnal & nocturnal surveys
sort(unique(dt$time2))   

dt$Period <- ifelse(dt$time2 < 6 | dt$time2 >20, "N", "D")
dt$Period <- ifelse(dt$time2==0, "0", dt$Period)              # Likely unknown times
table(dt$Period)                                              # A few nocturnal (5853), some 0 (125), most Diurnal (4038485)

dtcheck2 <- dt %>% group_by(pop_id, ope_id) %>% summarise(nD=n_distinct(day), nM=n_distinct(month), nY=n_distinct(year))     
range(dtcheck2$nD)
range(dtcheck2$nM)
range(dtcheck2$nY)       # one operation always takes place within one day (i.e., no multiple day operations)


# Locations (Regional) =========================================================

sum(is.na(dt$Region))             # 13147
unique(dt$Region)                 # mainland & Corsica

sort(unique(dt$pop_typ_id))       # 2 32 (two different projections)
sum(is.na(dt$pop_typ_id))         # 0

dt1 <- dt %>%
  filter(., pop_typ_id==2) %>%
  select(., pop_coordonnees_x, pop_coordonnees_y)
dt2 <- dt %>%
  filter(., pop_typ_id==32) %>%
  select(., pop_coordonnees_x, pop_coordonnees_y)

dt1 <- data.frame(lon=dt1$pop_coordonnees_x, lat=dt1$pop_coordonnees_y, pop_c_x=dt1$pop_coordonnees_x, pop_c_y=dt1$pop_coordonnees_y)
dt2 <- data.frame(lon=dt2$pop_coordonnees_x, lat=dt2$pop_coordonnees_y, pop_c_x=dt2$pop_coordonnees_x, pop_c_y=dt2$pop_coordonnees_y)
dt1T <- dt1[,c(1:2)]
dt2T <- dt2[,c(1:2)]
coordinates(dt1T) <- c("lon", "lat")
proj4string(dt1T) <- CRS("+init=epsg:2154") 

coordinates(dt2T) <- c("lon", "lat")
proj4string(dt2T) <- CRS("+init=epsg:27572") 

# Warning message:
#  In CPL_crs_from_input(x) :
#  GDAL Message 1: +init=epsg:XXXX syntax is deprecated. It might return a CRS with a non-EPSG compliant axis order.

CRS.new <- CRS("+init=epsg:4326") # WGS 84
df1 <-spTransform(dt1T, CRS.new)
df1 <- data.frame(df1, dt1[,c(3:4)])
df2 <-spTransform(dt2T, CRS.new)
df2 <- data.frame(df2, dt2[,c(3:4)])
sum(is.na(df1$lon))  # 0
sum(is.na(df1$lat))  # 0
sum(is.na(df2$lon))  # 0
sum(is.na(df2$lat))  # 0

dt$Latitude <- ifelse(dt$pop_typ_id==2, df1$lat[match(as.character(dt$pop_coordonnees_y), as.character(df1$pop_c_y))], NA)
dt$Latitude <- ifelse(dt$pop_typ_id==32, df2$lat[match(as.character(dt$pop_coordonnees_y), as.character(df2$pop_c_y))], dt$Latitude)

dt$Longitude <- ifelse(dt$pop_typ_id==2, df1$lon[match(as.character(dt$pop_coordonnees_x), as.character(df1$pop_c_x))], NA)
dt$Longitude <- ifelse(dt$pop_typ_id==32, df2$lon[match(as.character(dt$pop_coordonnees_x), as.character(df2$pop_c_x))], dt$Longitude)

sum(is.na(dt$Latitude))    # 0
sum(is.na(dt$Longitude))   # 0


# Check locations on global map ================================================

dtlocs <- dt %>% group_by(pop_id) %>% distinct(Latitude, Longitude)                                           # 4408
dtcheck3 <- dt %>% group_by(pop_id) %>% summarise(nlong=n_distinct(Longitude), nlat=n_distinct(Latitude))     # Always 1, OK


world_map <- map_data('world') 
ggplot(world_map) +
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  geom_point(data = dtlocs, aes(x = Longitude, y = Latitude), size = 2, colour = 'red') +
  labs(x = NULL, y = NULL) +
  theme_minimal()
ggplot(world_map) +
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  geom_point(data = dtlocs, aes(x = Longitude, y = Latitude), size = 2, colour = 'red') +
  xlim(-10,10) +
  ylim(40, 56)    # All OK except 31 pop ids below y=40º

dtcheck4 <- dt %>% group_by(pop_id) %>% filter(., Latitude < 40) %>% distinct(Latitude, Longitude, Region) 
sum(dt$Latitude < 40)        # 5689
dt <- dt[!dt$Latitude < 40,] # rm pop ids that fall outside expected area. 4457356

dtcheck4B <- dt %>% group_by(Latitude, Longitude) %>% summarise(n=n_distinct(pop_id)) #some pop_id in the same location


# Altitude =====================================================================

sum(is.na(dt$pop_altitude))      # 149185, NAs here are OK
range(dt$pop_altitude, na.rm=T)  # no negative number, OK
dtcheck5 <- dt %>% group_by(pop_id) %>% summarise(n=n_distinct(pop_altitude))                               # Always 1


# Area Sampled: ================================================================

dt <- dt %>%
  mef_ajouter_surf_calc()        # add areas

dtcheck6 <- dt %>% group_by(pop_id, ope_id) %>% summarise(nArea=n_distinct(ope_surface_calculee))           # OK, 1 area x operation
dtcheck7 <- dt %>% group_by(pop_id, ope_id) %>% summarise(nOp=n_distinct(tpe_libelle))                      # OK, always 1

sum(is.na(dt$ope_surface_calculee))            # 10732
sum(dt$ope_surface_calculee==0, na.rm=T)       # 8198

sort(unique(dt$pro_libelle[dt$ope_surface_calculee==0]))
sort(unique(dt$pro_libelle[is.na(dt$ope_surface_calculee)]))
sort(unique(dt$pro_libelle[dt$ope_surface_calculee >0]))

dt <- dt[!is.na(dt$ope_surface_calculee),]
dt <- dt[!dt$ope_surface_calculee==0,]         # rm operations with na or 0 in area, because it's not possible to calculate densities. 4438426
format(range(dt$ope_surface_calculee), scientific=F)


dtcheck8 <- dt %>% group_by(pop_id, pro_libelle, tpe_libelle, mop_libelle) %>% summarise(nY=n_distinct(annee))      
dt <- dt %>% group_by(pop_id, pro_libelle, tpe_libelle, mop_libelle) %>% filter(., length(unique(annee)) > 1) %>% ungroup() # remove a few records that after previous filtering now have data for 1 year only
# 4434923


# Revisit methods ==============================================================

dtcheck9 <- dt %>% group_by(pop_id, ope_id) %>% summarise(n=n_distinct(tpe_libelle))                         # Always 1
dtcheck10 <- dt %>% group_by(pop_id, pro_libelle, mop_libelle)  %>% summarise(n=n_distinct(tpe_libelle))     # Always 1

sort(unique(paste0(dt$pro_libelle, "_", dt$mop_libelle, "_", dt$tpe_libelle)))
sort(unique(paste0(dt$pro_libelle, "_", dt$mop_libelle)))

# NOTE:
# "Pêche complète à un ou plusieurs passages_A pied_Passage"            
# "Pêche complète à un ou plusieurs passages_En bateau_Passage"         
# "Pêche complète à un ou plusieurs passages_Mixte_Passage"             
# "Pêche par ambiances_A pied_Ambiance"                                 
# "Pêche par ambiances_En bateau_Ambiance"                              
# "Pêche par ambiances_Mixte_Ambiance"                                  
# "Pêche partielle par points (grand milieu)_A pied_Groupe de points"   
# "Pêche partielle par points (grand milieu)_En bateau_Groupe de points"
# "Pêche partielle par points (grand milieu)_Mixte_Groupe de points"    
# "Pêche partielle sur berge_A pied_Passage"                            
# "Pêche partielle sur berge_En bateau_Passage"                         
# "Pêche partielle sur berge_Mixte_Passage"  

# dt$pro_libelle and dt$tpe_libelle conatin the same info

# Test number of series & length of surveys using pro_libelle or pro_libelle + mop_libelle

dtcheck11 <- dt %>% group_by(pop_id, pro_libelle, mop_libelle)  %>% summarise(n=n_distinct(ope_id))
dtcheck12 <- dt %>% group_by(pop_id, pro_libelle)  %>% summarise(n=n_distinct(ope_id))


par(mfrow = c(1,2))
plot(table(dtcheck11$n))
plot(table(dtcheck12$n))
dev.off()                   # very similar distributions, hence keep mop_libelle in sample event id


# A Few checks =================================================================

dtcheckPass <- dt %>% group_by(pop_id, ope_id, pro_libelle, mop_libelle)  %>% 
  mutate(total_pass=max(pas_numero)) %>%
  summarise(nPass=n_distinct(total_pass)) # always 1, and there's NAs.


# Abundances & biomasses =======================================================
range(dt$lop_effectif)   # 1 - 11174, no NAs

# Individual counts: -----------------------------------------------------------
abundance <- dt %>% 
  group_by(pop_id,
           ope_id,
           pro_libelle, 
           mop_libelle,
           Latitude, 
           Longitude,
           pop_altitude,
           day,
           month,
           year,
           esp_code_alternatif,
           esp_nom_latin,
           ope_surface_calculee) %>% 
  summarise(n_ind = sum(lop_effectif)) %>% 
  ungroup() %>% 
  mutate(Abundance = 100 * n_ind / ope_surface_calculee) # aggregate abundances & compute density. 298984
range(abundance$n_ind)                             # 1 18853, some aggregation on dt
format(range(abundance$Abundance), scientific = F)   
str(abundance)
sum(abundance$n_ind) # 17M - 18M

# Biomasses: -------------------------------------------------------------------
range(lot_poissons$lop_poids, na.rm=T)
range(lot_poissons$lop_poids_estime, na.rm=T)
sum(is.na(lot_poissons$lop_poids) & is.na(lot_poissons$lop_poids_estime)) # 2504508 
sum(is.na(lot_poissons$lop_effectif))                                     # 0
#names(lot_poissons)[names(lot_poissons)=="lop_effectif"] <- "lop_effectif2"

lot_poissons$lop_poids[lot_poissons$lop_poids==0] <- NA
lot_poissons$lop_poids_estime[lot_poissons$lop_poids_estime==0] <- NA

sum(is.na(lot_poissons$lop_poids) & is.na(lot_poissons$lop_poids_estime)) # 3050227 (no weight info)

biomass <- dt %>% 
  left_join(y = lot_poissons %>% 
              select(lop_effectif,
                     lop_id,
                     lop_poids,
                     lop_poids_estime)) %>% 
  mutate(weight = ifelse(is.na(lop_poids),
                         lop_poids_estime,
                         lop_poids)) %>% 
  group_by(pop_id,
           ope_id,
           pro_libelle, 
           mop_libelle,
           Latitude, 
           Longitude,
           pop_altitude,
           day,
           month,
           year,
           esp_code_alternatif,
           esp_nom_latin,
           ope_surface_calculee) %>% 
  summarise(weight = sum(weight), n_ind=sum(lop_effectif)) %>% 
  ungroup() %>% 
  mutate(Biomass = 100 * weight / ope_surface_calculee) # aggregate biomasses & compute density. 298984
# NOTE: sum weight without na.rm=T because we do not want biomasses that correspond to less than the total 
# number of individuals sampled (i.e., this avoid inequivalent Abundance and Biomass)

identical(abundance$n_ind, biomass$n_ind)                # T

dt <- abundance %>% 
  left_join(y = biomass)                                 # 298984

sum(dt$n_ind==0)
sum(dt$Abundance==0)
sum(dt$weight==0, na.rm=T)                               
sum(dt$Biomass==0, na.rm=T)
sum(is.na(dt$weight))
sum(is.na(dt$Biomass))                                   # 124861
124861/298984*100                                        # approx. 60% of data contains both abundance & biomass


# Taxonomy =====================================================================

sum(is.na(dt$esp_nom_latin))     # 0
sort(unique(dt$esp_nom_latin))   # 118 different ones

dt$esp_nom_latin <- gsub("sp.", "sp", dt$esp_nom_latin)                      # Standardise to BT
dt$esp_nom_latin <- gsub("spp", "sp", dt$esp_nom_latin)                      # Standardise to BT

dt$esp_nom_latin[dt$esp_nom_latin=="Atherinapresbyter"] <- "Atherina presbyter"        # Correct typo
dt$esp_nom_latin[dt$esp_nom_latin=="Alosa fallax rhodanensis"] <- "Alosa fallax"       # Standardise to BT
dt$esp_nom_latin[dt$esp_nom_latin=="Carassius auratus auratus"] <- "Carassius auratus" # Standardise to BT
dt$esp_nom_latin[dt$esp_nom_latin=="Salmo trutta lacustris"] <- "Salmo trutta"         # Standardise to BT
dt$esp_nom_latin[dt$esp_nom_latin=="Petromyzontidae (famille)"] <- "Petromyzontidae"   # Standardise to BT

dt$esp_nom_latin[dt$esp_nom_latin=="Gymnocephalus cernuus"] <- "Gymnocephalus cernua"  # Taxonomic update
dt$esp_nom_latin[dt$esp_nom_latin=="Leuciscus cephalus"] <- "Squalius cephalus"        # Taxonomic update

dt$Genus <- str_split_fixed(dt$esp_nom_latin, " ", 2)[,1]
dt$Species <- str_split_fixed(dt$esp_nom_latin, " ", 2)[,2]
unique(dt$Species[dt$Genus %in% c("Abramis", "Astacidea", "Mugilidae", "Petromyzontidae", "Salmonidae")])
dt$Species[dt$Species==""] <- NA


dt$Family <- rep(NA, nrow(dt))
dt$Family[dt$Genus=="Astacidea"] <- "Astacidea"                       # Family or above in Family
dt$Genus[dt$Genus=="Astacidea"] <- NA
dt$Family[dt$Genus=="Cyprinidae"] <- "Cyprinidae"                     # Family
dt$Genus[dt$Genus=="Cyprinidae"] <- NA
dt$Family[dt$Genus=="Mugilidae"] <- "Mugilidae"                       # Family
dt$Genus[dt$Genus=="Mugilidae"] <- NA
dt$Family[dt$Genus=="Petromyzontidae"] <- "Petromyzontidae"           # Family
dt$Genus[dt$Genus=="Petromyzontidae"] <- NA
dt$Family[dt$Genus=="Salmonidae"] <- "Salmonidae"                     # Family
dt$Genus[dt$Genus=="Salmonidae"] <- NA

dt$Family[dt$Genus=="Hybride"] <- "Cyprinidae"                        # Standardise to BT
dt$Species[dt$Genus=="Hybride"] <- "sp1"
dt$Genus[dt$Genus=="Hybride"] <- NA

sort(unique(dt$Family))
sort(unique(dt$Genus))
sort(unique(dt$Species))

sum(is.na(dt$Family) & is.na(dt$Genus) & is.na(dt$Species))           # 0


# A Few checks =================================================================
dtOg <- mef_creer_passerelle() %>%
  mef_ajouter_ope_date() %>% 
  mef_ajouter_lots() %>% 
  mef_ajouter_esp()                                               # 5545203, core data
dtOg <- dtOg %>%
  left_join(lot_poissons)
test1A <- dtOg[dtOg$ope_id==6503 & dtOg$esp_nom_latin=="Cottus gobio",]
sum(test1A$lop_effectif)     # 992
sum(test1A$lop_poids)        # NA
sum(test1A$lop_poids_estime) # NA 
test1B <- dt[dt$ope_id==6503 & dt$esp_nom_latin=="Cottus gobio",]
test1B$n_ind                 # 992
test1B$weight                # NA
# NOTE: even though there are some available values for weights, total is OK as NA because 
# we only include fully equivalent individuals to weights records.

test2A <- dtOg[dtOg$ope_id==11830 & dtOg$esp_nom_latin=="Scardinius erythrophthalmus",] # OK
test2B <- dt[dt$ope_id==11830 & dt$esp_nom_latin=="Scardinius erythrophthalmus",]       # OK

test3A <- dtOg[dtOg$ope_id==33796 & dtOg$esp_nom_latin=="Phoxinus phoxinus",] 
sum(test3A$lop_effectif)     # 878
sum(test3A$lop_poids)        # 1512
test3B <- dt[dt$ope_id==33796 & dt$esp_nom_latin=="Phoxinus phoxinus",]      
test3B$n_ind                 # 878
test3B$weight                # 1512



# SampleDescription & data aggregation =========================================
mean(dt$ope_surface_calculee)                                         
dtcheck13 <- dt %>% group_by(pop_id, pro_libelle, mop_libelle)%>% summarise(minA=min(ope_surface_calculee),
                                                                 maxA=max(ope_surface_calculee),
                                                                 diffA=max(ope_surface_calculee)-min(ope_surface_calculee))
# Max difference of area sampled in a site over time is 0.259 km2
dtcheck14 <- dt %>% group_by(pop_id, pro_libelle, mop_libelle, ope_surface_calculee,
                              Latitude, Longitude, day, month, year)%>% summarise(operation=n_distinct(ope_id))
#View(dt[dt$pop_id=="7534",]) # a few of the identifiers above correspond to two operations, use operations in SampleDescription

rawdata <- dt %>%
  select(Abundance, Biomass, Family, Genus, Species, pop_id, ope_id, pro_libelle, 
         mop_libelle,
         ope_surface_calculee,
         Latitude, 
         Longitude,
         pop_altitude,
         day,
         month,
         year)

range(rawdata$Abundance)            # 0.0001 to 13550
range(rawdata$Biomass, na.rm=T)     # 0.0014 to 6041163
   

rawdata$Plot <- rep(NA, nrow(rawdata))
rawdata <- rawdata %>% relocate(Plot, .before=Latitude)

names(rawdata)[names(rawdata)=="pop_altitude"] <- "DepthElevation"
rawdata <- rawdata %>% relocate(DepthElevation, .before=day)

rawdata$StudyID <- rep(NA, nrow(rawdata))


rawdata$SampleDescription <- paste0(rawdata$pop_id, "_", rawdata$ope_id)
rawdata <- rawdata %>% relocate(SampleDescription, .before=Plot)


names(rawdata)[names(rawdata)=="day"] <- "Day"
names(rawdata)[names(rawdata)=="month"] <- "Month"
names(rawdata)[names(rawdata)=="year"] <- "Year"

str(rawdata)

rawdata$Day <- as.integer(rawdata$Day)
rawdata$Month <- as.integer(rawdata$Month)
rawdata$Year <- as.integer(rawdata$Year)


# Split ========================================================================
unique(paste0(rawdata$pro_libelle, "_", rawdata$mop_libelle))
rawdata$pro_libelle <- plyr::revalue(rawdata$pro_libelle, c("Pêche complète à un ou plusieurs passages"="Complete sampling single or multi pass",
                                                            "Pêche par ambiances"="Banks sampling stratified by habitat",
                                                            "Pêche partielle par points (grand milieu)"="Systematic point sampling",
                                                            "Pêche partielle sur berge"="Banks continuous sampling"))
rawdata$mop_libelle <- plyr::revalue(rawdata$mop_libelle, c("A pied"="On foot",
                                                            "En bateau"="On boat",
                                                            "Mixte"="Mixed"))

rawdata$ConcatMethods <- paste0(rawdata$pro_libelle, "_", rawdata$mop_libelle)
lr <- split(rawdata, f=rawdata$ConcatMethods)

# Grain size:
meanArealr <- lapply(lr, function(x) {mean(x$ope_surface_calculee)})
lr <- lapply(lr, function(x){within(x, rm(pop_id, pro_libelle, ope_id, mop_libelle,
                                          ope_surface_calculee, ConcatMethods))})
names(lr)

# Save =========================================================================
path <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIMEGithubMetadatas/Fish&Crayfish_ASPE_FrenchRivers_Irz_al_al_2022_RivFish_id_32_AFE/RawData"
for(i in names(lr)){
  write.csv(lr[[i]], paste0(path, "/", i,".csv"), row.names=F)
}


# Convex hulls =================================================================
# 1. Convert data points into point spatial object
# assumes your rawdata dataframe is called rawdata
dt_merged <- rawdata
dt_coord <- dt_merged %>% select(Longitude, Latitude) %>% distinct() %>%
  st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()

coordslr <- lapply(lr, function(x){dt_coord <- x %>% select(Longitude, Latitude) %>% distinct() %>%
  st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()})


# 2. Calculate convex hull, area and centroid
centroid <- dt_coord %>% st_convex_hull() %>% st_centroid() %>% unlist # get centroid
centroid
centroidlr <- lapply(coordslr, function(x){x %>% st_convex_hull() %>% st_centroid() %>% unlist}) 

# transform to mercator to get area in sq km
area <- st_transform(dt_coord, st_crs("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs")) %>%
  st_convex_hull() %>% st_area()  ##get area in sq km
area
arealr <- lapply(coordslr, function(x){st_transform(x, st_crs("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs")) %>%
    st_convex_hull() %>% st_area()}) 


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
  coord_sf(xlim = c(-20,20), ylim = c(20,70)) + 
  labs(x = NULL, y = NULL) +
  theme_minimal()


# End of script ################################################################
################################################################################

