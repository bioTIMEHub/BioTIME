################################################################################
# Curation Script: Henk Wolda Insect Collection (STRI-HWIC) - Light Traps Barro Colorado Island
# AFE
# August 2023
################################################################################


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
library(readxl)
library(measurements)
library(stringdist)


# Main source: =================================================================
# https://panamabiota.org/stri/collections/misc/collprofiles.php?collid=22

rm(list=ls())
setwd("C:/Users/afe1/OneDrive - University of St Andrews/PHD/BioTIMEGithub")
myd <- getwd()


# Read raw data files ==========================================================
files_dir <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIME/BioTIMENewStudiesCuration/Henk_Wolda_Collection_BCI_data_AFE"

dt <- read.csv(paste0(files_dir, "/lighttrap_1974-92/lighttrap_1974-92.csv"), h=T)    # Abundance records (weekly grain): 1540360
dh <- read.csv(paste0(files_dir, "/distribution_horiz.csv"), h=T)
gz <- read_excel(paste0(files_dir, "/gazetteer.xlsx"), sheet="gazetteer")             # Information about the locations
sps <- read.csv(paste0(files_dir, "/bci_species_2011.csv"), h=T)


# Check main data file structure ===============================================
head(dt)
str(dt)


# Abundances ===================================================================

sum(dt$upper==-9)  # 1433565
sum(dt$upper==0)   # 95856
sum(dt$lower==-9)  # 1433565
sum(dt$lower==0)   # 95467

sum(dt$total==-9)  # 7020
sum(dt$total==0)   # 1414611

dt <- dt %>%
  filter(dt$total>0|dt$lower>0|dt$upper>0) # 123589 observations
dt$upper[dt$upper==-9] <- 0
dt$lower[dt$lower==-9] <- 0
dt$total[dt$total==-9] <- 0

sum(dt$total==0)       # 4860
sum(dt$total>0 & dt$upper==0 & dt$lower==0)  # 106815 (only totals provided in many cases)

FALSE %in% (dt$total == dt$upper + dt$lower) # TRUE
dt1 <- subset(dt, dt$total>0)
dt1 <- dt1 %>%
  mutate(FLAG = case_when(upper + lower != total ~ "FLAG")) %>%
  filter(., FLAG=="FLAG")
range(dt1$upper[dt1$FLAG=="FLAG"])
range(dt1$lower[dt1$FLAG=="FLAG"])           # only totals are provided

dt$total <- ifelse(dt$total==0, dt$upper + dt$lower, dt$total)
range(dt$total)  # 1 to 2336


# Locations ====================================================================

gzBCI <- dplyr::filter(gz, grepl("Barro",site))
gzBCI   # three points, but all with the same Lat & Long info

dt$Latitude <- rep("9 09 00", nrow(dt))
dt$Longitude <- rep("-79 45 00", nrow(dt))
dt$Latitude = as.numeric(measurements::conv_unit(dt$Latitude, from = 'deg_min_sec', to = 'dec_deg')) 
dt$Longitude <- as.numeric(measurements::conv_unit(dt$Longitude, from = 'deg_min_sec', to = 'dec_deg'))

world_map <- map_data("world") 
world <- ggplot() + coord_fixed() +xlab("") + ylab("") 
world  <- world  + 
  geom_polygon(data=world_map, aes(x=long, y=lat, group=group), 
               colour="gray60", fill="gray60") 
points <- world  + 
  geom_point(data=dt, 
             aes(x=Longitude, y=Latitude, alpha=0.01)) 
points 


(points_zoom <- points +
    ylim(0,10)+
    xlim(-85,-75)) 


# Sampling Event Date: =========================================================

sort(unique(dt$year))
sort(unique(dt$week))


################################################################################
# Taxonomy =====================================================================

sum(is.na(dt$name))  # 0
sum(dt$name=="")     # 55827
sum(dt$name==" ")    # 0
sum(dt$name=="  ")   # 0
sum(is.null(dt$name))# 0
55827/123589*100     # 45%

dt$name2 <- dh$name[match(dt$species, dh$code)]
dt$name3 <- dh$name[match(dt$species2, dh$code)]

sort(unique(dt$name))
sum(dt$name=="")           # 55827
sum(is.na(dt$name))        # 0
dt$name[dt$name==""] <- NA # (substitute)

sum(dt$name3=="", na.rm=T) # 15036   
sum(is.na(dt$name3))       # 51534
dt$name3[dt$name3==""] <- NA # (substitute)

sum(dt$name2=="", na.rm=T) # 833
sum(is.na(dt$name2))       # 119842
dt$name2[dt$name2==""] <- NA # (substitute)


sum(is.na(dt$name) & !is.na(dt$name2))   # 2914
sum(is.na(dt$name) & !is.na(dt$name3))   # 2990
sum(!is.na(dt$name2) & !is.na(dt$name3)) # 2914 (take name3)
sum(!is.na(dt$name))                     # 67762
67762 + 2990 # 70752


dt$name <- ifelse(is.na(dt$name) & !is.na(dt$name3), dt$name3, dt$name) # when name is NA but name3 is not, name = name3
sum(!is.na(dt$name))                     # 70752
sum(is.na(dt$name))                      # 52837

 ################################################################################
# If trying subfamily: ---------------------------------------------------------
dt$subfam1 <- dh$sub_family[match(dt$species, dh$code)]
dt$subfam2 <- dh$sub_family[match(dt$species2, dh$code)]

sum(dt$subfam1=="", na.rm=T) # 159   
sum(is.na(dt$subfam1))       # 119842
sum(dt$subfam2=="", na.rm=T) # 3281   
sum(is.na(dt$subfam2))       # 51534
dt$subfam2[dt$subfam2==""] <- NA # (substitute)
sum(is.na(dt$name) & !is.na(dt$subfam2))   # 11993
sum(is.na(dt$name) & is.na(dt$subfam2))    # 40844
40844/123589*100                           # 33 %

# If trying family: ------------------------------------------------------------
dt$fam1 <- dh$family[match(dt$species, dh$code)]
dt$fam2 <- dh$family[match(dt$species2, dh$code)]

sum(dt$fam1=="", na.rm=T)  # 0   
sum(is.na(dt$fam1))        # 119842
sum(dt$fam2=="", na.rm=T)  # 0   
sum(is.na(dt$fam2))        # 51534

sum(is.na(dt$name) & !is.na(dt$fam2))   # 15015
sum(is.na(dt$name) & is.na(dt$fam2))    # 37822
37822/123589*100                        # 31 %


# If trying sup_family: --------------------------------------------------------
dt$supfam1 <- dh$sup_family[match(dt$species, dh$code)]
dt$supfam2 <- dh$sup_family[match(dt$species2, dh$code)]

sum(dt$supfam1=="", na.rm=T)  # 0   
sum(is.na(dt$supfam1))        # 119842
sum(dt$supfam2=="", na.rm=T)  # 0   
sum(is.na(dt$supfam2))        # 51534

sum(is.na(dt$name) & !is.na(dt$supfam2))   # 15015
sum(is.na(dt$name) & is.na(dt$supfam2))    # 37822
37822/123589*100                           # 31 %


# if trying tribe: -------------------------------------------------------------
dt$tribe1 <- dh$tribe[match(dt$species, dh$code)]
dt$tribe2 <- dh$tribe[match(dt$species2, dh$code)]

sum(dt$tribe1=="", na.rm=T)   # 1085  
sum(is.na(dt$tribe1))         # 119842
sum(dt$tribe2=="", na.rm=T)   # 19329 
dt$tribe2[dt$tribe2==""] <- NA # (substitute)
sum(is.na(dt$tribe2))         # 70863

sum(is.na(dt$name) & !is.na(dt$tribe2))    # 516
sum(is.na(dt$name) & is.na(dt$tribe2))     # 52321
52321/123589*100                           # 42 %


# Data in Family & Sup_Family (overlap): ---------------------------------------

unique(dh$tribe[is.na(dh$family)]) # 0
unique(dh$tribe[dh$family==""])    # ""

unique(dh$sup_family[is.na(dh$family)]) # 0
unique(dh$sup_family[dh$family==""])    # "Membracoidea" (only a few)
check <- unique(dh$code[dh$family=="" & dh$sup_family=="Membracoidea"])
check %in% dt$species   # FALSE
check %in% dt$species2  # FALSE

unique(dh$sub_family[is.na(dh$family)]) # 0
unique(dh$sub_family[dh$family==""])    # ""

################################################################################

names(dt)[names(dt)=="fam2"] <- "FamilyFinal"
sum(!is.na(dt$name) & is.na(dt$FamilyFinal))    # 13712
sum(!is.na(dt$name) & !is.na(dt$FamilyFinal))   # 57040
sum(is.na(dt$name) & is.na(dt$FamilyFinal))     # 37822
unique(dt$year[is.na(dt$name) & is.na(dt$FamilyFinal)])             # occur accross the series
dt$FLAG <- rep(NA, nrow(dt))
dt$FLAG <- ifelse(is.na(dt$FamilyFinal) & is.na(dt$name), "FLAG", NA)
sum(dt$FLAG=="FLAG", na.rm=T)


na_count_by_category <- dt %>%
  group_by(year) %>%
  summarise(na_count = sum(is.na(FLAG)),
            na_count2 = sum(is.na(FLAG))/(sum(is.na(FLAG))+sum(!is.na(FLAG))))
range(na_count_by_category$na_count2) # 60 to 85% of the yearly totals identified

na_count_by_category2 <- dt %>%
  group_by(year, week) %>%
  summarise(na_count = sum(is.na(FLAG)),
            na_count2 = sum(is.na(FLAG))/(sum(is.na(FLAG))+sum(!is.na(FLAG))))
range(na_count_by_category2$na_count2) # 39 to 100% of the sample totals


###############################################################################
# Approximate string matching : ------------------------------------------------
dtND <- subset(dt, dt$name=="" & is.na(dt$name2) & is.na(dt$name3))
dtND$codeAM <- dh$code[amatch(dtND$species2, dh$code, maxDist=Inf)]
dtND$nameAM <- dh$name[amatch(dtND$species2, dh$code, maxDist=Inf)]
dtND$familyAM <- dh$family[amatch(dtND$species2, dh$code, maxDist=Inf)]

37822/123589*100  # 31% unidentified

c1 <- dtND %>% 
  distinct(species2,species, name, name2, name3, codeAM, nameAM, familyAM)                       # some could be typos
c2 <- dtND %>% group_by(species) %>% summarise(nYear=n_distinct(year), nDate=n_distinct(date))   # variable
c3 <- dtND %>% group_by(species2) %>% summarise(nYear=n_distinct(year), nDate=n_distinct(date))  # variable
################################################################################


# Corrections of identified samples ============================================

# Name: ------------------------------------------------------------------------
sum(dt$name=="Unidentified", na.rm=T) # 3
dt$name[is.na(dt$name)] <- ""               # sth in the codes below cannot handle NAs
dt$FamilyFinal[is.na(dt$FamilyFinal)] <- "" # sth in the codes below cannot handle NAs


# "sp." to "sp"
# "n. sp" to "sp"
# "Amastris pseudoma culata" to "Amastris pseudomaculata"
# "Anormensis migrolimbata" to "Anormenis nigrolimbata"
# "Anormensis" to "Anormenis"  // Genus
# "Bolbonota (tubercunota) corrugata" to "Bolbonota corrugata"
# "Curtara (ardasona) magna" to "Curtara magna"
# "(c.) " to ""
# "Curtara (curtara) cumbresa" to "Curtara cumbresa"
# "(g.) " to ""
# "(m.) " to ""
# "Gypona (marganalana) targa" to ""Gypona targa"
# "Gyponana (zerana) secunda" to "Gyponana secunda"
# "Hecalapona (carapona) cedra" to "Hecalapona cedra"
# "(h.) " to ""
# "Hecalapona (hecalapona) brevisens" to "Hecalapona brevisens"
# "Patar sp." to "Patara sp"
# "Pintalis tacta" to "Pintalia tacta"
# "Polana (augusana)" to "Polana sp"
# "Polana (bohemonella) bohemani" to "Polana bohemani"
# "(p.) " to ""
# "Polana (polana) principia" to "Polana principia"
# "Polana (polanana) sp." to "Polana sp"
# "Polyamia/haldorus sp." to "Cicadellidae"
# "Ponana (neoponana) dulera" to "Ponana dulera"   
# "Ponana (neoponana) sp" to "Ponana sp"           
# "Ponana (polanana) pamana" to "Ponana pamana"    
# "Procyrta pectoralis fabricius" to "Procyrta pectoralis"
# "Scaphytopius (convelinus) marginelineatus" to "Scaphytopius marginelineatus"
# "Sphongophorus (lobocladisca) biclavatus" to "Sphongophorus biclavatus"
# "Trimedia sp./retiala proxima" to "Dictyopharidae"
# "Ugyops/eucanyra" to "Delphacidae"
# "Unerus/amplicephalus" to "Cicadellidae"
# "Unerusd sp." to "Unerus sp"
# "Unidentified"  [[rm]]
# "Vanduzeea mayana" to Vanduzea mayana"
# "Unidentified" to ""

dt$name <- gsub("sp.", "sp", dt$name)
dt$name[dt$name=="Amastris pseudoma culata"] <- "Amastris pseudomaculata"
dt$name[dt$name=="Anormensis migrolimbata"] <- "Anormenis nigrolimbata"
dt$name[dt$name=="Bolbonota (tubercunota) corrugata"] <- "Bolbonota corrugata"
dt$name[dt$name=="Curtara (ardasona) magna"] <- "Curtara magna"
dt$name <- gsub("\\(|\\)", "", dt$name)                                         # rm parenthesis
dt$name[dt$name=="Curtara curtara cumbresa"] <- "Curtara cumbresa"
dt$name[dt$name=="Gypona marganalana targa"] <- "Gypona targa"
dt$name[dt$name=="Gyponana zerana secunda"] <- "Gyponana secunda"
dt$name[dt$name=="Hecalapona carapona cedra"] <- "Hecalapona cedra"
dt$name[dt$name=="Hecalapona (hecalapona) brevisens"] <- "Hecalapona brevisens"
dt$name[dt$name=="Patar sp"] <- "Patara sp"
dt$name[dt$name=="Pintalis tacta"] <- "Pintalia tacta"
dt$name[dt$name=="Polana augusana"] <- "Polana sp"
dt$name[dt$name=="Polana bohemonella bohemani"] <- "Polana bohemani"
dt$name[dt$name=="Ponana neoponana dulera"] <- "Ponana dulera"
dt$name[dt$name=="Ponana neoponana sp"] <- "Ponana sp"
dt$name[dt$name=="Ponana polanana pamana"] <- "Ponana pamana"
dt$name[dt$name=="Polana polana principia"] <- "Polana principia"
dt$name[dt$name=="Polana polanana sp"] <- "Polana sp"
dt$name[dt$name=="Polyamia/haldorus sp"] <- "Cicadellidae"          # uncertain genus --> to family
dt$name[dt$name=="Scaphytopius convelinus marginelineatus"] <- "Scaphytopius marginelineatus"
dt$name[dt$name=="Sphongophorus lobocladisca biclavatus"] <- "Sphongophorus biclavatus"
dt$name[dt$name=="Procyrta pectoralis fabricius"] <- "Procyrta pectoralis" 
dt$name[dt$name=="Trimedia sp/retiala proxima"] <- "Dictyopharidae" # uncertain genus --> to family
dt$name[dt$name=="Ugyops/eucanyra"] <- "Delphacidae"                # uncertain genus --> to family
dt$name[dt$name=="Unerus/amplicephalus"] <- "Cicadellidae"          # uncertain genus --> to family
dt$name[dt$name=="Unerusd sp"] <- "Unerus sp"          
dt$name[dt$name=="Vanduzeea mayana"] <- "Vanduzea mayana" 
dt$name[dt$name=="Unidentified"] <- "" 

dt$name <- gsub(" c. ", " ", dt$name)
dt$name <- gsub(" n. ", " ", dt$name)
dt$name <- gsub(" g. ", " ", dt$name)
dt$name <- gsub(" m. ", " ", dt$name)
dt$name <- gsub(" h. ", " ", dt$name)
dt$name <- gsub(" p. ", " ", dt$name)

sort(unique(dt$name))

dt$GenusFinal <- str_split_fixed(dt$name, " ", 2)[,1]       # genus
dt$SpeciesFinal <- str_split_fixed(dt$name, " ", 2)[,2]     # epithet
sort(unique(dt$GenusFinal))
str(dt)

# "Anormensis"
# "Costanana."
# "Flatormensis"
# "Leiocyta"
# "Leiosita"
# "Pononella"
# "Portanos"

dt$GenusFinal[dt$GenusFinal=="Anormensis"] <- "Anormenis"   # corrected (GBIF)


dt[dt$GenusFinal =="Costanana.",]
dt$GenusFinal[dt$GenusFinal=="Costanana."] <- "Costanana"

dt[dt$GenusFinal =="Flatormensis",]        # Flatormenis griseoalba 
dt$SpeciesFinal[dt$GenusFinal=="Flatormensis"] <- "griseoalba"
dt$GenusFinal[dt$GenusFinal=="Flatormensis"] <- "Flatormenis"

dt[dt$GenusFinal =="Leiocyta",]
dt$GenusFinal[dt$GenusFinal=="Leiocyta"] <- "Leioscyta"

dt[dt$GenusFinal =="Leiosita",]
dt$SpeciesFinal[dt$GenusFinal=="Leiosita"] <- "beebei"
dt$GenusFinal[dt$GenusFinal=="Leiosita"] <- "Leioscyta"

dt[dt$GenusFinal =="Pononella",]
dt$GenusFinal[dt$GenusFinal=="Pononella"] <- "Ponanella"

dt[dt$GenusFinal =="Portanos",]
dt$GenusFinal[dt$GenusFinal=="Portanos"] <- "Portanus"

unique(dt$GenusFinal[grep("ae$", dt$GenusFinal)])
dt$FamilyFinal <- ifelse(dt$GenusFinal =="Delphacidae","Delphacidae", dt$FamilyFinal)
dt$FamilyFinal <- ifelse(dt$GenusFinal =="Cicadellidae", "Cicadellidae", dt$FamilyFinal)
dt$FamilyFinal <- ifelse(dt$GenusFinal =="Dictyopharidae", "Dictyopharidae", dt$FamilyFinal)
unique(dt$FamilyFinal)

dt$GenusFinal <- ifelse(dt$GenusFinal =="Delphacidae","", dt$GenusFinal)
dt$GenusFinal <- ifelse(dt$GenusFinal =="Cicadellidae", "", dt$GenusFinal)
dt$GenusFinal <- ifelse(dt$GenusFinal =="Dictyopharidae", "", dt$GenusFinal)


sort(unique(dt$SpeciesFinal))

dt[dt$SpeciesFinal=="aspr",]
dt$SpeciesFinal[dt$SpeciesFinal=="aspr"] <- "asper"

dt[dt$SpeciesFinal=="dispr",]
dt$SpeciesFinal[dt$SpeciesFinal=="dispr"] <- "dispar"

dt[dt$SpeciesFinal=="spctabilis",]
dt$SpeciesFinal[dt$SpeciesFinal=="spctabilis"] <- "spectabilis"


sort(unique(dt$FamilyFinal))      
sort(unique(dt$GenusFinal))
sort(unique(dt$SpeciesFinal))

sum(is.na(dt$FamilyFinal))
sum(is.na(dt$GenusFinal))
sum(is.na(dt$SpeciesFinal))

sum(dt$FamilyFinal=="" & dt$GenusFinal=="" & dt$SpeciesFinal=="") # 37822

dt1 <- subset(dt, is.na(dt$FLAG))
dt2 <- subset(dt, dt$FLAG=="FLAG")


dt2$newcode <- factor(dt2$species2, levels = unique(dt2$species2))
dt2$newcode2 <- paste0("sp", as.integer(dt2$newcode))
length(unique(dt2$species2))
length(unique(dt2$newcode2))
dt2$SpeciesFinal <- dt2$newcode2                                      # assign new standardised codes

dt2 <- within(dt2, rm(newcode, newcode2)) 

dt <- as.data.frame(rbind(dt1, dt2))


# rawData ======================================================================
rawData <- dt %>% group_by(FamilyFinal, GenusFinal, SpeciesFinal, Latitude, Longitude, week, year) %>%
  summarise(Abundance=sum(total)) # 102278

names(rawData) <- gsub("Final","",names(rawData))
names(rawData)[names(rawData)=="year"] <- "Year" 
rawData$Biomass <- rep(NA, nrow(rawData))
rawData <- rawData %>% relocate(c(Abundance, Biomass), .before = Family)

rawData$SampleDescription <- paste0(rawData$week, "_", rawData$Year)
rawData$Plot <- rep(NA, nrow(rawData))
rawData <- rawData %>% relocate(c(SampleDescription, Plot), .after = Species)

rawData$DepthElevation <- rep(NA, nrow(rawData))
rawData$Day <- rep(NA, nrow(rawData))
rawData$Month <- rep(NA, nrow(rawData))
rawData <- rawData %>% relocate(c(DepthElevation, Day, Month), .after = Longitude)

rawData$StudyID <- rep(NA, nrow(rawData))

names(rawData)
rawData <- within(rawData, rm(week))
range(rawData$Abundance) # 1 to 2336

sum(rawData$Family=="")
rawData$Family[rawData$Family==""] <- NA
sum(is.na(rawData$Family))

sum(rawData$Genus=="")
rawData$Genus[rawData$Genus==""] <- NA
sum(is.na(rawData$Genus))

sum(rawData$Species=="")
rawData$Species[rawData$Species==""] <- NA
sum(is.na(rawData$Species))

sum(is.na(rawData$Family) & is.na(rawData$Genus) & is.na(rawData$Species)) # 0


# Save =========================================================================

path <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIMEGithubMetadatas/Henk_Wolda_Collection_BCI_data_AFE"
write.csv(rawData, file=paste0(path, "/rawData.csv"), row.names = F)



# convex hulls =================================================================
# 1. Convert data points into point spatial object
dt_merged <- rawData
dt_coord <- dt_merged %>% select(Longitude, Latitude) %>% distinct() %>%
  st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()

# 2. Calculate convex hull, area and centroid
centroid <- dt_coord %>% st_convex_hull() %>% st_centroid() %>% unlist # get centroid
centroid

# transform to mercator to get area in sq km
area <- st_transform(dt_coord, st_crs("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs")) %>%
  st_convex_hull() %>% st_area()  ##get area in sq km
area # 0 because it's only one pair of central coords

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
  coord_sf(ylim=c(8,10), xlim=c(-81,-78)) + 
  labs(x = NULL, y = NULL) +
  theme_minimal()

# End of script ################################################################
################################################################################


