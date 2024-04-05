##########################################################################################
# Curation Script: PhytoplanktonBahíaBlancaEstuary2008_2010_Guinder_et_al_2022_AFE 
# AFE
# July 2022 & August 2023
##########################################################################################

# Libraries ==============================================================================
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


# Main source: ---------------------------------------------------------------------------
# https://doi.org/10.1007/s00227-010-1530-5
# https://doi.org/10.1007/s12237-016-0134-9
# https://doi.org/10.1016/j.scitotenv.2017.08.002

rm(list=ls())
setwd("C:/Users/afe1/OneDrive - University of St Andrews/PHD/BioTIMEGithub")
myd <- getwd()


# Read raw data files ====================================================================
files_dir <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIME/BioTIMENewStudiesCuration/PhytoplanktonBahíaBlancaEstuary2008_2010_Guinder_et_al_2022_AFE"

# NOTE: Slight re-formatting in Excel for an easier import:
# Total counts across taxa removed, headers unmerged for OK 
# load in R. Date info to "date" format for correct load in R.

dta <- read.csv(paste0(files_dir, "/Phytoplankton2008_2010_Abundance.csv"), h=T) # Read OK
dtb <- read.csv(paste0(files_dir, "/Phytoplankton2008_2010_Biomass.csv"), h=T)   # Read OK


# Initial formatting =====================================================================
# NOTE: I work on the abundance dataset & the biomass dataset separately because labels 
# and structure are slightly different
names(dta)
names(dtb)


l <- list("cv_a"=dta[,c(1:29)], 
          "bm_a"=dta[,c(1,30:57)],
          "cv_b"=dtb[,c(1, 3:30)], # Ignoring col of added total biomasses ("X.1")
          "bm_b"=dtb[,c(1,31:58)]) # Ignoring col of added total biomasses ("X.1")

rows.remove <- c("", "Diatoms", "Dinoflagellates", "Flagellates",
                 "Others")         # Taxonomic classification headers
names(l$cv_a)
l <- lapply(l, function(x) {names(x) <- paste0(x[1,], "_", x[2,]); x})
l <- lapply(l, function(x) {x[-c(1:2),]})
names(l$cv_a)
l <- lapply(l, function(x) {names(x)[names(x)=="_Diatoms"] <- "Species";x})
l <- lapply(l, function(x) {x[!x$Species %in% rows.remove,]})

l <- lapply(l, function(x) {gather(x, key="date", value="value", -1)}) # Long format
l <- lapply(l, function(x) {x$plot <- rep(NA, nrow(x)); x
                            x$currency <- rep(NA, nrow(x));x})
# assign plots:
l[[1]]$plot <- "CV"
l[[2]]$plot <- "BM"
l[[3]]$plot <- "CV"
l[[4]]$plot <- "BM"

# assign currencies:
l[[1]]$currency <- "abundance"
l[[2]]$currency <- "abundance"
l[[3]]$currency <- "biomass"
l[[4]]$currency <- "biomass"

df <- as.data.frame(do.call(rbind, l))
df$value <- as.numeric(df$value)
str(df)


# Check format & observations ============================================================
data_count <- df %>%
  group_by(currency, plot) %>%
  summarise(count = n_distinct(date)) # 28 for all

df$day <- as.integer(str_split_fixed(df$date, "/", 2)[,1])
sort(unique(df$day))        # OK

df$month <- as.integer(str_split_fixed(df$date, "/", 3)[,2])
sort(unique(df$month))      # OK

df$year <- as.integer(str_split_fixed(str_split_fixed(df$date, "/", 3)[,3], "_", 2)[,1])
sort(unique(df$year))       # OK

df$replicate <- str_split_fixed(df$date, "_", 2)[,2]
sort(unique(df$replicate))  # OK

# Taxonomy ===============================================================================
identical(sort(unique(l[[1]]$Species)), sort(unique(l[[3]]$Species))) # FALSE, some named differently
setdiff(sort(unique(l[[1]]$Species)), sort(unique(l[[3]]$Species)))
setdiff(sort(unique(l[[3]]$Species)), sort(unique(l[[1]]$Species)))   # OK, two unidentified dinoflagellates

sort(unique(df$Species))

# Standardize to BioTIME -----------------------------------------------------------------
df$Species <- gsub("sp.", "sp", df$Species)    
df$Species <- gsub("sp ", "sp", df$Species)  

df$Species[df$Species=="Cyclotella sp(5-12 um)"] <- "Cyclotella sp" 

df$Species[df$Species=="dinoflagellate 1"] <- "Dinoflagellata sp1"  
df$Species[df$Species=="dinoflagellate 2"] <- "Dinoflagellata sp2"  
df$Species[df$Species=="unident. dinoflagellate 1"] <- "Dinoflagellata sp1"  
df$Species[df$Species=="unident. dinoflagellate 2"] <- "Dinoflagellata sp2"  

df$Species[df$Species=="flagellates (10.5-20 um)"] <- "Flagellata sp1"  
df$Species[df$Species=="flagellates (2-3 um)"] <- "Flagellata sp4"      
df$Species[df$Species=="flagellates (3-6 um)"] <- "Flagellata sp3"      
df$Species[df$Species=="flagellates (5-10 um)"] <- "Flagellata sp2"    
df$Species[df$Species=="unident. Flagellate (15 um)"] <- "Flagellata sp5"

df$Species[df$Species=="Fragilaria sp(6 x 65 um)"] <- "Fragilaria sp"  
df$Species[df$Species=="Nanogimnodinoids (5um)"] <- "Nanogimnodinoids"  

df$Species[df$Species=="Navicula aff cincta"] <- "Navicula aff. cincta"  
df$Species[df$Species=="Navicula sp(15x120 um)"] <- "Navicula sp1" 
df$Species[df$Species=="Navicula sp(15x55 um)"] <- "Navicula sp2" 
df$Species[df$Species=="Navicula sp(5-25 um)"] <- "Navicula sp3" 

df$Species[df$Species=="Nitzchia sp(8x375 um)"] <- "Nitzschia sp5"           # Typo corrected
df$Species[df$Species=="Nitzschia aff sigma"] <- "Nitzschia aff. sigma"
df$Species[df$Species=="Nitzschia sp4 (12x120 um)"] <- "Nitzschia sp4"

df$Species[df$Species=="T. curviseriata"] <- "Thalassiosira curviseriata"
df$Species[df$Species=="T. pacifica"] <- "Thalassiosira pacifica"
df$Species[df$Species=="Thalassiosira sp"] <- "Thalassiosira sp1"
df$Species[df$Species=="Thalassiosira sp(5-10 um)"] <- "Thalassiosira sp2"

df$Species[df$Species=="Cyclotella striata "] <- "Cyclotella striata"        # Remove end space
df$Species[df$Species=="Prorocentrun micans"] <- "Prorocentrum micans"       # Typo corrected


sort(unique(df$Species))


df$Genus <- str_split_fixed(df$Species, " ", 2)[,1]
df$Species <- str_split_fixed(df$Species, " ", 2)[,2]
lfam <- c("Coccolithophores", "Dinoflagellata", "Flagellata", "Nanogimnodinoids", "Nanopennadas", "Nanothecates")
df$Family <- ifelse(df$Genus %in% lfam, df$Genus, NA)
df$Genus <- ifelse(df$Genus %in% lfam, NA, df$Genus)


unique(df$Family)  # OK
unique(df$Genus)   # OK
unique(df$Species) # OK

df$Species[df$Species==""] <- NA

sum(is.na(df$Family) & is.na(df$Genus) & is.na(df$Species))  # 0, OK


# Temporal Data ==========================================================================

dfc1 <- df %>% group_by(month, year, day, plot, currency) %>% summarise(n=n_distinct(replicate))  # Always 2, OK

dfc2 <- df %>% group_by(month, year, day, plot, currency) %>% distinct(replicate) 
# (Visually inspected) Always the same replicate codes for abundance and biomass 
# in one sampling event (except 1 case of what's likely a typo in replicate name).

sort(unique(df$Species[df$currency=="biomass" & df$day==29 & df$replicate=="I?" & df$plot=="CV"]))    # OK, idem
sort(unique(df$Species[df$currency=="abundance" & df$day==29 & df$replicate=="I1" & df$plot=="CV"]))  # OK, idem

df$replicate[df$currency=="biomass" & df$month==7 & df$day==29 & df$plot=="CV" & df$year==2009 & df$replicate=="I?"] <- "I1"   # Correct l? for correct merging below
df$date[df$date=="29/07/2009_I?"] <- "29/07/2009_I1" # Correct l? for correct merging below


# Locations===============================================================================
# NOTE: given by authors in raw data files.

df$latitude <- rep(NA, nrow(df))
df$longitude <- rep(NA, nrow(df))

df$latitude[df$plot=="CV"] <- -38.811846
df$longitude[df$plot=="CV"] <- -62.221676

df$latitude[df$plot=="BM"] <- -38.844913
df$longitude[df$plot=="BM"] <- -62.276846

world_map <- map_data("world") 
world <- ggplot() + coord_fixed() +xlab("") + ylab("") 
world  <- world  + 
  geom_polygon(data=world_map, aes(x=long, y=lat, group=group), 
               colour="gray60", fill="gray60") 
points <- world  + 
  geom_point(data=df, 
             aes(x=longitude, y=latitude, alpha=0.01)) 
points           # OK


#Zoom:
points_zoom <- points +
  ylim(-70,-30)+
  xlim(-100,-40)
points_zoom      # OK


sum(is.na(df$Latitude))   # 0
sum(is.na(df$Longitude))  # 0

# Currencies as cols =====================================================================
dfii <- spread(df, key="currency", value="value")  # 2744, OK (half of df)
str(dfii)

# Abundance: -----------------------------------------------------------------------------
sum(is.na(dfii$abundance))         # 2
sum(dfii$abundance==0, na.rm=TRUE) # 2304
sum(is.null(dfii$abundance))       # 0
sum(dfii$abundance<0, na.rm=TRUE)  # 0
sum(dfii$abundance=="", na.rm=TRUE)# 0

# Biomass: -------------------------------------------------------------------------------
sum(is.na(dfii$biomass))           # 0
sum(dfii$biomass==0, na.rm=TRUE)   # 2308
sum(is.null(dfii$biomass))         # 0
sum(dfii$biomass<0, na.rm=TRUE)    # 0
sum(dfii$biomass=="", na.rm=TRUE)  # 0


# Eliminate 0s: --------------------------------------------------------------------------
dfii$abundance[is.na(dfii$abundance)] <- 0                  # NAs into 0s
sum(dfii$abundance==0 & dfii$biomass==0)                    # 2306
dfii <- dfii[!with(dfii, abundance==0 & biomass==0),]       # 438 remaining observations
dfii$abundance[dfii$biomass==0]   # 2, OK
dfii$biomass[dfii$abundance==0]   # 0
dfii$biomass[dfii$biomass==0] <- NA

# View(dfii[is.na(dfii$biomass),])                          # true 0s in rawdata file


# SampleDescription & Data aggregation: =================================================
names(dfii) <- str_to_title(names(dfii))
rawdata <- dfii %>% group_by(Family, Genus, Species, Latitude, Longitude, Plot, Day, Month, Year) %>%
  summarise(Abundance=sum(Abundance), Biomass=sum(Biomass))  %>% ungroup() # 308 final observations

sum(is.na(rawdata$Abundance))      # 0, OK
sum(is.na(rawdata$Biomass))        # 2, OK
rawdata <- rawdata %>% relocate(c(Abundance, Biomass), .before=Family)

rawdata$SampleDescription <- paste0(rawdata$Plot, "_", rawdata$Day, "_", rawdata$Month, "_", rawdata$Year)
rawdata$Plot <- rep(NA, nrow(rawdata))
rawdata <- rawdata %>% relocate(c(SampleDescription, Plot), .before=Latitude)

rawdata$DepthElevation <- rep(-0.5, nrow(rawdata))
rawdata <- rawdata %>% relocate(c(DepthElevation), .after=Longitude)

rawdata$StudyID <- rep(NA, nrow(rawdata))
rawdata <- rawdata %>% relocate(c(StudyID), .after=Year)

names(rawdata)
str(rawdata)

range(rawdata$Abundance)        # 100 1308997 (Agrees with units in rawdata file)
range(rawdata$Biomass, na.rm=T) # 0.0142 150.1896 (Agrees with units in rawdata file)

path <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIMEGithubMetadatas/PhytoplanktonBahíaBlancaEstuary2008_2010_Guinder_et_al_2022_AFE"
write.csv(rawdata, file=paste0(path, "/rawdata.csv"), row.names = F)


# Convex hulls ==========================================================================
# 1. Convert data points into point spatial object
# assumes your rawdata dataframe is called rawdata
dt_merged <- rawdata
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
  coord_sf(xlim = c(-80, -40), ylim = c(-50,-30)) + 
  labs(x = NULL, y = NULL) +
  theme_minimal()


# End of script #########################################################################

