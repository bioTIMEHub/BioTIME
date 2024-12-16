##########################################################################################
# Curation Script: MicrozooplanktonBahíaBlancaEstuary2008_2010_Guinder_et_al_2022_AFE
# AFE
# August 2023
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
# https://doi.org/10.1016/j.jembe.2016.04.006

rm(list=ls())
setwd("C:/Users/afe1/OneDrive - University of St Andrews/PHD/BioTIMEGithub")
myd <- getwd()


# Read raw data files ====================================================================
files_dir <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIME/BioTIMENewStudiesCuration/MicrozooplanktonBahíaBlancaEstuary2008_2010_Guinder_et_al_2022_AFE"

# NOTE: Re-formatting of rawdata in Excel to ensure correct input of data (file as sent by authors & with no re-formatting is MicroPhyto_2008-2010)
# Two replicates selected for each sampling event, matching the codes 
# in Phytoplankton 2008-2010 whenever possible. 
# Checked: Abundance & Biomass replicate codes for each station always match!

dta <- read.csv(paste0(files_dir, "/Microzooplankton2008_2010_Abundance.csv"), h=T)
dtb <- read.csv(paste0(files_dir, "/Microzooplankton2008_2010_Biomass.csv"), h=T)      
   
names(dta)
names(dtb)


# Initial formatting =====================================================================

la <- list("cv"=dta[,c(1, 3:42)], "bm"=dta[,c(1,43:81)]) # Ignoring column of Biomass totals
lb <- list("cv"=dtb[,c(1, 3:42)], "bm"=dtb[,c(1,43:81)]) # Ignoring column of Biomass totals

la <- lapply(la, function(x) {names(x) <- paste0(names(x), "_", x[1,]); x})
lb <- lapply(lb, function(x) {names(x) <- paste0(names(x), "_", x[1,]); x})                             

rows_remove_a <- c("", 
                   "Tintinnids", "Naked ciliates", "Dinoflagellates", "Rotifers", "Otherts", "TOTAL", "MEAN", "SD") 
rows_remove_b <- c("", 
                   "Tintinnidos", "Ciliados desnudos", "Dinoflagelados", "Rot\xedferos", "Otros", "TOTAL", "MEDIA", "DS") 
names(la$cv)
names(lb$cv)
la <- lapply(la, function(x) {x[!x$X_Tintinnids %in% rows_remove_a,]})
lb <- lapply(lb, function(x) {x[!x$X_Tintinnidos %in% rows_remove_b,]})

la <- lapply(la, function(x) {gather(x, key="Sample", value="Value", -1)}) # long format
lb <- lapply(lb, function(x) {gather(x, key="Sample", value="Value", -1)}) # long format
la <- lapply(la, function(x) {names(x) <- c("Taxa", "Sample", "Value");x}) # long format
lb <- lapply(lb, function(x) {names(x) <- c("Taxa", "Sample", "Value");x}) # long format

la <- lapply(la, function(x) {x$plot <- rep(NA, nrow(x)); x
                              x$currency <- rep("abundance", nrow(x));x})
lb <- lapply(lb, function(x) {x$plot <- rep(NA, nrow(x)); x
                              x$currency <- rep("biomass", nrow(x));x})

la[[1]]$plot <- "CV"
la[[2]]$plot <- "BM"

lb[[1]]$plot <- "CV"
lb[[2]]$plot <- "BM"

da <- as.data.frame(do.call(rbind, la))
db <- as.data.frame(do.call(rbind, lb))

dt <- as.data.frame(rbind(da, db))


# Check format & abundances ==============================================================
data_count <- dt %>%
  group_by(currency, plot) %>%
  summarise(count = n_distinct(Sample)) # 39 BM, 40 CV

unique(dt$Sample)
dt$day <- as.integer(str_split_fixed(dt$Sample, "_", 3)[,2])
sort(unique(dt$day))    # OK

dt$month <- as.integer(str_split_fixed(dt$Sample, "_", 4)[,3])
sort(unique(dt$month))  # OK

dt$year <- str_split_fixed(str_split_fixed(dt$Sample, "_", 4)[,4],"_",2)[,1]
dt$year <- as.integer(str_split_fixed(dt$year, "\\.", 2)[,1])
sort(unique(dt$year))  # OK

dt$replicate <- str_split_fixed(dt$Sample, "_", 5)[,5]
sort(unique(dt$replicate))


# Taxonomy ===============================================================================

identical(da$Taxa, db$Taxa) # FALSE
setdiff(da$Taxa, db$Taxa)   # "Oblea sp."
setdiff(db$Taxa, da$Taxa)   # "Dino tipo Oblea"

dt$Taxa[dt$Taxa=="Dino tipo Oblea"] <- "Oblea sp."

sort(unique(dt$Taxa))

dt$Taxa <- gsub("sp.", "sp", dt$Taxa)
dt$Taxa <- gsub("sp ", "sp", dt$Taxa)

dt$Genus <- str_split_fixed(dt$Taxa, " ", 2)[,1]
dt$Species <- str_split_fixed(dt$Taxa, " ", 2)[,2]

dt$Family <- ifelse(dt$Genus %in% c("Acari", "Nauplii"), dt$Genus, NA)
dt$Genus <- ifelse(dt$Genus %in% c("Acari", "Nauplii"), NA, dt$Genus)

sort(unique(dt$Family))
sort(unique(dt$Genus))
sort(unique(dt$Species))

dt$Species[dt$Species==""] <- NA

sum(is.na(dt$Family) & is.na(dt$Genus) & is.na(dt$Species))  # 0, OK


# Temporal Data ==========================================================================

c1 <- dt %>% group_by(month, year, day, plot, currency) %>% summarise(n=n_distinct(replicate))  # 2 to 4, OK
c2 <- dt %>% group_by(month, year, day, plot, currency) %>% distinct(replicate) 

# Even the number of replicates per sampling event date:
set.seed(123)
c2B <- dt %>% group_by(month, year, day, plot) %>% distinct(replicate) %>% sample_n(2)          # select always 2 at random
c2B$code <- paste0(c2B$plot, "_", c2B$replicate, "_", c2B$day, "_", c2B$month, "_", c2B$year)
dt$code <- paste0(dt$plot, "_", dt$replicate, "_", dt$day, "_", dt$month, "_", dt$year)

dt <- dt[dt$code %in% c2B$code,] # keep only two reps per sampling event
c2C <- dt %>% group_by(month, year, day, plot) %>% summarise(n=n_distinct(replicate))

# Locations===============================================================================
# NOTE: given by authors in raw data files.

dt$latitude <- rep(NA, nrow(dt))
dt$longitude <- rep(NA, nrow(dt))

dt$latitude[dt$plot=="CV"] <- -38.811846
dt$longitude[dt$plot=="CV"] <- -62.221676

dt$latitude[dt$plot=="BM"] <- -38.844913
dt$longitude[dt$plot=="BM"] <- -62.276846

world_map <- map_data("world") 
world <- ggplot() + coord_fixed() +xlab("") + ylab("") 
world  <- world  + 
  geom_polygon(data=world_map, aes(x=long, y=lat, group=group), 
               colour="gray60", fill="gray60") 
points <- world  + 
  geom_point(data=dt, 
             aes(x=longitude, y=latitude, alpha=0.01)) 
points           # OK


#Zoom:
points_zoom <- points +
  ylim(-70,-30)+
  xlim(-100,-40)
points_zoom      # OK

sum(is.na(dt$Latitude))   # 0
sum(is.na(dt$Longitude))  # 0


# Currencies as cols =====================================================================
dt2 <- spread(dt, key="currency", value="Value")  # 1960, OK (half of df)
str(dt2)

# Abundance: -----------------------------------------------------------------------------
dt2$abundance <- as.numeric(dt2$abundance)
dt2$biomass <- as.numeric(dt2$biomass)
sum(is.na(dt2$abundance))         # 1
sum(dt2$abundance==0, na.rm=TRUE) # 1610
sum(is.null(dt2$abundance))       # 0
sum(dt2$abundance<0, na.rm=TRUE)  # 0
sum(dt2$abundance=="", na.rm=TRUE)# 0

# Biomass: -------------------------------------------------------------------------------
sum(is.na(dt2$biomass))           # 0
sum(dt2$biomass==0, na.rm=TRUE)   # 1611
sum(is.null(dt2$biomass))         # 0
sum(dt2$biomass<0, na.rm=TRUE)    # 0
sum(dt2$biomass=="", na.rm=TRUE)  # 0


# Eliminate 0s: --------------------------------------------------------------------------
dt2$abundance[is.na(dt2$abundance)] <- 0                  # NAs into 0s
sum(dt2$abundance==0)                                     # 2287
dt2 <- dt2[!dt2$abundance==0,]                            # 349 left
range(dt2$abundance)
range(dt2$biomass)


# SampleDescription & Data aggregation: =================================================
names(dt2) <- str_to_title(names(dt2))
names(dt2)
rawdata <- dt2 %>% group_by(Family, Genus, Species, Latitude, Longitude, Plot, Day, Month, Year) %>%
  summarise(Abundance=sum(Abundance), Biomass=sum(Biomass))  %>% ungroup() 

rawdata <- rawdata %>% relocate(c(Abundance, Biomass), .before=Family)
rawdata$SampleDescription <- paste0(rawdata$Plot, "_",  
                                    rawdata$Day, "_", rawdata$Month, "_", rawdata$Year)
rawdata$Plot <- rep(NA, nrow(rawdata))
rawdata <- rawdata %>% relocate(c(SampleDescription, Plot), .before=Latitude)

rawdata$DepthElevation <- rep(-0.5, nrow(rawdata))
rawdata <- rawdata %>% relocate(c(DepthElevation), .after=Longitude)

rawdata$StudyID <- rep(NA, nrow(rawdata))
rawdata <- rawdata %>% relocate(c(StudyID), .after=Year)

names(rawdata)
str(rawdata)


path <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIMEGithubMetadatas/MicrozooplanktonBahíaBlancaEstuary2008_2010_Guinder_et_al_2022_AFE"
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




