################################################################################
# Curation Script: Murray-Darling Basin Fish and Macroinvertebrate Survey (RivFishTIME id 23)
# Curator: AFE
# Date: August 2023
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



# Main sources =================================================================
# https://data.gov.au/data/dataset/murray-darling-basin-fish-and-macroinvertebrate-survey
# https://data.gov.au/dataset/ds-dga-7826d7c9-bcc5-48c0-832a-66aaedfe7b0f/details?q=

# For META & METHODS:
# https://www.mdba.gov.au/publications-and-data/publications/sustainable-rivers-audit-1-sra-1
# https://www.mdba.gov.au/publications-and-data/publications/sustainable-rivers-audit-2-sra-2 


# NOTES FROM DOCS ABOVE:
# SRA surveys for macroinv: only taxa composition (i.e., taxa list), not include in BT
# SRA Programme: Stratified random sampling


# Difference SRA & MDBFS:
# Keep a grain of site (this means that for the SRA years,
# many random sites will be removed since they were sampled only once
# however this will allow to keep SRA &  MDBFS sampling in the same
# time series.



# Read data: ===================================================================
mypath <- getwd() 
rawdatapath <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIME/BioTIMENewStudiesCuration/MurrayDarlingBasin_RivFishId23_AFE/2023/DataSheets/csvs"

setwd(rawdatapath)                       # Set directory to file location in order to read data
file_names <- dir()                      # 18
listdt <- list()
listdt <- lapply(file_names,read.csv)     
setwd(mypath)                            # Back to main dir

names(listdt) <- str_split_fixed(file_names, "\\.", 2)[,1]



# Check data: ==================================================================
dt <- as.data.frame(rbind(listdt[["tblCatch1"]],listdt[["tblCatch2"]], listdt[["tblCatch3"]], listdt[["tblCatch4"]])) # 171802, TOTAL CORRECT (had to read in multiple files bc of n of records limit for database export)
str(dt) # NOTE: tblCatch table contains information relating to observed species and species counts, at specific date/time and location
bio_dt <- as.data.frame(rbind(listdt[["tblBio1"]],listdt[["tblBio2"]], listdt[["tblBio3"]], listdt[["tblBio4"]]))     # ind weights
str(bio_dt)



# 1) Sampling programmes =======================================================
prog <- listdt[["tblSampleProgram"]]

sort(unique(prog$Theme))       #"Fish"  "Macro"
sort(unique(prog$Description)) # 17

unique(prog$Theme[prog$Description %like% "MDBFS"]) # Fish only

dt$Group <- prog$Theme[match(dt$ProgramID, prog$ProgramID)]
sum(is.na(dt$Group))

dt <- dt[!dt$Group=="Macro",]                       # 52610, removed macroinv data
prog <- prog[!prog$Theme=="Macro",]


dt$ProgramDes <- prog$Description[match(dt$ProgramID, prog$ProgramID)]
dt$StartProg <- prog$StartPeriod[match(dt$ProgramID, prog$ProgramID)]
dt$EndProg <- prog$EndPeriod[match(dt$ProgramID, prog$ProgramID)]
dt$Period <- ifelse(dt$ProgramID %in% c("PRG0001", "PRG0005", "PRG0008"), "SRA1", NA)
dt$Period <- ifelse(dt$ProgramID %in% c("PRG0403", "PRG0503", "PRG0601"), "SRA2", dt$Period)
dt$Period <- ifelse(dt$ProgramID %in% c("PRG0701", "PRG0801", "PRG0901"), "SRA3", dt$Period)
dt$Period <- ifelse(is.na(dt$Period), dt$ProgramDes, dt$Period)

length(unique(dt$Period)) # OK
unique(dt$Period)         # OK



# 2) Sampling Event Date =======================================================
dt$Day <- as.integer(str_split_fixed(dt$dateSamplingCommenced, "-",3)[,1])
sort(unique(dt$Day))
sum(is.na(dt$Day))

dt$Month <- str_split_fixed(dt$dateSamplingCommenced, "-",3)[,2]
dt$Month <- plyr::revalue(dt$Month, c("Apr"=4,
                                      "Dec"=12,
                                      "Feb"=2,
                                      "Jan"=1,
                                      "Jul"=7,
                                      "Jun"=6,
                                      "May"=5,
                                      "Mar"=3,
                                      "Nov"=11,
                                      "Oct"=10,
                                      "Aug"=8))
dt$Month <- as.integer(dt$Month)
sort(unique(dt$Month))
sum(is.na(dt$Month))

dt$Year <- str_split_fixed(dt$dateSamplingCommenced, "-",3)[,3]
dt$Year <- as.integer(paste0("20", dt$Year))
sort(unique(dt$Year))
sum(is.na(dt$Year))



# 3) Methods: ==================================================================
length(unique(dt$MethodCode))      # 36
met <- listdt[["tblMethod"]]

setdiff(unique(dt$MethodCode), unique(met$Abbreviation)) # "Backpack"

dt$MethodCode[dt$MethodCode=="Backpack"] <- "BackPack"

sort(unique(met$Description[met$Abbreviation %in% dt$MethodCode]))         
sort(unique(met$General.description[met$Abbreviation %in% dt$MethodCode]))
sort(unique(met$MethodType[met$Abbreviation %in% dt$MethodCode]))           

dt$Description <- met$Description[match(dt$MethodCode, met$Abbreviation)]
dt$GeneralDescription <- met$General.description[match(dt$MethodCode, met$Abbreviation)]
dt$MethodType <- met$MethodType[match(dt$MethodCode, met$Abbreviation)]

dt$MethodType[dt$MethodType=="ELectroBoat"] <- "ElectroBoat"

sum(is.na(dt$Description))
sum(is.na(dt$GeneralDescription))
sum(is.na(dt$MethodType))


sort(unique(dt$Description))
sort(unique(dt$GeneralDescription))
table(dt$GeneralDescription)
sort(unique(dt$MethodType))

dt$MethodTypeBT <- dt$MethodType
dt$MethodTypeBT <- ifelse(dt$MethodType=="Net", dt$GeneralDescription, dt$MethodTypeBT)
dt$MethodTypeBT <- plyr::revalue(dt$MethodTypeBT, c("Fyke daytime set"="FykeDaytimeSet",
                                                    "Fyke overnight set"="FykeOvernightSet",
                                                    "Trap-Baited"="TrapBaited",
                                                    "Trap-Unbaited"="TrapUnbaited"))
sort(unique(dt$MethodTypeBT))
table(dt$MethodTypeBT)


c2 <- dt %>% group_by(SiteID, ProgramID, operationCode) %>% summarise(nD=n_distinct(Day),
                                                            nM=n_distinct(Month),
                                                            nY=n_distinct(Year))           # 1
c2B <- dt %>% group_by(SiteID, ProgramID) %>% summarise(nD=n_distinct(Day),
                                                        nM=n_distinct(Month),
                                                        nY=n_distinct(Year))               # 1
c2C <- dt %>% group_by(SiteID, Period) %>% summarise(nD=n_distinct(Day),
                                                        nM=n_distinct(Month),
                                                        nY=n_distinct(Year),
                                                        nOP=n_distinct(operationCode))     # 1

c3 <- dt %>% group_by(SiteID, ProgramID, Day, Month, Year) %>% summarise(nM=n_distinct(MethodTypeBT)) # 1-4

dt <- dt %>%
  group_by(SiteID, ProgramID, Day, Month, Year) %>%     # Sampling event
  mutate(ConcatMethodTypeBT = paste(unique(MethodTypeBT), collapse = "_"))
c4 <- dt %>% 
  group_by(SiteID, ProgramID, Day, Month, Year) %>%
  summarise(n=n_distinct(ConcatMethodTypeBT))           # always 1, OK


# checks: ----------------------------------------------------------------------
unique(dt$MethodTypeBT[dt$SiteID=="72902" & dt$Period=="MDBFS 2022/2023"])
unique(dt$ConcatMethodTypeBT[dt$SiteID=="72902" & dt$Period=="MDBFS 2022/2023"])

unique(dt$MethodTypeBT[dt$SiteID=="66787" & dt$Period=="MDBFS 2016/2017"])
unique(dt$ConcatMethodTypeBT[dt$SiteID=="66787" & dt$Period=="MDBFS 2016/2017"])


c5A <- dt %>% group_by(SiteID, MethodTypeBT) %>% summarise(n=n_distinct(Year))
table(c5A$n) 

c5B <- dt %>% group_by(SiteID, ConcatMethodTypeBT) %>% summarise(n=n_distinct(Year))
table(c5B$n) 



# 4)Location ===================================================================
site <- listdt[["tblSite"]]
length(unique(site$SiteID)) # 23178

"FALSE" %in% (unique(dt$SiteID) %in% unique(site$SiteID)) # F: all sites with lat & long info

dt$Latitude <- site$latitude[match(dt$SiteID, site$SiteID)]
sum(is.na(as.numeric(dt$Latitude))) # 0
sum(dt$Latitude=="")                # 0 (also double space)

dt$Longitude <- site$longitude[match(dt$SiteID, site$SiteID)]
sum(is.na(as.numeric(dt$Longitude))) # 0
sum(dt$Longitude==" ")               # 0 (also double space)

dt$Latitude <- as.numeric(dt$Latitude)
dt$Longitude <- as.numeric(dt$Longitude)

c6 <- dt %>% group_by(SiteID) %>% summarise(nlat=n_distinct(Latitude), nlong=n_distinct(Longitude)) # OK, always 1
c6B <- dt %>% group_by(Latitude, Longitude) %>% summarise(nS=n_distinct(SiteID))                    # OK, always 1

world_map <- map_data('world') 
world <- ggplot(world_map) + 
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  coord_fixed() + 
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())

(points <- world + geom_point(data=dt, aes(x=Longitude, y=Latitude), shape=21))
(points_zoom <- points + coord_fixed(xlim=c(130,160), ylim=c(-40, -20))) 



# 5) DepthElevation ============================================================
range(site$elevation) # -9 to 1969

dt$DepthElevation <- site$elevation[match(dt$SiteID, site$SiteID)]
range(dt$DepthElevation)      # -9 1662
sum(dt$DepthElevation=="")    # 0, also (also checked double space)

range(dt$DepthElevation[dt$DepthElevation < 0]) #-9
unique(dt$SiteID[dt$DepthElevation < 0])        
sum(dt$DepthElevation < 0)                      # 4905

dt$DepthElevation[dt$DepthElevation < 0] <- NA  # Substitute for NAs
sum(is.na(dt$DepthElevation))                   # 4905

sum(dt$DepthElevation==0, na.rm=T)              # 0
range(dt$DepthElevation, na.rm=T)               # 2 to 1601 (elevation in m)

c6C <- dt %>% group_by(SiteID) %>% summarise(nEle=n_distinct(DepthElevation)) # OK, always 1



# 6) Taxonomy (Fish) ===========================================================
sps <- listdt[["tblTaxa"]]

length(unique(dt$TaxaCode)) # 45
unique(dt$TaxaCode) %in% unique(sps$TaxaCode) # always TRUE

head(sps, 2L)

dt$LatinName <- sps$nameScientific[match(dt$TaxaCode, sps$TaxaCode)]
dt$Genus <- sps$genus[match(dt$TaxaCode, sps$TaxaCode)]
dt$Species <- sps$species[match(dt$TaxaCode, sps$TaxaCode)]
sort(unique(dt$LatinName)) 
sort(unique(dt$Genus))  
sort(unique(dt$Species))

unique(dt$LatinName[dt$Genus==""])
unique(dt$LatinName[dt$Species==""])
dt$Genus[dt$Genus==""] <- "Galaxias"
dt$Species[dt$LatinName=="Galaxias oliros"] <- "oliros"
dt$Species[dt$LatinName=="Galaxias arcanus"] <- "arcanus"
dt$Species[dt$Species=="splendida tatei"] <- "splendida"
dt$Species[dt$Species=="spp"] <- "sp"
dt$Species[dt$Species=="stercusmuscarum fulvus"] <- "fulvus"
# IN GBIF (12-2-2023)
# Craterocephalus stercusmuscarum fulvus synonym of Craterocephalus fulvus
# Melanotaenia splendida tatei synonym of Melanotaenia splendida
    
dt$Family <- rep(NA, nrow(dt))

dt$Family <- ifelse(dt$Genus=="Gambusia", "Poeciliidae", dt$Family)
dt$Family <- ifelse(dt$Genus== "Cyprinus"| dt$Genus== "Carassius", "Cyprinidae", dt$Family)
dt$Family <- ifelse(dt$Genus=="Macquaria", "Percichthyidae", dt$Family)
dt$Family <- ifelse(dt$Genus=="Nematalosa", "Clupeidae", dt$Family)


dt$Family <- ifelse(dt$Genus=="Leiopotherapon", "Terapontidae", dt$Family)
dt$Family <- ifelse(dt$Genus=="Hypseleotris" | dt$Genus== "Philypnodon" | dt$Genus == "Mogurnda", "Eleotridae", dt$Family)
dt$Family <- ifelse(dt$Genus=="Melanotaenia", "Melanotaeniidae", dt$Family)
dt$Family <- ifelse(dt$Genus=="Retropinna", "Retropinnidae", dt$Family)

dt$Family <- ifelse(dt$Genus=="Craterocephalus", "Atherinidae", dt$Family)
dt$Family <- ifelse(dt$Genus=="Maccullochella" | dt$Genus== "Gadopsis" | dt$Genus== "Nannoperca", "Percichthyidae", dt$Family)
dt$Family <- ifelse(dt$Genus=="Neosilurus" | dt$Genus=="Tandanus", "Plotosidae", dt$Family)
dt$Family <- ifelse(dt$Genus=="Galaxias", "Galaxiidae", dt$Family)

dt$Family <- ifelse(dt$Genus=="Salmo" | dt$Genus== "Oncorhynchus", "Salmonidae", dt$Family)
dt$Family <- ifelse(dt$Genus=="Perca", "Percidae", dt$Family)
dt$Family <- ifelse(dt$Genus=="Ambassis", "Ambassidae", dt$Family)
dt$Family <- ifelse(dt$Genus=="Tinca", "Tincidae" , dt$Family)

dt$Family <- ifelse(dt$Genus=="Bidyanus", "Terapontidae" , dt$Family)
dt$Family <- ifelse(dt$Genus=="Misgurnus", "Cobitidae" , dt$Family)
dt$Family <- ifelse(dt$Genus=="Rutilis", "Leuciscidae" , dt$Family)
dt$Family <- ifelse(dt$Genus=="Porochilus", "Plotosidae" , dt$Family)


sort(unique(dt$Family))  
sort(unique(dt$Genus))
sort(unique(dt$Species))



# 7) Abundance & Biomass =======================================================
sum(is.na(dt$Abundance)) # 0, OK
range(dt$Abundance)      # 1 3150

dt$Biomass <- as.numeric(dt$Biomass)
range(dt$Biomass)        # 0.00 65835.12
sum(dt$Biomass==0)       # 23120
unique(dt$Period[dt$Biomass==0])


# add biomass ------------------------------------------------------------------
bio <- bio_dt
bio$taxaWeight <- as.numeric(bio$taxaWeight)
range(bio$taxaWeight, na.rm=T)  # 0.00 24063.53
bio <- bio[! (is.na(bio$taxaWeight) | bio$taxaWeight==0), ]

bio$Day <- as.numeric(str_split_fixed(bio$dateSamplingCommenced, "-", 3)[,1])
sort(unique(bio$Day))
bio$Month <- str_split_fixed(bio$dateSamplingCommenced, "-", 3)[,2]
sort(unique(bio$Month))
bio$Month <- plyr::revalue(bio$Month, c("Apr"=4,
                                      "Dec"=12,
                                      "Feb"=2,
                                      "Jan"=1,
                                      "Jul"=7,
                                      "Jun"=6,
                                      "May"=5,
                                      "Mar"=3,
                                      "Nov"=11,
                                      "Oct"=10))

bio$Year <- str_split_fixed(bio$dateSamplingCommenced, "-", 3)[,3]
bio$Year <- as.integer(paste0("20", bio$Year))
sort(unique(bio$Year))

sort(unique(dt$MethodCode))
sort(unique(bio$methodCode))
bio$methodCode[bio$methodCode=="Backpack"] <- "BackPack"

sort(unique(dt$TaxaCode))
sort(unique(bio$taxaCode))
unique(dt$TaxaCode) %in% unique(bio$taxaCode)
setdiff(unique(dt$TaxaCode), unique(bio$taxaCode))

bio$codeCalc <- paste0(bio$SiteID, "_", bio$ProgramID, "_", bio$methodCode,
                       bio$OperationCode, "_", bio$taxaCode, "_",
                       bio$Day, "_", bio$Month, "_", bio$Year)
bio$AbuCheck <- 1
bio <- bio %>% 
  group_by(codeCalc) %>%
  summarise(BioSum=sum(taxaWeight), AbuSum=sum(AbuCheck))
dt$codeCalc <- paste0(dt$SiteID, "_", dt$ProgramID, "_", dt$MethodCode,
                           dt$operationCode, "_", dt$TaxaCode, "_", 
                           dt$Day, "_", dt$Month, "_", dt$Year)
dt$AbuCheck <- bio$AbuSum[match(dt$codeCalc, bio$codeCalc)]
dt$BioSum <- bio$BioSum[match(dt$codeCalc, bio$codeCalc)]

c7 <- dt[which(dt$BioSum != 0 & dt$Biomass==0 & dt$Abundance==dt$AbuCheck),] # 2552 obs
dt$Biomass2 <- ifelse(dt$Biomass == 0 & dt$isBiomassCalculated==TRUE & dt$Abundance==dt$AbuCheck, dt$BioSum, dt$Biomass)
sum(dt$Biomass==0, na.rm=T)      # 23120
sum(is.na(dt$Biomass2))          # 20086
sum(dt$Biomass2==0, na.rm=T)     # 482


sum(dt$Biomass==dt$Biomass2, na.rm=T)

dt$Biomass2[dt$Biomass2==0] <- NA

# NOTE: unless biomasses are computed for the full observation, biomass is NA.
# This step is to avoid inequivalent abundance and biomass when aggregating the data
names(dt)
dt <- dt %>% group_by(SiteID, Period, operationCode, MethodTypeBT, Day, Month, Year, Family, Genus, Species) %>%
  mutate(HasFullBiomass = ifelse(any(is.na(Biomass2)), F, T)) %>%
  ungroup()
sum(dt$HasFullBiomass==F) 
sum(dt$HasFullBiomass==T) # 32042

dt$Biomass3 <- ifelse(dt$HasFullBiomass==F, NA, dt$Biomass2) # checked visually



# Methods II ===================================================================

c8 <- dt %>% group_by(SiteID, Period, MethodTypeBT, Day, Month, Year) %>% summarise(n=n_distinct(operationCode)) # 1-13
c8B <- dt %>% group_by(SiteID, Period, Day, Month, Year) %>% summarise(n=n_distinct(operationCode))            # 1-25
c8C <- dt %>% group_by(SiteID, Period, MethodTypeBT, Day, Month, Year) %>% distinct(operationCode)
# NOTE: keep OperationCode in Sample Desc to identify each sample.


c9 <- dt %>% group_by(SiteID, MethodTypeBT) %>% summarise(n=n_distinct(Year))
c9$ConcatRm <- paste0(c9$SiteID, "_", c9$MethodTypeBT)
torm <- unique(c9$ConcatRm[c9$n==1]) # 1729
dt$Concat <- paste0(dt$SiteID, "_", dt$MethodTypeBT)
dt <- dt[!dt$Concat %in% torm,]      # 32600
sort(unique(dt$MethodTypeBT))
table(dt$MethodTypeBT)               # removes FykeDaytimeSet


# rawData ======================================================================
names(dt)
rawData <- dt %>% group_by(Family, Genus, Species, SiteID, operationCode,
                           MethodTypeBT, Latitude, Longitude, DepthElevation,
                           Day, Month, Year) %>%
  summarise(Abundance=sum(Abundance), Biomass=sum(Biomass3))

range(rawData$Abundance)
range(rawData$Biomass, na.rm=T)
sum(is.na(rawData$Biomass))


names(rawData)
rawData <- rawData %>% relocate(c(Abundance, Biomass), .before=Family)
rawData$Plot <- rep(NA, nrow(rawData))
rawData$SampleDescription <- paste0(rawData$SiteID, "_", rawData$operationCode,
                                    "_", rawData$Day, "_", rawData$Month, "_",
                                    rawData$Year)
rawData <- rawData %>% relocate(c(SampleDescription, Plot), .before=Latitude)
rawData <- within(rawData, rm(SiteID, operationCode))
rawData$StudyID <- rep(NA, nrow(rawData))



# split ========================================================================
lr <- split(rawData, f=rawData$MethodTypeBT)   # 6 elements
lr <- lapply(lr, function(x){within(x, rm(MethodTypeBT))})



# Save =========================================================================
path <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIMEGithubMetadatas/MurrayDarlingBasin_RivFishId23_AFE/RawData"
for(i in names(lr)){
  write.csv(lr[[i]], paste0(path, "/", i,".csv"), row.names=F)
}



# Convex hulls =========================================================
# 1. Convert data points into point spatial object
# assumes your rawdata dataframe is called rawdata
dt_merged <- rawData
dt_merged$Latitude <- as.numeric(dt_merged$Latitude)
dt_merged$Longitude <- as.numeric(dt_merged$Longitude)
dt_coord <- dt_merged %>% select(Longitude, Latitude) %>% distinct() %>%
  st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()


coordslr <- lapply(lr, function(x){dt_coord <- x %>% select(Longitude, Latitude) %>% distinct() %>%
  st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()})

# 2. Calculate convex hull, area and centroid
centroid <- dt_coord %>% st_convex_hull() %>% st_centroid() %>% unlist # get centroid
centroidlr <- lapply(coordslr, function(x){x %>% st_convex_hull() %>% st_centroid() %>% unlist}) 

# transform to mercator to get area in sq km
area <- st_transform(dt_coord, st_crs("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs")) %>%
  st_convex_hull() %>% st_area()  ##get area in sq km
arealr <- lapply(coordslr, function(x){st_transform(x, st_crs("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs")) %>%
    st_convex_hull() %>% st_area()}) 


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
  coord_sf(xlim = c(130,160), ylim = c(-40,-20)) + 
  labs(x = NULL, y = NULL) +
  theme_minimal()


# End of script ################################################################
################################################################################
