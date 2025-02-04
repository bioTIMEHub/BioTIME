
# Curation Script ---------------------------------------------------------

# Dataset:  Regional Watershed Monitoring Program Fish Community Data (2018-2021)
# Location:  Canada
# Curator: Mike McWilliam
# Date: 17 / 7 / 2023

# Set up ------------------------------------------------------------------
rm(list=ls()) 
library("maps")
library("ggplot2")
library("reshape2")
library("stringr")

raw18 <- read.csv("RWMP_fish_Original/2018-watershed-fish-data-with-weights_updated.csv")
raw19 <- read.csv("RWMP_fish_Original/final_2019watershedfishdatawithweights-1.csv")
raw20 <- read.csv("RWMP_fish_Original/2020-rwmp-fish-community-data.csv")
raw21 <- read.csv("RWMP_fish_Original/2021-rwmp-fish-community-data.csv")
head(raw18)
head(raw19)
head(raw20)
head(raw21)

# site overlap
sites <- data.frame(site=unique(c(unique(raw18$StationNam), unique(raw19$STATIONNAM), unique(raw20$StationName), unique(raw20$StationName))))
sites$m18 <- match(sites$site, unique(raw18$StationNam))
sites$m19 <- match(sites$site, unique(raw19$STATIONNAM))
sites$m20 <- match(sites$site, unique(raw20$StationName))
sites$m21 <- match(sites$site, unique(raw21$StationName))
sites$m_1821 <- sites$m18 + sites$m21
sites

# most sites only sampled one year 
# select only sites sampled in 2018 and 2021 
sites[!is.na(sites$m_1821),]
site_use <- sites[!is.na(sites$m_1821),"site"]


dt18 <- raw18[raw18$StationNam %in% site_use,]
dt21 <- raw21[raw21$StationName %in% site_use,]

dt18$StationName <- dt18$StationNam
dt18$UTMNorthing <- dt18$UTMNorthin
dt18$Common_Name <- dt18$Common_Nam
dt18$TotalWeight <- dt18$TotalWeigh
head(dt18)
head(dt21)

align_cols <- c("StationName", "Watershed", "UTMNorthing", "UTMEasting", "UTMDatum", "SampleYear", "VisitDate", "Common_Name", "TotalWeight","TotalNum")
dt <- rbind(dt18[,align_cols], dt21[,align_cols])
head(dt)

# Primary field check -----------------------------------------------------

dt$Abundance <- as.numeric(dt$TotalNum)
dt$Biomass <- as.numeric(dt$TotalWeight)

# No negative values, zeroes, or NAs in abundance/biomass fields.
min(dt$Abundance) # check the minimum
dt <- subset(dt, Abundance > 0) # no zeros
min(dt$Abundance)
sum(dt$Abundance=="") # no blanks 

min(dt$Biomass) # check the minimum
dt <- subset(dt, Biomass > 0) # no zeros
min(dt$Biomass)
sum(dt$Biomass=="") # no blanks 

############## YEAR MONTH DAY (int/factors)
# no negative values, 0s or NAs, all are logical
# Year < 2023, month < 12, day < 31
# Date should be POSIXct

dt$Year <- format(as.Date(dt$VisitDate, "%m/%d/%Y"), "%Y")
dt$Month <- format(as.Date(dt$VisitDate, "%m/%d/%Y"), "%m")
dt$Day <- format(as.Date(dt$VisitDate, "%m/%d/%Y"), "%d")
head(dt)

############## LAT LONG (numeric)
# Latitude -90 to 90 / Longitude -180 to 180,  no blanks, no NAs

unique(dt$UTMDatum)

library("sf")
p1 <- st_as_sf(dt[,c("UTMEasting", "UTMNorthing")], coords = c("UTMEasting", "UTMNorthing"), crs = "+proj=utm +zone=17") # toronto in zone 17
p2 <- st_transform(p1, crs= "EPSG:4326")
p2 <- st_coordinates(p2)
head(p2)
dt$Latitude <- p2[,"Y"]
dt$Longitude <- p2[,"X"]
head(dt)

# check whether the GPS coordinates match expectations
points <- unique(dt[,c("Latitude", "Longitude")])
mid <- c(points[1,1], points[1,2])
a <- 10
lw_ratio <- 1

world_map <- map_data('world') 
world <- ggplot(world_map) + 
  geom_polygon(aes(x=long, y=lat, group=group)) +
  geom_point(data=points, aes(x=Longitude, y=Latitude), col="red", shape=21)+
  #coord_fixed()+ 
coord_cartesian(c(mid[2]-a, mid[2]+a), c(mid[1]-a*lw_ratio, mid[1]+a*lw_ratio))+
  labs(x='Longitude', y='Latitude')+
  theme_bw() + theme(panel.grid=element_blank(), aspect.ratio=lw_ratio)
world

# Taxonomic fields ---------------------------------------------------

# No NAs in taxonomic fields, remove all non-organism records
# misspellings check, but not taxonomic cleaning
# check that genera are genera, not family names (-idae/eae)

head(dt)
unique(dt$Common_Name)

# ----------- skip this section / use file below -----------#
library("taxize")
sci_names <- comm2sci(unique(dt$Common_Name))
ref <- as.data.frame(do.call("rbind", sci_names))
name_ref <- data.frame(spp = unique(dt$Common_Name))
name_ref$new <- ref$V1[match(name_ref$spp, rownames(ref))]
name_ref
# add the rest manually. 
name_ref$fill <- name_ref$new
name_ref$fill[name_ref$spp=="Oncorhynchus sp."] <- "Oncorhynchus sp."
name_ref$fill[name_ref$spp=="Rock Bass"] <- "Ambloplites rupestris"
name_ref$fill[name_ref$spp=="Brown Trout"] <- "Salmo trutta"
name_ref$fill[name_ref$spp=="Johnny/Tesselated Darter"] <- "Etheostoma olmstedi"
name_ref$fill[name_ref$spp=="Hornyhead Chub"] <- "Nocomis biguttatus"
name_ref$fill[name_ref$spp=="Spotfin Shiner"] <- "Cyprinella spiloptera"
name_ref$fill[name_ref$spp=="Salmon & Trout subfamily"] <- "remove"
name_ref[grepl("idae", name_ref$spp),"fill"] <- "remove"
name_ref
#write.csv(name_ref, "RWMP_fish_Original/spp_names_mm.csv")
# ------------------------------------------------------### 

spp_names <- read.csv("RWMP_fish_Original/spp_names_mm.csv")
dt$Taxon <- spp_names$fill[match(dt$Common_Name, spp_names$spp)]
dt <- dt[!dt$Taxon =="remove",]
head(dt)

dt$Genus <- word(dt$Taxon, 1)
dt$Species <- word(dt$Taxon, 2)
dt$Family <- ""

# check the species list for misspellings or non-BioTIME conventions
sort(unique(dt$Species))
sort(unique(dt$Genus))

# Secondary fields ---------------------------------------------------

# trawl, plot, transect etc / # elevation or depth 

head(dt)
dt$DepthElevation <- ""

head(dt)
dt$Plot <- dt$StationName
head(dt)

# Prepare raw data --------------------------------------------------------

dt$StudyID <- rep('', dim(dt)[1])

# aggregate abundance records that are same species, plot, and survey day.
dt_merged <- aggregate(Abundance ~ ., data = dt[,c("Family", "Genus", "Species","Year","Month", "Day", "Plot",  "Abundance", "Biomass","Latitude", "Longitude", "DepthElevation", "StudyID", "VisitDate", "Taxon")], sum)

dim(dt)[1]-dim(dt_merged)[1] # any change after aggregating 

ggplot(dt_merged[dt_merged$Taxon %in% unique(dt$Taxon)[1:20] & dt_merged$Plot %in% unique(dt$Plot)[1:50],], aes(x=as.Date(VisitDate, "%m/%d/%Y"), y=Abundance, group=Plot))+
geom_line()+facet_wrap(~Taxon)

dataset.name <- 'RWMP_fish'
dt_merged$SampleDescription <- as.factor(with(dt_merged, paste(Latitude, Longitude, Year, Month, Day, Plot, sep='_')))
length(levels(dt_merged$SampleDescription)) # 54 samples

# reorder columns by BioTIME format
dt_merged <- dt_merged[c('Abundance',
                         'Biomass',
                         'Family',
                         'Genus',
                         'Species',
                         'SampleDescription',
                         'Plot',
                         'Latitude',
                         'Longitude',
                         'DepthElevation',
                         'Day',
                         'Month',
                         'Year',
                         'StudyID')] 
                         
head(dt_merged) # final check :)
str(dt_merged)

# Export final ------------------------------------------------------------

write.csv(dt_merged, paste0(dataset.name, '_rawdata_MM.csv'), row.names=F)


