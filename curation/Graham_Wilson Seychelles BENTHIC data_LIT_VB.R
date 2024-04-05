## Curation script for Wilson and Graham: benthos and corals
## curator VB

setwd("D:/Dropbox/towards BioTIME v2/Vivi-dataset hunting files/Graham")
ds_name <- "Seychelles benthic cover"

library(ggplot2)
library(stringr)
library(lubridate)
library(tidyverse)
library(maps)
library(dplyr)


ds <- read.csv("Graham_Wilson Seychelles BENTHIC data_LIT.csv", head = TRUE)

# transform to long format
str(ds)

names(ds)
ds<- ds[,1:72]

str(ds)
# Acanthastrea is class character
table(ds$Acanthastrea) 
#there's an abundance down as only point (".")
# sum is 10 nonetheless so it's probably a zero
ds$Acanthastrea[ds$Acanthastrea == "."] <- 0
str(ds)
ds$Acanthastrea <- as.numeric(ds$Acanthastrea)
table(ds$Red.corticated)

# get to know the data 
table(ds$count, ds$year)# some years less replicates
table(ds$location, ds$year) # some years less sites
unique(ds$location) # fix mispells
ds$location[ds$location == "Mahe W granitic"] <- "Mahe W Granitic"
# 21 location as per papers
names(ds)

# wide to long format
ds_long <- gather(ds, taxon, abundance, Coraline.algae:cyanobacteria, 
                  factor_key=TRUE)
ds_long <- ds_long[ds_long$abundance>0,]
ds_long <- ds_long[!is.na(ds_long$abundance),]

dim(ds_long)
names(ds_long)
ds_long <- ds_long[,c(1,3,4,7,8)]
names(ds_long)

#check sums per sampling events = 10

ds_check <- ds_long %>% group_by(location,count,year) %>% 
  mutate(sum = sum(abundance))
summary(ds_check$sum) #yes, all good, but divide by 10 to get percentage cover
ds_long$abundance <- ds_long$abundance/10

# check genera
sort(unique(ds_long$taxon))
table(ds_long$taxon)

#because turbinaria is both coral and algae species, split dataset into coral and algae
# To do so attribute coral or algea to the genera. Save template to proceed.
#cat <- data.frame(taxon = unique(ds_long$taxon), 
#                    cat = rep("", length(unique(ds_long$taxon))))

#write.csv(cat, "genera_category.csv", row.names = FALSE)

cat <- read.csv("genera_category_VB.csv", head = TRUE)
ds_long2 <- merge (ds_long, cat)
# dim(ds_long)
# dim(ds_long2)
ds_long <- ds_long2

coords <- read.csv("location_coords.csv", head = TRUE)
ds_long2 <- merge(ds_long,coords, all.x = TRUE)
ds_long <- ds_long2


#split per taxa
ds_coral <- ds_long[ds_long$cat == "c",]
ds_algae <- ds_long[ds_long$cat == "a",]
dim(ds_coral)
dim(ds_algea)

ds_coral_name <- "Seychelles hard coral benthic cover"
ds_algae_name <- "Seychelles algae benthic cover"

# corals 
ds <- ds_coral
rawdata_coral <- data.frame(Abundance = rep("NULL",nrow(ds)),
                      Biomass = ds$abundance, 
                      Family = rep("NULL",nrow(ds)), 
                      Genus = ds$taxon,
                      Species = rep("sp",nrow(ds)),
                      SampleDescr =  as.factor(paste0(ds_coral_name, "_",
                                                      ds$location, "_",
                                                      ds$year,"_",
                                                      ds$count)),
                      Plot = ds$count,
                      Latitude = ds$lat, 
                      Longitude = ds$lon, 
                      DepthElevation = rep("NULL",nrow(ds)),
                      Day = rep("NULL",nrow(ds)), 
                      Month = rep("NULL",nrow(ds)), 
                      Year = ds$year,
                      StudyID = rep("NULL",nrow(ds)))
head(rawdata_coral)
summary(rawdata_coral)

write.csv(rawdata_coral, paste0(ds_coral_name, "_RAWDATA_VB.csv"), row.names = FALSE)

#algae
ds <- ds_algae
ds$taxon[ds$taxon == "Turbinaria.macroalgae"] <- "Turbinaria"
rawdata_algae <- data.frame(Abundance = rep("NULL",nrow(ds)),
                            Biomass = ds$abundance, 
                            Family = rep("NULL",nrow(ds)), 
                            Genus = ds$taxon,
                            Species = rep("sp",nrow(ds)),
                            SampleDescr =  as.factor(paste0(ds_algae_name, "_",
                                                            ds$location, "_",
                                                            ds$year,"_",
                                                            ds$count)),
                            Plot = ds$count,
                            Latitude = ds$lat, 
                            Longitude = ds$lon, 
                            DepthElevation = rep("NULL",nrow(ds)),
                            Day = rep("NULL",nrow(ds)), 
                            Month = rep("NULL",nrow(ds)), 
                            Year = ds$year,
                            StudyID = rep("NULL",nrow(ds)))
head(rawdata_algae)
summary(rawdata_algae)
write.csv(rawdata_algae, paste0(ds_algae_name, "_RAWDATA_VB.csv"), row.names = FALSE)


###code for calculating geographical features of the data
# metadata same for the 2 datasets

## map - use full dataset
ds <- ds_long
world_map <- map_data("world")
world <- ggplot() + coord_fixed() +xlab("") + ylab("")
world  <- world  +
  geom_polygon(data=world_map, aes(x=long, y=lat, group=group),
               colour="gray60", fill="gray60")
points <- world  +
  geom_point(data=as.data.frame(cbind(long = ds$lon, lat = ds$lat)),
             aes(x=long, y=lat, alpha=0.01))
points



##load libraries
library(sp)
library(rgeos)

# this also transforms it from WGS84 coordinate reference to a mercator projection in km for calculating area in sq km
points<- SpatialPoints(cbind(ds$lon,ds$lat)) #%>% 
# sp::spTransform(., CRS("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs"))


##2. Calculate convex hull, area and centroid
convhull<-gConvexHull(points)
gArea(convhull)  ##get area

# 0.0615608

###get centroid

convhull<-gConvexHull(points)
centroid<-gCentroid(convhull)    

##to get the coordinates
centroid@coords

# 55.56077 -4.530888