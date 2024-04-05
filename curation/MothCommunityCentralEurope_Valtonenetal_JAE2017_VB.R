###  Curation script for ###

###### upload libraries #####
library(ggplot2)
library(stringr)
library(lubridate)
library(tidyverse)
library(maps)

###### read dataset ######
mypath <- getwd()     
mypath

ds <- read.csv(file = paste0(mypath,"/Valtonen_etal_JAE.csv"), h=T)

# transform to long format
str(ds)
ds <- as.data.frame(ds %>% 
                      gather(Species, Abundance,  Abraxas_grossulariata:Zeuzera_pyrina))
str(ds)


# are individuals 2095664 as said in the study?
sum(ds$Abundance) 
# yes!


#### add and check locations
# coordinates taken from wikipedia and checked with map in Appendix 2
unique(ds$Site)
coord <- data.frame(rbind(c(Site = "Tompa",lat = 46.205,lon = 19.547),
                          c(Site = "Tolna",lat = 46.424,lon = 18.790),
                          c(Site = "Vargesztes",lat = 47.475,lon = 18.397),
                          c(Site = "Felsotarkany",lat = 47.973,lon = 20.416),
                          c(Site = "Gyula",lat = 46.65,lon = 21.28),
                          c(Site = "Repashuta",lat = 48.049,lon = 20.528),
                          c(Site = "Sopron",lat = 47.685,lon = 16.583)))
ds_coord <- merge (ds, coord, by = "Site")                
str(ds_coord)
ds_coord$lat <- as.numeric(ds_coord$lat)
ds_coord$lon <- as.numeric(ds_coord$lon)

## map

world_map <- map_data("world") 
world <- ggplot() + coord_fixed() +xlab("") + ylab("") 
world  <- world  + 
  geom_polygon(data=world_map, aes(x=long, y=lat, group=group), 
               colour="gray60", fill="gray60") 
points <- world  + 
  geom_point(data=ds_coord, 
             aes(x=lon, y=lat, alpha=0.01)) 
points

# zoom 
points_zoom <- points +
  ylim(45,49)+
  xlim(15,24.5)
points_zoom

# looks like the map in Appendix 2
ds<- ds_coord
##### check counts####
unique(ds$Abundance) # inspect values
is.numeric(ds$Abundance)

sum(ds$Abundance <= 0) # there's 136087 cases with 0 count
ds <- ds[ds$Abundance > 0,] # exclude them
sum(is.na(ds$Abundance)) # there's no NA counts
# ds <- ds[!is.na(ds$Abundance),]
sum(is.null(ds$Abundance)) # there's no NULL counts
# ds <- ds[ds$Abundance != "NULL",]
sum(ds$Abundance=="")  # there's no blanks
# ds <- ds[ds$Abundance != "NULL",]

dim(ds)  # 59707     4

##### check taxa ####
length(unique(ds$Species)) # 878 as per paper
ds$Genus_Species <- ds$Species
### organize in Genus and Species column
ds$Genus <- str_split_fixed(ds$Genus_Species, pattern = "_", n = 2) [,1]
ds$Species <- str_split_fixed(ds$Genus_Species, pattern = "_", n = 2) [,2]
str(ds)

sum(is.na(ds$Species)) # no NAs
sum(ds$Species=="") # no blanks
sum(is.null(ds$Species)) # no NULLs
sort(unique(ds$Species)) # sorting taxa to catch duplications and mispelling
# seems ok but checking a couple of species
unique(ds[ds$Species =="oo","Genus"]) # Dicycla oo exists
unique(ds[ds$Species =="or","Genus"]) # Tethea or exists

sum(is.na(ds$Genus)) # no NAs
sum(ds$Genus=="") # no blanks
sum(is.null(ds$Genus)) # no NULLs
sort(unique(ds$Genus)) # sorting taxa to catch duplications and mispelling
# seems ok

# when the dataset given is in matrix form (short format) 
# use spread and gather from tidyverse 


## detect taxa with "sp" and variants (e.g. "spp", "sp."...) 
#unique(ds$Species)[str_detect(unique(ds$Species), " sp")]

## detect no spaces -> spot taxa that aren't in "Genus species" format
#unique(ds$Species)[!str_detect(unique(ds$Species), " ")] # these need fixing

## add "sp" to genus levels
#ds$Species <- revalue(ds$Species, c("Pomatoschistus"= "Pomatoschistus sp", "Crangon"= "Crangon sp",
#                                "Gaidropsarus"="Gaidropsarus sp", "Ammodytes"= "Ammodytes sp",
#                                "Pandalus"="Pandalus sp"))

# # add family columns to species ided to family
# unique(ds$Species)[!str_detect(unique(ds$Species), " ")]
# ds$family <- ""
# index <- ds$Species == "Gobiidae" |  ds$Species == "Labridae" | 
#   ds$Species == "Ammodytidae" |  ds$Species == "Loliginidae"
# ds$family[index] <- ds$Species [index] 
# 
# ds$Species <- revalue(ds$Species, c("Gobiidae"= "", 
#                                 "Labridae"= "",
#                                 "Ammodytidae"="", 
#                                 "Gobiesocidae"= "",
#                                 "Loliginidae"=""))


##### check dates ####

# not necessary since it's over a year

# ds[,c("yearcollected","datecollected","daycollected","monthcollected")]
# 
# summary(as.POSIXct(ds$datecollected)) #Min and Max make sense
# summary(ds$yearcollected)
# summary(ds$monthcollected)
# summary(ds$daycollected) 

## check years
summary(ds$Year)
# seems right

sum(is.na(ds$Year)) # there's no NA counts
# ds <- ds[!is.na(ds$yearcollected),]
sum(ds$Year=="NULL") # there's no NULL counts
# ds <- ds[ds$yearcollected != "NULL",]
sum(ds$Year=="")  # there's no blanks
# ds <- ds[ds$yearcollected != "NULL",]

##### control coordinates ####
summary(ds$lon)
summary(ds$lat)

# check that there isn't any character
# is.numeric(ds$lon)
# is.numeric(ds$lat)

##### check if pooling is necessary 
### NO!

# #life stage
# ds$lifestage
# sum(!is.na(ds$lifestage))
# 
# #sex
# ds$sex
# ## this needs pooling! Create sample event descriptor per species
# 
# ds$SampleDescSpec <- as.factor(paste0(ds$latitude,"_",
#                                       ds$longitude,"_",
#                                       "TrawlSurvey1_",
#                                       ds$daycollected,"_",
#                                       ds$monthcollected,"_",
#                                       ds$yearcollected,"_",
#                                       ds$depth,"_",
#                                       ds$bottomdepth,"_",
#                                       ds$sname))
# 
# 
# ds2 <- as.data.frame(ds %>% 
#                        select(depth,longitude, latitude, sname, family, 
#                               datecollected, daycollected, monthcollected, 
#                               yearcollected, sex, depth, bottomdepth,
#                               SampleDescSpec, observedindividualcount) %>% 
#                        distinct())
# 
# ds3 <- merge(aggregate(ds2$observedindividualcount, 
#                        by=list(SampleDescSpec=ds2$SampleDescSpec), FUN=sum),ds2)
# 
# ds3 <- as.data.frame(ds3 %>% 
#                        distinct(SampleDescSpec,.keep_all = TRUE))


## there should be 223 site-year combination as per paper
ds %>%
  group_by(Year,Site) %>%
  count()
# yep!

dim(ds)
str(ds)
rawdata <- data.frame(Abundance = ds$Abundance,
                      Biomass = rep("NULL",nrow(ds)), 
                      Family = rep(NA,nrow(ds)), 
                      Genus = ds$Genus,
                      Species = ds$Species,
                      SampleDesc =  paste0(ds$lat,"_",
                                           ds$lon,"_",
                                           "MothCommunityCentralEurope_",
                                           #ds$daycollected,"_",
                                           #ds$monthcollected,"_",
                                           ds$Year),
                                           #ds$depth,"_",
                                           #ds$bottomdepth),
                      Plot = rep("NULL",nrow(ds)),
                      Latitude = ds$lat, 
                      Longitude = ds$lon,
                      DepthElevation = rep("NULL",nrow(ds)),
                      Day = rep("NULL",nrow(ds)),
                      Month = rep("NULL",nrow(ds)),
                      Year = ds$Year,
                      StudyID = rep("NULL",nrow(ds)))
head(rawdata)

### save raw data 
write.csv(rawdata, "MothCommunityCentralEurope_Valtonenetal_JAE2017_RAWDATA.csv", row.names = FALSE)

###code for calculating geographical features of the data

##load libraries
library(sp)
library(rgeos)

#Import the raw data file - has latitude & longitude

##1. Convert data points into point spatial object
points <- SpatialPoints(cbind(rawdata$Latitude,rawdata$Longitude))

##2. Calculate convex hull, area and centroid
convhull<-gConvexHull(points)
gArea(convhull)  ##get area

#5.073924

###get centroid
centroid<-gCentroid(convhull)    

##to get the coordinates
centroid@coords

# lon = 47.19404
# lat = 19.28479

############to plot
plot(points,cex=0.5,axes=F,pch=19)
axis(side=2,las=1,cex.axis=0.8, tck=-.01,line=-0.2)
axis(side=1,tck=-.01,labels=NA) ##1st just add the line
axis(side=1,lwd = 0, line = -0.6,cex.axis=.8, labels=T)   ##then add the labels (lwd=0 so it won't add #another line)

##add the convex hull
plot(convhull,add=T)

##add the centroid
points(centroid,col="green",pch=19,cex=1.2)

## looks right!


