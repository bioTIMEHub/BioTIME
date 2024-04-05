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

dsold <- read.csv(file = paste0(mypath,"/Lima et al/templatesLima_Eterovick.csv"), h=T)
ds <- read.csv(file = paste0(mypath,"/Lima et al/checked_data.csv"), h=T)
# check dataset
str(ds)
ds <- ds[,1:13]
str(ds)
## SampleDescriptios has lifestage <- it will need pooling.
## StudyID has a descriptor for the two fieldwork campaigns (1998-1999 and 2015-2016) <- delete it

ds$Genus <- as.factor(ds$Genus)
ds$Species <- as.factor(ds$Species)

summary(ds)

# delete NAs rows
dim(ds)
ds <- as.data.frame(ds %>%
                      drop_na(Abundance))
dim(ds)
summary(ds)

length(unique(c(ds$Longitude)))
length(unique(c(ds$Latitude)))
length(unique(c(ds$DepthElevation)))

ds %>%
  #filter(Longitude, Latitude, DepthElevation) %>%
  distinct(Longitude, Latitude, DepthElevation,StudyID)
ds %>%
  #filter(Longitude, Latitude, DepthElevation) %>%
  distinct(Longitude, Latitude, DepthElevation)

# seems ok

## map

world_map <- map_data("world") 
world <- ggplot() + coord_fixed() +xlab("") + ylab("") 
world  <- world  + 
  geom_polygon(data=world_map, aes(x=long, y=lat, group=group), 
               colour="gray60", fill="gray60") 
## coords are in SIRGAS 2000 / UTM zone 23S coordinate system. Translate them to degrees (WGS84)
ds$Longitude <- recode(ds$Longitude, '647788' = -43.5931205,
                                                '647560' = -43.5952736,
                                                '653118'= -43.542697,
                                                '652692' = -43.5466601,
                                                '648894' = -43.582686,
                                                '652681' = -43.5469279,
                                                '650540' = -43.5669298,
                                                '646955' = -43.6009576,
                                                '651952' = -43.5537674,
                                                '654265' = -43.5317489,
                                                '651148' = -43.5611615,
                                                '653485' = -43.5392675,
                                                '651203' = -43.560636,
                                                '650398' = -43.5682812,
                                                '652005' = -43.5532567,
                                                '651852' = -43.5547117)
ds$Latitude <- recode(ds$Latitude,'7866409' = -19.2902138,
                                              '7866196' = -19.2921549,
                                              '7870040' = -19.257012,
                                              '7868918' = -19.267181,
                                              '7867551' = -19.279815,
                                              '7870968' = -19.2486611,
                                              '7866333' = -19.2906968,
                                              '7865251' = -19.3007368,
                                              '7869769' = -19.2595484,
                                              '7869581' = -19.2610716,
                                              '7866544' = -19.2887451,
                                              '7870812' = -19.2500096,
                                              '7866517' = -19.2889849,
                                              '7866336' = -19.2906803,
                                              '7869687' = -19.2602853,
                                              '7869680' = -19.26036)

points <- world  + 
  geom_point(data=ds, 
             aes(x=Longitude, y=Latitude, alpha=0.01)) 
points

# zoom 
points_zoom <- points +
  ylim(-19.31,-19.22)+
  xlim(-43.6,-43.5)
points_zoom

# looks like the map in the paper - good!

##### check counts####
unique(ds$Abundance) # inspect values
is.numeric(ds$Abundance)

sum(ds$Abundance <= 0) # none
#ds <- ds[ds$Abundance > 0,] # exclude them
sum(is.na(ds$Abundance)) # there's no NA counts
# ds <- ds[!is.na(ds$Abundance),]
sum(is.null(ds$Abundance)) # there's no NULL counts
# ds <- ds[ds$Abundance != "NULL",]
sum(ds$Abundance=="")  # there's no blanks
# ds <- ds[ds$Abundance != "NULL",]

dim(ds)  # 1576   13

##### check taxa ####
length(unique(ds$Species)) # 33, 1 more than paper results. Likely OK, since they rarefied the data before presenting results.
sort(unique(ds$Species)) # they match the SM of the paper (extra species: Hylodes otavioi)

## detect taxa with "sp" and variants (e.g. "spp", "sp."...)  and change to "sp".
unique(ds$Species)[str_detect(unique(ds$Species), " sp")]
ds$Species <- plyr::revalue(ds$Species, c("Adenomera sp."= "Adenomera sp", "Vitreorana sp."= "Vitreorana sp"))

## detect no spaces -> spot taxa that aren't in "Genus species" format
unique(ds$Species)[!str_detect(unique(ds$Species), " ")] # these need fixing

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

ds[,c("Day","Month","Year")]


## check
summary(ds$Month) #98?? 
ds[ds$Month == 98, ] 
ds[ds$Year == 1999,"Month"] ##Must be 8 since other streams were sample on that day

ds$Month[ds$Month == 98] <- 8

summary(ds$Day)
# seems right

sum(is.na(ds$Year)) # there's no NA count
# ds <- ds[!is.na(ds$yearcollected),]
sum(ds$Year=="NULL") # there's no NULL counts
# ds <- ds[ds$yearcollected != "NULL",]
sum(ds$Year=="")  # there's no blanks
# ds <- ds[ds$yearcollected != "NULL",]

##### control coordinates ####
summary(ds$Longitude)
summary(ds$Latitude)

##### check if pooling is necessary -> YES
### Adults and Juveniles need to be pooled. thin info was put into "sampling description"

ds$SampleDescription

ds$SampleDescSpec <- as.factor(paste0(ds$Longitude,"_",
                                      ds$Latitude,"_",
                                      "AnuranCipo_",
                                      ds$Day,"_",
                                      ds$Month,"_",
                                      ds$Year,"_",
                                      ds$DepthElevation,"_river",
                                      ds$Plot,"_",
                                      ds$Species))


ds2 <- as.data.frame(ds %>%
                       select(DepthElevation,Longitude, Latitude, Species, Genus,
                              Day, Month, Year, Plot,
                              SampleDescSpec, Abundance))
dim(ds2)
length(ds2$SampleDescSpec)
length(unique(ds2$SampleDescSpec))

ds3 <- merge(aggregate(ds2$Abundance,
                       by=list(SampleDescSpec=ds2$SampleDescSpec), FUN=sum),ds2)

ds3 <- as.data.frame(ds3 %>%
                       distinct(SampleDescSpec,.keep_all = TRUE))


dim(ds)
str(ds)
str(ds3)
dim(ds3)
rawdata <- data.frame(Abundance = ds3$Abundance,
                      Biomass = rep("NULL",nrow(ds3)), 
                      Family = rep(NA,nrow(ds3)), 
                      Genus = ds3$Genus,
                      Species = ds3$Species,
                      SampleDesc =  paste0(ds3$Longitude,"_",
                                           ds3$Latitude,"_",
                                           "AnuranCipo_",
                                           ds3$Day,"_",
                                           ds3$Month,"_",
                                           ds3$Year,"_",
                                           ds3$DepthElevation),
                      #ds$depth,"_",
                      #ds$bottomdepth),
                      Plot = ds3$Plot,
                      Latitude = ds3$Latitude, 
                      Longitude = ds3$Longitude,
                      DepthElevation = ds3$DepthElevation,
                      Day = ds3$Day,
                      Month = ds3$Month,
                      Year = ds3$Year,
                      StudyID = rep("NULL",nrow(ds3)))
head(rawdata)
dim(rawdata)
### save raw data 
write.csv(rawdata, "AnuranCipo_Limaetal_PlosONE2019_RAWDATA.csv", row.names = FALSE)

###code for calculating geographical features of the data

##load libraries
library(sp)
library(rgeos)

#Import the raw data file - has latitude & longitude

##1. Convert data points into point spatial object
points <- SpatialPoints(cbind(rawdata$Longitude,rawdata$Latitude))

##2. Calculate convex hull, area and centroid
convhull<-gConvexHull(points)
gArea(convhull)  ##get area

# 0.001269201

###get centroid
centroid<-gCentroid(convhull)    

##to get the coordinates
centroid@coords

# lat = -19.27457
# lon = -43.56238

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


paste0(Longitude,"_",Latitude,"_","AnuranCipo_",Day,"_",Month,"_",Year,"_",DepthElevation)


########## fixing sample_desc and plot
library( readxl)

ds <- readxl::read_excel("~/Dropbox/towards Biotime v2/NewStudies/AnuranCipo_Limaetal_PlosONE2019_VB.xlsx", sheet = 1)
ds

sd <- str_sub(ds$SampleDesc, start = 1, end = 24)
sd2 <- str_sub(ds$SampleDesc, start = 36, end = 50)
ds$SampleDesc<- paste0(sd,sd2)
write.csv(ds, "~/Dropbox/towards Biotime v2/originalData/AnuranCipo_Limaetal.PlosONE2010_VB/AnuranCipo_Limaetal_PlosONE2019_rawdata_VB_checked.csv", row.names = FALSE)


library(sf)
library(clipr)

# 1. Convert data points into point spatial object
# assumes your rawdata dataframe is called rawdata
dt_coord <- ds%>% select(Longitude, Latitude) %>% distinct() %>%
  st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()

# 2. Calculate convex hull, area and centroid
centroid <- dt_coord %>% st_convex_hull() %>% st_centroid() %>% unlist # get centroid
write_clip(centroid[c(2,1)]) # copy as lat-long

# transform to mercator to get area in sq km
area <- st_transform(dt_coord, st_crs("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs")) %>%
  st_convex_hull() %>% st_area()  ##get area in sq km
write_clip(area)

# Plot the geometries -----------------------------------------------------

require(ggplot2)
world_map <- map_data('world') # check whether the GPS coordinates match expectations
ggplot(world_map) +
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  geom_point(data = dt_merged, aes(x = Longitude, y = Latitude), size = 2, shape = 21, colour = 'blue') +
  geom_point(aes(x = centroid[1], y = centroid[2]), size = 3, colour = 'green') +
  geom_sf(data = dt_coord %>% st_convex_hull(), colour = 'blue', fill = 'transparent') +
  coord_sf(xlim = c(-115, -110), ylim = c(30,35)) + # make sure these fit your points
  labs(x = NULL, y = NULL) +
  theme_minimal()