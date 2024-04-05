# title: "Georgia Coastal Ecosystems LTER"
# author: Anokhi Saha"
# date: "10 July 2023

# 1. Load the required packages

require(dplyr)
require(ggplot2)
require(stringr)
require(readxl)
require(maps)

# 2. Read in data
setwd("~/Documents/bio/Georgia Plants")
dt<- read.csv("GeorgiaPlantsData.csv", skip = 4)
View(dt)

# To get the data to load i had to remove the top 4 rows of data due to formatting to i have now added the headings back in. 
column_names <- c("Year", "Month", "Day", "Plot", "Zone", "Site", "Quadrat Area", "Species Code", "Species", "Shoot_Height", "Flowering_Status", "Plot_Disturbance", "Plant_Biomass")
names(dt) <- column_names

# 3. Criteria check 

# Are there more than two distinct years? (TRUE)
n_distinct(dt$Year) >= 2

# Check Year, month and day > (TRUE)
is.factor(dt$Year) | is.integer(dt$Year)
is.factor(dt$Month) | is.integer(dt$Month)
is.factor(dt$Day) | is.integer(dt$Day)

# Change 'Plant Biomass' to Biomass and check is numeric (TRUE)
colnames(dt)[13] = "Biomass"
is.numeric(dt$Biomass)

# Now test Species as a factor (TRUE)
dt$Species <- as.factor(dt$Species)
is.factor(dt$Species)

# Make sure none of the rows for Biomass are zero > 11 missing values in the biomass column
min(dt$Biomass) > 0 
missing_values <- is.na(dt$Biomass)
num_missing <- sum(missing_values)
cat("Number of missing values in Biomass column:", num_missing, "\n")

# This has removed the rows where there were no values for biomass
dt <- dt[!is.na(dt$Biomass),]

# Remove any extraneous columns that BioTIME does use
dt$Zone <- NULL
dt$`Quadrat Area` <- NULL
dt$`Species Code` <- NULL
dt$Shoot_Height <- NULL
dt$Flowering_Status <- NULL
dt$Plot_Disturbance <- NULL

# Add the latitude and Longitude
# Need to subset the data by Plot and add in data for each

dt.1 <- subset(dt, Plot=="SCSA")
dt.1$Latitude <- as.numeric(rep('31.32276675', nrow(dt.1)))
dt.1$Longitude <- as.numeric(rep('-81.373521', nrow(dt.1)))

dt.2 <- subset(dt, Plot=="ZSC1")
dt.2$Latitude <- as.numeric(rep('31.3283445', nrow(dt.2)))
dt.2$Longitude <- as.numeric(rep('-81.450896', nrow(dt.2)))

dt.3 <- subset(dt, Plot=="ZSC2")
dt.3$Latitude <- as.numeric(rep('31.3406405', nrow(dt.3)))
dt.3$Longitude <- as.numeric(rep('-81.453340', nrow(dt.3)))

# Then bind them back together
rbind(dt.1, dt.2, dt.3)
dt <- rbind(dt.1, dt.2, dt.3)

# Now we can plot on the map > It is where we would expect it to be!

world_map <- map_data('world') 
world <- ggplot(world_map) + 
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  coord_fixed() + 
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())
points <- world + geom_point(data=dt, aes(x=Longitude, y=Latitude), shape=21)
points + coord_fixed(xlim=c(-180,180), ylim=c(-90,90))
detach('package:maps')

# 2. Secondary field checks

# Make a copy of the taxon column
dt$SpeciesCheck <- dt$Species

# Check for any that end with the family name - in this case, none 
str_which(dt$SpeciesCheck, 'idae$|eae$')

# Check the genera
sort(unique(word(dt$SpeciesCheck, 1)))

# Check the species
sort(unique(word(dt$SpeciesCheck, 2)))

# Need to replace spp. with sp to fit the BiomTime standards
replace_g <- c(
  'spp.' = 'sp'
)

# Separate genus and species to own columns
dt$Genus <- word(dt$SpeciesCheck, 1) 
dt$Species <- word(dt$SpeciesCheck, start=2)

# Apply the replacement vector
dt$Species <- str_replace_all(dt$Species, replace_g)

# Remove the species check column
dt$SpeciesCheck <- NULL

# 3. Prepare for export

dt$Abundance <- rep('', nrow(dt))
dt$Family <- rep('', nrow(dt))
dt$DepthElevation <- rep('', nrow(dt))
dt$StudyID <- rep('', nrow(dt))

dt_merged <- dt %>% group_by(Abundance, Biomass, Family, Genus, Species, Plot, Site, Latitude, Longitude, DepthElevation, Day, Month, Year, StudyID) %>% 
  summarise(Biomass=sum(Biomass)) %>% ungroup() %>% arrange(Year, Family, Genus, Species)

dim(dt)[1]-dim(dt_merged)[1]

# Name the data set
dataset_name <- 'LTER_GeorgiaCoastalEcosystems'

dt_merged$SampleDescription <- as.factor(with(dt_merged, paste(dataset_name, Latitude, Longitude, Year, Month, Day, sep='_')))

length(levels(dt_merged$SampleDescription)) 

# Re-order and match formatting 

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
                         'StudyID')] %>% arrange(Year, Family, Genus, Species)
head(dt_merged)


## 4. Export and spreadsheet prep

setwd(file.choose() %>% dirname())
write.csv(dt_merged, paste0(getwd(), dataset_name, '_rawdata_AnS.csv'), row.names=F, na='')

# 5. Generate the meta-data for the contributor form

library(sp)
library(rgeos)

# 1.Convert data points into point spatial object
points <- SpatialPoints(cbind(dt$Longitude,dt$Latitude))

# 2. Calculate convex hull, area and centroid
convhull<-gConvexHull(points)
gArea(convhull)  ##get area

# 0.0004688855 = Area 

# 3. Get centroid
centroid<-gCentroid(convhull)    

# 4. To get the coordinates
centroid@coords

# lat = -81.4259
# lon = 31.33058

# to plot
plot(points,cex=0.5,axes=F,pch=19, xlab="Longitude", ylab="Latitude")
axis(side=2,las=1,cex.axis=0.8, tck=-.01,line=-0.2)
axis(side=1,tck=-.01,labels=NA) 
axis(side=1,lwd = 0, line = -0.6,cex.axis=.8, labels=T) 

# 5. Add the convex hull
plot(convhull,add=T)

# 6. Add the centroid
points(centroid,col="Red",pch=19,cex=1.2)
