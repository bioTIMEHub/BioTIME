### amends for thorns et al. #####

library(dplyr)
library(tidyverse)
library(ggplot2)
library(maps)
library(stringr)
library(lubridate)


### beetles ######
dt <- read.csv("C:/Users/vb42/Downloads/beetle.csv", sep = ";")
dim(dt)
str(dt)
dt$Latitude <- str_replace(dt$Latitude, ",", ".")
dt$Latitude <- unlist(as.numeric(dt$Latitude))
dt$Longitude <- str_replace(dt$Longitude, ",", ".")
dt$Longitude <- unlist(as.numeric(dt$Longitude))
colnames(dt)
range(dt$Year)
summary(dt)
unique(dt$Longitude)
unique(dt$Latitude)
sort(unique(dt$Plot))
sort(unique(dt$Species))
sort(unique(dt$Genus))
world_map <- map_data("world")
world <- ggplot() + coord_fixed() + xlab("") + ylab("")
world  <- world  +
  geom_polygon(
    data = world_map,
    aes(x = long, y = lat, group = group),
    colour = "gray60",
    fill = "gray60"
  )
points <- world  +
  geom_point(data = dt,
             aes(x = Longitude, y = Latitude, alpha = 0.01))
points
points_zoom <- points +
  xlim(13, 14) +
  ylim(49, 50)
points_zoom
dt$sample_desc <- as.factor(paste0(dt$Latitude, "_",
                                   dt$Longitude, "_",
                                   dt$Plot, "_",
                                   dt$Year))
dt$SampleDescSp <-
  paste0(dt$Genus, "_", dt$Species, "_", dt$sample_desc)
dt <- merge(aggregate(dt$Abundance,
                      by = list(SampleDescSp = dt$SampleDescSp), sum), dt)
dt <- dt %>%
  distinct(SampleDescSp, .keep_all = TRUE) %>%                # Remove duplicate rows
  select(!SampleDescSp)  %>%
  select(!Abundance)                                           # Remove ungrouped col (Biomass or Abundance)
names(dt)[names(dt) == "x"] <- "Abundance"
rawdata_beetles <- data.frame(
  Abundance = dt$Abundance,
  Biomass = rep(NA, nrow(dt)),
  Family =  rep(NA, nrow(dt)),
  Genus = dt$Genus,
  Species = dt$Species,
  SAMPLE_DESC = dt$sample_desc,
  PLOT = dt$Plot,
  Latitude = dt$Latitude,
  Longitude = dt$Longitude,
  DepthElevation = rep(NA, nrow(dt)),
  Day = rep(NA, nrow(dt)),
  Month = rep(NA, nrow(dt)),
  YEAR = dt$Year,
  StudyID = rep(NA, nrow(dt))
)
##### plants #####
dt <- read.csv("C:/Users/vb42/Downloads/plant.csv", sep = ";")
dim(dt)
str(dt)
dt$Latitude <- str_replace(dt$Latitude, ",", ".")
dt$Latitude <- unlist(as.numeric(dt$Latitude))
dt$Longitude <- str_replace(dt$Longitude, ",", ".")
dt$Longitude <- unlist(as.numeric(dt$Longitude))
dt$Abundance <- str_replace(dt$Abundance, ",", ".")
dt$Abundance <- unlist(as.numeric(dt$Abundance))
colnames(dt)
range(dt$Year)
summary(dt)
unique(dt$Longitude)
unique(dt$Latitude)
sort(unique(dt$Plot))
sort(unique(dt$Species))
sort(unique(dt$Genus))
points <- world  +
  geom_point(data = dt,
             aes(x = Longitude, y = Latitude, alpha = 0.01))
points
points_zoom <- points +
  xlim(13, 14) +
  ylim(49, 50)
points_zoom
dt$sample_desc <- as.factor(paste0(dt$Latitude, "_",
                                   dt$Longitude, "_",
                                   dt$Plot, "_",
                                   dt$Year))
dt$SampleDescSp <-
  paste0(dt$Genus, "_", dt$Species, "_", dt$sample_desc)
dt <- merge(aggregate(dt$Abundance,
                      by = list(SampleDescSp = dt$SampleDescSp), sum), dt)
dt <- dt %>%
  distinct(SampleDescSp, .keep_all = TRUE) %>%                # Remove duplicate rows
  select(!SampleDescSp)  %>%
  select(!Abundance)                                           # Remove ungrouped col (Biomass or Abundance)
names(dt)[names(dt) == "x"] <- "Abundance"
rawdata_plants <- data.frame(
  Abundance = dt$Abundance,
  Biomass = rep(NA, nrow(dt)),
  Family =  rep(NA, nrow(dt)),
  Genus = dt$Genus,
  Species = dt$Species,
  SAMPLE_DESC = dt$sample_desc,
  PLOT = dt$Plot,
  Latitude = dt$Latitude,
  Longitude = dt$Longitude,
  DepthElevation = rep(NA, nrow(dt)),
  Day = rep(NA, nrow(dt)),
  Month = rep(NA, nrow(dt)),
  YEAR = dt$Year,
  StudyID = rep(NA, nrow(dt))
)
##### moss soil ######
dt <- read.csv("C:/Users/vb42/Downloads/moss_soil.csv", sep = ";")
dim(dt)
str(dt)
dt$Latitude <- str_replace(dt$Latitude, ",", ".")
dt$Latitude <- unlist(as.numeric(dt$Latitude))
dt$Longitude <- str_replace(dt$Longitude, ",", ".")
dt$Longitude <- unlist(as.numeric(dt$Longitude))
colnames(dt)
range(dt$Year)
summary(dt)
unique(dt$Longitude)
unique(dt$Latitude)
##### moss obj #####
dt <- read.csv("C:/Users/vb42/Downloads/moss_obj.csv", sep = ";")
dim(dt)
str(dt)
dt$Latitude <- str_replace(dt$Latitude, ",", ".")
dt$Latitude <- unlist(as.numeric(dt$Latitude))
dt$Longitude <- str_replace(dt$Longitude, ",", ".")
dt$Longitude <- unlist(as.numeric(dt$Longitude))
colnames(dt)
range(dt$Year)
summary(dt)
unique(dt$Longitude)
unique(dt$Latitude)
sort(unique(dt$Plot))
sort(unique(dt$Species))
sort(unique(dt$Genus))
points <- world  +
  geom_point(data = dt,
             aes(x = Longitude, y = Latitude, alpha = 0.01))
points
points_zoom <- points +
  xlim(13, 14) +
  ylim(49, 50)
points_zoom
dt$sample_desc <- as.factor(paste0(dt$Latitude, "_",
                                   dt$Longitude, "_",
                                   dt$Plot, "_",
                                   dt$Year))
dt$SampleDescSp <-
  paste0(dt$Genus, "_", dt$Species, "_", dt$sample_desc)
dt <- merge(aggregate(dt$Abundance,
                      by = list(SampleDescSp = dt$SampleDescSp), sum), dt)
dt <- dt %>%
  distinct(SampleDescSp, .keep_all = TRUE) %>%                # Remove duplicate rows
  select(!SampleDescSp)  %>%
  select(!Abundance)                                           # Remove ungrouped col (Biomass or Abundance)
names(dt)[names(dt) == "x"] <- "Abundance"
rawdata_moss_obj <- data.frame(
  Abundance = dt$Abundance,
  Biomass = rep(NA, nrow(dt)),
  Family =  rep(NA, nrow(dt)),
  Genus = dt$Genus,
  Species = dt$Species,
  SAMPLE_DESC = dt$sample_desc,
  PLOT = dt$Plot,
  Latitude = dt$Latitude,
  Longitude = dt$Longitude,
  DepthElevation = rep(NA, nrow(dt)),
  Day = rep(NA, nrow(dt)),
  Month = rep(NA, nrow(dt)),
  YEAR = dt$Year,
  StudyID = rep(NA, nrow(dt))
)
##### lichen soil #####
dt <- read.csv("C:/Users/vb42/Downloads/lichen_soil.csv", sep = ";")
dim(dt)
str(dt)
dt$Latitude <- str_replace(dt$Latitude, ",", ".")
dt$Latitude <- unlist(as.numeric(dt$Latitude))
dt$Longitude <- str_replace(dt$Longitude, ",", ".")
dt$Longitude <- unlist(as.numeric(dt$Longitude))
dt$Abundance <- str_replace(dt$Abundance, ",", ".")
dt$Abundance <- unlist(as.numeric(dt$Abundance))
sum(is.na(dt$Longitude))
sum(is.na(dt$Latitude))
##### fungi #####
dt <- read.csv("C:/Users/vb42/Downloads/fungi.csv", sep = ";")
dim(dt)
str(dt)
dt$Latitude <- str_replace(dt$Latitude, ",", ".")
dt$Latitude <- unlist(as.numeric(dt$Latitude))
dt$Longitude <- str_replace(dt$Longitude, ",", ".")
dt$Longitude <- unlist(as.numeric(dt$Longitude))
sum(is.na(dt$Longitude))
sum(is.na(dt$Latitude))
colnames(dt)
range(dt$Year)
summary(dt)
unique(dt$Longitude)
unique(dt$Latitude)
sort(unique(dt$Plot))
sort(unique(dt$Species))
sort(unique(dt$Genus))
points <- world  +
  geom_point(data = dt,
             aes(x = Longitude, y = Latitude, alpha = 0.01))
points
points_zoom <- points +
  xlim(13, 14) +
  ylim(49, 50)
points_zoom
dt$sample_desc <- as.factor(paste0(dt$Latitude, "_",
                                   dt$Longitude, "_",
                                   dt$Plot, "_",
                                   dt$Year))
dt$SampleDescSp <-
  paste0(dt$Genus, "_", dt$Species, "_", dt$sample_desc)
dt <- merge(aggregate(dt$Abundance,
                      by = list(SampleDescSp = dt$SampleDescSp), sum), dt)
dt <- dt %>%
  distinct(SampleDescSp, .keep_all = TRUE) %>%                # Remove duplicate rows
  select(!SampleDescSp)  %>%
  select(!Abundance)                                           # Remove ungrouped col (Biomass or Abundance)
names(dt)[names(dt) == "x"] <- "Abundance"
rawdata_fungi <- data.frame(
  Abundance = dt$Abundance,
  Biomass = rep(NA, nrow(dt)),
  Family =  rep(NA, nrow(dt)),
  Genus = dt$Genus,
  Species = dt$Species,
  SAMPLE_DESC = dt$sample_desc,
  PLOT = dt$Plot,
  Latitude = dt$Latitude,
  Longitude = dt$Longitude,
  DepthElevation = rep(NA, nrow(dt)),
  Day = rep(NA, nrow(dt)),
  Month = rep(NA, nrow(dt)),
  YEAR = dt$Year,
  StudyID = rep(NA, nrow(dt))
)
##### lichen_obj #####
dt <- read.csv("C:/Users/vb42/Downloads/lichen_obj.csv", sep = ";")
dim(dt)
str(dt)
dt$Latitude <- str_replace(dt$Latitude, ",", ".")
dt$Latitude <- unlist(as.numeric(dt$Latitude))
dt$Longitude <- str_replace(dt$Longitude, ",", ".")
dt$Longitude <- unlist(as.numeric(dt$Longitude))
sum(is.na(dt$Longitude))
sum(is.na(dt$Latitude))
colnames(dt)
range(dt$Year)
summary(dt)
unique(dt$Longitude)
unique(dt$Latitude)
sort(unique(dt$Plot))
sort(unique(dt$Species))
sort(unique(dt$Genus))
points <- world  +
  geom_point(data = dt,
             aes(x = Longitude, y = Latitude, alpha = 0.01))
points
points_zoom <- points +
  xlim(13, 14) +
  ylim(49, 50)
points_zoom
dt$sample_desc <- as.factor(paste0(dt$Latitude, "_",
                                   dt$Longitude, "_",
                                   dt$Plot, "_",
                                   dt$Year))
dt$SampleDescSp <-
  paste0(dt$Genus, "_", dt$Species, "_", dt$sample_desc)
dt <- merge(aggregate(dt$Abundance,
                      by = list(SampleDescSp = dt$SampleDescSp), sum), dt)
dt <- dt %>%
  distinct(SampleDescSp, .keep_all = TRUE) %>%                # Remove duplicate rows
  select(!SampleDescSp)  %>%
  select(!Abundance)                                           # Remove ungrouped col (Biomass or Abundance)
names(dt)[names(dt) == "x"] <- "Abundance"
rawdata_lichen_obj <- data.frame(
  Abundance = dt$Abundance,
  Biomass = rep(NA, nrow(dt)),
  Family =  rep(NA, nrow(dt)),
  Genus = dt$Genus,
  Species = dt$Species,
  SAMPLE_DESC = dt$sample_desc,
  PLOT = dt$Plot,
  Latitude = dt$Latitude,
  Longitude = dt$Longitude,
  DepthElevation = rep(NA, nrow(dt)),
  Day = rep(NA, nrow(dt)),
  Month = rep(NA, nrow(dt)),
  YEAR = dt$Year,
  StudyID = rep(NA, nrow(dt))
)
##### bird #####
dt <- read.csv("C:/Users/vb42/Downloads/bird.csv", sep = ";")
dim(dt)
str(dt)
dt$Latitude <- str_replace(dt$Latitude, ",", ".")
dt$Latitude <- unlist(as.numeric(dt$Latitude))
dt$Longitude <- str_replace(dt$Longitude, ",", ".")
dt$Longitude <- unlist(as.numeric(dt$Longitude))
sum(is.na(dt$Longitude))
sum(is.na(dt$Latitude))
colnames(dt)
range(dt$Year)
summary(dt)
unique(dt$Longitude)
unique(dt$Latitude)
sort(unique(dt$Plot))
sort(unique(dt$Species))
sort(unique(dt$Genus))
points <- world  +
  geom_point(data = dt,
             aes(x = Longitude, y = Latitude, alpha = 0.01))
points
points_zoom <- points +
  xlim(13, 14) +
  ylim(49, 50)
points_zoom
dt$sample_desc <- as.factor(paste0(dt$Latitude, "_",
                                   dt$Longitude, "_",
                                   dt$Plot, "_",
                                   dt$Year))
dt$SampleDescSp <-
  paste0(dt$Genus, "_", dt$Species, "_", dt$sample_desc)
dt <- merge(aggregate(dt$Abundance,
                      by = list(SampleDescSp = dt$SampleDescSp), sum), dt)
dt <- dt %>%
  distinct(SampleDescSp, .keep_all = TRUE) %>%                # Remove duplicate rows
  select(!SampleDescSp)  %>%
  select(!Abundance)                                           # Remove ungrouped col (Biomass or Abundance)
names(dt)[names(dt) == "x"] <- "Abundance"
rawdata_bird <- data.frame(
  Abundance = dt$Abundance,
  Biomass = rep(NA, nrow(dt)),
  Family =  rep(NA, nrow(dt)),
  Genus = dt$Genus,
  Species = dt$Species,
  SAMPLE_DESC = dt$sample_desc,
  PLOT = dt$Plot,
  Latitude = dt$Latitude,
  Longitude = dt$Longitude,
  DepthElevation = rep(NA, nrow(dt)),
  Day = rep(NA, nrow(dt)),
  Month = rep(NA, nrow(dt)),
  YEAR = dt$Year,
  StudyID = rep(NA, nrow(dt))
)
