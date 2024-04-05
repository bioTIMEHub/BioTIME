require(dplyr)
require(ggplot2)
require(stringr)
require(readxl)
require(maps)
require(tidyr)

rm(list = ls())
# setwd("../")
# Dataset original file path: ./Dongting_2011_2015/2011-2015年洞庭湖洲滩植物群落长期监测数据集V1.xlsx
# Rename it to ./Dongting_2011_2015/data/Dongting_2011_2015.xlsx
# To avoid read_excel reading Chinese error

setwd("./Dongting_2011_2015/data/")
# sampling methods
# 本数据集的数据来自洞庭湖湿地生态系统综合观测场和洞庭湖湿地生态系统辅助观测场。
# 其位置的坐标分别为： 
# 综合观测场 112.7857° –112.7985° E、 29.4563° –29.4683° N
# DTMZH01ABC_01 洞庭湖湿地生态系统综合观测场苔草土壤生物采样地; 
# DTMZH01ABC_02 洞庭湖湿地生态系统综合观测场南荻土壤生物采样地;
# 辅助观测场 113.0607° –113.0707° E、 29.2394° –29.2501° N
# DTMFZ01ABC_01 洞庭湖湿地生态系统辅助观测场水蓼土壤生物采样地

# 洞庭湖湿地生态系统综合观测场设置有苔草和南荻土壤生物采样地，
# 主要监测的植被类型为苔草和南荻，洞庭湖湿地生态系统辅助观测场设置有水蓼土壤生物采样地，
# 主要监测的植被类型为水蓼。在时间范围上，本数据集的数据产生于 2011–2015 年，
# 选取每年 2 月， 4 月， 10 月和 12 月份的数据，
# 但 2012 年和 2015 年 10 月份监测样地还未完全退水，因此选用 11 月份的数据。

### area coordinate

# load libraries
library(sf)
library(clipr)

# 综合观测场 DTMZH01ABC

lat_zh_1 <- 29.4563
lat_zh_2 <- 29.4683
long_zh_1 <- 112.7857
long_zh_2 <- 112.7985


# 1. Convert data points into point spatial object
df_coord_zh <- tibble(Longitude = c(long_zh_1, long_zh_2, long_zh_2, long_zh_1), 
                      Latitude = c(lat_zh_1, lat_zh_1, lat_zh_2, lat_zh_2)) %>%
  st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()

# 2. Calculate convex hull, area and centroid
centroid_zh <- df_coord_zh %>% st_convex_hull() %>% st_centroid() %>% unlist # get centroid

# 112.7921  29.4623

# calculate area
st_area(df_coord_zh %>% st_convex_hull())*0.000001
# 1.653563 sq km

# 辅助观测场 DTMFZ01ABC_01 

lat_fz_1 <- 29.2394
lat_fz_2 <- 29.2501
long_fz_1 <- 113.0607
long_fz_2 <- 113.0707


# 1. Convert data points into point spatial object
df_coord_fz <- tibble(Longitude = c(long_fz_1, long_fz_2, long_fz_2, long_fz_1), 
                      Latitude = c(lat_fz_1, lat_fz_1, lat_fz_2, lat_fz_2)) %>%
  st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()

# 2. Calculate convex hull, area and centroid
centroid_fz <- df_coord_fz %>% st_convex_hull() %>% st_centroid() %>% unlist # get centroid

# 112.7921  29.4623

# calculate area
st_area(df_coord_fz %>% st_convex_hull())*0.000001
# 1.154359 sq km

# calculate study area centroid

# 1. Convert data points into point spatial object
df_coord <- tibble(Longitude = c(long_zh_1, long_zh_2, long_zh_2, long_zh_1, long_fz_1, long_fz_2, long_fz_2, long_fz_1), 
                      Latitude = c(lat_zh_1, lat_zh_1, lat_zh_2, lat_zh_2, lat_fz_1, lat_fz_1, lat_fz_2, lat_fz_2)) %>%
  st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()

# 2. Calculate convex hull, area and centroid
centroid <- df_coord %>% st_convex_hull() %>% st_centroid() %>% unlist # get centroid

# 112.92465  29.35709

# import our data from the Excel spreadsheet
colNamesCN <- read_excel('./Dongting_2011_2015.xlsx', sheet=1, range=cell_rows(1:2), col_names=T, na='') %>%
  colnames()
# "生态站代码", "年", "月", "日", "样地代码", "样地名称", "样地类别", "样方号", "样方面积（m×m）", "植物种名"
# "拉丁名", "株（丛）数（株或丛/样方）", "叶层平均高度（cm）", "生殖枝平均高度（cm）", "盖度（%）", "物候期", "备注"
colNames <- c('Location', 'Year', 'Month', 'Day', 'Site', 'SiteDescription', 'SiteType', 'Qurat', 'Q_Area', 'ChineseName',
              'Taxon', 'Abundance', 'Biomass', 'BranchHeight', 'Coverage', 'Phenology', 'Notes')


dt <- read_excel('./Dongting_2011_2015.xlsx', sheet=1, range=cell_rows(2:1238), col_names=F, na='') %>%
  setNames(colNames) %>%
  select(c(Year, Month, Day, Site, Taxon, Abundance, Biomass)) %>%
  mutate_all(stringi::stri_trans_general, "latin-ascii") %>%
  mutate(Taxon = str_trim(Taxon)) %>%
  mutate(Taxon = str_replace_all(Taxon, fixed("."), " ")) %>%
  mutate(Taxon = str_replace_all(Taxon, fixed("  "), " ")) %>%
  mutate(Genus = word(Taxon, 1)) %>%
  mutate(Species = word(Taxon, 2)) %>%
  mutate(Taxon = paste(Genus, Species))


## structure check

dim(dt) # check dimensions, returns row and column counts
summary(dt)

## data type check

# Abundance or biomass: numeric
# Coordinates: numeric
# Dates: POSIXct or 
# Year, month, day columns,: integers or factors
# Plot: factors or integers
# DepthElevation: numeric or factors (if they're a treatment category)
# Taxonomic: characters or factors

# are there 2 or more unique years in the dataset?
n_distinct(dt$Year) >= 2
# n_distinct is identical to length(unique(x)) in base R, which counts the unique values

dim(dt) # check dimensions, returns row and column counts
summary(dt)

# Abundance and/or biomass, latitude and longitude numeric?
# Deal with coordinates later
is.numeric(dt$Abundance) # FALSE
is.numeric(dt$Biomass) # FALSE

# Year, month and day must be integers or factors
# | means or
is.factor(dt$Year) | is.integer(dt$Year) # FALSE
is.factor(dt$Month) | is.integer(dt$Month) # FALSE
is.factor(dt$Day) | is.integer(dt$Day) # FALSE

# Secondary fields such as trawl, plot, transect etc must be factors or integers? NA
# Secondary fields such as elevation or depth will normally be numeric unless in a treatment format. NA here
# Date should be POSIXct (not applicable in this case) NA, just year

# Taxonomic fields must be characters or factors?
is.factor(dt$Taxon) | is.character(dt$Taxon) # TRUE
is.factor(dt$Site) | is.character(dt$Site) # TRUE

# fixes
# The Abundance column to numeric
dt$Abundance <- as.numeric(dt$Abundance)
dt$Biomass <- as.numeric(dt$Biomass)
# The year column here is numeric so convert it to factor
dt$Year <- as.factor(dt$Year)
dt$Month <- as.factor(dt$Month)
dt$Day <- as.factor(dt$Day)
# and characters for the taxonomic fields

## fields
# Abundance
min(dt$Abundance) > 0 # no zeroes? 
sum(is.na(dt$Abundance)) # 5 NAs
sum(dt$Abundance=="") > 0 # no blanks

# Biomass
min(dt$Abundance) > 0 # no zeroes?
sum(is.na(dt$Biomass)) # 107 NAs
sum(dt$Abundance=="") > 0 # no blanks

# Year < 2021, month < 12, day < 31
summary(dt[,1])
summary(dt[,2])
summary(dt[,3])

# if there are rows that need to be removed
dt <- dt[!is.na(dt$Abundance),]
# dt <- dt[!which(dt$Abundance == 0),]
# check again
min(dt$Abundance) > 0 # no zeroes?
sum(dt$Abundance=="") > 0 # no blanks

dt <- dt %>%
  mutate(Latitude = as.numeric(case_when(str_detect(Site, "DTMZH01ABC") ~ '29.4623',
                                         str_detect(Site, "DTMFZ01ABC") ~ '29.24475'))) %>%
  mutate(Longitude = as.numeric(case_when(str_detect(Site, "DTMZH01ABC") ~ '112.7921',
                                          str_detect(Site, "DTMFZ01ABC") ~ '113.06570')))
  

## map

world_map <- map_data('world') # check whether the GPS coordinates match expectations
world <- ggplot(world_map) + 
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  coord_fixed() + 
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())
points <- world + geom_point(data=dt %>% distinct(Latitude, Longitude, Year), 
                             aes(x=Longitude, y=Latitude, fill = Year), shape=21)

points + coord_fixed(xlim=c(100,120), ylim=c(20,40))

## nomenclature

# check that genera are genera, not family names (-idae/eae)
# this returns the record index number if there are any
str_which(dt$Taxon, 'idae$|eae$')
# check the species list for misspellings or non-BioTIME taxonomic convention names
# Do visual checks before splitting taxa up into the three columns.
sort(unique(dt$Taxon)) %>% word(., 2) %>% unique() # check species
# this keeps IDs adjacent to their same-genus misspellings, but only looking at the last few words to check

sort(unique(word(dt$Taxon, 1))) # check genera

replace <- c("Caerx" = "Carex", "Sp" = "sp")

dt <- dt %>%
  mutate(Taxon = str_replace_all(Taxon, replace)) %>% # use the replacement vector to replace typos
  mutate(Genus = word(Taxon, 1)) %>% # separate genus to its own column
  mutate(Species = word(Taxon, 2)) %>% # species to its own column. Only the second word to ignore subspecies
  mutate(Taxon = NULL)


### prepare and export
dt$Plot <- rep('', nrow(dt))
dt$DepthElevation <- rep('', nrow(dt))
dt$StudyID <- rep('', nrow(dt))
dt$Family <- rep('', nrow(dt))

# dt %>% distinct(Taxon, Genus, Species) %>% View()
# check visually, but do this in your console.


# aggregate abundance records that are same species, plot, and survey day.
dt_merged <- dt %>% group_by_at(vars(-Abundance)) %>% 

    summarise(Abundance=sum(Abundance)) %>% ungroup() %>% arrange(Year, Family, Genus, Species)

dim(dt)[1]-dim(dt_merged)[1] # any change in aggregating?

# save the dataset name as an object so we save some typing
dataset_name <- 'CERN_Dongting_Lake_Beach_plant_communities_2011-2015'
# put in as many non-blank fields unique to the sampling event
# If plot, depthelevation, month, and day are available, I'd add those in too
dt_merged$SampleDescription <- as.factor(with(dt_merged, paste(Site, Year, sep='_')))

length(levels(dt_merged$SampleDescription)) # how many sampling events? 


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
head(dt_merged) # final check!


## Export and spreadsheet prep

dir <- "../Curated/"
write.csv(dt_merged, paste0(dir, dataset_name, '_ZY_rawdata.csv'), row.names=F, na='') # replace your initials here
