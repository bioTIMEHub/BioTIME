require(dplyr)
require(ggplot2)
require(stringr)
require(readxl)
require(maps)
require(tidyr)

rm(list = ls())
# setwd("../")
# Dataset original file path: ./HulunBuir_2009_2015/2009-2015呼伦贝尔贝加尔针茅草甸草原植物群落组成数据集（完善后上传版）.xlsx
# Rename it to ./HulunBuir_2009_2015/data/HulunBuir_2009_2015_Grass.xlsx
# To avoid read_excel reading Chinese error

setwd("./HulunBuir_2009_2015/data/")
# sampling methods
# 在长期定位观测样地随机选取 10 个样方面积为 1 m× 1 m 的重复，调查和记录每个样方内的所有植物种。
# 其中：植物高度采用直尺测定，针对每一物种随机选取 5 株植物测量高度，计算其平均值；
# 生物量采用样方法，分种齐地剪割，用 65℃烘干至恒重，并用电子天平称重。观测时间为生长季 7 月下旬至 8 月上旬
# 地理范围为 49°20'52"– 49°21'16"N， 120°6'50"– 120°7'28"E；

angle2dec <- function(angle) {
  angle <- as.character(angle)
  x <- do.call(rbind, strsplit(angle, split=' '))
  x <- apply(x, 1L, function(y) {
    y <- as.numeric(y)
    y[1] + y[2]/60 + y[3]/3600
  })
  return(x)
}

lat_1 <- angle2dec("49 20 52") %>% as.numeric()
lat_2 <- angle2dec("49 21 16") %>% as.numeric()
long_1 <- angle2dec("120 6 50") %>% as.numeric()
long_2 <- angle2dec("120 7 28") %>% as.numeric()


### area coordinate

# load libraries
library(sf)
library(clipr)

# 1. Convert data points into point spatial object
# assumes your rawdata dataframe is called rawdata
df_coord <- tibble(Longitude = c(long_1, long_2, long_2, long_1), Latitude = c(lat_1, lat_1, lat_2, lat_2)) %>%
  st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()

# 2. Calculate convex hull, area and centroid
centroid <- df_coord %>% st_convex_hull() %>% st_centroid() %>% unlist # get centroid

# 120.11917  49.35111

# calculate area
st_area(df_coord %>% st_convex_hull()) * 0.000001
# 0.5667917 sq km

# import our data from the Excel spreadsheet
colNamesCN <- read_excel('data/HulunBuir_2009_2015_Grass.xlsx', sheet=1, range=cell_rows(1:2), col_names=T, na='') %>%
  colnames()
# "草地类型"              "样方号"                "取样日期"              "植物名称"              "拉丁名"               
# "自然高度(cm)"          "多度（株.m-2）"        "地上绿色现存量(g.m-2)"
colNames <- c('Site', 'Sample', 'Date', 'TaxonCN', 'Taxon', 'Height', 'Abundance', 'Biomass')


dt <- read_excel('data/HulunBuir_2009_2015_Grass.xlsx', sheet=1, range=cell_rows(2:1153), col_names=F, na='') %>%
  setNames(colNames) %>%
  mutate(Date = str_replace_all(Date, c("40400" = "2010-8-10", "40767" = "2011-8-12"))) %>%
  mutate(Year = format(as.Date(Date), '%Y')) %>%
  mutate(Month = format(as.Date(Date), '%m')) %>%
  mutate(Day = format(as.Date(Date), '%d')) %>%
  select(-c(Site, Date, TaxonCN, Biomass)) %>%
  mutate(Taxon = str_replace_all(Taxon, c("Scorzoneradivaricata Turcz." = "Scorzonera divaricata"))) %>%
  mutate(Taxon = word(Taxon, 1, 2)) %>%
  relocate(Year, Month, Day) %>%
  rename(Biomass = Height)

# are there 2 or more unique years in the dataset?
n_distinct(dt$Year) >= 2
# n_distinct is identical to length(unique(x)) in base R, which counts the unique values

dim(dt) # check dimensions, returns row and column counts
summary(dt)

# Abundance and/or biomass, latitude and longitude numeric?
# Deal with coordinates later
is.numeric(dt$Abundance) # T
is.numeric(dt$Biomass) # T

# Year, month and day must be integers or factors
# | means or
is.factor(dt$Year) | is.integer(dt$Year) # F
is.factor(dt$Month) | is.integer(dt$Month) # F
is.factor(dt$Day) | is.integer(dt$Day) # F


# Secondary fields such as trawl, plot, transect etc must be factors or integers? NA
# Secondary fields such as elevation or depth will normally be numeric unless in a treatment format. NA here
# Date should be POSIXct (not applicable in this case) NA, just year

# Taxonomic fields must be characters or factors?
is.factor(dt$Taxon) | is.character(dt$Taxon) # T
is.factor(dt$Sample) | is.character(dt$Sample) # F


# fixes
# The year column here is numeric so convert it to factor
dt$Year <- as.factor(dt$Year) 
dt$Month <- as.factor(dt$Month)
dt$Day <- as.factor(dt$Day)
# and characters for the taxonomic fields
dt$Sample <- as.factor(dt$Sample)


## fields
# Abundance
min(dt$Abundance) > 0 # no zeroes?
sum(dt$Abundance=="") > 0 # no blanks

# Year < 2021, month < 12, day < 31
summary(dt[,1])
summary(dt[,2])
summary(dt[,3])

## pool abundance
dt %>% group_by(Year, Taxon) %>% 
  summarise(Abundance=sum(Abundance)) %>% ungroup()

dt$Latitude <- rep(49.35111, nrow(dt))
dt$Longitude <- rep(120.11917, nrow(dt))


## map

world_map <- map_data('world') # check whether the GPS coordinates match expectations
world <- ggplot(world_map) + 
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  coord_fixed() + 
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())
points <- world + geom_point(data=dt %>% distinct(Latitude, Longitude, Year), 
                             aes(x=Longitude, y=Latitude, fill = Year), shape=21)

points + coord_fixed(xlim=c(110,130), ylim=c(40,55))


## nomenclature

# check that genera are genera, not family names (-idae/eae)
# this returns the record index number if there are any
str_which(dt$Taxon, 'idae$|eae$')
# check the species list for misspellings or non-BioTIME taxonomic convention names
# Do visual checks before splitting taxa up into the three columns.
sort(unique(dt$Taxon)) %>% word(., start=2, end=-1) %>% unique()
# this keeps IDs adjacent to their same-genus misspellings, but only looking at the last few words to check

sort(unique(word(dt$Taxon, 1))) # check genera

# misspelling
# "lanceolala" = "lanceolata"

## taxa fix
replace_s <- c("lanceolala" = "lanceolata")

dt <- dt %>%
  mutate(Taxon = str_replace_all(Taxon, replace_s)) %>% # use the replacement vector to replace typos
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
dataset_name <- 'CERN_HulunBuir_Stipa_Baicalensis_meadow_steppe_2009-2015'
# put in as many non-blank fields unique to the sampling event
# If plot, depthelevation, month, and day are available, I'd add those in too
dt_merged$SampleDescription <- as.factor(with(dt_merged, paste(Year, Sample, sep='_')))
length(levels(dt_merged$SampleDescription)) # how many sampling events? 70


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
                         'StudyID')] %>% arrange(Year, SampleDescription, Family, Genus, Species)
head(dt_merged) # final check!


## export
dir <- "../Curated/"
write.csv(dt_merged, paste0(dir, dataset_name, '_ZY_rawdata.csv'), row.names=F, na='') # replace your initials here

