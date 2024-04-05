require(dplyr)
require(ggplot2)
require(stringr)
require(readxl)
require(maps)

rm(list = ls())
## setwd("../")
# Dataset original file path: ./Gansu_Linze/实体数据/LZD_SWSJ_荒漠植物群落植被空间分布格局.xls
# Rename it to ./Gansu_Linze/data/LZD_SWSJ_Sampling.xls
# To avoid read_excel reading Chinese error

setwd("./Gansu_Linze/data/")

# sampling methods
# 生物采样: 每次从6 个样方中取得6 份样品作为6 次重复, 样方编码分别为 A01、 A02、 A03、A04、 A05、 A06。

# import our data from the Excel spreadsheet
dt_zh <- read_excel('./LZD_SWSJ_Sampling.xls', sheet=1, range=cell_rows(3:21), col_names=T, na='')

colnames(dt_zh)
# "年"             "植物种名"       "拉丁名"         "高度（m）"      "密度（株/㎡ ）"

# 综合观测场荒漠植物群落空间分布格局变化  LZDZH02 观测场面积100m×100m, 正方形。 
# 观测场外围50m 为破坏性采样地。 中心点坐标: 100°07'09″E, 39°24'52″N
name_zh <- read_excel('./LZD_SWSJ_Sampling.xls', sheet=1, range=cell_rows(1), col_names=F, na='') %>%
  as.character()

angle2dec <- function(angle) {
  angle <- as.character(angle)
  x <- do.call(rbind, strsplit(angle, split=' '))
  x <- apply(x, 1L, function(y) {
    y <- as.numeric(y)
    y[1] + y[2]/60 + y[3]/3600
  })
  return(x)
}

lat_zh <- angle2dec("39 24 52") %>% as.numeric()
long_zh <- angle2dec("100 07 09") %>% as.numeric()


## rename column names
# Height (m), Abundance (ind/㎡)
colnames(dt_zh) <- c('Year', 'ChineseName', 'Taxon', 'Height', 'Abundance')


## check year
n_distinct(dt_zh$Year) >= 2


## add Site column

dt_zh$Site <- rep('LZDZH02ABC_01', nrow(dt_zh))

## structure check

dim(dt_zh) # check dimensions, returns row and column counts
summary(dt_zh)


## data type check

# Abundance or biomass: numeric
# Coordinates: numeric
# Dates: POSIXct or 
# Year, month, day columns,: integers or factors
# Plot: factors or integers
# DepthElevation: numeric or factors (if they're a treatment category)
# Taxonomic: characters or factors

# Abundance and/or biomass
is.numeric(dt_zh$Abundance) 

# Year, month and day must be integers or factors
# | means or
is.factor(dt_zh$Year) | is.integer(dt_zh$Year)

# Secondary fields such as trawl, plot, transect etc must be factors or integers? NA
# Secondary fields such as elevation or depth will normally be numeric unless in a treatment format. NA here
# Date should be POSIXct (not applicable in this case) NA, just year

# Taxonomic fields must be characters or factors?
is.factor(dt_zh$Taxon) | is.character(dt_zh$Taxon)

## fixes
dt_zh$Year <- as.factor(dt_zh$Year)


## remove unnecessary cols

# ChineseName isn't needed
dt_zh$ChineseName <- NULL
# 
dt_zh <- rename(dt_zh, Biomass = Height)

## fields
# Abundance/Density
min(dt_zh$Abundance) > 0 # no zeroes?
sum(dt_zh$Abundance=="") > 0 # no blanks

# Year < 2021, month < 12, day < 31
summary(dt_zh[,1])

## pool abundance
dt_zh %>% group_by(Year, Taxon) %>% 
  summarise(Abundance=sum(Abundance)) %>% ungroup()

dt_zh$Latitude <- rep(lat_zh, nrow(dt_zh))
dt_zh$Longitude <- rep(long_zh, nrow(dt_zh))


## map

world_map <- map_data('world') # check whether the GPS coordinates match expectations
world <- ggplot(world_map) + 
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  coord_fixed() + 
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())
points <- world + geom_point(data=dt_zh %>% distinct(Latitude, Longitude, Site), 
                             aes(x=Longitude, y=Latitude, fill = Site), shape=21)

points + coord_fixed(xlim=c(85,110), ylim=c(30,50))


## nomenclature

# check that genera are genera, not family names (-idae/eae)
# this returns the record index number if there are any
str_which(dt_zh$Taxon, 'idae$|eae$')
# check the species list for misspellings or non-BioTIME taxonomic convention names
# Do visual checks before splitting taxa up into the three columns.
sort(unique(dt_zh$Taxon)) %>% word(., start=2, end=-1)
# this keeps IDs adjacent to their same-genus misspellings, but only looking at the last few words to check

sort(unique(word(dt_zh$Taxon, 1))) # check genera
# sort(unique(dt$Family)) # check family

## tax fix replace

replace_g <- c("Suaede" = "Suaeda")

dt_zh <- dt_zh %>%
  mutate(Taxon = str_replace_all(Taxon, replace_g)) %>%
  mutate(Genus = word(Taxon, 1)) %>%
  mutate(Species = word(Taxon, 2)) %>%
  mutate(Taxon = NULL)

### prepare and export
dt_zh$Family <- rep('', nrow(dt_zh))
dt_zh$Plot <- rep('', nrow(dt_zh))
dt_zh$DepthElevation <- rep('', nrow(dt_zh))
dt_zh$Day <- rep('', nrow(dt_zh))
dt_zh$Month <- rep('', nrow(dt_zh))
dt_zh$StudyID <- rep('', nrow(dt_zh))


# aggregate abundance records that are same species, plot, and survey day.
df_merged <- dt_zh %>% group_by_at(vars(-Abundance)) %>% 
  summarise(Abundance=sum(Abundance)) %>% ungroup() %>% arrange(Year, Family, Genus, Species)

dim(dt_zh)[1]-dim(df_merged)[1] # any change in aggregating?

## save dataset
# save the dataset name as an object so we save some typing
dataset_name <- 'CERN_Gansu_Linze_desert_plant_communities_LZDZH02ABC_01_2005-2008'
# put in as many non-blank fields unique to the sampling event
# If plot, depthelevation, month, and day are available, I'd add those in too
df_merged$SampleDescription <- as.factor(with(df_merged, paste(Site, Year, sep='_')))
length(levels(df_merged$SampleDescription)) # how many sampling events?


df_merged <- df_merged[c('Abundance',
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
head(df_merged) # final check!


## export
dir <- "../Curated/"
write.csv(df_merged, paste0(dir, dataset_name, '_ZY_rawdata.csv'), row.names=F, na='') # replace your initials here
