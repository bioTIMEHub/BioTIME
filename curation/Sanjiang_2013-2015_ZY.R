require(dplyr)
require(ggplot2)
require(stringr)
require(readxl)
require(maps)
require(tidyr)

rm(list = ls())
# setwd("../")
# Dataset original file path: ./Sanjiang_2000_2015/三江平原沼泽生态系统植物群落2000年–2015年数据集 20191008.xlsx
# Rename it to ./Sanjiang_2000_2015/data/Sanjiang_2000_2015.xlsx
# To avoid read_excel reading Chinese error

setwd("./Sanjiang_2000_2015/data/")
# sampling methods
# 群落生物量
# 群落地上生物量由分种样方调查数据计算而来；群落地下根系生物量采用土坑法实测获取。
# 土坑尺寸 2005 年为 0.01 m2， 2015 年为 0.04 m2。
# 原始数据观测频率为：地上生物量， 1 次/年（生物量高峰期）；地下生物量， 1 次/10 年。
# 数据产品频率为： 1 次/1 年，选用生物量高峰期的实测数据。
# 三江平原沼泽生态系统植物群落数据集的观测样地为三江站常年积水区综合观测场与季节性积水区辅助观测场，
# 1 # 常年积水区综合观测场场地信息
# 样地名称 常年积水区综合观测场
# 样地代码 SJMZH01
# 地理位置 47°35′10″N 133°30′03″E 三江站试验场内
# 生态系统类型 毛薹草沼泽
# 面积 8 hm2

# 2 # 季节性积水区辅助观测场场地信息
# 样地名称 季节性积水区辅助观测场
# 样地代码 SJMFZ01
# 地理位置 47°35′04″N 133°30′09″E 三江站试验场内
# 生态系统类型 狭叶甜茅-小叶章沼泽
# 面积 4.5 hm


angle2dec <- function(angle) {
  angle <- as.character(angle)
  x <- do.call(rbind, strsplit(angle, split=' '))
  x <- apply(x, 1L, function(y) {
    y <- as.numeric(y)
    y[1] + y[2]/60 + y[3]/3600
  })
  return(x)
}

lat_zh <- angle2dec("47 35 10") %>% as.numeric()
long_zh <- angle2dec("133 30 03") %>% as.numeric()

lat_fz <- angle2dec("47 35 04") %>% as.numeric()
long_fz <- angle2dec("133 30 09") %>% as.numeric()


# import our data from the Excel spreadsheet
colNamesCN <- read_excel('./Sanjiang_2000_2015.xlsx', sheet=3, range=cell_rows(1:2), col_names=T, na='') %>%
  colnames()
#  "年", "月", "样地代码", "样方数", "样方面积（m×m）", "植物种名", "株丛数（株丛/样方）", "叶层平均高度（cm）"
# "地上绿色部分总干重（g/样方）", "备注"
colNames <- c('Year', 'Month', 'Site', 'Quard_n', 'Q_Area', 'ChineseName', 'Abundance', 'Height', 'Biomass', 'Notes')

dt_zh <- read_excel('./Sanjiang_2000_2015.xlsx', sheet=3, range=cell_rows(2:198), col_names=F, na='') %>%
  setNames(colNames) %>%
  filter(Quard_n == 10 & Q_Area == "1×1") %>%
  select(c(Site, Year, Month, ChineseName, Abundance, Height))

dt_fz <- read_excel('./Sanjiang_2000_2015.xlsx', sheet=4, range=cell_rows(2:168), col_names=F, na='') %>%
  setNames(colNames) %>%
  filter(Quard_n == 10 & Q_Area == "1×1") %>%
  select(c(Site, Year, Month, ChineseName, Abundance, Height))

dt <- rbind(dt_zh, dt_fz) %>%
  rename(Biomass = Height)

colNamesListCN <- read_excel('./Sanjiang_2000_2015.xlsx', sheet=5, range=cell_rows(1:2), col_names=T, na='') %>%
  colnames()
# "拉丁科名", "科名", "拉丁名", "中文名"
colNamesListEN <- c("Family", "FamilyCN", "Taxon", "ChineseName")

cnNames <- read_excel('./Sanjiang_2000_2015.xlsx', sheet=5, range=cell_rows(2:45), col_names=F, na='')%>%
  setNames(colNamesListEN) %>%
  mutate_all(stringi::stri_trans_general, "latin-ascii") %>%
  mutate(Family = str_trim(Family, "right")) %>%
  mutate(Genus = word(Taxon, 1)) %>%
  mutate(Species = word(Taxon, 2)) %>%
  select(-c(FamilyCN))

df <- left_join(dt, cnNames)

df$Family[df$ChineseName == "蒿属一种"] <- "Asteraceae"
df$Genus[df$ChineseName == "蒿属一种"] <- "Artemisia"
df$Species[df$ChineseName == "蒿属一种"] <- "sp."
df$Family[df$ChineseName == "薹草一种"] <- "Cyperaceae"
df$Genus[df$ChineseName == "薹草一种"] <- "Carex"
df$Species[df$ChineseName == "薹草一种"] <- "sp."

df$Family[df$ChineseName == "大穗苔草"] <- "Cyperaceae"
df$Genus[df$ChineseName == "大穗苔草"] <- "Carex"
df$Species[df$ChineseName == "大穗苔草"] <- "rhynchophysa"

df$Taxon <- paste(df$Genus, df$Species)
## check year
n_distinct(df$Year) >= 2


## structure check
dim(df) # check dimensions, returns row and column counts
summary(df)


## data type check

# Abundance or biomass: numeric
# Coordinates: numeric
# Dates: POSIXct or 
# Year, month, day columns,: integers or factors
# Plot: factors or integers
# DepthElevation: numeric or factors (if they're a treatment category)
# Taxonomic: characters or factors

# Abundance and/or biomass, here: Height as Biomass
is.numeric(df$Biomass) # TRUE
is.numeric(df$Abundance) # TRUE


# Year, month and day must be integers or factors
# | means or
is.factor(df$Year) | is.integer(df$Year) # FALSE
is.factor(df$Month) | is.integer(df$Month) # FALSE


# Secondary fields such as trawl, plot, transect etc must be factors or integers? NA
# Secondary fields such as elevation or depth will normally be numeric unless in a treatment format. NA here
# Date should be POSIXct (not applicable in this case) NA, just year

# Taxonomic fields must be characters or factors?
is.factor(df$Taxon) | is.character(df$Taxon)
is.factor(df$Family) | is.character(df$Family)
# TRUE


## fixes
df$Year <- as.factor(df$Year)
df$Month <- as.factor(df$Month)


## remove unnecessary cols

## fields
# Abundance/Density
min(df$Biomass) > 0 # no zeroes?
sum(df$Biomass=="") > 0 # no blanks

# Year < 2021, month < 12, day < 31
summary(df[,2])
summary(df[,3])

## pool abundance

##
df$Latitude <- c(rep(lat_zh, nrow(df)))
df$Longitude <- c(rep(long_zh, nrow(df)))

## map

world_map <- map_data('world') # check whether the GPS coordinates match expectations
world <- ggplot(world_map) + 
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  coord_fixed() + 
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())
points <- world + geom_point(data=df %>% distinct(Latitude, Longitude, Year), 
                             aes(x=Longitude, y=Latitude, fill = Year), shape=21)

points + coord_fixed(xlim=c(120,140), ylim=c(35,55))


## nomenclature

# check that genera are genera, not family names (-idae/eae)
# this returns the record index number if there are any
str_which(df$Taxon, 'idae$|eae$')
# check the species list for misspellings or non-BioTIME taxonomic convention names
# Do visual checks before splitting taxa up into the three columns.
sort(unique(df$Taxon)) %>% word(., start=2, end=-1) %>% unique()
# this keeps IDs adjacent to their same-genus misspellings, but only looking at the last few words to check

sort(unique(word(df$Taxon, 1))) # check genera

## taxa fix
replace_s <- c("pseudo-curaica" = "pseudocuraica")

df <- df %>%
  mutate(Taxon = str_replace_all(Taxon, replace_s)) %>% # use the replacement vector to replace typos
  mutate(Genus = word(Taxon, 1)) %>% # separate genus to its own column
  mutate(Species = word(Taxon, 2)) %>% # species to its own column. Only the second word to ignore subspecies
  mutate(Taxon = NULL)


### prepare and export
df$Plot <- rep('', nrow(df))
df$DepthElevation <- rep('', nrow(df))
df$Day <- rep('', nrow(df))
df$Month <- rep('', nrow(df))
df$StudyID <- rep('', nrow(df))

# aggregate abundance records that are same species, plot, and survey day.

df_merged <- df %>% group_by_at(vars(-Biomass)) %>% 
  summarise(Biomass=sum(Biomass)) %>% ungroup() %>% arrange(Year, Family, Genus, Species)

dim(df)[1]-dim(df_merged)[1] # any change in aggregating?

## save dataset
# save the dataset name as an object so we save some typing
dataset_name <- 'CERN_Sanjiang_Plain_plant_community_marsh_2013-2015'
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


## Export and spreadsheet prep

dir <- "../Curated/"
write.csv(df_merged, paste0(dir, dataset_name, '_ZY_rawdata.csv'), row.names=F, na='') # replace your initials here
