require(dplyr)
require(ggplot2)
require(stringr)
require(readxl)
require(maps)
require(tidyr)

rm(list = ls())
# setwd("../")
# Dataset original file path: ./Shaanxi_Qinling/实体数据/2011-2015年洞庭湖洲滩植物群落长期监测数据集V1.xlsx
# Rename it to ./Shaanxi_Qinling/data/1-QLF_Tree.xlsx
#              ./Shaanxi_Qinling/data/2-QLF_Shrub.xlsx
#              ./Shaanxi_Qinling/data/3-QLF_Grass.xlsx
# To avoid read_excel reading Chinese error

setwd("./Shaanxi_Qinling/data/")
# sampling method
# 油松林综合观测场 QLFZH01
# 油松林综合观测场树干径流采样地 (QLFZH01CHG _01 ) 样地面积为20m×20m, 属于永久样地, 
# 中心点坐标, 东经: 108°26'51″, 北纬: 33°26'10″。

angle2dec <- function(angle) {
  angle <- as.character(angle)
  x <- do.call(rbind, strsplit(angle, split=' '))
  x <- apply(x, 1L, function(y) {
    y <- as.numeric(y)
    y[1] + y[2]/60 + y[3]/3600
  })
  return(x)
}

lat <- angle2dec("33 26 10") %>% as.numeric()
long <- angle2dec("108 26 51") %>% as.numeric()


colNamesCN <- read_excel('./1-QLF_Tree.xlsx', sheet=1, range=cell_rows(2:3), col_names=T, na='') %>%
  colnames()
# "年"              "植物种名"        "株数（株/样地）" "平均胸径"        "平均高度（m）"            "树干"            "树枝"           
#  "树叶"            "树皮"            "地上部"          "地下部"
colNamesT <- c("Year", "ChineseName", "Abundance", "DBH", "Height", "TrunkDW", "BranchDW", "LeafDW", "BarkDW", "AboveDW", "BelowDW")

dt_tree <- read_excel('./1-QLF_Tree.xlsx', sheet=1, range=cell_rows(4:24), col_names=F, na='') %>%
  setNames(colNamesT) %>%
  select(c(Year, ChineseName, Abundance, Height))


colNamesCN <- read_excel('./2-QLF_Shrub.xlsx', sheet=1, range=cell_rows(2:3), col_names=T, na='') %>%
  colnames()
# "年"                        "植物种名"                  "株（丛）数（株或丛/样地）" "平均高度（m）"             "枝干干重"                 
# "叶干重"                    "地上部总干重"              "地下部总干重"  
colNamesS <- c("Year", "ChineseName", "Abundance", "Height", "TrunkDW", "LeafDW", "AboveDW", "BelowDW")

dt_shrub <- read_excel('./2-QLF_Shrub.xlsx', sheet=1, range=cell_rows(3:20), col_names=F, na='') %>%
  setNames(colNamesS) %>%
  select(c(Year, ChineseName, Abundance, Height))


colNamesCN <- read_excel('./3-QLF_Grass.xlsx', sheet=1, range=cell_rows(2:3), col_names=T, na='') %>%
  colnames()
# "年"                        "植物种名"                  "株（丛）数（株或丛/样地）" "平均高度（cm）"   
colNamesG <- c("Year", "ChineseName", "Abundance", "Height")

dt_grass <- read_excel('./3-QLF_Grass.xlsx', sheet=1, range=cell_rows(3:28), col_names=F, na='') %>%
  setNames(colNamesG) %>%
  select(c(Year, ChineseName, Abundance, Height)) %>%
  mutate(Height = Height * 0.01)

dt <- rbind(dt_tree, dt_shrub, dt_grass) %>%
  setNames(c("Year", "ChineseName", "Abundance", "Biomass"))

unique(dt$ChineseName)

dt$Taxon <- rep("", nrow(dt))

dt$Taxon[dt$ChineseName == "四照花"] <- "Cornus kousa"
dt$Taxon[dt$ChineseName == "鹅耳枥"] <- "Carpinus turczaninowii"
dt$Taxon[dt$ChineseName == "枫杨"] <- "Pterocarya stenoptera"
dt$Taxon[dt$ChineseName == "苦木"] <- "Picrasma quassioides"
dt$Taxon[dt$ChineseName == "红桦"] <- "Betula albosinensis"
dt$Taxon[dt$ChineseName == "华北落叶松"] <- "Larix gmelinii"
dt$Taxon[dt$ChineseName == "华山松"] <- "Pinus armandii"
dt$Taxon[dt$ChineseName == "五角枫"] <- "Acer truncatum"
dt$Taxon[dt$ChineseName == "漆树"] <- "Toxicodendron vernicifluum"
dt$Taxon[dt$ChineseName == "青榨槭"] <- "Acer davidii"
dt$Taxon[dt$ChineseName == "锐齿栎"] <- "Quercus aliena"
dt$Taxon[dt$ChineseName == "野樱桃"] <- "Prunus szechuanica"
dt$Taxon[dt$ChineseName == "油松"] <- "Pinus tabuliformis"
dt$Taxon[dt$ChineseName == "湖北花楸"] <- "Sorbus hupehensis"
dt$Taxon[dt$ChineseName == "栓翅卫矛"] <- "Euonymus phellomanus"
dt$Taxon[dt$ChineseName == "刚毛忍冬"] <- "Lonicera hispida"
dt$Taxon[dt$ChineseName == "托叶樱桃"] <- "Prunus stipulacea"
dt$Taxon[dt$ChineseName == "白檀"] <- "Santalum album"
dt$Taxon[dt$ChineseName == "披针胡颓子"] <- "Elaeagnus lanceolata"
dt$Taxon[dt$ChineseName == "草瑞香"] <- "Diarthron linifolium"
dt$Taxon[dt$ChineseName == "粗榧"] <- "Cephalotaxus sinensis"
dt$Taxon[dt$ChineseName == "虎榛子"] <- "Ostryopsis davidiana"
dt$Taxon[dt$ChineseName == "桦叶荚蒾"] <- "Viburnum betulifolium"
dt$Taxon[dt$ChineseName == "鸡屎藤"] <- "Paederia foetida"
dt$Taxon[dt$ChineseName == "木姜子"] <- "Litsea sp."
dt$Taxon[dt$ChineseName == "木蓝"] <- "Indigofera tinctoria"
dt$Taxon[dt$ChineseName == "南蛇藤"] <- "Celastrus orbiculatus"
dt$Taxon[dt$ChineseName == "三叶木通"] <- "Akebia trifoliata"
dt$Taxon[dt$ChineseName == "托柄菝葜"] <- "Smilax discotis"
dt$Taxon[dt$ChineseName == "五味子"] <- "Schisandra chinensis"
dt$Taxon[dt$ChineseName == "白花堇菜"] <- "Viola lactiflora"
dt$Taxon[dt$ChineseName == "费菜"] <- "Sedum aizoon" # ?
dt$Taxon[dt$ChineseName == "过路黄"] <- "Lysimachia christiniae"
dt$Taxon[dt$ChineseName == "和尚菜"] <- "Adenocaulon himalaicum"
dt$Taxon[dt$ChineseName == "黄花油点菜"] <- NA
dt$Taxon[dt$ChineseName == "尖叶香青"] <- "Anaphalis acutifolia"
dt$Taxon[dt$ChineseName == "金粟兰"] <- "Chloranthus spicatus"
dt$Taxon[dt$ChineseName == "荩草"] <- "Arthraxon hispidus"
dt$Taxon[dt$ChineseName == "华山鳞毛蕨"] <- "Dryopteris goeringiana"
dt$Taxon[dt$ChineseName == "茜草"] <- "Rubia cordifolia"
dt$Taxon[dt$ChineseName == "苔草"] <- "Carex spp."
dt$Taxon[dt$ChineseName == "野青茅"] <- "Deyeuxia arundinacea"
dt$Taxon[dt$ChineseName == "异叶泽兰"] <- "Eupatorium heterophyllum"
dt$Taxon[dt$ChineseName == "玉竹"] <- "Polygonatum odoratum"
dt$Taxon[dt$ChineseName == "变叶风毛菊"] <- "Saussurea mutabilis"
dt$Taxon[dt$ChineseName == "穿龙薯蓣"] <- "Dioscorea nipponica"
dt$Taxon[dt$ChineseName == "东亚唐松草"] <- "Thalictrum minus"
dt$Taxon[dt$ChineseName == "黄腺香青"] <- "Anaphalis aureopunctata"
dt$Taxon[dt$ChineseName == "茅莓"] <- "Rubus parvifolius"
dt$Taxon[dt$ChineseName == "青菅"] <- "Carex breviculmis"
dt$Taxon[dt$ChineseName == "野棉花"] <- "Eriocapitella vitifolia"
dt$Taxon[dt$ChineseName == "羽裂华蟹甲草"] <- "Sinacalia tangutica"

dt <- dt %>%
  drop_na(Taxon)

## check year
n_distinct(dt$Year) >= 2


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

# Abundance and/or biomass, here: density (?) calculation (?) times site area???
is.numeric(dt$Abundance) # TRUE
is.numeric(dt$Biomass) # TRUE

# Year, month and day must be integers or factors
# | means or
is.factor(dt$Year) | is.integer(dt$Year) # FALSE

# Secondary fields such as trawl, plot, transect etc must be factors or integers? NA
# Secondary fields such as elevation or depth will normally be numeric unless in a treatment format. NA here
# Date should be POSIXct (not applicable in this case) NA, just year

# Taxonomic fields must be characters or factors?
is.factor(dt$Taxon) | is.character(dt$Taxon) # TRUE


## fixes
dt$Year <- as.factor(dt$Year)
dt$ChineseName <- NULL

# Year < 2021, month < 12, day < 31
summary(dt[,1])

## pool abundance

##
dt$Latitude <- c(rep(lat, nrow(dt)))
dt$Longitude <- c(rep(long, nrow(dt)))

## map

world_map <- map_data('world') # check whether the GPS coordinates match expectations
world <- ggplot(world_map) + 
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  coord_fixed() + 
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())
points <- world + geom_point(data=dt %>% distinct(Latitude, Longitude, Year), 
                             aes(x=Longitude, y=Latitude, fill = Year), shape=21)

points + coord_fixed(xlim=c(95,125), ylim=c(20,40))


## nomenclature

# check that genera are genera, not family names (-idae/eae)
# this returns the record index number if there are any
str_which(dt$Taxon, 'idae$|eae$')
# check the species list for misspellings or non-BioTIME taxonomic convention names
# Do visual checks before splitting taxa up into the three columns.
sort(unique(dt$Taxon)) %>% word(., start=2, end=-1) %>% unique()
# this keeps IDs adjacent to their same-genus misspellings, but only looking at the last few words to check

sort(unique(word(dt$Taxon, 1))) # check genera

dt <- dt %>%
  mutate(Genus = word(Taxon, 1)) %>% # separate genus to its own column
  mutate(Species = word(Taxon, 2)) %>% # species to its own column. Only the second word to ignore subspecies
  mutate(Taxon = NULL) %>%
  mutate(Site = "QLFZH01")


### prepare and export
dt$Family <- rep('', nrow(dt))
dt$Plot <- rep('', nrow(dt))
dt$DepthElevation <- rep('', nrow(dt))
dt$Day <- rep('', nrow(dt))
dt$Month <- rep('', nrow(dt))
dt$StudyID <- rep('', nrow(dt))


# aggregate abundance records that are same species, plot, and survey day.

dt_merged <- dt %>% group_by_at(vars(-Abundance)) %>% 
  summarise(Abundance=sum(Abundance)) %>% ungroup() %>% arrange(Year, Family, Genus, Species)

dim(dt)[1]-dim(dt_merged)[1] # any change in aggregating?

## save dataset
# save the dataset name as an object so we save some typing
dataset_name <- 'CERN_Shaanxi_Qinling_Forest_Ecological_Station_tree_shrub_2007-2008'
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

