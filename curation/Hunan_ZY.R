require(dplyr)
require(ggplot2)
require(stringr)
require(readxl)
require(maps)
require(tidyr)

rm(list = ls())
# setwd("../")
# Dataset original file path: ./Hunan_Huitong_fir/实体数据/HNHTF_会同杉木林生物量数据_杉木人工林活地被物样地调查表.xlsx
# Dataset original file path: ./Hunan_Huitong_fir/实体数据/HNHTF_会同杉木林养分数据_湖南省会同杉木林地植物名录.xlsx
# Rename it to ./Hunan_Huitong_fir/data/sampling.xlsx
# Rename it to ./Hunan_Huitong_fir/data/vegetation_list.xlsx
# To avoid read_excel reading Chinese error

setwd("./Hunan_Huitong_fir/data/")
# sampling methods

# 生物采样方法说明: 设置固定标准地, 测定标准地的海拔、 坡向、 坡度和坡位; 
# 每年调查一次树木的名称、 胸径、 树高, 选取标准木进行树干解析, 调查林分的生物量。


# 会同亚热带杉木人工林综合观测场永久样地 (2 区) (HGFZH01ABC_01)

# 会同亚热带杉木人工林综合观测场永久样地 (3 区) (HGFZH01ABC_02)

# 会同亚热带杉木人工林综合观测场永久样地 (7 区) (HGFZH01ABC_03)

angle2dec <- function(angle) {
  angle <- as.character(angle)
  x <- do.call(rbind, strsplit(angle, split=' '))
  x <- apply(x, 1L, function(y) {
    y <- as.numeric(y)
    y[1] + y[2]/60 + y[3]/3600
  })
  return(x)
}

lat <- angle2dec("26 50 00") %>% as.numeric()
long <- angle2dec("109 45 00") %>% as.numeric()

## 

# import our data from the Excel spreadsheet
# remove 1992 inconsistent sampling plot


colNamesCN <- read_excel('./sampling.xlsx', sheet=1, range=cell_rows(2:3), col_names=T, na='') %>%
  colnames()
# "地点"       "样方"       "面积（m2）" "种名"       "鲜重（g）"  "备注"   
colNames <- c('Location', 'Sample', 'Area', 'ChineseName', 'Biomass', 'Note')


dt <- read_excel('./sampling.xlsx', sheet=1, range=cell_rows(72:393), col_names=F, na='') %>%
  setNames(colNames) %>%
  mutate_all(stringi::stri_trans_general, "latin-ascii") %>%
  tidyr::fill(., c(Location, Sample, Note)) %>%
  mutate(Year = str_extract(Note, "(\\d)+")) %>%
  mutate(Site = str_extract(Note, "[:upper:]+")) %>%
  mutate(Site = str_replace_all(Site, c("III" = "HGFZH01ABC_02", 
                                        "VII" = "HGFZH01ABC_03", 
                                        "II" = "HGFZH01ABC_01"))) %>%
  mutate(Location = NULL) %>%
  mutate(Area = NULL) %>%
  mutate(Note = NULL)


dt$Sample[dt$Sample == "山洼"] <- "Valley"
dt$Sample[dt$Sample == "山麓"] <- "Foothills"
dt$Sample[dt$Sample == "山坡"] <- "Montane"

cnNames <- read_excel('./vegetation_list.xlsx', sheet=1, col_names=F, na='') %>%
  setNames("Raw") %>%
  mutate_all(stringi::stri_trans_general, "latin-ascii") %>% 
  mutate(CladeCN = case_when(str_detect(Raw, ',') ~ Raw)) %>%
  mutate(Clade = word(CladeCN, 2)) %>%
  tidyr::fill(Clade) %>%
  mutate(FamilyCN = case_when(str_detect(Raw, '^(\\d)') ~ Raw)) %>%
  mutate(Family = str_extract(FamilyCN, "[a-zA-z]+")) %>%
  mutate(Family = str_trim(Family)) %>%
  tidyr::fill(Family) %>%
  mutate(TaxonCN = case_when(!str_detect(Raw, '^(\\d)') & !str_detect(Raw, ',') ~ Raw)) %>%
  mutate(ChineseName = str_extract(TaxonCN, "[^a-zA-Z\\(\\s]+")) %>%
  mutate(Taxon = str_extract(TaxonCN, "[a-zA-z\\.\\s]+")) %>%
  mutate(Taxon = str_trim(Taxon)) %>%
  mutate(Taxon = str_replace_all(Taxon, fixed("."),  ". ")) %>%
  mutate(Genus = word(Taxon, 1)) %>%
  mutate(Genus = str_trim(Genus)) %>%
  mutate(Species = word(Taxon, 2, -1)) %>%
  mutate(Species = str_trim(Species)) %>%
  mutate(Species = word(Species, 1)) %>%
  drop_na(Taxon)


df <- left_join(dt, cnNames) %>%
  mutate(Raw = NULL) %>%
  mutate(CladeCN = NULL) %>%
  mutate(FamilyCN = NULL) %>%
  mutate(TaxonCN = NULL)

df_cn <- df %>%
  group_by(ChineseName, Genus, Species) %>%
  summarise()

df_genus <- df %>%
  filter(str_detect(Genus, '\\.')|is.na(Taxon) == TRUE) %>%
  group_by(Family, Taxon, Genus, ChineseName, Species) %>%
  summarise() %>%
  relocate(ChineseName, Family, Taxon)


df$Genus[df$ChineseName == "绵毛猕猴桃"] <- "Actinidia"
df$Genus[df$ChineseName == "阔叶猕猴桃"] <- "Actinidia"
df$Genus[df$ChineseName == "野漆树"] <- "Toxicodendron"
df$Genus[df$ChineseName == "芒尖苔草"] <- "Carex"
df$Genus[df$ChineseName == "亚大苔草"] <- "Carex"
df$Genus[df$ChineseName == "香附子"] <- "Cyperus"
df$Genus[df$ChineseName == "糠皮树"] <- "Mallotus"
df$Genus[df$ChineseName == "千年桐"] <- "Vernicia"
df$Genus[df$ChineseName == "苦槠"] <- "Castanopsis"
df$Genus[df$ChineseName == "芒"] <- "Miscanthus"
df$Genus[df$ChineseName == "梵天花"] <- "Urena"
df$Genus[df$ChineseName == "红蓼"] <- "Persicaria"
df$Genus[df$ChineseName == "珍珠菜"] <- "Lysimachia"
df$Genus[df$ChineseName == "高粱泡"] <- "Rubus"
df$Genus[df$ChineseName == "空心泡"] <- "Rubus"
df$Genus[df$ChineseName == "母草"] <- "Lindernia"
df$Genus[df$ChineseName == "细齿柃"] <- "Eurya"
df$Genus[df$ChineseName == "九里光"] <- "Senecio"
df$Genus[df$ChineseName == "人字草"] <- "Kummerowia"
df$Genus[df$ChineseName == "勾藤"] <- "Uncaria"
df$Genus[df$ChineseName == "天艺箕"] <- "Dicranopteris" #大芒萁? 
df$Genus[df$ChineseName == "山樟"] <- "Cinnamomum" #香樟?
df$Genus[df$ChineseName == "白鹤草"] <- "Rhinacanthus"
df$Genus[df$ChineseName == "苦竹"] <- "Pleioblastus"
df$Genus[df$ChineseName == "菝葜"] <- "Smilax"
df$Genus[df$ChineseName == "葱木"] <- "Aralia"
df$Genus[df$ChineseName == "蕨"] <- "Pteridium"
df$Genus[df$ChineseName == "酢浆草"] <- "Oxalis"
df$Genus[df$ChineseName == "醉鱼"] <- "Buddleja"
df$Genus[df$ChineseName == "野桂花"] <- "Osmanthus"
df$Genus[df$ChineseName == "铁芒箕"] <- "Dicranopteris"
df$Genus[df$ChineseName == "鱼腥草"] <- "Houttuynia"
df$Genus[df$ChineseName == "鸭趾草"] <- "Commelina"

df$Species[df$ChineseName == "九里光"] <- "scandens"
df$Species[df$ChineseName == "人字草"] <- "striata"
df$Species[df$ChineseName == "勾藤"] <- "rhynchophylla"
df$Species[df$ChineseName == "天艺箕"] <- "ampla" #大芒萁?
df$Species[df$ChineseName == "山樟"] <- "bodinieri" #香樟?
df$Species[df$ChineseName == "白鹤草"] <- "nasutus"
df$Species[df$ChineseName == "苦竹"] <- "amarus"
df$Species[df$ChineseName == "菝葜"] <- "china"
df$Species[df$ChineseName == "葱木"] <- "chinensis"
df$Species[df$ChineseName == "蕨"] <- "aquilinum"
df$Species[df$ChineseName == "酢浆草"] <- "corniculata"
df$Species[df$ChineseName == "醉鱼"] <- "lindleyana"
df$Species[df$ChineseName == "野桂花"] <- "yunnanensis"
df$Species[df$ChineseName == "铁芒箕"] <- "linearis"
df$Species[df$ChineseName == "鱼腥草"] <- "cordata"
df$Species[df$ChineseName == "鸭趾草"] <- "communis"

df$Family[df$ChineseName == "九里光"] <- "Asteraceae"
df$Family[df$ChineseName == "人字草"] <- "Fabaceae"
df$Family[df$ChineseName == "勾藤"] <- "Rubiaceae"
df$Family[df$ChineseName == "天艺箕"] <- "Gleicheniaceae" #大芒萁?
df$Family[df$ChineseName == "山樟"] <- "Lauraceae" #香樟?
df$Family[df$ChineseName == "白鹤草"] <- "Acanthaceae"
df$Family[df$ChineseName == "苦竹"] <- "Poaceae"
df$Family[df$ChineseName == "菝葜"] <- "Smilacaceae"
df$Family[df$ChineseName == "葱木"] <- "Araliaceae"
df$Family[df$ChineseName == "蕨"] <- "Dennstaedtiaceae"
df$Family[df$ChineseName == "酢浆草"] <- "Oxalidaceae"
df$Family[df$ChineseName == "醉鱼"] <- "Scrophulariaceae"
df$Family[df$ChineseName == "野桂花"] <- "Oleaceae"
df$Family[df$ChineseName == "铁芒箕"] <- "Gleicheniaceae"
df$Family[df$ChineseName == "鱼腥草"] <- "Saururaceae"
df$Family[df$ChineseName == "鸭趾草"] <- "Commelinaceae"

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

# Abundance and/or biomass, here: density (?) calculation (?) times site area???
is.numeric(df$Biomass)
# FALSE

# Year, month and day must be integers or factors
# | means or
is.factor(df$Year) | is.integer(df$Year)
# TRUE

# Secondary fields such as trawl, plot, transect etc must be factors or integers? NA
# Secondary fields such as elevation or depth will normally be numeric unless in a treatment format. NA here
# Date should be POSIXct (not applicable in this case) NA, just year

# Taxonomic fields must be characters or factors?
is.factor(df$Taxon) | is.character(df$Taxon)
# TRUE


## fixes
df$Biomass <- as.numeric(df$Biomass)


## remove unnecessary cols

# ChineseName isn't needed
df <- df %>%
  mutate(Taxon = paste(Genus, Species)) %>%
  mutate(ChineseName = NULL) %>%
  mutate(Clade = NULL)
  
## fields
# Abundance/Density
min(df$Biomass) > 0 # no zeroes?
sum(df$Biomass=="") > 0 # no blanks

# Year < 2021, month < 12, day < 31
summary(df[,4])

## pool abundance
df %>% group_by(Year, Taxon) %>% 
  summarise(Biomass=sum(Biomass)) %>% ungroup()

df$Latitude <- c(rep(lat, nrow(df)))
df$Longitude <- c(rep(long, nrow(df)))


## map

world_map <- map_data('world') # check whether the GPS coordinates match expectations
world <- ggplot(world_map) + 
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  coord_fixed() + 
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())
points <- world + geom_point(data=df %>% distinct(Latitude, Longitude, Site), 
                             aes(x=Longitude, y=Latitude, fill = Site), shape=21)

points + coord_fixed(xlim=c(95,115), ylim=c(20,50))

## nomenclature

# check that genera are genera, not family names (-idae/eae)
# this returns the record index number if there are any
str_which(df$Taxon, 'idae$|eae$')
# check the species list for misspellings or non-BioTIME taxonomic convention names
# Do visual checks before splitting taxa up into the three columns.
# sort(unique(df$Taxon)) %>% word(., 2)
# # this keeps IDs adjacent to their same-genus misspellings, but only looking at the last few words to check
# 
# sort(unique(df$Taxon)) %>% word(., 1) # check genera

# or
sort(unique(df$Taxon)) ## check whole Taxon
sort(unique(df$Family)) # check family

##
replace_t <- c("Ampelopsis cantoninensis" = "Ampelopsis cantoniensis",
               "Clerodendron cyrtophyllum" = "Clerodendrum cyrtophyllum",
               "Dichroa ferbrifuga" = "Dichroa febrifuga" ,
               "Ixeris denticulate" = "Ixeris denticulata", 
               "Lindernia cxustacea" = "Lindernia crustacea", 
               "Persicaria orientale" = "Persicaria orientalis", 
               "Rubus alceaefolius" = "Rubus alceifolius")

df <- df %>%
  mutate(Taxon = str_replace_all(Taxon, replace_t)) %>%
  mutate(Genus = word(Taxon, 1)) %>%
  mutate(Species = word(Taxon, 2)) %>%
  mutate(Taxon = NULL)


### prepare and export
df$Abundance <- rep('', nrow(df))
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
dataset_name <- 'CERN_Hunan_Huitong_Fir_Forest_community_biomass_1973-2007'
# put in as many non-blank fields unique to the sampling event
# If plot, depthelevation, month, and day are available, I'd add those in too
df_merged$SampleDescription <- as.factor(with(df_merged, paste(Sample, Site, Year, sep='_')))

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

