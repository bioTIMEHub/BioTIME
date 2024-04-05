require(dplyr)
require(ggplot2)
require(stringr)
require(readxl)
require(maps)
require(tidyr)

rm(list = ls())
# setwd("../")
# Dataset original file path: ./Zhejiang_Tiantong_B/实体数据/天童生物数据_土壤动物种类与密度.xlsx
# Dataset original file path: ./Zhejiang_Tiantong_B/实体数据/天童生物数据_动物名录.xlsx
# Rename it to ./Zhejiang_Tiantong_B/data/LZD_SWSJ_Sampling.xls
# Rename it to ./Zhejiang_Tiantong_B/data/Animal_list.xlsx
# To avoid read_excel reading Chinese error

setwd("./Zhejiang_Tiantong_B/data/")
# sampling methods
# 栲树林综合观测场位于天童国家森林公园天童寺旁边, 是保护比较完整的森林核心地带, 该观测场始建于1992 年,
# 2007 年扩建观测投影面积为50m×50m, 海拔196m, 地理位置为东经121°47'12″, 北纬29°48'29″。 


angle2dec <- function(angle) {
  angle <- as.character(angle)
  x <- do.call(rbind, strsplit(angle, split=' '))
  x <- apply(x, 1L, function(y) {
    y <- as.numeric(y)
    y[1] + y[2]/60 + y[3]/3600
  })
  return(x)
}

lat <- angle2dec("29 48 29") %>% as.numeric()
long <- angle2dec("121 47 12") %>% as.numeric()

## 

# import our data from the Excel spreadsheet
colNamesCN <- read_excel('./Animal_Sampling.xlsx', sheet=1, range=cell_rows(2:3), col_names=T, na='') %>%
  colnames()
# "年"              "月"              "日"              "观测场名称"      "动物名称"        "密度（ind m-2）"
colNames <- c('Year', 'Month', 'Day', 'Site', 'ChineseName', 'Abundance')


dt <- read_excel('./Animal_Sampling.xlsx', sheet=1, range=cell_rows(3:162), col_names=F, na='') %>%
  setNames(colNames) %>%
  tidyr::fill(., c(Year, Month, Day, Site)) %>%
  mutate(ChineseName = case_when(
    !str_detect(ChineseName, "幼虫") ~ ChineseName,
    str_detect(ChineseName, "幼虫") ~ str_remove(ChineseName, "幼虫")
  ))

unique(dt$Site)
# "栲树林综合观测场"   "木荷林辅助观测场"   "马尾松林辅助观测场" "常绿灌丛辅助观测场"


dt$Site[dt$Site == "栲树林综合观测场"] <- "TTFZH01"
dt$Site[dt$Site == "木荷林辅助观测场"] <- "TTFFZ01"
dt$Site[dt$Site == "马尾松林辅助观测场"] <- "TTFFZ02"
dt$Site[dt$Site == "常绿灌丛辅助观测场"] <- "TTFFZ03"


cnNames <- read_excel('./Animal_list.xlsx', sheet=2, range = cell_rows(2:132), col_names=T, na='') %>%
  setNames(c("ClassCE", "OrderCE", "SuborderCE", "FamilyCE", "SubfamilyCE", "GenusCE", "SpeciesCE")) %>%
  tidyr::fill(., c("ClassCE", "OrderCE", "SuborderCE", "FamilyCE", "SubfamilyCE", "GenusCE")) %>%
  mutate_all(stringi::stri_trans_general, "latin-ascii")

cnNames$OrderCE[cnNames$OrderCE == "双尾纲(双尾目)(Diplura)"] <- "双尾目(Diplura)"
cnNames$OrderCE[cnNames$ClassCE == "原尾钢(Protura)"] <- "原尾目(Protura)"

cnNames <- cnNames %>%  
  mutate(ClassCN = str_extract(ClassCE, "[^a-zA-Z\\(\\s]+")) %>%
  mutate(ClassEN = str_extract(ClassCE, "(?<=\\().+?(?=\\))")) %>%
  mutate(OrderCN = str_extract(OrderCE, "[^a-zA-Z\\(\\s]+")) %>%
  mutate(OrderEN = str_extract(OrderCE, "(?<=\\().+?(?=\\))")) %>%
  mutate(SuborderCN = str_extract(SuborderCE, "[^a-zA-Z\\(\\s]+")) %>%
  mutate(SuborderEN = str_extract(SuborderCE, "(?<=\\().+?(?=\\))")) %>%
  mutate(FamilyCN = str_extract(FamilyCE, "[^a-zA-Z\\(\\s]+")) %>%
  mutate(FamilyEN = str_extract(FamilyCE, "(?<=\\().+?(?=\\))")) %>%
  mutate(SubfamilyCN = str_extract(SubfamilyCE, "[^a-zA-Z\\(\\s]+")) %>%
  mutate(SubfamilyEN = str_extract(SubfamilyCE, "(?<=\\().+?(?=\\))")) %>%
  mutate(GenusCN = str_extract(GenusCE, "[^a-zA-Z\\(\\s]+")) %>%
  mutate(GenusEN = str_extract(GenusCE, "(?<=\\().+?(?=\\))")) %>%
  mutate(SpeciesCN = str_extract(SpeciesCE, "[^a-zA-Z\\(\\s]+")) %>%
  mutate(SpeciesEN = str_extract(SpeciesCE, "(?<=\\().+?(?=\\))")) %>%
  select(-c(ClassCE, OrderCE, SuborderCE, FamilyCE, SubfamilyCE, GenusCE, SpeciesCE)) %>%
  mutate_at(seq(2,14,2), str_trim, "left") %>%
  mutate_at(seq(2,14,2), str_replace_all, fixed(". "),  ".") %>%
  mutate_at(seq(2,14,2), str_replace_all, fixed("."),  ". ") %>%
  mutate(OrderCN = case_when(
    !str_detect(OrderCN, "幼虫") ~ OrderCN,
    str_detect(OrderCN, "幼虫") ~ str_remove(OrderCN, "幼虫")
  )) %>%
  mutate_all(replace_na, "")


cnNamesLong <- cnNames %>%
  select(-c(GenusCN, SpeciesCN, SuborderCN, SubfamilyCN, SuborderEN, SubfamilyEN, GenusEN, SpeciesEN)) %>%
  pivot_longer(c(ClassCN, OrderCN, FamilyCN), names_to = "Classification", values_to = "name") %>%
  group_by(name, ClassEN, OrderEN, FamilyEN, Classification) %>%
  summarise() %>%
  setNames(c("ChineseName", "Class", "Order", "Family", "Classification")) %>%
  mutate(Name = case_when(str_detect(ChineseName, "纲") ~ Class,
                          str_detect(ChineseName, "目") ~ Order,
                          str_detect(ChineseName, "科") ~ Family)) %>%
  group_by(ChineseName, Name) %>%
  summarise()

df <- left_join(dt, cnNamesLong) %>%
  rename(Family = Name) %>%
  mutate(ChineseName = NULL)

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

# Abundance and/or biomass
is.numeric(df$Abundance) # TRUE


# Year, month and day must be integers or factors
# | means or
is.factor(df$Year) | is.integer(df$Year) # FALSE
is.factor(df$Month) | is.integer(df$Month) # FALSE
is.factor(df$Day) | is.integer(df$Day) # FALSE


# Secondary fields such as trawl, plot, transect etc must be factors or integers? NA
# Secondary fields such as elevation or depth will normally be numeric unless in a treatment format. NA here
# Date should be POSIXct (not applicable in this case) NA, just year

# Taxonomic fields must be characters or factors?
is.factor(df$Family) | is.character(df$Family) # TRUE


## fixes
df$Year <- as.factor(df$Year)
df$Month <- as.factor(df$Month)
df$Day <- as.factor(df$Day)


## remove unnecessary cols

## fields
# Abundance/Density
min(df$Abundance) > 0 # no zeroes?
sum(df$Abundance=="") > 0 # no blanks

# Year < 2021, month < 12, day < 31
summary(df[,1])
summary(df[,2])
summary(df[,3])

## pool abundance

##
df$Latitude <- c(rep(lat, nrow(df)))
df$Longitude <- c(rep(long, nrow(df)))

## map

world_map <- map_data('world') # check whether the GPS coordinates match expectations
world <- ggplot(world_map) + 
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  coord_fixed() + 
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())
points <- world + geom_point(data=df %>% distinct(Latitude, Longitude, Year), 
                             aes(x=Longitude, y=Latitude, fill = Year), shape=21)

points + coord_fixed(xlim=c(110,130), ylim=c(20,40))

## nomenclature

sort(unique(df$Family, 1)) # check genera
# sort(unique(dt$Family)) # check family

replace_f <- c("Blattoptera" = "Blattodea",
               "Homopera" = "Homoptera",
               "O. opisthopora" = "Opisthopora",
               "Rotatoria" = "Rotifera",
               "Symphyia" = "Symphyla")


df <- df %>%
  mutate(Family = str_replace_all(Family, replace_f))


### prepare and export
df$Biomass <- rep('', nrow(df))
df$Plot <- rep('', nrow(df))
df$DepthElevation <- rep('', nrow(df))
df$StudyID <- rep('', nrow(df))
df$Species <- rep('', nrow(df))
df$Genus <- rep('', nrow(df))

# aggregate abundance records that are same species, plot, and survey day.

df_merged <- df %>% group_by_at(vars(-Abundance)) %>% 
  summarise(Abundance=sum(Abundance)) %>% ungroup() %>% arrange(Year, Family, Genus, Family)

dim(df)[1]-dim(df_merged)[1] # any change in aggregating?

## save dataset
# save the dataset name as an object so we save some typing
dataset_name <- 'CERN_Zhejiang_Tiantong_soil_macrofauna_community_abundance_2007-2009'
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

