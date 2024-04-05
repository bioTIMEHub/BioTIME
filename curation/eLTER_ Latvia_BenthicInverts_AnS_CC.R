# Title:  Benthic invertebrates at three sites at River Salaca - Latvia
# Author: Anokhi Saha

# ----------------------------
# INDEX: 
#   I.    Loading the data      Line 12 - 72
#   II.   Criteria checks       Line 74 - 130
#   III.  Errors in the data    Line 133 - 175 
#   IV.   Prepare for export    Line 177 - 220
#   V.    Metadata              Line 223 - 264

# ----------------------------

# 1. Load the required packages
require(readxl)
require(maps)
require(lubridate)
require(stringr)
require(dplyr)

# 2. Read in data and check it 

# Sheet one: Site = Vecate
dt.1 <- read_excel("/Users/cherchow/Dropbox/towards BioTIME v2/originalData/HaasePilotto/S091-S093.xlsx", sheet=1, skip=2, col_names=T, na='')

# Sheet two: Site = Mazsalaca
dt.2 <- read_excel("/Users/cherchow/Dropbox/towards BioTIME v2/originalData/HaasePilotto/S091-S093.xlsx", sheet=2, skip=2, col_names=T, na='')

# Sheet three: Site = Vecsalaca
dt.3 <- read_excel("/Users/cherchow/Dropbox/towards BioTIME v2/originalData/HaasePilotto/S091-S093.xlsx", sheet=3, skip=2, col_names=T, na='')

sites <- read_excel("/Users/cherchow/Dropbox/towards BioTIME v2/originalData/HaasePilotto/S091-S093.xlsx", sheet=5, skip=1, col_names=T, na='')[1:4]
colnames(sites)[4] <- 'DepthElevation'

 # merge together to curate but split again later.

# 3. Add in the data for the longitude, latitude, and altitude, before combining the sheets

# They have not given the lats and longs in the correct format (-180 to 180) 
#         > I need to figure out what form they are in and then use a converter
#         > For coordinates in latitude and longitude, we work primarily in WGS84
#         > I used an online converter called epsg.io to covert the co-ordinates 
#           between ESRI:102440 LKS 1992 Latvia to EPSG:4326 WGS 84

#transform
require(sf)
# they were switched around,lat long
sites <- sites %>% st_as_sf(., coords = c('Latitude', 'Longitude'), crs = '+proj=tmerc +lat_0=0 +lon_0=24 +k=0.9996 +x_0=500000 +y_0=0 +ellps=GRS80 +units=m +no_defs +type=crs') %>% st_transform(., crs = 'WGS84') %>% st_coordinates() %>% bind_cols(sites[-c(2:3)])
colnames(sites)[1:2] <- c('Longitude', 'Latitude')

# 4. Bind the sheets back together 
dt <- bind_rows(dt.1, dt.2, dt.3) %>% left_join(., sites, by="Site")
View(dt)

# 5. Check where the co-ordinates are on the map
world_map <- map_data('world') 
world <- ggplot(world_map) + 
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  coord_fixed() + 
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())
points <- world + geom_point(data=dt, aes(x=Longitude, y=Latitude), shape=21)
points + coord_fixed(xlim=c(10,30), ylim=c(45,70))
detach('package:maps')
# This has successfully plotted the three sights in the expected locations

rm(dt.1, dt.2, dt.3)

# ----------------------------

# We can use R to check the criteria: 
# - Dataset consists of at least 2 years of sampling (they do not have to be consecutive) = YES
# - Dataset consists of entire assemblages, not just populations = YES
# - Data should record abundance, biomass or both. = YES (abundance)
# - Sampling methods *and effort* are consistent through time. = YES
# - Individuals are identified to species level where possible. = YES

dim(dt)
summary(dt)

# Does the data set record data over more than two years?
# To do this, the sampling date has to be split into day, month, and year
#   > The output says TRUE
dt$Year <- year(dt$`Sampling date`)
dt$Month <- month(dt$`Sampling date`)
dt$Day <- day(dt$`Sampling date`)
dt$`Sampling date` <- NULL
n_distinct(dt$Year) >= 2

# Make Year, Month and Day factors
#   > The output says TRUE
is.factor(dt$Year) | is.integer(dt$Year)
is.factor(dt$Month) | is.integer(dt$Month)
is.factor(dt$Day) | is.integer(dt$Day)

# Change 'Density' to 'Abundance' and check it is numeric
#   > The output says TRUE
colnames(dt)[3] = "Abundance"
is.numeric(dt$Abundance)

# Check data again -> looks good 
str(dt)

# 7. Primary field check 

# Abundance needs to be more than zero for each row
# > The output says FALSE
min(dt$Abundance) > 0
dt %>% filter(Abundance == 0)

# just 3 records to filter out
dt <- dt %>% filter(Abundance > 0)

# ----------------------------

# 8. Secondary field check - Errors in the data

# I am going to split the taxon into the two words:
dt$Species <- dt$Taxon
dt[is.na(dt$Species),]
dt <- dt %>% filter(!is.na(dt$Species)) # there's an NA, remove
sort(unique(dt$Species))
# isolate out family level and above
unique(dt$Species[str_detect(dt$Species, 'Gen\\.* sp.$')])
dt$Family <- ''
dt$Family[str_detect(dt$Species, 'Gen. sp.$')] <- dt$Species[str_detect(dt$Species, 'Gen. sp.$')] %>% str_remove(., '\\sGen. sp.*$')
dt$Species[str_detect(dt$Species, 'Gen. sp.$')] <- '' # and remove

sort(unique(dt$Species))

replace = c('\\s\\(Glossiphonia heteroclita\\)' = '',
            'sp\\.$' = 'sp',
            'Habroleptoides\\/Paraleptophlebia' = '')

dt$Family[str_detect(dt$Species, 'Habroleptoides\\/Paraleptophlebia')] <- 'Leptophlebiidae' # downgrade uncertian genus to family
dt$Species <- str_replace_all(dt$Species, replace)

sort(unique(dt$Species))
dt$Genus <- word(dt$Species, 1)
dt$Species <- word(dt$Species, 2) # only keep the second word. lots of subsp
dt %>% distinct(Family, Genus, Species) %>% 
  arrange(Family, Genus, Species) %>% View

dt$Taxon <- NULL
# ----------------------------

# check samples
dt %>% distinct(Site, Year, Month, Day) %>% arrange(Site, Year, Month, Day) %>% View
# generally monthly samples
sites$Site

dt$SampleDescription = with(dt, paste(Year, Month, sep = '_'))

dt_merged1 <- dt %>% filter(Site == 'Salaca_1_Vecate') %>% select(!Site) %>% # split by site
  group_by(SampleDescription, Family, Genus, Species) %>% mutate(Abundance = sum(Abundance)) %>% # aggregate records
  ungroup() %>% arrange(Year, Month, Day, SampleDescription, Family, Genus, Species) %>% 
  mutate(Biomass = '', Plot = '') %>% 
  relocate(Abundance, Biomass, Family, Genus, Species, SampleDescription, Plot, Latitude, Longitude, DepthElevation, Day, Month, Year)
nrow(dt[dt$Site == 'Salaca_1_Vecate',]) - nrow(dt_merged1) # any change in aggregating? no

dt_merged2 <- dt %>% filter(Site == 'Salaca_2_Mazsalaca') %>% select(!Site) %>% # split by site
  group_by(SampleDescription, Family, Genus, Species) %>% mutate(Abundance = sum(Abundance)) %>% # aggregate records
  ungroup() %>% arrange(Year, Month, Day, SampleDescription, Family, Genus, Species) %>% 
  mutate(Biomass = '', Plot = '') %>% 
  relocate(Abundance, Biomass, Family, Genus, Species, SampleDescription, Plot, Latitude, Longitude, DepthElevation, Day, Month, Year)
nrow(dt[dt$Site == 'Salaca_2_Mazsalaca',]) - nrow(dt_merged2) # any change in aggregating? no

dt_merged3 <- dt %>% filter(Site == 'Salaca_3_Vecsalaca') %>% select(!Site) %>% # split by site
  group_by(SampleDescription, Family, Genus, Species) %>% mutate(Abundance = sum(Abundance)) %>% # aggregate records
  ungroup() %>% arrange(Year, Month, Day, SampleDescription, Family, Genus, Species) %>% 
  mutate(Biomass = '', Plot = '') %>% 
  relocate(Abundance, Biomass, Family, Genus, Species, SampleDescription, Plot, Latitude, Longitude, DepthElevation, Day, Month, Year)
nrow(dt[dt$Site == 'Salaca_3_Vecsalaca',]) - nrow(dt_merged3) # any change in aggregating? no

View(dt_merged2)
dataset_name = 'eLTER_RiverSalacaLatvia_BenthicInverts'
write.csv(dt_merged1, paste0(dataset_name, '_Vecate_rawdata_AnS_CC.csv'), row.names=F, na='')
write.csv(dt_merged2, paste0(dataset_name, '_SkanaisMazsalaca_rawdata_AnS_CC.csv'), row.names=F, na='')
write.csv(dt_merged3, paste0(dataset_name, '_Vecsalaca_rawdata_AnS_CC.csv'), row.names=F, na='')




