# Norway (Benthic invertebrates) Final Script # 

# 1. Load the required packages

require(dplyr)
require(ggplot2)
require(lubridate)
require(stringr)
require(readxl)

# 2. Read in data

# Since the different sites are on different sheets, read them in separately and then combine later

dt.1<- read_excel('/Users/cherchow/Dropbox/towards BioTIME v2/originalData/HaasePilotto/S097-S100.xlsx',sheet=1, skip=3, col_names=T, na='')
dt.2<- read_excel('/Users/cherchow/Dropbox/towards BioTIME v2/originalData/HaasePilotto/S097-S100.xlsx',sheet=2, skip=3, col_names=T, na='')
dt.3<- read_excel('/Users/cherchow/Dropbox/towards BioTIME v2/originalData/HaasePilotto/S097-S100.xlsx',sheet=3, skip=3, col_names=T, na='')
dt.4<- read_excel('/Users/cherchow/Dropbox/towards BioTIME v2/originalData/HaasePilotto/S097-S100.xlsx',sheet=4, skip=3, col_names=T, na='')

# We can now combine the sheets together 
dt <- bind_rows(dt.1, dt.2, dt.3, dt.4)
rm(dt.1, dt.2, dt.3, dt.4)

sites <- read_excel('/Users/cherchow/Dropbox/towards BioTIME v2/originalData/HaasePilotto/S097-S100.xlsx',sheet=5, skip=1, col_names=T, na='')
# transform coordinates to WGS84 degrees
require(sf)
colnames(sites)[2:3] <- c('LatUTM', 'LonUTM')
sites <- st_as_sf(sites[2:3], coords = c('LonUTM', 'LatUTM'), crs = 25833) %>%
  st_transform(., crs = 4326) %>% st_coordinates() %>% .[, c(2,1)] %>% bind_cols(sites, .)
colnames(sites)[4:6] <- c('DepthElevation', 'Latitude', 'Longitude')
sites$LatUTM <- NULL
sites$LonUTM <- NULL

dt <- left_join(dt, sites, by="Site")

# 3. Check data 

# Visual check of data shows:
#   Need to break up 'date' into day, year, month? or leave in POSIXct form? DONE
#   Taxon should be separated into genus and species 
#   Read paper and find out what a kick sample is (abundance/ biomass) -> I think it is abundance 
#   Taxon is in the character form, this is good 
#   Kick sample is numeric, this is good 

# Are there more than two distinct years? (yes)
dt$Year <- word(dt$`Sampling date`, sep = "-", start=1) %>% as.integer
dt$Month <- word(dt$`Sampling date`, sep = "-", start=2) %>% as.integer
dt$Day <- word(dt$`Sampling date`, sep = "-", start=3) %>% as.integer
dt$`Sampling date` <- NULL

n_distinct(dt$Year) >= 2

# Check Year, month and day
is.factor(dt$Year) | is.integer(dt$Year)
is.factor(dt$Month) | is.integer(dt$Month)
is.factor(dt$Day) | is.integer(dt$Day)

# Change Kick Sample to abundance and check is numeric TRUE 
colnames(dt)[3] = "Abundance"
is.numeric(dt$Abundance)

# Check data again -> looks good 
str(dt)
summary(dt)

# Notes, when merging the data back together, you need to go back and fix the sampling data again 
# Just re-reun lines 52-92

# Check whether the GPS coordinates match expectations --> They do! 
library(maps)
library(ggplot2)
world_map <- map_data('world') 
world <- ggplot(world_map) + 
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  coord_fixed() + 
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())
points <- world + geom_point(data=dt, aes(x=Longitude, y=Latitude), shape=21)
points + coord_fixed(xlim=c(5,15), ylim=c(57,67))
detach('package:maps')


# Make a new column for the species 
dt$Species <- dt$Taxon

sort(unique(dt$Taxon))

# Now to separate the genus and the species 
dt$Genus <- word(dt$Species, 1)
dt$Species <- word(dt$Species, start=2)
dt$Taxon <- NULL

# check the species list for misspellings or non-BioTIME taxonomic convention names
# Check species
sort(unique(word(dt$Species, start=2, end=-1)))
# Check genera
sort(unique(word(dt$Species, 1)))
# Check combined
sort(unique(dt$Species))

# Looking at the data, here are some problems:
#   A. muliebris/hispida 
#   Acari
#   Agapetus spp.
#   Apatania spp.
#   Athripsodes sp.
#   Capnia sp.
#   Ceratopogonidae
#   Chironomidae
#   Coleoptera
#   Diptera
#   Diptera sp.
#   Dytiscidae
#   Glossosoma spp.
#   Hydraenidae
#   Hydropsyche spp.
#   Hydroptila spp.
#   Isoperla spp.
#   Leptoceridae
#   Leuctra fusca/digitata
#   Limnephilidae
#   Limnephilus sp.
#   Micrasema spp.
#   Nematomorpha (Gordius)
#   Oligochaeta
#   Pericoma spp.
#   Platyhelminthes
#   Potamophylax spp.
#   Psychodidae
#   Sialis sp.
#   Simuliidae
#   Tipulidae

# I have created a replacement vector to fix the problems

replace <- c(
  'spp.\\s*$|sp.\\s*$' = 'sp',
  "A. muliebris/hispida" = "Apatania sp",
  "Leuctra fusca/digitata" = "Leuctra sp",
  "Nematomorpha \\(Gordius\\)" = "Gordius sp",
  'Diptera sp' = 'Diptera')

dt$Species <- str_replace_all(dt$Species, replace)
sort(unique(dt$Species))
dt$Family <- ''
unique(dt$Species[str_count(dt$Species, '\\s') == 0]) # test the catch all for non genus taxa
dt$Family[str_count(dt$Species, '\\s') == 0] <- dt$Species[str_count(dt$Species, '\\s') == 0]
dt$Species[str_count(dt$Species, '\\s') == 0] <- NA
sort(unique(dt$Species))

dt$Genus <- word(dt$Species, 1)
dt$Species <- word(dt$Species, 2, -1)
dt$Taxon <- NULL

# STEP 5: Prepare curated data for export
dt %>% distinct(Year, Month, Day, Site) %>% arrange(Year, Month, Day, Site) %>% View
dt %>% group_by(Site) %>% summarise(sampledates = length(unique(paste0(Year, Month, Day))))
# not just monthly, but pretty consistent across sites
dt$SampleDescription <- with(dt, paste(Year, Month, Day, Site, sep = "_"))
n_distinct(dt$SampleDescription)
dt$Site <- NULL

# aggregate abundance records that are same species, plot, and survey day.
dt_merged <- dt %>% group_by_at(vars(-Abundance)) %>% # group by everything BUT abundance/biomass
  summarise(Abundance = sum(Abundance)) %>% # aggregate
  ungroup() %>% arrange(Year, Month, Day, SampleDescription, Family, Genus, Species) %>% 
  mutate(Biomass = '', Plot = '') %>% # replace with whatever columns need to be added
  relocate(Abundance, Biomass, Family, Genus, Species, SampleDescription, Plot, Latitude, Longitude, DepthElevation, Day, Month, Year) # reorder columns
nrow(dt) - nrow(dt_merged) # any change in aggregating? 5 records

View(dt_merged) # final check :)

# STEP 6: Export curated data and prepare spreadsheet
write.csv(dt_merged, 'Norway_BenthicInverts_AnS_CC_rawdata.csv', row.names=F, na='')
clipr::write_clip(dt_merged)


library(sf)
library(clipr)

# 1. Convert data points into point spatial object
# assumes your rawdata dataframe is called rawdata
dt_coord <- dt_merged %>% select(Longitude, Latitude) %>% distinct() %>%
  st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()

# 2. Calculate convex hull, area and centroid
centroid <- dt_coord %>% st_convex_hull() %>% st_centroid() %>% unlist # get centroid
write_clip(centroid[c(2,1)]) # copy as lat-long

# transform to mercator to get area in sq km
area <- st_transform(dt_coord, st_crs("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs")) %>%
  st_convex_hull() %>% st_area()  ##get area in sq km
write_clip(area)



