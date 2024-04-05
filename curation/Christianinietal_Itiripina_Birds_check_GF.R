

## Curators check of author-submitted data
## Neotropical cerrado birds in Itirapina, Brazil
## Cher Chow

dt <- read.csv('Originals/christianinietal_itiripina_birds.csv', header = T)

str(dt)
summary(dt) # overall ranges checks
n_distinct(dt$Plot)
sort(unique(dt$Plot))

dt %>% distinct(Year, Month, Day, Plot)
dt$Plot <- str_extract(dt$Plot, '[:alnum:]+(?=\\_)')
dt %>% distinct(Year, Month, Day, Plot) %>% arrange(Year, Month, Day, Plot)
# only Val plots go back to 2004

# do plots/samples have distinct coordinates

dt %>% distinct(Plot, Latitude, Longitude) %>% arrange(Plot, Latitude, Longitude)
# per plot, except for Val1, 2, 3

dt$SampleDescription <- with(dt, paste(Year, Month, Day, Plot, sep = "_"))
dt$Plot <- ''

sort(unique(paste(dt$Genus, dt$Species, sep = ' ')))
dt$Species <- str_replace_all(dt$Species, '^sp.$', 'sp')

library(sf)
library(clipr)

# 1. Convert data points into point spatial object
# assumes your rawdata dataframe is called rawdata
dt_coord <- dt %>% select(Longitude, Latitude) %>% distinct() %>%
  st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()

# 2. Calculate convex hull, area and centroid
centroid <- dt_coord %>% st_convex_hull() %>% st_centroid() %>% unlist # get centroid
write_clip(centroid[c(2,1)]) # copy as lat-long

# use author provided area
write.csv(dt, 'christianinietal_itiripina_birds_rawdata_CC.csv', row.names = F)
