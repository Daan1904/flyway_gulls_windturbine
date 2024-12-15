

#libraries
library(dplyr)
library(tidyverse)
library(sf)
library(mapview)


# show tibbles with all the columns!
options(dplyr.width = Inf)

### Read file (one example)
klm_211307 <- read_csv("C:\\Users\\kentie\\OneDrive - NIOZ\\WORK\\NIOZ\\Meeuwen\\Studenten\\2024\\Flyway Ecology\\211307_010821-310322.txt")
klm_211307

# make spatial

klm_211307_sf <- st_as_sf(x = klm_211307, 
                   coords = c("Longitude2", "Latitude2"),
                   crs = 4326) 



# plot op interactive map
mapview(klm_211307_sf)


# remove empty points (Latitude = 0)
klm_211307 <- klm_211307 %>% filter(Latitude2 >0)
klm_211307_sf <- klm_211307_sf %>% filter(Latitude >0)
mapview(klm_211307_sf)

# make line between points
klm_211307_sf %>%
  summarise(do_union = F) %>%
  st_cast("LINESTRING") -> klm_211307_sf_line

mapview(klm_211307_sf) + mapview(klm_211307_sf_line)











