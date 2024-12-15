#Check for new packages
renv::restore()

#libraries
library(tidyverse)
library(sf)
library(mapview)
library(lubridate)


# show tibbles with all the columns!
options(dplyr.width = Inf)

### Read file (one example)
klm_201105 <- read.csv("C:/Users/daank/OneDrive - University of Twente/Documents/Rijksuniversiteit Groningen/Year 1 - 24-25/Flyway Ecology/Mini project - Gull migration wind farms/Bird tracks/201105_010821-310322.txt")
klm_201105

# make dates
klm_201105$UTC_datetime <- dmy_hms(klm_201105$UTC_datetime) 

# make spatial

klm_201105_sf <- st_as_sf(x = klm_201105, 
                   coords = c("Longitude2", "Latitude2"),
                   crs = 4326) 



# plot op interactive map
mapview(klm_201105_sf)


# remove empty points (Latitude = 0)
klm_201105 <- klm_201105 |> 
  dplyr::filter(Latitude2 >0)
klm_201105_sf <- klm_201105_sf |> 
  dplyr::filter(Latitude >0)
mapview(klm_201105_sf)

# make line between points
klm_201105_sf |>
  dplyr::summarise(do_union = F) |>
  st_cast("LINESTRING") -> klm_201105_sf_line

mapview(klm_201105_sf) + mapview(klm_201105_sf_line)










