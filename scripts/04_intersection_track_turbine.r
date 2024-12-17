
#libraries
library(dplyr)
library(tidyverse)
library(sf)
library(mapview)
library(lubridate)

# show tibbles with all the columns!
options(dplyr.width = Inf)

######## Read file (one example) 
klm_211282 <- read_csv("C:/Users/jeroe/OneDrive/Documenten/RUG/Master/FE Flyway Ecology/Mini_project_windturbines/tracks/211282_010821-310322.txt")
klm_211307 <- read_csv("C:/Users/jeroe/OneDrive/Documenten/RUG/Master/FE Flyway Ecology/Mini_project_windturbines/tracks/211307_010821-310322.txt")
klm_211093 <- read_csv("C:/Users/jeroe/OneDrive/Documenten/RUG/Master/FE Flyway Ecology/Mini_project_windturbines/tracks/201093_20220331_134700.csv")
klm_211096 <- read_csv("C:/Users/jeroe/OneDrive/Documenten/RUG/Master/FE Flyway Ecology/Mini_project_windturbines/tracks/201096_20220331_000000.csv")
klm_211097 <- read_csv("C:/Users/jeroe/OneDrive/Documenten/RUG/Master/FE Flyway Ecology/Mini_project_windturbines/tracks/201097_20220331_000000.csv")
klm_211098 <- read_csv("C:/Users/jeroe/OneDrive/Documenten/RUG/Master/FE Flyway Ecology/Mini_project_windturbines/tracks/201098_20220331_000000.csv")
klm_211103 <- read_csv("C:/Users/jeroe/OneDrive/Documenten/RUG/Master/FE Flyway Ecology/Mini_project_windturbines/tracks/201103_20220331_000000.csv")
klm_211110 <- read_csv("C:/Users/jeroe/OneDrive/Documenten/RUG/Master/FE Flyway Ecology/Mini_project_windturbines/tracks/201110_20220331_000000.csv")
klm_211112 <- read_csv("C:/Users/jeroe/OneDrive/Documenten/RUG/Master/FE Flyway Ecology/Mini_project_windturbines/tracks/201112_20220331_000000.csv")
klm_211113 <- read_csv("C:/Users/jeroe/OneDrive/Documenten/RUG/Master/FE Flyway Ecology/Mini_project_windturbines/tracks/201113_20220331_000000.csv")

windturbines <- read_sf("C:/Users/jeroe/OneDrive/Documenten/RUG/Master/FE Flyway Ecology/Mini_project_windturbines/export.kml") 

# make dates
klm_211282$UTC_datetime <- dmy_hms(klm_211282$UTC_datetime)
klm_211307$UTC_datetime <- dmy_hms(klm_211307$UTC_datetime) 
klm_211093$UTC_datetime <- dmy_hms(klm_211093$UTC_datetime)

# make spatial
klm_211282_sf <- st_as_sf(x = klm_211282, 
                   coords = c("Longitude2", "Latitude2"),
                   crs = 4326) 
klm_211307_sf <- st_as_sf(x = klm_211307, 
                   coords = c("Longitude2", "Latitude2"),
                   crs = 4326)
klm_211093_sf <- st_as_sf(x = klm_211093,
                   coords = c("Longitude", "Latitude"),
                   crs = 4326)
klm_211096_sf <- st_as_sf(x = klm_211096,
                   coords = c("Longitude", "Latitude"),
                   crs = 4326)
klm_211097_sf <- st_as_sf(x = klm_211097,
                   coords = c("Longitude", "Latitude"),
                   crs = 4326)
klm_211098_sf <- st_as_sf(x = klm_211098,
                   coords = c("Longitude", "Latitude"),
                   crs = 4326)
klm_211103_sf <- st_as_sf(x = klm_211103,
                   coords = c("Longitude", "Latitude"),
                   crs = 4326)
klm_211110_sf <- st_as_sf(x = klm_211110,
                   coords = c("Longitude", "Latitude"),
                   crs = 4326)
klm_211112_sf <- st_as_sf(x = klm_211112,
                   coords = c("Longitude", "Latitude"),
                   crs = 4326)
klm_211113_sf <- st_as_sf(x = klm_211113,
                   coords = c("Longitude", "Latitude"),
                   crs = 4326)

# remove empty points (Latitude = 0)
klm_211282 <- klm_211282 %>% filter(Latitude2 >0)
klm_211282_sf <- klm_211282_sf %>% filter(Latitude >0)
mapview(klm_211282_sf)

klm_211307 <- klm_211307 %>% filter(Latitude2 >0)
klm_211307_sf <- klm_211307_sf %>% filter(Latitude >0)
mapview(klm_211307_sf)


############################################
#Loading of overall combined data set
#WE CAN FILTER THIS DATA SET TO ONLY INCLUDE THE IBERIAN DATA POINTS!!!! MAKES THE FILE MUCH SMALLER
combined_data_csv <- read_csv("C:/Users/daank/OneDrive - University of Twente/Documents/Rijksuniversiteit Groningen/Year 1 - 24-25/Flyway Ecology/Mini project - Gull migration wind farms/Bird tracks/combined_gps_tracks.csv") |>
  #Filter for the Iberian region only, latitude between 45.1 and 34.8 and longitude between -10.2 and 4.9
  dplyr::filter(Latitude > 34.8, Latitude < 45.1, Longitude > -10.2, Longitude < 4.9) |>
  dplyr::filter(Latitude > 0) #Filter out the empty points

#Make the CSV file spatial
combined_data_csv_sf <- st_as_sf(x = combined_data_csv, 
                                 coords = c("Longitude", "Latitude"),
                                 crs = 4326)



############################################
#Original script

# make line between points STILL REQUIRE A LINE PER DEVICE_ID!!!!
combined_data_csv_sf_line <- combined_data_csv_sf %>%
  group_by(device_id) %>%          # Group data by bird ID
  summarise(do_union = FALSE) %>%  # Prevent merging of geometries
  st_cast("LINESTRING")            # Cast points to lines within each group 

# make buffer around windturbines
windturbines_100m <- st_buffer(windturbines, 200)
windturbines_1000m <- st_buffer(windturbines, 1000)
windturbines_2000m <- st_buffer(windturbines, 2000)

combined_data_csv_sf_10000 <- st_buffer(combined_data_csv_sf_line, 10000)

# intersection between 2000m buffer of individual and windturbines
#DONT WE NEED HERE TO USE THE BUFFERED WINDTURBINES DATA HERE???
intersection_10000 <- st_intersection(combined_data_csv_sf_10000, windturbines)

#### plot the interactive maps with individual tracks and wind turbines ####
combined_data_csv_sf$device_id <- as.factor(combined_data_csv_sf$device_id)
combined_data_csv_sf_line$device_id <- as.factor(combined_data_csv_sf_line$device_id)
mapview(combined_data_csv_sf, zcol = "device_id") + mapview(combined_data_csv_sf_line, zcol = "device_id")

mapview(combined_data_csv_sf_10000, , map.types = "CartoDB.Positron") + # map.types sets the theme to light theme
  mapview(windturbines, col.regions = "red", color = "red",alpha = 0.2, cex = 2)+ 
  mapview(intersection_10000_1)

#WHAT DOES THIS PART DO???
windturbines_combined <- rbind(intersection_10000_1, intersection_10000_2)
mapview(windturbines_combined


