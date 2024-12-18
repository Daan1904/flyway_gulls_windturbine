######## Read file (one example) 
klm_211282 <- read.csv("C:/Users/daank/OneDrive - University of Twente/Documents/Rijksuniversiteit Groningen/Year 1 - 24-25/Flyway Ecology/Mini project - Gull migration wind farms/Bird tracks/211278_010821-310322.txt")
klm_211307 <- read.csv("C:/Users/daank/OneDrive - University of Twente/Documents/Rijksuniversiteit Groningen/Year 1 - 24-25/Flyway Ecology/Mini project - Gull migration wind farms/Bird tracks/201105_010821-310322.txt")


windturbines <- read_sf("C:/Users/daank/OneDrive - University of Twente/Documents/Rijksuniversiteit Groningen/Year 1 - 24-25/Flyway Ecology/Mini project - Gull migration wind farms/QGIS_FE2024/overpass_turbo_single_turbines.kml")


# make spatial
klm_211282_sf <- st_as_sf(x = klm_211282, 
                          coords = c("Longitude2", "Latitude2"),
                          crs = 4326) 
klm_211307_sf <- st_as_sf(x = klm_211307, 
                          coords = c("Longitude2", "Latitude2"),
                          crs = 4326)

# make dates
klm_211282$UTC_datetime <- dmy_hms(klm_211282$UTC_datetime)
klm_211307$UTC_datetime <- dmy_hms(klm_211307$UTC_datetime) 

# remove empty points (Latitude = 0)
klm_211282 <- klm_211282 %>% filter(Latitude2 >0)
klm_211282_sf <- klm_211282_sf %>% filter(Latitude >0)
mapview(klm_211282_sf)

klm_211307 <- klm_211307 %>% filter(Latitude2 >0)
klm_211307_sf <- klm_211307_sf %>% filter(Latitude >0)
mapview(klm_211307_sf)

# make line between points
klm_211282_sf_line <- klm_211282_sf %>%
  summarise(do_union = F) %>%
  st_cast("LINESTRING") 
klm_211307_sf_line <- klm_211307_sf %>%
  summarise(do_union = F) %>%
  st_cast("LINESTRING") 

# make buffer around windturbines
windturbines_100m <- st_buffer(windturbines, 200)
windturbines_1000m <- st_buffer(windturbines, 1000)
windturbines_2000m <- st_buffer(windturbines, 2000)

klm_211307_10000 <- st_buffer(klm_211307_sf_line, 10000)
klm_211282_10000 <- st_buffer(klm_211282_sf_line, 10000)




###########################################
# Make one long file of all separate GPS track files
library(readr)

# Specify the folder containing the GPS files
folder_path <- "C:/Users/daank/OneDrive - University of Twente/Documents/Rijksuniversiteit Groningen/Year 1 - 24-25/Flyway Ecology/Mini project - Gull migration wind farms/Bird tracks"

##CSV
# Get a list of all CSV files in the folder
file_list_csv <- list.files(path = folder_path, pattern = "*.csv", full.names = TRUE)

# Read and combine all files into one data frame
combined_data_csv1 <- file_list_csv |>
  lapply(read_csv) |>   # Read each file into a data frame
  bind_rows()           # Combine all data frames row-wise

comb_csv_coord <- combined_data_csv1 |>
  select(device_id, UTC_datetime, Latitude, Longitude, Altitude_m)

# Save the combined data to a new CSV file if needed
write_csv(combined_data_csv, "C:/Users/daank/OneDrive - University of Twente/Documents/Rijksuniversiteit Groningen/Year 1 - 24-25/Flyway Ecology/Mini project - Gull migration wind farms/Bird tracks/combined_gps_tracks.csv")

# View the combined data
head(combined_data_csv)

#TXT
file_list_txt <- list.files(path = folder_path, pattern = "*.txt", full.names = TRUE)

# Read and combine all files into one data frame
combined_data_txt <- file_list_txt |>
  lapply(read_delim, delim = ",") |>  # Adjust delimiter if needed
  bind_rows()                          # Combine all data frames row-wise

comb_txt_coord <- combined_data_txt |>
  select(device_id, UTC_datetime, Latitude2, Longitude2, Altitude_m) |>
  rename(Latitude = Latitude2, Longitude = Longitude2)

#Save file
write_csv(combined_data_txt, "C:/Users/daank/OneDrive - University of Twente/Documents/Rijksuniversiteit Groningen/Year 1 - 24-25/Flyway Ecology/Mini project - Gull migration wind farms/Bird tracks/txt_combined_gps_tracks.csv")

#Combine CSV and TXT files
# make dates for txt
comb_txt_coord$UTC_datetime <- dmy_hms(comb_txt_coord$UTC_datetime)

combined_data_coord <- bind_rows(comb_csv_coord, comb_txt_coord)

# Save the combined data to a CSV file
write_csv(combined_data_coord, "C:/Users/daank/OneDrive - University of Twente/Documents/Rijksuniversiteit Groningen/Year 1 - 24-25/Flyway Ecology/Mini project - Gull migration wind farms/Bird tracks/combined_gps_tracks_coord.csv")


#Make the CSV file spatial
combined_data_csv_sf <- st_as_sf(x = combined_data_csv, 
                             coords = c("Longitude", "Latitude"),
                             crs = 4326)


#Plotting of the csv combined file using mapview, while identifying individual birds
mapview(combined_data_csv_sf, zcol = "device_id")




