#Check for new packages
renv::restore()

#libraries
library(tidyverse)
library(lubridate)


# show tibbles with all the columns!
options(dplyr.width = Inf)

#######################################################
### Read file
wind <- read.csv("C:/Users/daank/OneDrive - University of Twente/Documents/Rijksuniversiteit Groningen/Year 1 - 24-25/Flyway Ecology/Mini project - Gull migration wind farms/Global-Wind-Power-Tracker-June-2024.csv") |>
  dplyr::select(Country.Area, Project.Name, Capacity..MW., Installation.Type, Status, Start.year,
                Latitude, Longitude, Location.accuracy, Wiki.URL) |>
  dplyr::filter(Start.year <= 2024, Status == "operating") |>
  as_tibble()
wind

### Save file
write.csv(wind, "C:/Users/daank/OneDrive - University of Twente/Documents/Rijksuniversiteit Groningen/Year 1 - 24-25/Flyway Ecology/Mini project - Gull migration wind farms/windfarm_loc.csv", row.names = FALSE)


