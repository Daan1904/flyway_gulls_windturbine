#Check for new packages
renv::restore()

#libraries
library(tidyverse)
library(lubridate)


# show tibbles with all the columns!
options(dplyr.width = Inf)

#######################################################
### Read file 1
wind <- read.csv("C:/Users/daank/OneDrive - University of Twente/Documents/Rijksuniversiteit Groningen/Year 1 - 24-25/Flyway Ecology/Mini project - Gull migration wind farms/Global-Wind-Power-Tracker-June-2024.csv")
wind



