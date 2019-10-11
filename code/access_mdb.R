# access .mdb from R

library(Hmisc)
library(tidyverse)
library(fs)
library(janitor)


# set path to .mdb
mdblink <- as_fs_path("~/Downloads/JJLAB_DB_v1.1_backend_mdb.mdb")

#mdblink <- as_fs_path("/Volumes/rapeek/public/eFlows/California_Hydro_Geomorphic_Classification.mdb")

# see table names:
mdb.get(mdblink, tables=TRUE)

# get single table
samples <- mdb.get(mdblink, tables="samples") %>% 
  # clean names w janitor
  clean_names()

# view fields
names(samples)

# Bonus: Make it Spatial --------------------------------------------------

library(sf)
library(mapview)
# make data into sf layer
ref_gages_sf <- st_as_sf(ref_gages, coords = c("longdd","latdd"), remove = F, crs=4326)
names(ref_gages_sf)

# view
mapview(ref_gages_sf)
