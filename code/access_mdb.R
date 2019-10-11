# access .mdb from R

library(Hmisc)
library(tidyverse)
library(fs)
library(janitor)


# set path to .mdb
mdblink <- as_fs_path("~/Downloads/JJLAB_DB_v1.1_backend_mdb.mdb") # local version

mdblink <- as_fs_path("/Volumes/projects3/jj_lab/database/JJLAB_DB_v1.1backend_mdb.mdb") # through VPN


# see table names:
mdb.get(mdblink, tables=TRUE)

# get single table
coll_info <- mdb.get(mdblink, tables="collection_info", stringsAsFactors=F) 

# pull the data in and clean the column names at the same time
coll_info_clean <- mdb.get(mdblink, tables="collection_info") %>% 
  # clean column names w janitor (makes all spaces/uppercase/periods into lower case and underscores)
  clean_names()


# Make the Datetime R Friendly --------------------------------------------

library(lubridate)

# one way
coll_info$collect.date <- as.Date(mdy_hms(coll_info$collect.date))

summary(coll_info$collect.date)

# get rid of standard date in the collect.time 
coll_info <- coll_info %>% tidyr::separate(collect.time, into=c("date_old", "colltime24"), " ")

# make a final datetime col
coll_info$collect.datetime <- ymd_hms(paste0(coll_info$collect.date, " ", coll_info$colltime24))



# Make a Map of Samples ---------------------------------------------------

sites <- mdb.get(mdblink, tables="sites", stringsAsFactors=F) 

library(sf)
library(mapview)

# make data into sf layer
sites_sf <- sites %>% 
  filter(!is.na(site.long)) %>%  # need to remove all NAs 
  st_as_sf(coords = c("site.long","site.lat"), remove = F, crs=4326)

names(sites_sf)

# view
mapview(sites_sf)
