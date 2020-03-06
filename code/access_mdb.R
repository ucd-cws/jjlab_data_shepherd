# access .mdb from R

library(Hmisc)
library(tidyverse)
library(fs)


# set path to .mdb
#mdblink <- as_fs_path("~/Downloads/JJLAB_DB_v1.1_backend_mdb.mdb") # local version

# on macosx: 
# "/Volumes/jj_lab/database/JJLAB_DB_v1.1backend_dbm.mdb" or "/Volumes/projects3/jj_lab/database/JJLAB_DB_v1.1backend_dbm.mdb"

mdblink <- as_fs_path("/Volumes/jj_lab/database/JJLAB_DB_v1.1backend_dbm.mdb") # through VPN

# see table names:
mdb.get(mdblink, tables=TRUE)

# get single table
coll_info <- mdb.get(mdblink, tables="collection_info", stringsAsFactors=F) 

# drop the "attributes" component that gets added to the dataframe (annoying but won't cause trouble)

# Here's a function to remove the attrs:
clear.labels <- function(x) {
  if(is.list(x)) {
    for(i in 1 : length(x)) class(x[[i]]) <- setdiff(class(x[[i]]), 'labelled') 
    for(i in 1 : length(x)) attr(x[[i]],"label") <- NULL
  }
  else {
    class(x) <- setdiff(class(x), "labelled")
    attr(x, "label") <- NULL
  }
  return(x)
}

# now run the function
coll_info<- clear.labels(coll_info)

# Make the Datetime R Friendly --------------------------------------------

library(lubridate)

# Using lubridate::mdy_hms()
coll_info$collect.date <- as.Date(mdy_hms(coll_info$collect.date))
summary(coll_info$collect.date)

# get rid of standard date in the collect.time (this should only HH:MM)
coll_info <- coll_info %>% tidyr::separate(collect.time, into=c("date_old", "colltime24"), " ") %>% 
  select(-date_old) # then drop the weird standard date

# make a final datetime col
coll_info$collect.datetime <- ymd_hms(paste0(coll_info$collect.date, " ", coll_info$colltime24))

# reorder cols:
coll_info <- coll_info %>% select(lab.row.no:collect.date, collect.datetime, colltime24, water.year.cap, 
                                  run:collect.qaqc)

# check and see!
summary(coll_info)


# Get Sites ---------------------------------------------------------------

# read in sites table
sites <- mdb.get(mdblink, tables="sites", stringsAsFactors=F) 
sites<- clear.labels(sites)

# look at the tally of different data types
sites %>% group_by(site.type) %>% tally() # so need to update these to reflect difference in spelling

# fix the site.type discrepancies
sites <- sites %>% 
  mutate(site.type = case_when(
    grepl("stationary", ignore.case = TRUE, site.type) ~ "stationary",
    grepl("trawl", ignore.case = TRUE, site.type) ~ "trawl",
    grepl("unknown", ignore.case = TRUE, site.type) ~ "unknown",
    TRUE ~ site.type
  ))

# double check and view group tallies
sites %>% group_by(site.type) %>% tally() # yay
sites %>% group_by(region) %>% tally()
sites %>% group_by(project.code) %>% tally()

# Make a Map of Sites by Site.Type --------------------------------------------------------

library(sf)
library(mapview)

# make data into sf layer
sites_sf <- sites %>% 
  filter(!is.na(site.long)) %>%  # need to remove all NAs 
  st_as_sf(coords = c("site.long","site.lat"), remove = F, crs=4326)

sites_sf %>% group_by(project.code) %>% tally()
names(sites_sf)

# view
mapview(sites_sf, zcol="site.type", layer.name="Site Type")
