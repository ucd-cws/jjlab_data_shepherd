# Read DATABASE IN Access (.mdb) from R

library(tidyverse)
library(fs)
library(lubridate)
library(janitor)

# WINDOWS: CONNECT TO MDB -------------------------------------------------

library(RODBC)
library(DBI)
library(odbc)

# this won't work if the file is in use...so best to use a local copy or backup copy
# db_con <- "X:/jj_lab/database/JJLAB_DB_v1.1backend_dbm.mdb"

# CHANGE TO FULL PATH FOR FILE YOU'RE USING (LOCAL COPY OR RECENT BACKUP)
db_con <- "X:/jj_lab/database/backups/JJLAB_DB_v1.1backend_backup_20200226_2.mdb"

# check drivers here: odbcListDrivers:
# should see Microsoft Access Driver (.mdb, .accdb) with a few different things

# connect to the DB
accdb_con <- dbConnect(drv = odbc(), .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",db_con,";"))

# list table names in DB:
dbListTables(accdb_con)

# list tables that start with "s"
dbListTables(accdb_con, table_name="s%")

# read table in
coll_info <- DBI::dbReadTable(conn = accdb_con, name = "collection_info")

# Using lubridate to fix dates
coll_info$collect_date <- ymd(coll_info$collect_date)
summary(coll_info$collect_date)

# standardize all the col names:
coll_info <- janitor::clean_names(coll_info)

# Read in Samples Table ---------------------------------------------------

# get single table
samples <- DBI::dbReadTable(conn = accdb_con, name = "samples")
samples %>% group_by(lab_id) %>% distinct() %>% tally()

# Get Sites ---------------------------------------------------------------

# read in sites table
sites <- DBI::dbReadTable(conn = accdb_con, name = "sites")

# look at the tally of different data types
sites %>% group_by(site_type) %>% tally() # so need to update these to reflect difference in spelling

# fix the site.type discrepancies
sites <- sites %>% 
  mutate(site_type = case_when(
    grepl("stationary", ignore.case = TRUE, site_type) ~ "stationary",
    grepl("trawl", ignore.case = TRUE, site_type) ~ "trawl",
    grepl("unknown", ignore.case = TRUE, site_type) ~ "unknown",
    TRUE ~ site_type
  ))

# double check and view group tallies
sites %>% group_by(site_type) %>% tally() # yay
sites %>% group_by(region) %>% tally()
sites %>% group_by(project_code) %>% tally()

# Make a Map of Sites by Site.Type --------------------------------------------------------

library(sf)
library(mapview)

# make data into sf layer
sites_sf <- sites %>% 
  filter(!is.na(site_long)) %>%  # need to remove all NAs 
  st_as_sf(coords = c("site_long","site_lat"), remove = F, crs=4326)

sites_sf %>% group_by(project_code) %>% tally()
names(sites_sf)

# view
mapview(sites_sf, zcol="site_type", layer.name="Site Type")
