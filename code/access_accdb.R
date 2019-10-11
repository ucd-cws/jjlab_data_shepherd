# access_accdb

# this may only work on windows computer:
# assuming you are connected via the VPN to the server:

library(odbc)
library(DBI)
#library(dbplyr)
suppressPackageStartupMessages(library(dplyr))

#con <- dbConnect(odbc::odbc(), .connection_string = "Driver={Microsoft Access Driver (*.mdb, *.accdb)};DB=./data/JJLAB_DB_active.accdb")


#acclink <- "/Volumes/rapeek/public/JJLAB_DB/JJLAB_DB_active.accdb"

# cs <- "Driver=Microsoft Access Driver (*.mdb, *.accdb);DBQ=~/Downloads/JJLAB_DB_active.accdb"
# con <- dbConnect(odbc::odbc(), .connection_string = cs)
# DBI::dbListTables(con)
# tbl(con, "Table1") %>% select(Place) %>% collect 

