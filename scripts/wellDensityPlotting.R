# script to plot EPA-NHU estimates of 2010 well density in Texas census blocks

remove( list = ls() )

library(sf)
library(tidyverse)
library(raster)
library(here)
library(dplyr)
library(rgdal)

# import the EPA-NHU estimates of 2010 well destinities at block level in TX
wellDensEst <- read.csv( "data//Well_Estimates//Blocks By State//final_estimates_blocks_TX.csv" ) %>% 
  mutate( GEOID10 = as.character( GEOID ) )

# Import the 2010 boundaries and join well estimates to block shapefile
sf <- st_read( "data//shapefiles//TX_block_2010.shp" ) %>%
  dplyr::select( GEOID10, STATEFP10, COUNTYFP10, ALAND10, AWATER10 ) %>% 
  # st_transform(crs = 2163) %>% 
  left_join( wellDensEst )

# Export prepared data to a geopackage
# st_write( sf, "data//geopackage//wellDensEst_TX_blk_2010.gpkg", "wellDensEst_TX_blk_2010", driver = "GPKG")

st_write( sf, dsn = "data//shapefiles//wellDensEst_TX_blk_2010.shp" )

#####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# template code from "01_dataPrep.R"

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
# # If downloaded all at once, tihs table should contain both housing units and population
# tblHU_POP10 <- read.csv(here("data/tables/nhgis_ds172_2010_blck_grp.csv"))%>%
#   dplyr::select(GISJOIN,H7V001,IFC001)
# colnames(tblHU_POP10) <- c("GISJOIN","Population","Housing_Units")
# 
# bg2010 <- st_read(here("data/shapefiles/US_blck_grp_2010.shp"))%>%
#   dplyr::select(GISJOIN,STATEFP10,COUNTYFP10,ALAND10,AWATER10)%>%
#   left_join(tblHU_POP10)%>%
#   mutate(YEAR = "2010")%>%
#   dplyr::select(GISJOIN,YEAR,STATEFP10,COUNTYFP10,ALAND10,AWATER10,
#                 Population,Housing_Units)
# 
# # Export prepared data to a geopackage
# st_write(bg2010, here("data/geopackage/nhgis_block_groups.gpkg"),"US_block_groups_2010",driver = "GPKG")