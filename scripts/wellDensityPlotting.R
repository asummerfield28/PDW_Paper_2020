# script to plot EPA-NHU estimates of 2010 well density in Texas census blocks

remove( list = ls() )

library(sf)
library(tidyverse)
library(raster)
library(here)
library(dplyr)
library(rgdal)

#####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# load EPA data
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# import the EPA-NHU estimates of 2010 well destinities at block level in TX
wellDensEst <- read.csv( "data//Well_Estimates//Blocks By State//final_estimates_blocks_TX.csv" ) %>% 
  mutate( GEOID10 = as.character( GEOID ) )

# Import the 2010 boundaries and join well estimates to block shapefile
# code adapted from "01_dataPrep.R"
sf <- st_read( "data//shapefiles//TX_block_2010.shp" ) %>%
  dplyr::select( GEOID10, STATEFP10, COUNTYFP10, ALAND10, AWATER10 ) %>% 
  # st_transform(crs = 2163) %>% 
  left_join( wellDensEst )

# Export prepared data to a shapefile
st_write( sf, dsn = "data//shapefiles//wellDensEst_TX_blk_2010.shp" )

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# load USGS data
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# load USGS raster (already clipped to TX)

# convert raster cells to points

# join the points from above to block shape file, group_by block, and sum
  # to yield USGS estimate for well users per block


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# analysis at multiple scales
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# pick an urban county, pick a rural county
# need TX demo and income info


