remove( list = ls() )

library(sf)
library(tidyverse)
library(raster)
library(here)
library(dplyr)

#####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# load data
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# load EPA estimates
epaRaw <- read.csv( "data\\Well_Estimates\\Blocks By State\\final_estimates_blocks_TX.csv" )

usgsRaw <- read.csv( "data\\USGS_REM_WellUsingPop_TX_blk.csv" )

# select just: geoid, county name, population, epa est, and usgs est
epa <- epaRaw %>% 
  dplyr::select( GEOID, County, Population_Block, Population_Served_Est ) %>% 
  rename( population = Population_Block ) %>% 
  rename( epaWellUserPop = Population_Served_Est )

usgs <- usgsRaw %>% 
  dplyr::select( GEOID10, SUM_grid_code ) %>% 
  rename( usgsWellUserPop = SUM_grid_code ) %>% 
  rename( GEOID = GEOID10 )

# join usgs to epa data
wellEstimates <- epa %>% 
  left_join( usgs, by = 'GEOID' )

wellEstimateTotals <- wellEstimates %>% 
  summarise( epaWellUserPop = sum(epaWellUserPop, na.rm = TRUE),
             usgsWellUserPop = sum(usgsWellUserPop, na.rm = TRUE) )

#####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# manipulate data
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# well estimates in small blocks
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

smBlkThrsh <- 1
wellEstSmallBlocks <- wellEstimates %>% 
  mutate( epaWellUserPop_SmBlk = ifelse( population < smBlkThrsh, epaWellUserPop, 0 ) ) %>% 
  mutate( usgsWellUserPop_SmBlk = ifelse( population < smBlkThrsh, usgsWellUserPop, 0 ) ) %>% 
  mutate( smallBlockFlag = ifelse( population < smBlkThrsh, 1, 0 ) ) %>% 
  summarise( epaWellUserPop_SmBlk = sum(epaWellUserPop_SmBlk, na.rm = TRUE),
             usgsWellUserPop_SmBlk = sum(usgsWellUserPop_SmBlk, na.rm = TRUE),
             count = sum( smallBlockFlag ) )

smBlkThrshVec <- c(1,25,50,100,200,400,800,1600,3200,6400)
epaSmBlkVec <- c()
usgsSmBlkVec <- c()

for (x in smBlkThrshVec) {
  
  smBlkThrsh <- x
  wellEstSmallBlocks <- wellEstimates %>% 
    mutate( epaWellUserPop_SmBlk = ifelse( population < smBlkThrsh, epaWellUserPop, 0 ) ) %>% 
    mutate( usgsWellUserPop_SmBlk = ifelse( population < smBlkThrsh, usgsWellUserPop, 0 ) ) %>% 
    mutate( smallBlockFlag = ifelse( population < smBlkThrsh, 1, 0 ) ) %>% 
    summarise( epaWellUserPop_SmBlk = sum(epaWellUserPop_SmBlk, na.rm = TRUE),
               usgsWellUserPop_SmBlk = sum(usgsWellUserPop_SmBlk, na.rm = TRUE),
               count = sum( smallBlockFlag ) )
  
  epaSmBlkVec <- c( epaSmBlkVec, wellEstSmallBlocks[1,1])
  usgsSmBlkVec <- c( usgsSmBlkVec, wellEstSmallBlocks[1,2])
  
}

wellEstSmallBlocksDf <- data.frame( smBlkThrshVec, epaSmBlkVec, usgsSmBlkVec )

wellEstSmallBlocksDf <- wellEstSmallBlocksDf %>% 
  mutate( error = (epaSmBlkVec - usgsSmBlkVec)/usgsSmBlkVec*100 )

# plot
ggplot( wellEstSmallBlocksDf ) +
  geom_point( aes( x = smBlkThrshVec, y = error ) ) +
  labs( x = 'Maximum Block Population', y = 'Percent Error (%)')
  

  
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# group by county
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
wellEstimatesByCounty <- wellEstimates %>% 
  group_by( County ) %>% 
  summarise( countyPop = sum(population), epaWellUserCntyPop = sum(epaWellUserPop, na.rm = TRUE),
             usgsWellUserCntyPop = sum( usgsWellUserPop, na.rm = TRUE) )

# plot at county level
wellUserCntyPop_vs_cntyPop <- ggplot(wellEstimatesByCounty) +
  geom_point( aes( x = countyPop, y = epaWellUserCntyPop ) ) +
  geom_point( aes( x = countyPop, y = usgsWellUserCntyPop ), color = 'blue' )
wellUserCntyPop_vs_cntyPop

# plot at block level
wellUserPop_vs_pop <- ggplot( wellEstimates ) +
  geom_point( aes( x = population, y = epaWellUserPop ) ) +
  geom_point( aes( x = population, y = usgsWellUserPop ), color = 'blue' ) + 
  labs( x = 'Block Population', y = 'Estimated Well Using Population' ) +
  ylim( 0, 3750 ) +
  xlim( 0, 7500 )
wellUserPop_vs_pop

wellUserPop_vs_pop_zoom <- ggplot( wellEstimates ) +
  geom_point( aes( x = population, y = epaWellUserPop ) ) +
  geom_point( aes( x = population, y = usgsWellUserPop ), color = 'blue' ) + 
  labs( x = 'Block Population', y = 'Estimated Well Using Population' ) +
  ylim( 0, 1000 ) +
  xlim( 0, 1000 )
wellUserPop_vs_pop_zoom

  


