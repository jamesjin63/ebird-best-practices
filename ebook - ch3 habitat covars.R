############################################################################################
###                      Introduction to eBird Best Practices                            ###
###                     working through exercises in the eBook                           ### 
###     from https://cornelllabofornithology.github.io/ebird-best-practices/intro.html   ###
############################################################################################

###~~~ Chapter 3: Habitat Covariates ~~~###
  #Restart with clean workspace

### 1. Getting set up with MODIS
#Load packages
library(sf) #Already loaded in Chapter 2
library(raster)
library(MODIS)
library(velox)
library(viridis)
library(tidyverse)

# Load downloaded ebird data
ebird<- read_csv("data/ebd_woothr_june_bcr27_zf.csv")

#Resolve namespace conflicts
select <- dplyr::select
projection <- raster::projection

# MODIS login and setup
MODIS::EarthdataLogin(usr = "username", pwd = "password")

###~~~~ IMPORTANT SIDENOTE: it seems that MODIS has a maskWater() function ~~~~###
  #~where you can keep land only, or land + oceancoastlines and lake shorelines + shallow inland water, and the rest becomes NA~#


###~~~ I also can't seem to install hdg4 on my laptop...so maybe just do this with desktop, where it is already installed ~~#

### 2. Figure out which tiles we need to download from MODIS to cover our area of interest
# Determine the bcr 27 boundary
bcr <- read_sf("data/gis-data.gpkg", "bcr") %>% 
  filter(bcr_code == 27) %>% 
  # project to the native modis projection
  st_transform(crs = paste("+proj=sinu +lon_0=0 +x_0=0 +y_0=0",
                           "+a=6371007.181 +b=6371007.181 +units=m +no_defs"))
    #TODO: need to figure out how to transform my data to MODIS project? Or can I just use the above?

# get list of tiles required to cover this bcr through MODIS
tiles <- getTile(bcr)

