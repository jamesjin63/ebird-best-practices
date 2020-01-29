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

### 2. Figure out which tiles we need to download from MODIS to cover our area of interest