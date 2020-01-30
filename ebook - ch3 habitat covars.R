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
#MODIS::EarthdataLogin(usr = "username", pwd = "password")
  #Only need to run once???

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
tiles@tile

#TODO: to figure out the extent of the region we want data for, look at the help filt for getTile() from MODIS package
    #Can use a bbox or boundary coordinates (from Extent), which are assumed to be in EPSG:4326

# Determine the earliest year of ebird data
(begin_year <- format(min(ebird$observation_date), "%Y.01.01"))

# Determine the end date for ebird data
(end_year <- format(max(ebird$observation_date), "%Y.12.31"))



###~~~~~~~~~~~~~~~~ GDAL TROUBLESHOOTING SEGWAY ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###
# Doesn't want to work...based on advice from this website, let's try resetting paths
#https://gis.stackexchange.com/questions/183230/gdal-not-available-in-modis-r-package

# Looking into how MODIS is currently set up
MODIS::MODISoptions()
#STORAGE:
#localArcPath : /var/folders/23/xzcpx71s5ln7tm15d7_c1sy00000gn/T/RtmpdvhBU6/MODIS_ARC/ 
#outDirPath   : /var/folders/23/xzcpx71s5ln7tm15d7_c1sy00000gn/T/RtmpdvhBU6/MODIS_ARC/PROCESSED/ 

MODIS:::checkTools("GDAL")
#Checking availability of GDAL:
#  OK, GDAL 2.2.4, released 2018/03/19 found!

MODIS:::checkTools("MRT")
#Checking availability of MRT:
#  'MRT_HOME' not set/found! MRT is NOT enabled! See: 'https://lpdaac.usgs.gov/tools/modis_reprojection_tool'

Sys.getenv("PATH")
#[1] "/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/Library/Frameworks/GDAL.framework/Programs:/Library/TeX/texbin:/opt/X11/bin"


# Let's try resetting paths
MODISoptions(localArcPath = "/Users/Michelle 1/Documents/Documents/MODIS/Downloads",
             outDirPath = "/Users/Michelle 1/Documents/R projects/ebird best practices/data")
        #Changing this because the help file says its important to change asap once download MODIS package

Sys.setenv(MRT_DATA_DIR = "/Users/Michelle 1/Documents/MODIS/data",
           MRT_HOME = "/Users/Michelle 1/Documents/MODIS/bin",
           PATH = "/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/Library/Frameworks/GDAL.framework/Programs:/Library/TeX/texbin:/opt/X11/bin")

MODIS::MODISoptions(gdalPath = "/Library/Frameworks/GDAL.framework/Versions/2.2/Programs")

MODIS::MODISoptions()
MODIS:::checkTools("MRT")
MODIS:::checkTools("GDAL")
  #Seems to have worked???

# Argh...now it needs either wget or curl...but apparently this is easily remeadied
# Open Terminal (and as long as Homebrew is already install) type:
# curl --version 
# THis should confirm that curl is already installed, which is what is expected
# If want/need to install wget type:
# brew install wget

#Argh. For some reason this still isn't working. Both curl and wget are installed
  #and I'm still getting errors that sh: can't find them, ie wget: or curl: command not found
  #I have no idea how to make R find them
  #need to change the system PATH back to the original version. and change teh gdal one via options. now works.

#Note that MODIS needs an older openssl version available from:
# https://github.com/tebelorg/Tump/releases/download/v1.0.0/openssl.rb
#See the stackover flow at https://stackoverflow.com/questions/59006602/dyld-library-not-loaded-usr-local-opt-openssl-lib-libssl-1-0-0-dylib
#BUT...now my openssl for everything else is not up to date. Hmmm. Not great. Maybe see if Ian can help me 
  #fix MODIS to accept the new version???
  #It will run if I uninstall the new version and install the old version..partial fix for the moment

#Now it doesn't recognize the .hdf (= HDF4) format???? Why??? Terminal says it recognizes this file!!!!
system("gdalinfo --formats")
    #However, running this says it does not accept HDF4 as a format. Arghhhhhhhh

###~~~~~~~~~~~~~~~~ GDAL TROUBLESHOOTING SEGWAY OVER ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###


### 3. Download tiles and combine into a single raster for each year
tifs <- runGdal(product = "MCD12Q1", 
                collection = "006", 
                SDSstring = "01", 
                extent = bcr %>% st_buffer(dist = 10000), 
                begin = begin_year, 
                end = end_year, 
                outDirPath = "data", 
                job = "modis",
                MODISserverOrder = "LPDAAC") %>% 
  pluck("MCD12Q1.006") %>% 
  unlist()
