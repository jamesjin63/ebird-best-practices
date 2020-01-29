############################################################################################
###                      Introduction to eBird Best Practices                            ###
###                     working through exercises in the eBook                           ### 
###     from https://cornelllabofornithology.github.io/ebird-best-practices/intro.html   ###
############################################################################################

###~~~ Chapter 1 and 2: Set up and using auk with a downloaded ebd ~~~###

### 1. Set up
# load packages
library(auk)
library(lubridate)
library(sf)
library(gridExtra)
library(tidyverse)

# resolve namespace conflicts
select <- dplyr::select

# Set the relative path ebd for auk to access ebird files, 
  #in this case data/ebird in your home directory
  #note that website says to use "~/data/ebird/" as your path, which does NOT work for me,
  #but maybe that is a windows vs mac syntax thing????
auk_set_ebd_path("data/ebird") #Note that this is where you would put the downloaded, large ebd files NOT the output
    #as below, the output just goes into the "data" folder

# Setup the data directory...this is only if using auk to filter the very large ebd
  #dir.create("data", showWarnings = F)

# See Chapter 2 for how to input, filter, and then output from a very large downloaded ebd file
    #As I do not have such a file, I will just start following along in the next section
    #i.e. using the ebd output that has been filtered down to wood thrushes in june from bcr27

# Need to set up objects that link to the file paths of the output objects though
  #This is modified from the ebook, as their file path does not work 
  #If you have more subfolder, just include in the first argument (i.e. "data/ebird")
f_ebd <- file.path("data", "ebd_woothr_june_bcr27.txt")
f_sampling <- file.path("data", "ebd_checklists_june_bcr27.txt")

# Combine the EBD and the SED into R to produce ONE object with presence/absence data ("zero-filled")
ebd_zf <- auk_zerofill(f_ebd, f_sampling, collapse = T)
  #Note that during an import, two processing steps occur by default:
    #1) auk_rollup() which drops all observations not identified to species (i.e. "duck") and rolls up subspecies observations to the species level
    #2) auk_unique() which keeps only one copy of checklists imported multiple times by a group

### Chapter 2: Cleaning up imported data

#~ Data cleaning still to do: ~#
  #Change time to a decimal between 0 and 24, from a character class
  class(ebd_zf$time_observations_started)
  #Change distance travelled from a 'stationary' checklist to '0', from NA
  ebd_zf %>% 
    filter(protocol_type == "Stationary") %>% 
    summarize(min_dist = min(effort_distance_km),
              max_dist = max(effort_distance_km),
              min_area = min(effort_area_ha),
              max_area = max(effort_area_ha))
  #Convert observation counts entered with an 'X' (i.e. species present but no count data) converted to NA
  table(ebd_zf$observation_count)
#~~~                         ~~~#

# Make a function for converting time
  time_to_decimal <- function(x) {
    x <- hms(x, quiet = T)
    hour(x) + minute(x) / 60 + second(x) / 3600
  }

# Clean up variables
ebd_zf <- ebd_zf %>% 
  mutate(
    # convert X to NA
    observation_count = if_else(observation_count == "X", #condition (must be logical)
                                NA_character_,            #value if condition is true, 
                                observation_count),       #value if condition is false
    observation_count = as.integer(observation_count),
    
    # effort_distance_km to 0 for non-travelling counts
    effort_distance_km = if_else(protocol_type != "Traveling", 
                                 0, 
                                 effort_distance_km),  #Note that the effort_area_ha still stays 'NA' for stationary counts
    
    # convert time to decimal hours since midnight
    time_observations_started = time_to_decimal(time_observations_started),
    
    # split date into year and day of year
    year = year(observation_date),
    day_of_year = yday(observation_date))
      
# Additional filtering to account for variation in detectability, so limit to checklists that are:  
    #(1) under 5hr long, 
    #(2) under 5km in lenght, 
    #(3) have 10 or fewer observers, and 
    #(4) only data from the past 10 years
ebd_zf_filtered <- ebd_zf %>% 
  filter(
    # effort filters
    duration_minutes <= 5 * 60,
    effort_distance_km <= 5,
    # last 10 years of data
    year >= 2010,
    # 10 or fewer observers
    number_observers <= 10)

# Remove redundant variables
ebird <- ebd_zf_filtered %>% 
  select(checklist_id, observer_id, sampling_event_identifier,
         scientific_name,
         observation_count, species_observed, 
         state_code, locality_id, latitude, longitude,
         protocol_type, all_species_reported,
         observation_date, year, day_of_year,
         time_observations_started, 
         duration_minutes, effort_distance_km,
         number_observers)

# Write csv file with the results
  #I've already downloaded this, so no need to re-write it, but just as an FYI
#write_csv(ebird, "data/ebd_woothr_june_bcr27_zf.csv", na = "")

### Chapter 2: Tips and tricks for filtering and decreasing ebd size
# There are some good tips and tricks for how to use auk
# How to use and request custom downloads
# And other ways to decrease the size of the EBD file you need to deal with
  #and/or shorten filtering time
