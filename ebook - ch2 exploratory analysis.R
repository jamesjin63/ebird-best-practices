############################################################################################
###                      Introduction to eBird Best Practices                            ###
###                     working through exercises in the eBook                           ### 
###     from https://cornelllabofornithology.github.io/ebird-best-practices/intro.html   ###
############################################################################################

###~~~ Chapter 2 con't: Exploratory analysis and data viz ~~~###
  #Note that in theory you would go straight from object produced in Chapter 1 and early part of Ch 2
  #But to make sure we are using the same data as the eBook, we'll remove those and load their data

### 1. Re-start R, load packages, and load ebird data 
# load packages
library(lubridate)
library(sf)
library(gridExtra)
library(tidyverse)

# Load downloaded data
ebird<- read_csv("data/ebd_woothr_june_bcr27_zf.csv")

### 2. Spatial data
  # Aparently the eBird people downloaded this fron Natural Earth (https://www.naturalearthdata.com/downloads/)
    #which is a seriously amazing resource for mapping

#TODO: figure how how to download a similar map for my AIV data

# Load and project the gis data
ebd_map_proj <- st_crs(102003) #Note I change the name compared to the ebook, as otherwise I'm overwriting a package. Bad bad bad

ne_land <- read_sf("data/gis-data.gpkg", "ne_land") %>% #I believe the "ne_land" is a particular layer from the .gpkg file
  st_transform(crs = ebd_map_proj) %>% 
  st_geometry()

bcr <- read_sf("data/gis-data.gpkg", "bcr") %>% 
  st_transform(crs = ebd_map_proj) %>% 
  st_geometry()

ne_country_lines <- read_sf("data/gis-data.gpkg", "ne_country_lines") %>% 
  st_transform(crs = ebd_map_proj) %>% 
  st_geometry()

ne_state_lines <- read_sf("data/gis-data.gpkg", "ne_state_lines") %>% 
  st_transform(crs = ebd_map_proj) %>% 
  st_geometry()

# Prepare ebird data for mapping
ebird_sf <- ebird %>% 
  #convert to spatial points
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
  st_transform(crs = ebd_map_proj) %>% 
  select(species_observed)

###Map
#Set up plot margins in default plotting device
par(mar = c(0.25, 0.25, 0.25, 0.25))
  
#Set up plot area
plot(st_geometry(ebird_sf), col = NA)

#Contextual gis data
plot(ne_land, col = "#dddddd", border = "#888888", lwd = 0.5, add = T)
plot(bcr, col = "#cccccc", border = NA, add = T)
plot(ne_state_lines, col = "#ffffff", lwd = 0.75, add = TRUE)
plot(ne_country_lines, col = "#ffffff", lwd = 1.5, add = TRUE)

#ebird observations
  #not observed
plot(st_geometry(ebird_sf),
     pch = 19, cex = 0.1, col = alpha("#555555", 0.25),
     add = TRUE)
  #observed
plot(filter(ebird_sf, species_observed) %>% st_geometry(),
     pch = 19, cex = 0.3, col = alpha("#4daf4a", 1),
     add = TRUE)

#legend
legend("bottomright", bty = "n",
       col = c("#555555", "#4daf4a"),
       legend = c("eBird checklists", "Wood Thrush sightings"),
       pch = 19)
box()
par(new = TRUE, mar = c(0, 0, 3, 0))
title("Wood Thrush eBird Observations\nJune 2010-2019, BCR 27")

### 2. Exploring time of day observer bias
# summarize data by hourly bins
breaks <- 0:24
labels <- breaks[-length(breaks)] + diff(breaks) / 2
ebird_tod <- ebird %>% 
  mutate(tod_bins = cut(time_observations_started, 
                        breaks = breaks, 
                        labels = labels,
                        include.lowest = TRUE),
         tod_bins = as.numeric(as.character(tod_bins))) %>% 
  group_by(tod_bins) %>% 
  summarise(n_checklists = n(),
            n_detected = sum(species_observed),
            det_freq = mean(species_observed))

# histogram of number of checklists across 24hour day
g_tod_hist <- ggplot(ebird_tod) +
  aes(x = tod_bins, y = n_checklists) +
  geom_col(width = mean(diff(breaks)), color = "grey30", fill = "grey50") +
  scale_x_continuous(breaks = seq(0, 24, by = 3), limits = c(0, 24)) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Hours since midnight",
       y = "# checklists",
       title = "Distribution of observation start times")

# frequency of detection
g_tod_freq <- ggplot(ebird_tod %>% filter(n_checklists > 100)) +
  aes(x = tod_bins, y = det_freq) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(0, 24, by = 3), limits = c(0, 24)) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Hours since midnight",
       y = "% checklists with detections",
       title = "Detection frequency")

# combine
grid.arrange(g_tod_hist, g_tod_freq)


### 3. Exploring checklist duration
# summarize data by 30 minute bins
breaks <- seq(0, 5, by = 0.5)
labels <- breaks[-length(breaks)] + diff(breaks) / 2
ebird_dur <- ebird %>% 
  mutate(dur_bins = cut(duration_minutes / 60, 
                        breaks = breaks, 
                        labels = labels,
                        include.lowest = TRUE),
         dur_bins = as.numeric(as.character(dur_bins))) %>% 
  group_by(dur_bins) %>% 
  summarise(n_checklists = n(),
            n_detected = sum(species_observed),
            det_freq = mean(species_observed))

# histogram of number of checklists per duration
g_dur_hist <- ggplot(ebird_dur) +
  aes(x = dur_bins, y = n_checklists) +
  geom_col(width = mean(diff(breaks)), color = "grey30", fill = "grey50") +
  scale_x_continuous(breaks = 0:5) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Checklist duration (hours)",
       y = "# checklists",
       title = "Distribution of checklist durations")

# frequency of detection per duration
g_dur_freq <- ggplot(ebird_dur %>% filter(n_checklists > 100)) +
  aes(x = dur_bins, y = det_freq) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = 0:5) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Checklist duration (hours)",
       y = "% checklists with detections",
       title = "Detection frequency")

# combine
grid.arrange(g_dur_hist, g_dur_freq)


### 4. Exploring distance travelled
    #Don't forget we've already truncated data to those travelling LESS than 5km!!!
# summarize data by 500m bins
breaks <- seq(0, 5, by = 0.5)
labels <- breaks[-length(breaks)] + diff(breaks) / 2
ebird_dist <- ebird %>% 
  mutate(dist_bins = cut(effort_distance_km, 
                         breaks = breaks, 
                         labels = labels,
                         include.lowest = TRUE),
         dist_bins = as.numeric(as.character(dist_bins))) %>% 
  group_by(dist_bins) %>% 
  summarise(n_checklists = n(),
            n_detected = sum(species_observed),
            det_freq = mean(species_observed))

# histogram
g_dist_hist <- ggplot(ebird_dist) +
  aes(x = dist_bins, y = n_checklists) +
  geom_col(width = mean(diff(breaks)), color = "grey30", fill = "grey50") +
  scale_x_continuous(breaks = 0:5) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Distance travelled (km)",
       y = "# checklists",
       title = "Distribution of distance travelled")

# frequency of detection
g_dist_freq <- ggplot(ebird_dist %>% filter(n_checklists > 100)) +
  aes(x = dist_bins, y = det_freq) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = 0:5) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Distance travelled (km)",
       y = "% checklists with detections",
       title = "Detection frequency")

# combine
grid.arrange(g_dist_hist, g_dist_freq)

  #Based on this data exploration, they feel it is appropriate to summarize land cover within 2.5km
  #As the majority of checklists are short (less than 0.5km)
  #I really need to look at this for AIV data if I merge together!!!!

### 5. Exploring number of observers
  #Remember we have already restricted to those with 10 or less observers!!
# summarize data
breaks <- 0:10
labels <- 1:10
ebird_obs <- ebird %>% 
  mutate(obs_bins = cut(number_observers, 
                        breaks = breaks, 
                        label = labels,
                        include.lowest = TRUE),
         obs_bins = as.numeric(as.character(obs_bins))) %>% 
  group_by(obs_bins) %>% 
  summarise(n_checklists = n(),
            n_detected = sum(species_observed),
            det_freq = mean(species_observed))

# histogram
g_obs_hist <- ggplot(ebird_obs) +
  aes(x = obs_bins, y = n_checklists) +
  geom_col(width = mean(diff(breaks)), color = "grey30", fill = "grey50") +
  scale_x_continuous(breaks = 1:10) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "# observers",
       y = "# checklists",
       title = "Distribution of the number of observers")

# frequency of detection
g_obs_freq <- ggplot(ebird_obs %>% filter(n_checklists > 100)) +
  aes(x = obs_bins, y = det_freq) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = 1:10) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "# observers",
       y = "% checklists with detections",
       title = "Detection frequency")

# combine
grid.arrange(g_obs_hist, g_obs_freq)


### Clean up workspace
rm(ne_country_lines, ne_land, ne_state_lines, bcr)
rm(breaks, labels, ebird_dist, ebird_dur, ebird_obs, ebird_tod)
rm(g_dist_freq, g_dist_hist, g_dur_freq, g_dur_hist, g_obs_freq, g_obs_hist, g_tod_freq, g_tod_hist)
