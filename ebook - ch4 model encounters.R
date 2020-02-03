############################################################################################
###                      Introduction to eBird Best Practices                            ###
###                     working through exercises in the eBook                           ### 
###     from https://cornelllabofornithology.github.io/ebird-best-practices/intro.html   ###
############################################################################################

###~~~ Chapter 4: Modeling Encounter Rates ~~~###
    #Restart with clean workspace

### 1. Getting set up 
# Load packages
library(sf)
library(raster)
library(dggridR)
library(lubridate)
library(ranger)
library(scam)  #For running GAMs that can be constrained to only increase
library(PresenceAbsence)
library(verification)
library(edarf)
library(ebirdst)
library(fields)
library(gridExtra)
library(tidyverse)

# Resolve namespace conflicts
select <- dplyr::select
projection <- raster::projection
map <- purrr::map

# Set random number seed to ensure fully repeatable results
set.seed(1)

# Setup output directory for saved results
if (!dir.exists("output")) {
  dir.create("output")
}

# eBird data
ebird <- read.csv("data/ebd_woothr_june_bcr27_zf.csv") %>% 
  mutate(year = year(observation_date))

# Modis habitat covariates
habitat <-  read.csv("data/pland-elev_location-year.csv") %>% 
  mutate(year = as.integer(year))

# combine ebird and habitat data
ebird_habitat <- inner_join(ebird, habitat, by = c("locality_id", "year"))

# Prediction surface
pred_surface <- read_csv("data/pland-elev_prediction-surface.csv")

# Lastest year of landcover data
mac_lc_year <- max(pred_surface$year)
r <- raster("data/prediction-surface.tif")

# Load GIS data for making maps
map_proj <- st_crs(102003)

ne_land <- read_sf("data/gis-data.gpkg", "ne_land") %>% 
  st_transform(crs = map_proj) %>% 
  st_geometry()

bcr <- read_sf("data/gis-data.gpkg", "bcr") %>% 
  st_transform(crs = map_proj) %>% 
  st_geometry()

ne_country_lines <- read_sf("data/gis-data.gpkg", "ne_country_lines") %>% 
  st_transform(crs = map_proj) %>% 
  st_geometry()

ne_state_lines <- read_sf("data/gis-data.gpkg", "ne_state_lines") %>% 
  st_transform(crs = map_proj) %>% 
  st_geometry()

### 2. Spatiotemporal subsampling toy example

# bounding box to generate points from
bb <- st_bbox(c(xmin = -0.1, xmax = 0.1, ymin = -0.1, ymax = 0.1), 
              crs = 4326) %>% 
  st_as_sfc() %>% 
  st_sf()

# random points
pts <- st_sample(bb, 500) %>% 
  st_sf(as.data.frame(st_coordinates(.)), geometry = .) %>% 
  rename(lat = Y, lon = X)

# contruct a hexagonal grid with ~ 5 km between cells
dggs <- dgconstruct(spacing = 5)

# for each point, get the grid cell
pts$cell <- dgGEO_to_SEQNUM(dggs, pts$lon, pts$lat)$seqnum

# sample one checklist per grid cell
pts_ss <- pts %>% 
  group_by(cell) %>% 
  sample_n(size = 1) %>% 
  ungroup()

# generate polygons for the grid cells
hexagons <- dgcellstogrid(dggs, unique(pts$cell), frame = FALSE) %>% 
  st_as_sf()

# plot
ggplot() +
  geom_sf(data = hexagons) +
  geom_sf(data = pts, size = 0.5) +
  geom_sf(data = pts_ss, col = "red") +
  theme_bw()


### 3. Spatiotemporal subsampling of eBird data

# generate hexagonal grid with ~ 5 km betweeen cells
dggs <- dgconstruct(spacing = 5)

# get hexagonal cell id and week number for each checklist
checklist_cell <- ebird_habitat %>% 
  mutate(cell = dgGEO_to_SEQNUM(dggs, longitude, latitude)$seqnum,
         year = year(observation_date),
         week = week(observation_date))

# sample only ONE checklist per grid cell per week
# and sample detection/non-detection independently (hence group by species_observed which is T or F)
ebird_ss <- checklist_cell %>% 
  group_by(species_observed, year, week, cell) %>% 
  sample_n(size = 1) %>% 
  ungroup()


### 4. Evaluate how subsampling impacts the prevalence of detections compared to non-detections 

# original data
nrow(ebird_habitat)

count(ebird_habitat, species_observed) %>% 
  mutate(percent = n / sum(n))


# after sampling
nrow(ebird_ss)

count(ebird_ss, species_observed) %>% 
  mutate(percent = n / sum(n))

# Subsampling decreases the overall number of checklists while increases the prevalence of detections 
  #(from ~4% to ~7%)
  #The increased detections is important b/c it means your random forest model will run better
  #BUT it affects your prevalence rate which is REALLY REALLY REALLY important to remember for model interpeation

### 5. Evaluating impacts of subsampling on spatial distribution of observations

# convert checklists to spatial features
all_pts <- ebird_habitat %>%  
  st_as_sf(coords = c("longitude","latitude"), crs = 4326) %>%
  st_transform(crs = map_proj) %>% 
  select(species_observed)

ss_pts <- ebird_ss %>%  
  st_as_sf(coords = c("longitude","latitude"), crs = 4326) %>%
  st_transform(crs = map_proj) %>% 
  select(species_observed)

both_pts <- list(before_ss = all_pts, after_ss = ss_pts)

# map
p <- par(mfrow = c(2, 1))

for (i in seq_along(both_pts)) {
  par(mar = c(0.25, 0.25, 0.25, 0.25))
  
  # set up plot area
  plot(st_geometry(both_pts[[i]]), col = NA)
  
  # contextual gis data
  plot(ne_land, col = "#dddddd", border = "#888888", lwd = 0.5, add = TRUE)
  plot(bcr, col = "#cccccc", border = NA, add = TRUE)
  plot(ne_state_lines, col = "#ffffff", lwd = 0.75, add = TRUE)
  plot(ne_country_lines, col = "#ffffff", lwd = 1.5, add = TRUE)
  
  # ebird observations
  
  # not observed
  plot(st_geometry(both_pts[[i]]),
       pch = 19, cex = 0.1, col = alpha("#555555", 0.25),
       add = TRUE)
  
  # observed
  plot(filter(both_pts[[i]], species_observed) %>% st_geometry(),
       pch = 19, cex = 0.3, col = alpha("#4daf4a", 0.5),
       add = TRUE)
  
  # legend
  legend("bottomright", bty = "n",
         col = c("#555555", "#4daf4a"),
         legend = c("Non-detection", "Detection"),
         pch = 19)
  box()
  par(new = TRUE, mar = c(0, 0, 3, 0))
  if (names(both_pts)[i] == "before_ss") {
    title("Wood Thrush eBird Observations\nBefore subsampling")
  } else {
    title("After subsampling")
  }
}
par(p)
# For Wood Thrush, subsampling detections and non-detections dealt with class imbalenced,
  #which is assessed by looking at prevalence rate and seeing if models can predict validation data well
# But this may not be the case for very rare species, where you might need to keep all detections
  #or even oversample the detections. See refs in ebook for more details

### 6. Random Forests to model detection/non-detection of wood thrush from covariates
  #Need to split the data into 80% for training and 20% for testing

# Split data
ebird_split <- ebird_ss %>% 
  # select only the columns to be used in the model
  select(species_observed,
         year, day_of_year,
         time_observations_started, duration_minutes,
         effort_distance_km, number_observers, 
         starts_with("pland_"),
         starts_with("elevation_")) %>% 
  drop_na()

# split 80/20
ebird_split <- ebird_split %>% 
  split(if_else(runif(nrow(.)) <= 0.8, "train", "test"))

map_int(ebird_split, nrow)

# Use ranger package to built a balanced forest, with replacement (i.e. using bootstrapping)
  #I don't understand most of the specifications, see the ebook for details
  #probability = T indicates that it will predict probabilities (vs. returning only the most probable case)

# ranger needs know the proportion of detections from the dataset
detection_freq <- mean(ebird_split$train$species_observed) #This works because it is a logical class currently

# ranger requires a factor response to do classification
ebird_split$train$species_observed <- factor(ebird_split$train$species_observed)

# grow random forest
rf <- ranger(formula =  species_observed ~ ., 
             data = ebird_split$train,
             importance = "impurity",
             probability = TRUE,
             replace = TRUE, 
             sample.fraction = c(detection_freq, detection_freq))
  #Apparently the output of ranger is a matrix of probabilities for each class, 
    #and the bit we are interested in is the probability of detections, which is the second column
  

### 7. Model calibration

# make predictions on training data
occ_pred <- rf$predictions[, 2] #Note that as predictions of encounters, these are all numeric values between 0 and 1

# convert the observered response back to a numeric value from factor
occ_obs <- ebird_split$train$species_observed %>% 
  as.logical() %>% 
  as.integer()

rf_pred_train <- tibble(obs = occ_obs, pred = occ_pred) %>% 
  drop_na()

# fit calibration model using a binomial GAM, using real observed encounter rate as the response variable
  #and predicted encounter rate as the predictor variable
  #Note they say they find a Gaussian constrained GAM to be more stable than a logistic constrained GAM
  #even though with a Gaussian curve the predicted values can be above 1 or below 0 (which is not true!)
calibration_model <- scam(obs ~ s(pred, k = 5, bs = "mpi"), 
                          gamma = 1.4,
                          data = rf_pred_train)

# calculate the average observed encounter rates for different 
# categories of estimated encounter rates 
average_encounter <- rf_pred_train %>%
  mutate(pred_cat = cut(rf_pred_train$pred, breaks = seq(0, 1, by=0.02))) %>%
  group_by(pred_cat) %>%
  summarise(pred = mean(pred), obs = mean(obs), checklist_count = n()) %>%
  ungroup()
  #note that the checklist count here is just the nrows() from the grouping variable (pred_cat), not copied from original data somehow

# plot; use this as a diagnostic tool to see how well our model is predicting compared to observed data
cal_pred <- tibble(pred = seq(0, 1, length.out = 100))

cal_pred <- predict(calibration_model, cal_pred, type = "response") %>% 
  bind_cols(cal_pred, calibrated = .)

ggplot(cal_pred) +
  aes(x = pred, y = calibrated) +
  geom_line() +
  geom_point(data = average_encounter, 
             aes(x = pred, y = obs, size = sqrt(checklist_count)),
             show.legend = FALSE, shape = 1) +
  labs(x = "Estimated encounter rate",
       y = "Observed encounter rate",
       title = "Calibration model")
  #So we can see that estimated encounters are generally larger than observed encounters,
    #as the points don't fit very tightly to the line (i.e. model is NOT well calibrated);
    #however, the model can distinguish the relative ranking well, as larger estimated encounters have larger observed encounter 
    #i.e. the points follow a line rather than forming a cloud


### 8. Model Assessment (aka model validation with 20% 'test' subset)

# predict on test data using calibrated model
p_fitted <- predict(rf, data = ebird_split$test, type = "response")

# extract probability of detection
p_fitted <- p_fitted$predictions[, 2]

# calibrate
p_calibrated <- predict(calibration_model, 
                        newdata = tibble(pred = p_fitted), 
                        type = "response")

rf_pred_test <- data.frame(id = seq_along(p_calibrated),
                           # actual detection/non-detection
                           obs = ebird_split$test$species_observed,
                           # uncalibrated prediction
                           fit = p_fitted,
                           # calibrated prediction
                           cal = p_calibrated) %>%
  # constrain probabilities to 0-1
  mutate(cal = pmin(pmax(cal, 0), 1)) %>% 
  drop_na()

# mean squared error (mse)
mse_fit <- mean((rf_pred_test$obs - rf_pred_test$fit)^2, na.rm = TRUE)
mse_cal <- mean((rf_pred_test$obs - rf_pred_test$cal)^2, na.rm = TRUE)

# pick threshold to maximize kappa
  #As several of the predictive performance metrics need the predicted probabilities to be classified into "detected" vs "not detected"
  #so we use this threshold to do so, and have decided to choose it based on a value that maximizes Kappa
opt_thresh <- optimal.thresholds(rf_pred_test, opt.methods = "MaxKappa")

# calculate accuracy metrics: auc, kappa, sensitivity, specificity, brier
metrics_fit <- rf_pred_test %>% 
  select(id, obs, fit) %>% 
  presence.absence.accuracy(threshold = opt_thresh$fit, 
                            na.rm = TRUE, 
                            st.dev = FALSE)

metrics_cal <- rf_pred_test %>% 
  select(id, obs, cal) %>% 
  presence.absence.accuracy(threshold = opt_thresh$cal, 
                            na.rm = TRUE, 
                            st.dev = FALSE)

rf_assessment <- tibble(
  model = c("RF", "Calibrated RF"),
  mse = c(mse_fit, mse_cal),
  sensitivity = c(metrics_fit$sensitivity, metrics_cal$sensitivity),
  specificity = c(metrics_fit$specificity, metrics_cal$specificity),
  auc = c(metrics_fit$AUC, metrics_cal$AUC),
  kappa = c(metrics_fit$Kappa, metrics_cal$Kappa)
)
#knitr::kable(rf_assessment, digits = 3)
print(rf_assessment, digits = 3)


### 9. Model Interpretation - Covariates: Predictor Importance
  #From the random forest model this is a measure of the predictive power of the each covariate
  #Technically it is an aveage Gini index
  #Essentially, larger values = predictor is more important to the model
  #But high importance does NOT tell us the direction of the relationship with detection (see partial dependence)

pi <- enframe(rf$variable.importance, "predictor", "importance")

# plot
ggplot(pi) + 
  aes(x = fct_reorder(predictor, importance), y = importance) +
  geom_col() +
  geom_hline(yintercept = 0, size = 2, colour = "#555555") +
  scale_y_continuous(expand = c(0, 0)) +
  coord_flip() +
  labs(x = NULL, 
       y = "Predictor Importance (Gini Index)") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.grid.major.x = element_line(colour = "#cccccc", size = 0.5))


### 9. Model Interpretation - Covariates: Partial Dependence
  #From the random forest model, and with the edarf package
  #these plots show the marginal effect of a given predictor on the encounter rate, averaged across the other predictors
  #Note that this does not display any of the interactions which are part of our random forest model
  #(apparently they are there even though we did not explicitly specify them in the RF model)

# top 9 predictors other than date
top_pred <- pi %>% 
  filter(!predictor %in% c("year", "day_of_year")) %>% 
  top_n(n = 9, wt = importance) %>% 
  arrange(desc(importance))

# calculate partial dependence for each predictor
# map is used to iteratively apply partial_dependence to each predictor
pd <- top_pred %>% 
  mutate(pd = map(predictor, partial_dependence, 
                  fit = rf, 
                  data = ebird_split$train,
                  n = c(25, 1000)),
         pd = map(pd, ~ .[, c(1, 3)]),
         pd = map(pd, set_names, nm = c("value",  "encounter_rate"))) %>% 
  unnest(cols = pd)

# calibrate predictions
pd$encounter_rate <- predict(calibration_model, 
                             newdata = tibble(pred = pd$encounter_rate), 
                             type = "response") %>% 
  as.numeric()

# constrain probabilities to 0-1
pd$encounter_rate <- pmin(pmax(pd$encounter_rate, 0), 1)

# plot
ggplot(pd) +
  aes(x = value, y = encounter_rate) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~ as_factor(predictor), nrow = 3, scales = "free") +
  labs(x = NULL, y = "Encounter Rate") +
  theme_minimal() +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.line = element_line(color = "grey60"),
        axis.ticks  = element_line(color = "grey60"))

### 10. Model Prediction
  #We are predicting for a STANDARD EBIRD CHECKLIST 
  #i.e. 1km, 1hr travelling count at peak time of detection for this particular species
