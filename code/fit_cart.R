# Details -----------------------------------------------------------------
## Script name: fit_rf_classification
##
## Purpose of script: Using the rgb bands and the various calculated indices,
## the species present in each pixel can be classified using random forest. The
## result is a raster image where each pixel is classified relative to the
## training set provided. Here, polygons were manually created over known areas
## to create the reference set.
##
## Author: Sam Woodman
##
## Date Created: 2022-11-29
##
## Email: samuel.woodman@gmail.com
##
## Inputs:
##    - ft_rgb_indices: GeoTIFF of rgb indices calculated using uavRst
##    - masked_cropped_raster_30cm: GeoTIFF of cleaned drone imagery
##    - manual_classes: GPKG of manually delineated surface clasess
##
## Outputs:
##
##
## Notes:
##    - Script developed using the following link:
##      https://rspatial.org/terra/rs/5-supclassification.html
##    - Boardwalk and Flux Tower are removed from manual_classes since these
##    are masked from the raster image and decreased the accuracy to
##    preliminary RF models
##    - Two approaches are used below in dealing with the island that is a mix
##    of both Hordeum and Bulrush:
##      - Keep the island as a separate class to be classified. This appraoch
##      aligns with Larry's observation that the island is a mix of both these
##      species but results in a less accurate model.
##      - Remove the island as a class and allow the model to predict sections
##      of the island as one of either species. This increases model accuracy
##      but may not align with real-world observations.
##    - see link below for explanations of accuracy measures
##    http://gsp.humboldt.edu/olm/Courses/GSP_216/lessons/accuracy/metrics.html
##
##
##
# Options -----------------------------------------------------------------

options(scipen = 6, digits = 4)

# Packages ----------------------------------------------------------------

library(tidyverse)
library(here)
library(terra)
library(rpart)

# Load data ---------------------------------------------------------------

## Rasters
ft_rgb_indices <- rast(here("data/processed/ft_rgb_indices.tif"))
masked_cropped_raster_30cm <- rast(here("data/processed/masked_cropped_raster_30cm.tif"))

## Vectors
classes <- vect(here("data/raw/manual_classes.gpkg"))

# Preprocess --------------------------------------------------------------

## Combine rasters
### raster dataset need to be combined so both RGB and RGB indices are in one
### object
ft_rast <- c(masked_cropped_raster_30cm, ft_rgb_indices)

## Select classes of interest
### remove unwanted classes for RF
classes_sub <- terra::subset(classes,
                             classes$name %in% c("Bulrush",
                                                 "Hordeum",
                                                 "Barren",
                                                 "Island"))

classes_sub_no_island <- terra::subset(classes,
                                       classes$name %in% c("Bulrush",
                                                           "Hordeum",
                                                           "Barren"))

## Sample classes
### number of sample points was selected arbitrarily
set.seed(1287)
classes_pnts <- spatSample(classes_sub, 50000, "random")
classes_pnts_no_island <- spatSample(classes_sub_no_island, 50000, "random")

### insepect sample distribution
as.data.frame(classes_pnts) %>%
  group_by(name) %>%
  tally()

as.data.frame(classes_pnts_no_island) %>%
  group_by(name) %>%
  tally()

## Prepare df

### convert the sample points to a matrix with coordinates for each location
sample_xy <- as.matrix(geom(classes_pnts)[,c('x','y')])
sample_xy_no_island <- as.matrix(geom(classes_pnts_no_island)[,c('x','y')])

### extract the values of the ft raster at each sample coordinate
class_df <- terra::extract(ft_rast, sample_xy)
class_df_no_island <- terra::extract(ft_rast, sample_xy_no_island)

### create df for modelling by combining raster values and manual id names
sampdata <- data.frame(class = classes_pnts$name, class_df)
sampdata_no_island <- data.frame(class = classes_pnts_no_island$name,
                                 class_df_no_island)

# Train model -------------------------------------------------------------

cartmodel <- rpart(as.factor(class)~., data = sampdata,
                   method = 'class', minsplit = 5)
print(cartmodel)
plot(cartmodel, uniform=TRUE, main="Classification Tree")
text(cartmodel, cex = 1)

cartmodel_no_island <- rpart(as.factor(class)~., data = sampdata_no_island,
                             method = 'class', minsplit = 5)
print(cartmodel_no_island)
plot(cartmodel_no_island, uniform=TRUE, main="Classification Tree")
text(cartmodel_no_island, cex = 1)

# Predict outcome ---------------------------------------------------------

classified <- predict(ft_rast, cartmodel, na.rm = TRUE)
classified
plot(classified)

classified_no_island <- predict(ft_rast, cartmodel_no_island, na.rm = TRUE)
classified_no_island
plot(classified_no_island)

# Plot prediction ---------------------------------------------------------

## Plot land cover class based on highest liklihood from cart model
### Island included
lc <- which.max(classified)
lc

cls <- c("barren","bulrush","hordeum", "island")
df <- data.frame(id = 1:4, class=cls)
levels(lc) <- df
lc

plot(lc)

### No island
lc_ni <- which.max(classified_no_island)
lc_ni

cls_ni <- c("barren","bulrush","hordeum")
df_ni <- data.frame(id = 1:3, class=cls_ni)
levels(lc_ni) <- df_ni
lc_ni

plot(lc_ni)

# Model evaluation --------------------------------------------------------

## Island included

### k-fold validataion
#### data is split into k groups and the model is refit with one group being
#### used for model testing while the remaining will be used for model training
set.seed(1967)

k <- 5 # number of folds
j <- sample(rep(1:k, each = round(nrow(sampdata))/k))
table(j)

#### train and test the model five times, each time computing the predictions
#### and storing
x <- list()
for (k in 1:5) {
  train <- sampdata[j!= k, ]
  test <- sampdata[j == k, ]
  cart <- rpart(as.factor(class)~., data=train, method = 'class',
                minsplit = 5)
  pclass <- predict(cart, test, na.rm = TRUE)
  # assign class to maximum probablity
  pclass <- apply(pclass, 1, which.max)
  # create a data.frame using the reference and prediction
  x[[k]] <- cbind(test$class, as.integer(pclass))
}

#### combine the five list elements into a single dataframe and calculate
#### confusion matrix
y <- do.call(rbind, x)
y <- data.frame(y)
colnames(y) <- c('observed', 'predicted')
# confusion matrix
conmat <- table(y)
# change the name of the classes
colnames(conmat) <- c("barren","bulrush","hordeum", "island")
rownames(conmat) <- c("barren","bulrush","hordeum", "island")
print(conmat)

### Calculate overall accuracy
n <- sum(conmat) # number of total cases/samples
diag <- diag(conmat) # number of correctly classified cases per class
OA <- sum(diag) / n # overall accuracy
OA

### Calculate kappa statistics
#### kappa represents the overall agreement between categorical datasets
rowsums <- apply(conmat, 1, sum) # observed (true) cases per class
p <- rowsums / n

colsums <- apply(conmat, 2, sum) # predicted cases per class
q <- colsums / n

expAccuracy <- sum(p*q)
kappa <- (OA - expAccuracy) / (1 - expAccuracy)
kappa

### Producer and user accuracy
PA <- diag / colsums # Producer accuracy
UA <- diag / rowsums # User accuracy
outAcc <- data.frame(producerAccuracy = PA, userAccuracy = UA)
outAcc

## No island

### k-fold validataion
#### data is split into k groups and the model is refit with one group being
#### used for model testing while the remaining will be used for model training
set.seed(1967)

k <- 5 # number of folds
j <- sample(rep(1:k, each = round(nrow(sampdata_no_island))/k))
table(j)

#### train and test the model five times, each time computing the predictions
#### and storing
x <- list()
for (k in 1:5) {
  train <- sampdata_no_island[j!= k, ]
  test <- sampdata_no_island[j == k, ]
  cart <- rpart(as.factor(class)~., data=train, method = 'class',
                minsplit = 5)
  pclass <- predict(cart, test, na.rm = TRUE)
  # assign class to maximum probablity
  pclass <- apply(pclass, 1, which.max)
  # create a data.frame using the reference and prediction
  x[[k]] <- cbind(test$class, as.integer(pclass))
}

#### combine the five list elements into a single dataframe and calculate
#### confusion matrix
y <- do.call(rbind, x)
y <- data.frame(y)
colnames(y) <- c('observed', 'predicted')
# confusion matrix
conmat <- table(y)
# change the name of the classes
colnames(conmat) <- c("barren","bulrush","hordeum")
rownames(conmat) <- c("barren","bulrush","hordeum")
print(conmat)

### Calculate overall accuracy
n <- sum(conmat) # number of total cases/samples
diag <- diag(conmat) # number of correctly classified cases per class
OA_ni <- sum(diag) / n # overall accuracy
OA_ni

### Calculate kappa statistics
#### kappa represents the overall agreement between categorical datasets
rowsums <- apply(conmat, 1, sum) # observed (true) cases per class
p <- rowsums / n

colsums <- apply(conmat, 2, sum) # predicted cases per class
q <- colsums / n

expAccuracy <- sum(p*q)
kappa_ni <- (OA - expAccuracy) / (1 - expAccuracy)
kappa_ni

### Producer and user accuracy
PA <- diag / colsums # Producer accuracy
UA <- diag / rowsums # User accuracy
outAcc_ni <- data.frame(producerAccuracy = PA, userAccuracy = UA)
outAcc_ni


# Save output -------------------------------------------------------------

writeRaster(lc, here("data/processed/ml_class_island.tif"))
writeRaster(lc_ni, here("data/processed/ml_class_no_island.tif"))
