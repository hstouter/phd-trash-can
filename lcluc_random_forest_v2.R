# Hannah Stouter
# HLS 
# May 22, 2024


setwd("/Users/hstouter/Desktop/NASA_LCLUC/HLS_NDVI_R/")

# install.packages("rgdal")
# install.packages("spatialRF")

library(sp)
library(raster)
library(terra)
library(sf)
library(dplyr)
library(tidyr)
library(randomForest)
library(caTools)
library(gridExtra)
library(compositions)
library(rpart)
library(ggplot2)
library(ggridges)
library(lattice)
library(caret)




# Load HLS raster 

HLS <- stack("HLS_clip.tif")
HLS

str(HLS)

# plot(HLS)
print(HLS)

EVI <- 2.5 * ((HLS$B5-HLS$B4) / (HLS$B5 + 6 * HLS$B4 - 7.5 * HLS$B2 + 1))
# plot(EVI)

rf_stack <- stack(HLS[[c("B1", "B2", "B3", "B4", "B5", "B6", "B7", "B9", "B10", "B11")]], EVI)
names(rf_stack)[11] <- "EVI"

#extract pixels values from HLS 
rf_stack_pixels <- getValues(rf_stack)
head(rf_stack_pixels)

# Load and merge label data 

agriculture <- st_read("labels_shp/agriculture_HLS.shp")
bare <- st_read("labels_shp/bare_HLS.shp")
built <- st_read("labels_shp/built_HLS.shp")
forest <- st_read("labels_shp/forest_HLS.shp")
oil_palm_plantation <- st_read("labels_shp/oil_palm_plantation_HLS.shp")
water <- st_read("labels_shp/water_HLS.shp")

#set CRS
agriculture <- st_set_crs(agriculture, 4326)
bare <- st_set_crs(bare, 4326)
built <- st_set_crs(built, 4326)
forest <- st_set_crs(forest, 4326)
oil_palm_plantation <- st_set_crs(oil_palm_plantation, 4326)
water <- st_set_crs(water, 4326)


# subset columns 
agriculture <- agriculture[, c("class", "geometry")]
bare <- bare[, c("class", "geometry")]
built <- built[, c("class", "geometry")]
forest <- forest[, c("class", "geometry")]
oil_palm_plantation <- oil_palm_plantation[, c("class", "geometry")]
water <- water[, c("class", "geometry")]


# create a dataframe with all the labels 
all_labels <- rbind(agriculture, bare, built, forest, oil_palm_plantation, water)
all_labels


# Convert "geometry" column into seperate x y coordinate column
coords <- st_coordinates(all_labels$geometry)

all_labels <- cbind(all_labels, coords)

print(all_labels)


# Extract information about each band for each label point 
extracted_values <- raster::extract(rf_stack, all_labels)


# Combine extracted data and label data 
all_LU_geom <- cbind(all_labels, extracted_values)



#Drop geometry column 
all_LU <- all_LU_geom %>% st_drop_geometry()
str(all_LU)


# Save output file to matrix 
write.csv(all_LU_geom, file = "labels_HLS_data.csv") #save with geometry 
write.csv(all_LU, file = "labels_HLS_data_no_geom.csv") #save without geometry 

##### CREATE THREE DATASETS TO TEST THE ACCURACY OF THE RANDOM FOREST WITH DIFFERENT NUMBERS OF LAND USE TYPES #####

# Create data set with only forest and non-forest labels 
forest <- if_else(all_LU$class == "forest", "forest", "non-forest")

# Combine forest and dataset will all land use types 
forest_LU <- cbind(forest, all_LU)

# Remove "class" columns with so there are only the "forest" "nonforest" labels 
forest_LU <- forest_LU %>% dplyr::select(-class)
str(forest_LU)

# Create dataset with only labels for "forest", "water" and "bare"
forest_water_bare <- if_else(all_LU$class == "forest"| all_LU$class == "water" | all_LU$class == "bare", all_LU$class , "non-forest")

# Create dataset with only labels for "forest", "water" and "bare"
# forest_opp <- if_else(all_LU$class == "forest"| all_LU$class == "oil_palm_plantation", all_LU$class , "non-forest")



# Combine forest and dataset will all land use types 
forest_water_bare_LU <- cbind(forest_water_bare, all_LU)
# forest_water_bare_ag_built_LU <- cbind(forest_water_bare_ag_built, all_LU)
# forest_opp_LU <- cbind(forest_opp, all_LU)

# Remove "class" columns with so there are only the "forest", "water", "bare" and "nonforest" labels 
forest_water_bare_LU <- forest_water_bare_LU %>% dplyr::select(-class)
# forest_water_bare_ag_LU <- forest_water_bare_ag_LU %>% dplyr::select(-class)
# forest_water_bare_ag_built_LU <- forest_water_bare_ag_built_LU %>% dplyr::select(-class)
# forest_opp_LU <- forest_opp_LU %>% dplyr::select(-class)


##### SUBSET DATA INTO TEST AND TRAINING ####


# Subset the data into training and testing data sets by creating a random number column for all data sets
forest_LU$random <- runif(nrow(forest_LU), 0, 1)
forest_water_bare_LU$random <- runif(nrow(forest_water_bare_LU), 0, 1)
all_LU$random <- runif(nrow(all_LU), 0, 1)


# Subset 70% of data into training dataset
train_forest <- subset(forest_LU, forest_LU$random <0.70)
train_forest_water_bare <- subset(forest_water_bare_LU, forest_water_bare_LU$random <0.70)
train_all_LU <- subset(all_LU, all_LU$random <0.70)



# Subset 30% into the test dataset
test_forest <- subset(forest_LU, forest_LU$random >0.70)
test_forest_water_bare<- subset(forest_water_bare_LU, forest_water_bare_LU$random >0.70)
test_all_LU <- subset(all_LU, all_LU$random >0.70)

# Remove random column 
train_forest <- train_forest %>% dplyr::select(-random)
train_forest_water_bare <- train_forest_water_bare %>% dplyr::select(-random)
train_all_LU <- train_all_LU %>% dplyr::select(-random)


# convert land use type column to factor  
train_forest$forest <- as.factor(train_forest$forest)
train_forest_water_bare$forest_water_bare <- as.factor(train_forest_water_bare$forest_water_bare)
train_all_LU$class <- as.factor(train_all_LU$class)


test_forest$forest <- as.factor(test_forest$forest)
test_forest_water_bare$forest_water_bare <- as.factor(test_forest_water_bare$forest_water_bare)
test_all_LU$class <- as.factor(test_all_LU$class)


##### RANDOM FOREST ##### 


# 500 trees, different number of land use classes 

# Forest and Non-forest, 500 trees
RF_forest <- randomForest(train_forest$forest~., data = train_forest)
RF_forest
varImpPlot(RF_forest, main = "Forest/Non-forest (500 trees)")

# Forest, water, bare, non-forest, 500 trees
RF_forest_water_bare <- randomForest(train_forest_water_bare$forest_water_bare~., data = train_forest_water_bare)
RF_forest_water_bare
varImpPlot(RF_forest_water_bare, main = "Forest, Water, Bare (500 trees)")

# All-land use types, 500 trees
RF_all_LU <- randomForest(train_all_LU$class~., data = train_all_LU)
RF_all_LU 
varImpPlot(RF_all_LU, main = "All land use types (500 trees)")


# 1000 trees 

# Forest and Non-forest, 1000 trees
RF_forest_1 <- randomForest(train_forest$forest~., data = train_forest, ntree = 1000)
RF_forest_1
varImpPlot(RF_forest_1)

# Forest, water, bare, non-forest, 1000 trees
RF_forest_water_bare_1 <- randomForest(train_forest_water_bare$forest_water_bare~., data = train_forest_water_bare, ntree = 1000)
RF_forest_water_bare_1
varImpPlot(RF_forest_water_bare)

# All-land use types, 1000 trees
RF_all_LU_1 <- randomForest(train_all_LU$class~., data = train_all_LU, ntree = 1000)
RF_all_LU_1 
varImpPlot(RF_all_LU_1)


# 10000 trees

# Forest and Non-forest, 10000 trees
RF_forest_10 <- randomForest(train_forest$forest~., data = train_forest, ntree = 10000)
RF_forest_10
varImpPlot(RF_forest_10)

# Forest, water, bare, non-forest, 10000 trees
RF_forest_water_bare_10 <- randomForest(train_forest_water_bare$forest_water_bare~., data = train_forest_water_bare, ntree = 10000)
RF_forest_water_bare_10
varImpPlot(RF_forest_water_bare_10)

# All-land use types, 1000 trees
RF_all_LU_10 <- randomForest(train_all_LU$class~., data = train_all_LU, ntree = 10000)
RF_all_LU_10 
varImpPlot(RF_all_LU_10)

# 10000 trees

# # Forest and Non-forest, 100,000 trees
# RF_forest_100 <- randomForest(train_forest$forest~., data = train_forest, ntree = 100000)
# RF_forest_100
# varImpPlot(RF_forest_100)
# 
# # Forest, water, bare, non-forest, 100,000 trees
# RF_forest_water_bare_100 <- randomForest(train_forest_water_bare$forest_water_bare~., data = train_forest_water_bare, ntree = 100000)
# RF_forest_water_bare_100
# varImpPlot(RF_forest_water_bare_100)
# 
# # All-land use types, 100,000 trees
# RF_all_LU_100 <- randomForest(train_all_LU$class~., data = train_all_LU, ntree = 100000)
# RF_all_LU_100
# varImpPlot(RF_all_LU_100)


# Predict model with 500 trees onto test data and accuracy assessment 

# Forest 
predict_RF_forest <- predict(RF_forest, newdata=test_forest, type="class") 

# Determine kappa accuracy 
kappa_forest <- confusionMatrix(test_forest$forest, predict_RF_forest)
kappa_forest

kappa_forest_table <- kappa_forest$table
kappa_forest_table

kappa_forest_data <- as.data.frame(kappa_forest_table)


ggplot()+
  geom_tile(aes(x = kappa_forest_data$Reference, y = kappa_forest_data$Prediction, 
                fill = kappa_forest_data$Freq))+
  theme_classic()


# Builting table with total values 
total_cols <- colSums(kappa_forest_table)
total_rows <- rowSums(kappa_forest_table)
forest_accuracy <- rbind(kappa_forest_table, 'total' = colSums(kappa_forest_table))
forest_accuracy <- cbind(forest_accuracy, 'total' = rowSums(forest_accuracy))
forest_accuracy

forest_accuracy_data <- as.data.frame(forest_accuracy)

actual <- forest_accuracy_data %>% gather(forest_accuracy_data, forest:users)

hm <- hm %>% gather(x, value, a:j)


forest_heatmap <- ggplot()+
  geom_heat()

# Producers accuracy 
producers_accuracy <- diag(kappa_forest_table) / colSums(kappa_forest_table)
forest_accuracy <- rbind(forest_accuracy, "producers" = producers_accuracy)
forest_accuracy

# Users accuracy 
users_accuracy <- diag(kappa_forest_table) / rowSums(kappa_forest_table)
forest_accuracy <- cbind(forest_accuracy, "users" = users_accuracy)

# Overall accuracy
forest_accuracy

# Forest, water, bare, non-forest 
predict_RF_forest_water_bare<- predict(RF_forest_water_bare, newdata=test_forest_water_bare, type="class") 

# Kappa accuracy 
kappa_forest_water_bare <- confusionMatrix(test_forest_water_bare$forest_water_bare, predict_RF_forest_water_bare)
kappa_forest_water_bare

kappa_forest_water_bare_table <- kappa_forest_water_bare$table
kappa_forest_water_bare_table

# Builting table with total values 
total_cols <- colSums(kappa_forest_water_bare_table)
total_rows <- rowSums(kappa_forest_water_bare_table)
forest_water_bare_accuracy <- rbind(kappa_forest_water_bare_table, 'total' = colSums(kappa_forest_water_bare_table))
forest_water_bare_accuracy <- cbind(forest_water_bare_accuracy, 'total' = rowSums(forest_water_bare_accuracy))
forest_water_bare_accuracy

# Producers accuracy 
producers_accuracy <- diag(kappa_forest_water_bare_table) / colSums(kappa_forest_water_bare_table)
producers_accuracy_round <- round(producers_accuracy, 3)
forest_water_bare_accuracy <- rbind(forest_water_bare_accuracy, "producers" = producers_accuracy_round)
forest_water_bare_accuracy

# Users accuracy 
users_accuracy <- diag(kappa_forest_water_bare_table) / rowSums(kappa_forest_water_bare_table)
users_accuracy_round <- round(users_accuracy, 3)
forest_water_bare_accuracy <- cbind(forest_water_bare_accuracy, "users" = users_accuracy_round)

forest_water_bare_accuracy

# All land use types 
predict_RF_all_LU<- predict(RF_all_LU, newdata=test_all_LU, type="class")

# Kappa accuracy 
kappa_all_LU <- confusionMatrix(test_all_LU$class, predict_RF_all_LU)
kappa_all_LU

kappa_all_LU_table <- kappa_all_LU$table
kappa_all_LU_table

all_LU_data <- as.data.frame(kappa_all_LU_table)

# Normalize to percentages
all_LU_data <- all_LU_data %>%
  mutate(Percentage = Freq / sum(Freq) * 100)

all_LU_data$Percentage <- round(all_LU_data$Percentage, digits = 0)

ggplot()+
  geom_tile(aes(x=all_LU_data$Prediction, y = all_LU_data$Reference, fill = all_LU_data$Freq))+
  geom_text(aes(x=all_LU_data$Prediction, y = all_LU_data$Reference, label = all_LU_data$Freq), color = "white")+
  labs(fill = "Percent accurate")+
  scale_fill_continuous(type = "gradient") +
  theme_classic()





# Builting table with total values 
total_cols <- colSums(kappa_all_LU_table)
total_rows <- rowSums(kappa_all_LU_table)
all_LU_accuracy <- rbind(kappa_all_LU_table, 'total' = colSums(kappa_all_LU_table))
all_LU_accuracy <- cbind(all_LU_accuracy, 'total' = rowSums(all_LU_accuracy))
all_LU_accuracy

# Producers accuracy 
producers_accuracy <- diag(kappa_all_LU_table) / colSums(kappa_all_LU_table)
producers_accuracy_round <- round(producers_accuracy, 3)
all_LU_accuracy <- rbind(all_LU_accuracy, "producers" = producers_accuracy_round)
all_LU_accuracy

# Users accuracy 
users_accuracy <- diag(kappa_all_LU_table) / rowSums(kappa_all_LU_table)
users_accuracy_round <- round(users_accuracy, 3)
all_LU_accuracy <- cbind(all_LU_accuracy, "users" = users_accuracy_round)

all_LU_accuracy





# # Forest and Oil Palm
# predict_RF_opp<- predict(RF_opp_LU, newdata=test_opp_LU, type="class") 
# table(predict_RF_opp)
# table(test_opp_LU$forest_opp)

# Predict model with 1000 trees onto test data 

# Forest 
predict_RF_forest_1 <- predict(RF_forest_1, newdata=test_forest, type="class") 
table(predict_RF_forest_1)
table(test_forest$forest)

# Forest, water, bare, non-forest 
predict_RF_forest_water_bare_1<- predict(RF_forest_water_bare_1, newdata=test_forest_water_bare, type="class") 
table(predict_RF_forest_water_bare_1)
table(test_forest_water_bare$forest_water_bare)


# All land use types 
predict_RF_all_LU_1<- predict(RF_all_LU_1, newdata=test_all_LU, type="class") 
table(predict_RF_all_LU_1)
table(test_all_LU$class)


# Predict model with 10000 trees onto test data 

# Forest 
predict_RF_forest_10 <- predict(RF_forest_10, newdata=test_forest, type="class") 
table(predict_RF_forest_10)
table(test_forest$forest)

# Forest, water, bare, non-forest 
predict_RF_forest_water_bare_10<- predict(RF_forest_water_bare_10, newdata=test_forest_water_bare, type="class") 
table(predict_RF_forest_water_bare_10)
table(test_forest_water_bare$forest_water_bare)


# All land use types 
predict_RF_all_LU_10<- predict(RF_all_LU_10, newdata=test_all_LU, type="class") 
table(predict_RF_all_LU_10)
table(test_all_LU$class)

# Predict model with 100,000 trees onto test data 

# Forest 
predict_RF_forest_100 <- predict(RF_forest_100, newdata=test_forest, type="class") 
table(predict_RF_forest_100)
table(test_forest$forest)

# Forest, water, bare, non-forest 
predict_RF_forest_water_bare_100 <- predict(RF_forest_water_bare_100, newdata=test_forest_water_bare, type="class") 
table(predict_RF_forest_water_bare_100)
table(test_forest_water_bare$forest_water_bare)


# All land use types 
predict_RF_all_LU_100<- predict(RF_all_LU_100, newdata=test_all_LU, type="class") 
table(predict_RF_all_LU_100)
table(test_all_LU$class)
# Memory error with 100,000 trees 


##### PREDICT LAND USE OVER ENTIRE AREA ####
#Extract coordinates for each cell in the HLS raster 

# get number of pixel
cell_indices <- 1:ncell(rf_stack)

# extract coordinates from each pixel 
HLS_coords <- xyFromCell(rf_stack, cell_indices)

rf_stack_dat <- as.data.frame(rf_stack)
head(rf_stack_dat)

# combine coordinates with band values 
HLS_pixels_coords <- cbind(HLS_coords, rf_stack_dat)


# convert to matrix 
HLS_pixels_coords <- as.matrix(HLS_pixels_coords)


# create an empty row for forest to match test data set
forest <- rep(NA, nrow(HLS_pixels_coords))

# rename columns to match test data set 
HLS_forest <- cbind(forest, HLS_pixels_coords)
colnames(HLS_forest)[2] <- "X"
colnames(HLS_forest)[3] <- "Y"

head(HLS_forest)

#### PREDICT FOREST ####

# 500 trees
predict_RF_HLS_forest <- predict(RF_forest, newdata=HLS_forest, type="class") 
table(predict_RF_HLS_forest)

# covert to dataframe & rename column 
forest_raster_dat <- as.data.frame(predict_RF_HLS_forest)
colnames(forest_raster_dat)[1] <- "forest"

# convert columns to numeric 
forest_raster_dat$forest <- if_else(forest_raster_dat$forest == "forest", 1, 0)
# 1 = forest 
# 0 = nonforest 

#set no data value to -1
forest_raster_dat$forest <- if_else(is.na(forest_raster_dat$forest), -1, forest_raster_dat$forest)

# convert to matrix to export as raster 
forest_raster_dat <- as.matrix(forest_raster_dat)

# Export to raster
forest_raster <- raster(forest_raster_dat)


# Set the extent, coordinate reference system and resolution to match original HLS raster 
extent(forest_raster) <- extent(HLS)
crs(forest_raster) <- crs(rf_stack)
res(forest_raster) <- res(rf_stack)

# view raster 
plot(forest_raster, main = "500 trees")

# Save output raster as geoTIFF
writeRaster(forest_raster, filename = "forest_raster_500_rfstack.tif", format = "GTiff")


# 1000 trees
predict_RF_HLS_forest_1 <- predict(RF_forest_1, newdata=HLS_forest, type="class") 
table(predict_RF_HLS_forest_1)

# covert to dataframe & rename column 
forest_raster_dat_1 <- as.data.frame(predict_RF_HLS_forest_1)
colnames(forest_raster_dat_1)[1] <- "forest"

# convert columns to numeric 
forest_raster_dat_1$forest <- if_else(forest_raster_dat_1$forest == "forest", 1, 0)
# 1 = forest 
# 0 = nonforest 

#set no data value to -1
forest_raster_dat_1$forest <- if_else(is.na(forest_raster_dat_1$forest), -1, forest_raster_dat_1$forest)

# convert to matrix to export as raster 
forest_raster_dat_1 <- as.matrix(forest_raster_dat_1)

# Export to raster
forest_raster_1 <- raster(forest_raster_dat_1)


# Set the extent, coordinate reference system and resolution to match original HLS raster 
extent(forest_raster_1) <- extent(rf_stack)
crs(forest_raster_1) <- crs(rf_stack)
res(forest_raster_1) <- res(rf_stack)

# view raster 
plot(forest_raster_1, main = "1000 Trees")

# Save output raster as geoTIFF
writeRaster(forest_raster_1, filename = "forest_raster_1_rf_stack.tif", format = "GTiff")



#### PREDICT FOREST, WATER, BARE ####

# Copy dataset and rename forest column to match test dataset for forest_water_bare
HLS_forest_water_bare <- HLS_forest 
colnames(HLS_forest_water_bare)[1] <- "class"

predict_RF_HLS_forest_water_bare <- predict(RF_forest_water_bare, newdata=HLS_forest_water_bare, type="class") 
table(predict_RF_HLS_forest_water_bare)

# covert to dataframe & rename column 
forest_water_bare_raster_dat <- as.data.frame(predict_RF_HLS_forest_water_bare)
colnames(forest_water_bare_raster_dat)[1] <- "class"


# Specify the mapping from factor levels to numeric values
factor_to_numeric <- c("forest" = 1, "water" = 2, "bare" = 3, "non-forest" = 0)

# Convert the factor column to a numeric column using the specified mapping
forest_water_bare_raster_dat <- forest_water_bare_raster_dat %>%
  mutate(class = as.numeric(factor_to_numeric[as.character(class)]))


# convert NA to -1 
forest_water_bare_raster_dat$class <- if_else(is.na(forest_water_bare_raster_dat$class), 
                                              -1, 
                                              forest_water_bare_raster_dat$class)


# 1 = forest 
# 2 = water
# 3 = bare 
# 0 = nonforest 
# -1 = no data value 



# convert to matrix to export as raster 
forest_water_bare_raster <- as.matrix(forest_water_bare_raster_dat)

# Export to raster
forest_water_bare_raster <- raster(forest_water_bare_raster)


# Set the extent, coordinate reference system and resolution to match original HLS raster 
extent(forest_water_bare_raster) <- extent(rf_stack)
crs(forest_water_bare_raster) <- crs(rf_stack)
res(forest_water_bare_raster) <- res(rf_stack)

# view raster 
plot(forest_water_bare_raster , main = "Three land use classes (500 trees)")

# Save output raster as geoTIFF
writeRaster(forest_water_bare_raster, filename = "forest_water_bare_raster_rf_stack.tif", format = "GTiff")

### 10,000 TREES FOREST, WATER, BARE ###

# Copy dataset and rename forest column to match test dataset for forest_water_bare
HLS_forest_water_bare_10 <- HLS_forest 
colnames(HLS_forest_water_bare_10)[1] <- "class"

predict_RF_HLS_forest_water_bare_10 <- predict(RF_forest_water_bare_10, newdata=HLS_forest_water_bare, type="class") 
table(predict_RF_HLS_forest_water_bare_10)

# covert to dataframe & rename column 
forest_water_bare_raster_dat_10 <- as_data_frame(predict_RF_HLS_forest_water_bare_10)
colnames(forest_water_bare_raster_dat_10)[1] <- "class"

# Specify the mapping from factor levels to numeric values
factor_to_numeric <- c("forest" = 1, "water" = 2, "bare" = 3, "non-forest" = 0)

# Convert the factor column to a numeric column using the specified mapping
forest_water_bare_raster_dat_10 <- forest_water_bare_raster_dat_10 %>%
  mutate(class = as.numeric(factor_to_numeric[as.character(class)]))

# convert NA to -1 
forest_water_bare_raster_dat_10$class <- if_else(is.na(forest_water_bare_raster_dat_10$class), 
                                                 -1, 
                                                 forest_water_bare_raster_dat_10$class)


# 1 = forest 
# 2 = water
# 3 = bare 
# 0 = nonforest 
# -1 = no data value 



# convert to matrix to export as raster 
forest_water_bare_raster_10 <- as.matrix(forest_water_bare_raster_dat_10)

# Export to raster
forest_water_bare_raster_10 <- raster(forest_water_bare_raster_10)


# Set the extent, coordinate reference system and resolution to match original HLS raster 
extent(forest_water_bare_raster_10) <- extent(rf_stack)
crs(forest_water_bare_raster_10) <- crs(rf_stack)
res(forest_water_bare_raster_10) <- res(rf_stack)


# view raster 
plot(forest_water_bare_raster_10, main = "Three land use classes (10000 trees)")

# Save output raster as geoTIFF
writeRaster(forest_water_bare_raster_10, filename = "forest_water_bare_raster_10_rf_stack.tif", format = "GTiff")



##### PREDICT ALL LAND USE TYPES #####

# 500 trees
# Copy dataset and rename forest column to match test dataset for forest_water_bare
HLS_all_LU <- HLS_forest 
colnames(HLS_all_LU)[1] <- "class"

predict_RF_HLS_all_LU <- predict(RF_all_LU, newdata=HLS_all_LU, type="class") 
table(predict_RF_HLS_all_LU)

# covert to dataframe & rename column 
HLS_all_LU_dat <- as.data.frame(predict_RF_HLS_all_LU)
colnames(HLS_all_LU_dat)[1] <- "class"

# convert columns to numeric 
# Specify the mapping from factor levels to numeric values
factor_to_numeric_LU <- c("forest" = 1, "water" = 2, "bare" = 3, "agriculture" = 4, "built" = 5, "oil_palm_plantation" = 6,"non-forest" = 0)

# Convert the factor column to a numeric column using the specified mapping
HLS_all_LU_dat <- HLS_all_LU_dat %>%
  mutate(class = as.numeric(factor_to_numeric_LU[as.character(class)]))

HLS_all_LU_dat$class <- if_else(is.na(HLS_all_LU_dat$class), 
                                -1, 
                                HLS_all_LU_dat$class)




# 1 = forest 
# 2 = water
# 3 = bare 
# 4 = agriculture
# 5 = built
# 6 = oil_palm_plantation


# -1 = no data value 



# convert to matrix to export as raster 
HLS_all_LU_dat_raster <- as.matrix(HLS_all_LU_dat)



# Export to raster
HLS_all_LU_dat_raster <- raster(HLS_all_LU_dat_raster)


# Set the extent, coordinate reference system and resolution to match original HLS raster 
extent(HLS_all_LU_dat_raster) <- extent(HLS)
crs(HLS_all_LU_dat_raster) <- crs(HLS)
res(HLS_all_LU_dat_raster) <- res(HLS)

# view raster 
plot(HLS_all_LU_dat_raster, main = "All LU Classes (500 trees)")

# Save output raster as geoTIFF
writeRaster(HLS_all_LU_dat_raster, filename = "HLS_all_LU_dat_raster_rf_stack.tif", format = "GTiff")

all_LU_Test <- all_LU

all_LU_Test$class <- as.factor(all_LU_Test$class)


# 1000 trees 

# Copy dataset and rename forest column to match test dataset for forest_water_bare
HLS_all_LU_1 <- HLS_forest 
colnames(HLS_all_LU_1)[1] <- "class"

predict_RF_HLS_all_LU_1 <- predict(RF_all_LU_1, newdata=HLS_all_LU, type="class") 
table(predict_RF_HLS_all_LU_1)

# covert to dataframe & rename column 
HLS_all_LU_dat_1 <- as_data_frame(predict_RF_HLS_all_LU_1)
colnames(HLS_all_LU_dat_1)[1] <- "class"

# convert columns to numeric 
# Specify the mapping from factor levels to numeric values
factor_to_numeric_LU <- c("forest" = 1, "water" = 2, "bare" = 3, "agriculture" = 4, "built" = 5, "oil_palm_plantation" = 6,"non-forest" = 0)

# Convert the factor column to a numeric column using the specified mapping
HLS_all_LU_dat_1 <- HLS_all_LU_dat_1 %>%
  mutate(class = as.numeric(factor_to_numeric_LU[as.character(class)]))

HLS_all_LU_dat_1$class <- if_else(is.na(HLS_all_LU_dat_1$class), 
                                  -1, 
                                  HLS_all_LU_dat_1$class)



# 1 = forest 
# 2 = water
# 3 = bare 
# 4 = agriculture
# 5 = built
# 6 = oil_palm_plantation


# -1 = no data value 



# convert to matrix to export as raster 
HLS_all_LU_dat_1_raster <- as.matrix(HLS_all_LU_dat_1)

# Export to raster
HLS_all_LU_dat_1_raster <- raster(HLS_all_LU_dat_1_raster)


# Set the extent, coordinate reference system and resolution to match original HLS raster 
extent(HLS_all_LU_dat_1_raster) <- extent(HLS)
crs(HLS_all_LU_dat_1_raster) <- crs(HLS)
res(HLS_all_LU_dat_1_raster) <- res(HLS)

# view raster 
plot(HLS_all_LU_dat_1_raster, main = "All LU Classes (1000 trees)")

# Save output raster as geoTIFF
writeRaster(HLS_all_LU_dat_1_raster, filename = "All_LU_raster_1_rf_stack.tif", format = "GTiff")






