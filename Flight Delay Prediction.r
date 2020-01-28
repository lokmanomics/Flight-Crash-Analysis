library(readr)
library(tibble)

flights <- read.csv2('./404528202_T_ONTIME_REPORTING/404528202_T_ONTIME_REPORTING.csv', sep=",", header=TRUE, stringsAsFactors = FALSE)
# View top of dataset
head(flights)

print(flights)

library(dplyr)

set.seed(10) #seed number is set to ensure reproducibility

flightsampled <- sample_frac(flights,0.05) #random downsample 5%

print(flightsampled) #Inspect full dataset dimension.

# Group flights 

ontime <- flightsampled[!is.na(flightsampled$ARR_DEL15) & flightsampled$ARR_DEL15!="" & !is.na(flightsampled$DEP_DEL15) & flightsampled$DEP_DEL15!="",]

ontime$DAY_OF_WEEK <- as.factor(ontime$DAY_OF_WEEK)
ontime$DISTANCE <- as.integer(ontime$DISTANCE)
ontime$CANCELLED <- as.integer(ontime$CANCELLED)
ontime$DIVERTED <- as.integer(ontime$DIVERTED)
ontime$ORIGIN <- as.factor(ontime$ORIGIN)
ontime$DEP_TIME_BLK <- as.factor(ontime$DEP_TIME_BLK)
ontime$OP_CARRIER <- as.factor(ontime$OP_CARRIER)
ontime$ARR_DEL15 <- as.factor(ontime$ARR_DEL15)
ontime$DEP_DEL15 <-as.factor(ontime$DEP_DEL15)
ontime$DEST <- as.factor(ontime$DEST)

plot(ontime$ARR_DEL15 ~ ontime$DAY_OF_WEEK)

# PART 2: TRAINING DATA

library(caret)

set.seed(13) 

# Select columns to be used in algorithm training

feature<- c("ARR_DEL15", "DAY_OF_WEEK", "OP_CARRIER", "DEST","ORIGIN","DEP_TIME_BLK")

# Created sorted version of the ontime data

ontime_sorted <- ontime[,feature] 

# Select data to put into training

training_index <- createDataPartition(ontime_sorted$ARR_DEL15, p=0.75, list=FALSE)

# Create training & testing dataset

training_data <- ontime_sorted[training_index,] 
testing_data <- ontime_sorted[training_index,] 

# METHOD 1: Logistic Regression

log_reg_mod <- train(ARR_DEL15 ~ ., data = training_data, method = "glm", family = "binomial",
                     trControl=trainControl(method = "cv", number = 5, repeats = 5))

# Predict

log_reg_predict <- predict(log_reg_mod, testing_data)

# Confusion matrix 

confusion_matrix_reg <- confusionMatrix(log_reg_predict, testing_data[,"ARR_DEL15"])
confusion_matrix_reg


