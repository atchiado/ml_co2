library(tidyverse)
library(rsample)
library(recipes)
library(caret)
library(pROC)


## LOAD DATA ----------
ml_co2_data <- read.csv(file = "~/Desktop/work/data/r/ml_co2/CO2_emission.csv")


## DATA PREP ----------
co2_data <- ml_co2_data %>%
  select(-c(Model_Year, Make, Model)) %>%
  mutate_if(is.ordered, factor, ordered = FALSE) %>%
  filter(Vehicle_Class %in% c("Compact", "Mid-size", "Pickup truck: Standard Special", "Subcompact", "SUV: Small", "SUV: Standard"))

set.seed(123)

# split data into training and test sets
co2_split <- createDataPartition(y = co2_data$Vehicle_Class, p = 0.7, list = FALSE)
co2_train <- co2_data[co2_split, ]
co2_test <- co2_data[-co2_split, ]

# check distribution in training and test sets
prop.table(table(co2_train$Vehicle_Class)) * 100
prop.table(table(co2_test$Vehicle_Class)) * 100
prop.table(table(co2_data$Vehicle_Class)) * 100


## MODELING ----------
# training and train control
control <- trainControl(method = "repeatedcv", repeats = 3)
knn_fit <- train(Vehicle_Class ~ ., data = co2_train, method = "knn", trControl = control,
                preProcess = c("center","scale"), tuneLength = 20)

# check model accuracy
knn_fit
plot(knn_fit)
knn_prediction <- predict(knn_fit, newdata = co2_test)
confusionMatrix(knn_prediction, as.factor(co2_test$Vehicle_Class))
mean(knn_prediction == co2_test$Vehicle_Class)

# applying random forest for model improvement
rf_fit <- train(Vehicle_Class ~ ., data = co2_train, method = "rf", trControl = control,
                preProcess = c("center","scale"), tuneLength = 20)

# check model accuracy
rf_fit
plot(rf_fit)
rf_predict <- predict(rf_fit, newdata = co2_test) 
confusionMatrix(rf_predict, as.factor(co2_test$Vehicle_Class))


