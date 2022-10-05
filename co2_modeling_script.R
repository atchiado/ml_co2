library(tidyverse)
library(rsample)
library(recipes)
library(caret)


## LOAD DATA ----------
ml_co2_data <- read.csv(file = "~/Desktop/work/data/r/ml_co2/CO2_emission.csv")


## DATA PREP ----------
co2_data <- ml_co2_data %>%
  select(-c(Model_Year, Make, Model)) %>%
  mutate_if(is.ordered, factor, ordered = FALSE)

set.seed(123)

# split data into training and test sets
co2_split <- createDataPartition(y = co2_data$Vehicle_Class, p = 0.7, list = FALSE)
co2_train <- co2_data[co2_split, ]
co2_test <- co2_data[-co2_split, ]


