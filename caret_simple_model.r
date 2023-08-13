##ML00NB

library(tidyverse)
library(caret)
library(mlbench)

data()

data("BostonHousing")

#rename
df <- BostonHousing

#complete date ?
complete.cases(df)
mean(complete.cases(df))

##split
split_data <- function(df, train_size = 0.8) {
  set.seed(42)
  n <- nrow(df)
  id <- sample(1:n, size = train_size*n)
  train_df <- df[id, ]
  test_df <- df[-id, ]
  list(train = train_df, test = test_df)
}

prep_data <- split_data(df)

train_data <- prep_data[[1]]
test_data <- prep_data[[2]]

##train
set.seed(42)
model <- train(medv ~ rm + b + crim,
               data = train_data,
               method = "lm")

model$finalModel

##score predict
p <- predict(model, newdata = test_data)

##evaluate
cal_mae <- function(actual, predict) {
  error <- actual - predict
  mean(abs(error))
}

cal_mse <- function(actual, predict) {
  error <- actual - predict
  mean(error**2)
}

cal_rmse <- function(actual, predict) {
  error <- actual - predict
  sqrt(mean(error**2))
}

cal_mae(test_data$medv, p)
cal_mse(test_data$medv, p)
cal_rmse(test_data$medv, p)

## save model file .RDS
saveRDS(model, "lm_model.RDS")

## when use model
model <- readRDS("lm_model.RDS")
predict(model, newdata = BostonHousing[1:20, ])

## Train Control
## Change resampling boot, LOOCV, K-fold
set.seed(42)
ctrl <- trainControl(
  method = "cv", #cv mean k-fold CV
  number = 5, #number either 5 or 10
  verboseIter = TRUE #everytime train show iteration in console
)

model <- train(medv ~ rm + b + crim,
               data = train_data,
               method = "lm",
               trControl = ctrl)

## variable importance
varImp(model)

## add preprocess data before train model, standardized
set.seed(42)
ctrl <- trainControl(
  method = "cv", #cv mean k-fold CV
  number = 5, #number either 5 or 10
  verboseIter = TRUE #everytime train show iteration in console
)

model <- train(medv ~ rm + b + crim,
               data = train_data,
               method = "lm",
               preProcess = c("center", "scale"),
               trControl = ctrl)

## K-nearest neighbour "knn"

set.seed(42)
ctrl <- trainControl(
  method = "cv", #cv mean k-fold CV
  number = 5, #number either 5 or 10
  verboseIter = TRUE #everytime train show iteration in console
)

model <- train(medv ~ rm + b + crim + lstat + age,
               data = train_data,
               method = "knn",
               preProcess = c("range", "zv", "nzv"),
               trControl = ctrl)

## hyperparameter tuning in train process

model <- train(medv ~ rm + b + crim + lstat + age,
               data = train_data,
               method = "knn",
               preProcess = c("range", "zv", "nzv"),
               tuneLength = 5,
               trControl = ctrl)

## train final model use specific K=5

model_k5 <- train(medv ~ rm + b + crim + lstat + age,
               data = train_data,
               method = "knn",
               tuneGrid = data.frame(k=5),
               preProcess = c("range", "zv", "nzv"),
               trControl = trainControl(method = "none"))

# predict train and test from model_k5

p_train <- predict(model_k5)
p_test <- predict(model_k5, newdata = test_data)

#evaluate model_k5
rmse_train <- cal_rmse(train_data$medv, p_train)
rmse_test <- cal_rmse(test_data$medv, p_test)
rmse_train; rmse_test

## tuneGrid specific 5, 7, 13 etc

set.seed(42)
ctrl <- trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE
)

model <- train(medv ~ rm + b + crim + lstat + age,
                  data = train_data,
                  method = "knn",
                  tuneGrid = data.frame(k=c(5, 7, 13)),
                  preProcess = c("center","scale"),
                  trControl = ctrl)

## Tune Metric RMSE --> Rsquared
model <- train(medv ~ rm + b + crim + lstat + age,
               data = train_data,
               method = "knn",
               metric ="Rsquared",
               tuneGrid = data.frame(k=c(5, 7, 13)),
               preProcess = c("center","scale"),
               trControl = ctrl)

## Technic trainControl repeatCV

set.seed(42)
ctrl <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 5,
  verboseIter = TRUE
)
