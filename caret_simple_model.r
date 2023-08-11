##ML00

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










