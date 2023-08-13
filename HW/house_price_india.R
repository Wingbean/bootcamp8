#load library
library(tidyverse)
library(caret)
library(mlbench)
library(readxl)

# Load data0
df1<-read_excel("hpi.xlsx", sheet = 1)
df2<-read_excel("hpi.xlsx", sheet = 2)

df1 <- df1 %>%
  select(grade = `grade of the house`,
         school = `Number of schools nearby`,
         distair = `Distance from the airport`,
         lvarea = `living area`,
         bedroom = `number of bedrooms`,
         Price)

#complete date ?
complete.cases(df1)
mean(complete.cases(df1))

complete.cases(df2)
mean(complete.cases(df2))

# Split
split_data <- function(df) {
  set.seed(42)
  n <- nrow(df)
  id <- sample(1:n, size = 0.8*n)
  train_df <- df[id, ]
  test_df <- df[-id, ]
  list(train_df, test_df)
}

prep_data <- split_data(df1)
train_data <- prep_data[[1]]
test_data <- prep_data[[2]]

# Train
model <- train(Price ~ grade + school + distair + lvarea,
               data = train_data,
               method = "lm")

model$finalModel

# score predict
p <- predict(model, newdata = test_data)

# Evaluate
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

model
cal_mae(test_data$Price, p)
cal_mse(test_data$Price, p)
cal_rmse(test_data$Price, p)

varImp(model)









