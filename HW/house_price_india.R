##HW0
#load library
library(tidyverse)
library(caret)
library(mlbench)
library(readxl)

# Load data
df1<-read_excel("hpi.xlsx", sheet = 1)
df2<-read_excel("hpi.xlsx", sheet = 2)

#complete date ?
mean(complete.cases(df1))
mean(complete.cases(df2))

# Split full df1
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
model <- train(Price ~ .,
               data = train_data[ ,-c(1,2)],
               method = "lm")

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
##-----------------------------------------##
## log price for correct rt skew

df1_log <- df1 %>%
  mutate(log_price = log(Price))

mean(complete.cases(df1_log))

# Split full df1_log

prep_data_log <- split_data(df1_log)
train_data_log <- prep_data_log[[1]]
test_data_log <- prep_data_log[[2]]

# Train + cal train error from exp(log)
set.seed(42)
model_log <- train(log_price ~ .,
               data = train_data_log[ , -c(1,2,23)], #remove price,id,date
               method = "lm")

p_log_train <- predict(model_log, newdata = train_data_log)

paste("MAE_expo_train : ", cal_mae(exp(train_data_log$log_price), exp(p_log_train)))
paste("MSE_expo_train : ", cal_mse(exp(train_data_log$log_price), exp(p_log_train)))
paste("RMSE_expo_train : ", cal_rmse(exp(train_data_log$log_price), exp(p_log_train)))

paste("MAE_log_train : ", cal_mae(train_data_log$log_price, p_log_train))
paste("MSE_log_train : ", cal_mse(train_data_log$log_price, p_log_train))
paste("RMSE_log_train : ", cal_rmse(train_data_log$log_price, p_log_train))

varImp(model_log)

# score predict
p_log_test <- predict(model_log, newdata = test_data_log)

# Evaluate expo log
paste("MAE_expo_test : ", cal_mae(exp(test_data_log$log_price), exp(p_log_test)))
paste("MSE_expo_test : ", cal_mse(exp(test_data_log$log_price), exp(p_log_test)))
paste("RMSE_expo_test : ", cal_rmse(exp(test_data_log$log_price), exp(p_log_test)))

#cal_mae(test_data_log$Price, exp(p_log_test))
#cal_mse(test_data_log$Price, exp(p_log_test))
#cal_rmse(test_data_log$Price, exp(p_log_test))

paste("MAE_expo_train : ", cal_mae(exp(train_data_log$log_price), exp(p_log_train)))
paste("MSE_expo_train : ", cal_mse(exp(train_data_log$log_price), exp(p_log_train)))
paste("RMSE_expo_train : ", cal_rmse(exp(train_data_log$log_price), exp(p_log_train)))

paste("MAE_log_test : ", cal_mae(test_data_log$log_price, p_log_test))
paste("MSE_log_test : ", cal_mse(test_data_log$log_price, p_log_test))
paste("RMSE_log_test : ", cal_rmse(test_data_log$log_price, p_log_test))

paste("MAE_log_train : ", cal_mae(train_data_log$log_price, p_log_train))
paste("MSE_log_train : ", cal_mse(train_data_log$log_price, p_log_train))
paste("RMSE_log_train : ", cal_rmse(train_data_log$log_price, p_log_train))

model_log
#

##-----------------------------------------##
# select varImp from above model for create new model
# subset data to df1_s

df1_s <- df1_log %>%
  select(lat = Lattitude,
         grade = `grade of the house`,
         bld_yr = `Built Year`,
         lv_area = `living area`,
         no_view = `number of views`,
         log_price,Price)

split_data(df1_s)
prep_data_s <- split_data(df1_s)
train_data_s <- prep_data_s[[1]]
test_data_s <- prep_data_s[[2]]

set.seed(42)
model_log_s <- train(log_price ~ lat + grade + bld_yr + lv_area + no_view,
                   data = train_data_s,
                   method = "lm")

p_log_s_train <- predict(model_log_s, newdata = train_data_s)

## evaluate train model_log_s
paste("MAE_log_s_train : ", cal_mae(train_data_s$log_price, p_log_s_train))
paste("MSE_log_s_train : ", cal_mse(train_data_s$log_price, p_log_s_train))
paste("RMSE_log_s_train : ", cal_rmse(train_data_s$log_price, p_log_s_train))

paste("MAE_expo_s_train : ", cal_mae(exp(train_data_s$log_price), exp(p_log_s_train)))
paste("MSE_expo_s_train : ", cal_mse(exp(train_data_s$log_price), exp(p_log_s_train)))
paste("RMSE_expo_s_train : ", cal_rmse(exp(train_data_s$log_price), exp(p_log_s_train)))

# test model_log_s
p_log_s_test <- predict(model_log_s, newdata = test_data_s)

# evaluate test model_log_s
paste("MAE_log_s_test : ", cal_mae(test_data_s$log_price, p_log_s_test))
paste("MSE_log_s_test : ", cal_mse(test_data_s$log_price, p_log_s_test))
paste("RMSE_log_s_test : ", cal_rmse(test_data_s$log_price, p_log_s_test))

paste("MAE_expo_s_test : ", cal_mae(exp(test_data_s$log_price), exp(p_log_s_test)))
paste("MSE_expo_s_test : ", cal_mse(exp(test_data_s$log_price), exp(p_log_s_test)))
paste("RMSE_expo_s_test : ", cal_rmse(exp(test_data_s$log_price), exp(p_log_s_test)))

##-----------------------------------------##

# TrainControl repeatCV

set.seed(42)
ctrl <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 5,
  verboseIter = TRUE
)

#~~~~~~~~~~~~Final model~~~~~~~~~~~~~~~~#

set.seed(42)
model_log_s <- train(log_price ~ lat + grade + bld_yr + lv_area + no_view,
                     data = train_data_s,
                     method = "lm",
                     preProcess = c("center", "scale"),
                     trControl = ctrl)





