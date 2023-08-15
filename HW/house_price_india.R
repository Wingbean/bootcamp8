##HW00sum
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

r_train <- function(A,P,M = model) {
  mae_log <- cal_mae(A$log_price, P)
  mse_log <- cal_mse(A$log_price, P)
  rmse_log <- cal_rmse(A$log_price, P)
  mae_expo <- cal_mae(exp(A$log_price), exp(P))
  mse_expo <- cal_mse(exp(A$log_price), exp(P))
  rmse_expo <- cal_rmse(exp(A$log_price), exp(P))
  print("--Evaulation of TRAIN--");
  print(paste("MAE_log_train : ",mae_log)) ;
  print(paste("MSE_log_train : ",mse_log)) ;
  print(paste("RMSE_log_train : ",rmse_log)) ;
  print(paste("MAE_expo_train : ",mae_expo)) ;
  print(paste("MSE_expo_train : ",mse_expo)) ;
  print(paste("RMSE_expo_train : ",rmse_expo)) ;
  print(paste("MAE_model : "  ,      M[[4]][[4]])) ;
  print(paste("Rsquared_model : "  , M[[4]][[3]])) ;
  print(paste("RMSE_model : " ,      M[[4]][[2]])) ;
  list(mae_log, mse_log, rmse_log, mae_expo, mse_expo, rmse_expo)
}

r_test <- function(A,P,M = model) {
  mae_log <- cal_mae(A$log_price, P)
  mse_log <- cal_mse(A$log_price, P)
  rmse_log <- cal_rmse(A$log_price, P)
  mae_expo <- cal_mae(exp(A$log_price), exp(P))
  mse_expo <- cal_mse(exp(A$log_price), exp(P))
  rmse_expo <- cal_rmse(exp(A$log_price), exp(P))
  print("--Evaulation of TEST--");
  print(paste("MAE_log_test : ",mae_log)) ;
  print(paste("MSE_log_test : ",mse_log)) ;
  print(paste("RMSE_log_test : ",rmse_log)) ;
  print(paste("MAE_expo_test : ",mae_expo)) ;
  print(paste("MSE_expo_test : ",mse_expo)) ;
  print(paste("RMSE_expo_test : ",rmse_expo)) ;
  print(paste("MAE_model : "  ,      M[[4]][[4]])) ;
  print(paste("Rsquared_model : "  , M[[4]][[3]])) ;
  print(paste("RMSE_model : " ,      M[[4]][[2]])) ;
  list(mae_log, mse_log, rmse_log, mae_expo, mse_expo, rmse_expo)
}

print(paste("MAE_test : "  , cal_mae(test_data$Price, p)))
print(paste("MSE_test : "  , cal_mse(test_data$Price, p)))
print(paste("RMSE_test : " , cal_rmse(test_data$Price, p)))
print(paste("MAE_model : "  ,      model[[4]][[4]]))
print(paste("Rsquared_model : "  , model[[4]][[3]]))
print(paste("RMSE_model : " ,      model[[4]][[2]]))

#cal_mae(test_data$Price, p)
#cal_mse(test_data$Price, p)
#cal_rmse(test_data$Price, p)

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

model_log_r_train <- r_train(train_data_log, p_log_train, model_log)
#paste("MAE_expo_train : ", cal_mae(exp(train_data_log$log_price), exp(p_log_train)))
#paste("MSE_expo_train : ", cal_mse(exp(train_data_log$log_price), exp(p_log_train)))
#paste("RMSE_expo_train : ", cal_rmse(exp(train_data_log$log_price), exp(p_log_train)))
#paste("MAE_log_train : ", cal_mae(train_data_log$log_price, p_log_train))
#paste("MSE_log_train : ", cal_mse(train_data_log$log_price, p_log_train))
#paste("RMSE_log_train : ", cal_rmse(train_data_log$log_price, p_log_train))

varImp(model_log)

# score predict
p_log_test <- predict(model_log, newdata = test_data_log)

# Evaluate expo log
model_log_r_test <- r_test(test_data_log, p_log_test, model_log)

#paste("MAE_expo_test : ", cal_mae(exp(test_data_log$log_price), exp(p_log_test)))
#paste("MSE_expo_test : ", cal_mse(exp(test_data_log$log_price), exp(p_log_test)))
#paste("RMSE_expo_test : ", cal_rmse(exp(test_data_log$log_price), exp(p_log_test)))
#paste("MAE_expo_train : ", cal_mae(exp(train_data_log$log_price), exp(p_log_train)))
#paste("MSE_expo_train : ", cal_mse(exp(train_data_log$log_price), exp(p_log_train)))
#paste("RMSE_expo_train : ", cal_rmse(exp(train_data_log$log_price), exp(p_log_train)))
#paste("MAE_log_test : ", cal_mae(test_data_log$log_price, p_log_test))
#paste("MSE_log_test : ", cal_mse(test_data_log$log_price, p_log_test))
#paste("RMSE_log_test : ", cal_rmse(test_data_log$log_price, p_log_test))
#paste("MAE_log_train : ", cal_mae(train_data_log$log_price, p_log_train))
#paste("MSE_log_train : ", cal_mse(train_data_log$log_price, p_log_train))
#paste("RMSE_log_train : ", cal_rmse(train_data_log$log_price, p_log_train))


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

prep_data_s <- split_data(df1_s)
train_data_s <- prep_data_s[[1]]
test_data_s <- prep_data_s[[2]]

set.seed(42)
model_log_s <- train(log_price ~ lat + grade + bld_yr + lv_area + no_view,
                   data = train_data_s,
                   method = "lm")

p_log_s_train <- predict(model_log_s, newdata = train_data_s)

## evaluate train model_log_s
model_log_s_r_train <- r_train(train_data_s, p_log_s_train, model_log_s)

#paste("MAE_log_s_train : ", cal_mae(train_data_s$log_price, p_log_s_train))
#paste("MSE_log_s_train : ", cal_mse(train_data_s$log_price, p_log_s_train))
#paste("RMSE_log_s_train : ", cal_rmse(train_data_s$log_price, p_log_s_train))
#paste("MAE_expo_s_train : ", cal_mae(exp(train_data_s$log_price), exp(p_log_s_train)))
#paste("MSE_expo_s_train : ", cal_mse(exp(train_data_s$log_price), exp(p_log_s_train)))
#paste("RMSE_expo_s_train : ", cal_rmse(exp(train_data_s$log_price), exp(p_log_s_train)))

# test model_log_s
p_log_s_test <- predict(model_log_s, newdata = test_data_s)

# evaluate test model_log_s
model_log_s_r_test <- r_test(test_data_s, p_log_s_test, model_log_s)

#paste("MAE_log_s_test : ", cal_mae(test_data_s$log_price, p_log_s_test))
#paste("MSE_log_s_test : ", cal_mse(test_data_s$log_price, p_log_s_test))
#paste("RMSE_log_s_test : ", cal_rmse(test_data_s$log_price, p_log_s_test))
#paste("MAE_expo_s_test : ", cal_mae(exp(test_data_s$log_price), exp(p_log_s_test)))
#paste("MSE_expo_s_test : ", cal_mse(exp(test_data_s$log_price), exp(p_log_s_test)))
#paste("RMSE_expo_s_test : ", cal_rmse(exp(test_data_s$log_price), exp(p_log_s_test)))

##-----------------------------------------##
##-----------------------------------------##
##-----------------------------------------##

#~~~~~~~~~~~~~~~~Final model~~~~~~~~~~~~~~~~#

# TrainControl repeatCV

set.seed(42)
ctrl <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 5,
  verboseIter = TRUE
)

set.seed(42)
model_lm_final <- train(log_price ~ lat + grade + bld_yr + lv_area + no_view,
                     data = train_data_s,
                     method = "lm",
                     preProcess = c("center", "scale"),
                     trControl = ctrl)

p_lm_final_train <- predict(model_lm_final, newdata = train_data_s)

## evaluate train model_lm_final

model_lm_final_r_train <- r_train(train_data_s, p_lm_final_train, model_lm_final)

#paste("MAE_log_s_train : ", cal_mae(train_data_s$log_price, p_log_s_train))
#paste("MSE_log_s_train : ", cal_mse(train_data_s$log_price, p_log_s_train))
#paste("RMSE_log_s_train : ", cal_rmse(train_data_s$log_price, p_log_s_train))
#paste("MAE_expo_s_train : ", cal_mae(exp(train_data_s$log_price), exp(p_log_s_train)))
#paste("MSE_expo_s_train : ", cal_mse(exp(train_data_s$log_price), exp(p_log_s_train)))
#paste("RMSE_expo_s_train : ", cal_rmse(exp(train_data_s$log_price), exp(p_log_s_train)))


# test model_lm_final
p_lm_final_test <- predict(model_lm_final, newdata = test_data_s)

# evaluate test model_log_s

model_lm_final_r_test <- r_test(test_data_s, p_lm_final_test, model_lm_final)

#paste("MAE_log_s_test : ", cal_mae(test_data_s$log_price, p_log_s_test))
#paste("MSE_log_s_test : ", cal_mse(test_data_s$log_price, p_log_s_test))
#paste("RMSE_log_s_test : ", cal_rmse(test_data_s$log_price, p_log_s_test))
#paste("MAE_expo_s_test : ", cal_mae(exp(test_data_s$log_price), exp(p_log_s_test)))
#paste("MSE_expo_s_test : ", cal_mse(exp(test_data_s$log_price), exp(p_log_s_test)))
#paste("RMSE_expo_s_test : ", cal_rmse(exp(test_data_s$log_price), exp(p_log_s_test)))

##-----------------------------------------##
##---------switch method to knn-------------##

model_log_s <- train(log_price ~ lat + grade + bld_yr + lv_area + no_view,
                     data = train_data_s,
                     method = "knn",
                     tuneGrid = data.frame(k=c(5, 7, 13)),
                     preProcess = c("center", "scale"),
                     #preProcess = c("range", "zv", "nzv"),
                     trControl = ctrl)

##----------Summary Evaluation-------------##
#create list MAE-train/test, MSE-train/test, RMSE-train/test, Rsquared train
#create list model, model_log, model_log_s, model_lm_final
#then create df to compare evaluation



