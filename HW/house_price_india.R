##HW00sum
#load library
library(tidyverse)
library(caret)
library(mlbench)
library(readxl)
library(ggplot2)

# Load data
df1<-read_excel("hpi.xlsx", sheet = 1)
df2<-read_excel("hpi.xlsx", sheet = 2)

#complete date ?
mean(complete.cases(df1)) #result should be 1
mean(complete.cases(df2)) #result should be 1

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

# Visualized data ~ Price
ggplot(df1, aes(Price/1000)) +
  geom_histogram(bins = 100, fill="blue") +
  theme_minimal() +
  labs(
    title = "Visualized Real price by histogram",
    subtitle = "Right skew",
    x = "Real price x1000",
    caption = "Source: data.world"
  )

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

# show error of model
print(paste("MAE_test : "  , cal_mae(test_data$Price, p)))
print(paste("MSE_test : "  , cal_mse(test_data$Price, p)))
print(paste("RMSE_test : " , cal_rmse(test_data$Price, p)))
print(paste("MAE_model : "  ,      model[[4]][[4]]))
print(paste("Rsquared_model : "  , model[[4]][[3]]))
print(paste("RMSE_model : " ,      model[[4]][[2]]))

##-----------------------------------------##

## log price for correct rt skew

df1_log <- df1 %>%
  mutate(log_price = log(Price))

mean(complete.cases(df1_log))

# Split full df1_log

prep_data_log <- split_data(df1_log)
train_data_log <- prep_data_log[[1]]
test_data_log <- prep_data_log[[2]]

# Visualized data ~ log_price
ggplot(df1_log, aes(log_price)) +
  geom_histogram(bins = 100, fill="green") +
  theme_minimal()

# Train + cal train error from exp(log)
set.seed(42)
model_log <- train(log_price ~ .,
               data = train_data_log[ , -c(1,2,23)], #remove price,id,date
               method = "lm")

p_log_train <- predict(model_log, newdata = train_data_log)

model_log_r_train <- r_train(train_data_log, p_log_train, model_log)

# score predict
p_log_test <- predict(model_log, newdata = test_data_log)

# Evaluate expo log
model_log_r_test <- r_test(test_data_log, p_log_test, model_log)




##-----------------------------------------##
# select varImp from above model for create new model

print("variable importance of --> model")
print(varImp(model))
print("variable importance of --> model_log")
print(varImp(model_log))

# subset data df1_log use 5 imp var to df1_s 

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

model_log_s <- train(log_price ~ lat + grade + bld_yr + lv_area + no_view,
                   data = train_data_s,
                   method = "lm")

p_log_s_train <- predict(model_log_s, newdata = train_data_s)

## evaluate train model_log_s
model_log_s_r_train <- r_train(train_data_s, p_log_s_train, model_log_s)

# test model_log_s
p_log_s_test <- predict(model_log_s, newdata = test_data_s)

# evaluate test model_log_s
model_log_s_r_test <- r_test(test_data_s, p_log_s_test, model_log_s)

#~~~~~~~~~~~~~~~~Final model~~~~~~~~~~~~~~~~#

# TrainControl repeatCV

set.seed(42)
ctrl <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 5,
  verboseIter = TRUE
)

model_lm_final <- train(log_price ~ lat + grade + bld_yr + lv_area + no_view,
                     data = train_data_s,
                     method = "lm",
                     preProcess = c("center", "scale"),
                     trControl = ctrl)

p_lm_final_train <- predict(model_lm_final, newdata = train_data_s)

## evaluate train model_lm_final

model_lm_final_r_train <- r_train(train_data_s, p_lm_final_train, model_lm_final)

# test model_lm_final
p_lm_final_test <- predict(model_lm_final, newdata = test_data_s)

# evaluate test model_log_s

model_lm_final_r_test <- r_test(test_data_s, p_lm_final_test, model_lm_final)

##---------switch method to knn-------------##

model_knn_final <- train(log_price ~ lat + grade + bld_yr + lv_area + no_view,
                         data = train_data_s,
                         method = "knn",
                         tuneGrid = data.frame(k=c(5, 7, 13)),
                         preProcess = c("center", "scale"),
                         #preProcess = c("range", "zv", "nzv"),
                         trControl = ctrl)

p_knn_final_train <- predict(model_knn_final, newdata = train_data_s)

## evaluate train model_knn_final

model_knn_final_r_train <- r_train(train_data_s, p_knn_final_train, model_knn_final)

# test model_knn_final
p_knn_final_test <- predict(model_knn_final, newdata = test_data_s)

# evaluate test model_knn_final

model_knn_final_r_test <- r_test(test_data_s, p_knn_final_test, model_knn_final)

##-----------------------------------------##
##----------Summary Evaluation-------------##

#create vector model, model_log, model_log_s, model_lm_final, model_knn_final
model_name <- c("model",
                "model_log",
                "model_log_s",
                "model_lm_final",
                "model_knn_final")

#create vector MAE-train/test, MSE-train/test, RMSE-train/test, Rsquared train
mae_train_log <- c(0,
                   model_log_r_train[[1]],
                   model_log_s_r_train[[1]],
                   model_lm_final_r_train[[1]],
                   model_knn_final_r_train[[1]])

mae_train <- c(model[[4]][[4]],
               model_log_r_train[[4]],
               model_log_s_r_train[[4]],
               model_lm_final_r_train[[4]],
               model_knn_final_r_train[[4]])

mae_test_log <- c(0,
                  model_log_r_test[[1]],
                  model_log_s_r_test[[1]],
                  model_lm_final_r_test[[1]],
                  model_knn_final_r_test[[1]])

mae_test <- c(model[[4]][[4]],
              model_log_r_test[[4]],
              model_log_s_r_test[[4]],
              model_lm_final_r_test[[4]],
              model_knn_final_r_test[[4]])

mse_train_log <- c(0, 
                   model_log_r_train[[2]], 
                   model_log_s_r_train[[2]], 
                   model_lm_final_r_train[[2]],
                   model_knn_final_r_train[[2]])

mse_train <- c(0, 
               model_log_r_train[[5]], 
               model_log_s_r_train[[5]], 
               model_lm_final_r_train[[5]],
               model_knn_final_r_train[[5]])

mse_test_log <- c(0, 
                  model_log_r_test[[2]], 
                  model_log_s_r_test[[2]], 
                  model_lm_final_r_test[[2]],
                  model_knn_final_r_test[[2]])

mse_test <- c(0, 
              model_log_r_test[[5]], 
              model_log_s_r_test[[5]], 
              model_lm_final_r_test[[5]],
              model_knn_final_r_test[[5]])

rmse_train_log <- c(0, 
                    model_log_r_train[[3]], 
                    model_log_s_r_train[[3]], 
                    model_lm_final_r_train[[3]],
                    model_knn_final_r_train[[3]])

rmse_train <- c(model[[4]][[2]], 
                model_log_r_train[[6]], 
                model_log_s_r_train[[6]], 
                model_lm_final_r_train[[6]],
                model_knn_final_r_train[[6]])

rmse_test_log <- c(0, 
                   model_log_r_test[[3]], 
                   model_log_s_r_test[[3]], 
                   model_lm_final_r_test[[3]],
                   model_knn_final_r_test[[3]])

rmse_test <- c(model[[4]][[2]], 
               model_log_r_test[[6]], 
               model_log_s_r_test[[6]], 
               model_lm_final_r_test[[6]],
               model_knn_final_r_test[[6]])

rsquared_model_train <- c(model[[4]][[3]], 
                          model_log[[4]][[3]], 
                          model_log_s[[4]][[3]], 
                          model_lm_final[[4]][[3]],
                          model_knn_final[[4]][[3,3]])

sum_eva <- data.frame(model_name,
                      mae_train_log, mae_train, 
                      mae_test_log, mae_test, 
                      mse_train_log, mse_train, 
                      mse_test_log, mse_test,
                      rmse_train_log, rmse_train, 
                      rmse_test_log, rmse_test,
                      rsquared_model_train)

print(sum_eva)
print(model_knn_final)