##ML

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







