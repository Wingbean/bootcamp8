## install package
install.packages(c("caret", "tidyverse", "mlbench"))

## load library
library(caret)
library(tidyverse)
library(mlbench)

data("PimaIndiansDiabetes")
df <- PimaIndiansDiabetes

## 1. split data
set.seed(42)
n <- nrow(df)
id <- sample(1:n, size=0.8*n)
train_data <- df[id, ]
test_data <- df[-id, ]

# Train model
my_grid<- expand.grid(alpha = 0:1,
                      lambda = seq(0.0005, 0.05, length=20))

ctrl <- trainControl(method = "cv",
                     number = 5,
                     verboseIter = TRUE)
set.seed(42)
glmnet_model <- train(diabetes ~ .,
                        data = train_data,
                        method = "glmnet",
                        tuneGrid = my_grid,
                        trControl = ctrl)

# rpart decision tree

install.packages("rpart")
library(rpart)

tree_model <- train(diabetes ~ .,
                      data = train_data,
                      method = "rpart",
                      trControl = ctrl)

# score
p<- predict(glmnet_model, newdata = test_data)

# evaluate
confusionMatrix(p, reference = test_data$diabetes,
                positive =  "pos",
                mode = "prec_recall")
