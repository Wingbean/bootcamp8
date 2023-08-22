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
library(rpart.plot)

tree_model <- train(diabetes ~ .,
                      data = train_data,
                      method = "rpart",
                      tuneGrid = expand.grid(cp=c(0.02,0.1,0.25)),
                      trControl = ctrl)

# score
p<- predict(rf_model, newdata = test_data)

# evaluate
confusionMatrix(p, reference = test_data$diabetes,
                positive =  "pos",
                mode = "prec_recall")

# rpart plot
rpart.plot(tree_model$finalModel)

# random forest model
# mtyr hyperparameter

rf_model <- train(diabetes ~ . ,
                  data = train_data,
                  method = "rf",
                  tuneLength = 5,
                  trControl = ctrl)
rf_model




