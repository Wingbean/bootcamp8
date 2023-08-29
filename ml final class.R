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

# resample() => compare model performance
# predict diabetes

model1 <- train(diabetes ~ .,
                data = train_data,
                method = "glm",
                trControl = trainControl(
                  method = "cv", number = 5
                  ))

model2 <- train(diabetes ~ .,
                data = train_data,
                method = "rpart",
                trControl = trainControl(
                  method = "cv", number = 5
                ))

model3 <- train(diabetes ~ .,
                data = train_data,
                method = "rf",
                trControl = trainControl(
                  method = "cv", number = 5
                ))

model4 <- train(diabetes ~ .,
                data = train_data,
                method = "glmnet",
                trControl = trainControl(
                  method = "cv", number = 5
                ))

model5 <- train(diabetes ~ .,
                data = train_data,
                method = "nnet",
                trControl = trainControl(
                  method = "cv", number = 5
                ))


# resamples

list_models = list(
  logistic = model1,
  tree = model2,
  randomForest = model3,
  glmnet = model4,
  nnet = model5
)

result <- resamples(list_models)

summary(result)

# K-Mean------------------

## k-means for clustering (aka. segmentation)
## load data from `mlbench` library
data("BostonHousing")

## rename dataset
df <- BostonHousing
glimpse(df)

## subset columns
subset_df <- df %>%
  select(crim, rm, age, lstat, medv) %>%
  as_tibble()

## test different k (k= 2-5)
km_result <- kmeans(x = subset_df, centers = 5) #center ต้องกำหนดเอง

## membership [1,2,3,4,5]
subset_df$cluster <- km_result$cluster

subset_df %>%
  group_by(cluster) %>%
  summarise(avg_price = mean(medv),
            avg_rooms = mean(rm))

# normalization min max scaling
normalize_data <- function(x){
  (x-min(x)) / (max(x) - min(x))
}

normalize_data(subset_df$medv)

# apply to all column in dataframe
subset_df_norm <- apply(subset_df, MARGIN = 2, normalize_data)





