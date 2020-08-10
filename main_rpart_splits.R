library(tidyverse)
library(rpart)
library(rpart.plot)
library(caret)
source(file = "rpart_predictor.R", local = TRUE)

data(iris)

df_data <- iris[,1:4] %>%
  as.data.frame()

label <- iris[,5]
class_names <- levels(label)
nFeatures <- ncol(df_data)
feature_names <- colnames(df_data)

TrainData <- df_data %>%
  dplyr::mutate(Clusters = label)

ctrl <- rpart::rpart.control(cp = 0, maxdepth = 30, minsplit = 5)

model <- rpart::rpart(formula = Clusters ~ .,
                      data = TrainData,
                      method = "class",
                      xval = 1000,
                      parms = list(split = "information"),
                      control = ctrl)

rpart.plot::rpart.plot(model)
rpart.plot::prp(model)

rp <- rpart.predictor$new(rpart_model = model, feature_names, class_names)


# predict df_data by rules
print("rule based prediction")
df_predict <- rp$predict(df_data)

# print comfusion matrix
cm <- caret::confusionMatrix(factor(label, levels = class_names), factor(df_predict[["predict"]], levels = class_names))
print("confusion matrix")
print(cm)

print("Balanced accuracy of optimal filtered rules:")
cm$byClass %>%
  as.data.frame() %>%
  dplyr::select(c("Balanced Accuracy")) %>%
  tidyr::drop_na() %>%
  unlist() %>%
  as.numeric() %>%
  mean() %>%
  print()



# get leaf specific rules
print("leaf specific rules")
rp$df_leaf_rules %>%
  print()

# get class specific rules:
print("class specific rules")
rp$df_class_rules %>%
  print()
print(rp)

