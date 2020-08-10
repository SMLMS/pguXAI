library(tidyverse)
library(caret)
library(lime)
source(file = "lime_predictor.R", local = TRUE)


df_data<-read.csv("ActualDataForClassification.csv")[,2:7]
label <- read.csv("ActualDataForClassification.csv")[,1]
level_temp <- c("class_1", "class_2", "class_3", "class_4", "class_5")
label <- as.factor(level_temp[label])

# split data in train and test data
seed <-42
set.seed(seed)
trainIndex <- caret::createDataPartition(label,
                                         p = .8,
                                         list = FALSE,
                                         times = 1)

# define data and label sets for training and test
# df_data <- iris[,1:4]
# label <- iris[,5]
df_train <- df_data[trainIndex,]
df_test <- df_data[-trainIndex,]
label_train <- label[trainIndex]
label_test <- label[-trainIndex]
feature_names <- colnames(df_data)
classes <- levels(label)

# train random forest by caret as randomForest library is not implemented in lime.
set.seed(seed)
model_caret <- caret::train(x = df_train,
                            y = label_train,
                            method = 'rf',
                            ntree = 1500,
                            tuneGrid = data.frame(mtry = 1*sqrt(length(names(df_train))-1)))

label_predict <- predict(model_caret, newdata = df_test)

# define positive classification events
label_positive <- label_test[label_test == label_predict]

df_positive = df_test[label_test == label_predict,]

# Create an explainer object
explainer <- lime::lime(df_train, model_caret)

# Explain new observation
explanation <- lime::explain(df_test, explainer, n_labels = 1, n_features = length(feature_names))

# create instance of lime predictor
lp = lime.predictor$new(explanation, feature_names = feature_names, class_names = classes, filter_request = FALSE)
# print un-filtered rules
print("Non filtered Rules:")
print(lp)

df_limePredict <-lp$predict(df_test)

cm <- caret::confusionMatrix(factor(label_test, levels = classes), factor(df_limePredict[["predict"]], levels = classes))
print(cm)
print("Balanced accuracy of non filtered rules:")
reference_balanced_accuracy <- cm$byClass %>%
  as.data.frame() %>%
  dplyr::select(c("Balanced Accuracy")) %>%
  tidyr::drop_na() %>%
  unlist() %>%
  as.numeric() %>%
  na.omit() %>%
  mean()
print(reference_balanced_accuracy)

#screen optimal filter parameter
thr <- seq(-3, 3, 0.1)
balanced_accuracy <- rep(0.0, length(thr))


for (i in seq(length(thr))){
  lp$train(explanation, filter_request = TRUE, thr = thr[i])
  df_limePredict <-lp$predict(df_test)
  cm <- caret::confusionMatrix(factor(label_test, levels = classes), factor(df_limePredict[["predict"]], levels = classes))
  balanced_accuracy[i] <- cm$byClass %>%
    as.data.frame() %>%
    dplyr::select(c("Balanced Accuracy")) %>%
    tidyr::drop_na() %>%
    unlist() %>%
    as.numeric() %>%
    na.omit() %>%
    mean()
}

df_balanced_accuracy <- tibble::tibble(thr = thr,
                                       balanced_accuracy = balanced_accuracy)

df_balanced_accuracy %>%
  ggplot2::ggplot() +
  ggplot2::geom_point(mapping = aes(x = thr, y=balanced_accuracy))

# get maximum
opt_thr <- df_balanced_accuracy[["thr"]][which.max(df_balanced_accuracy[["balanced_accuracy"]])]

# run lime_predictor with optimal filter parameters
if (max(na.omit(balanced_accuracy)) > reference_balanced_accuracy){
  lp$train(explanation, filter_request = TRUE, thr = opt_thr)
} else{
  lp$train(explanation, filter_request = FALSE)
}
df_limePredict <-lp$predict(df_test)

# print comfusion matrix
cm <- caret::confusionMatrix(factor(label_test, levels = classes), factor(df_limePredict[["predict"]], levels = classes))
print("Balanced accuracy of optimal filtered rules:")
cm$byClass %>%
  as.data.frame() %>%
  dplyr::select(c("Balanced Accuracy")) %>%
  tidyr::drop_na() %>%
  unlist() %>%
  as.numeric() %>%
  mean() %>%
  print()

# pringt optimal rule set
print("Optimal filtered Rules:")
print(lp)
