library("tidyverse")
library("rpart")
library("rpart.plot")
library("pguXAI")
data("iris")

main = function(){
  df_data = tibble::as_tibble(iris)
  grid <- expand.grid(cp = c(0.01), maxdepth = c(1,2), minsplit = c(100)) %>%
    tibble::as_tibble()

  gs <- pguXAI::gridSearchCV.rpart$new(df_data, label = "Species", grid = grid, k=10, repeats=5)
  gs$train()
  gs$scores %>%
    #dplyr::select(c("accuracy", "auc_roc", "Balanced Accuracy")) %>%
    print()

  ctrl <- gs$best_model_parameter(score = "auc_roc")


  model <- rpart::rpart(Species~., data = df_data, control = ctrl, parms = list(split = "gini"))
  rpart.plot::rpart.plot(model)
  print(ctrl)

  cv <- pguXAI::crossVal.rpart$new(df_data, label = "Species", ctrl = ctrl)
  cv$train(repeats = 5)
  print(cv)
  plot(cv)
}


main()
