library(tidyverse)
library(pguXAI)
library(FactoMineR)
library(caret)

main = function(){
  # load data set and remove class labels
  df_data <- iris %>%
    dplyr::select(-Species)

  # define true class labels
  classes_true <- iris$Species

  # define nuber of components for pca and number of clusters for kmeans
  nComponents <- 2
  nCluster <- 10

  # pre-scale the data for pca
  PreProcessor <- caret::preProcess(x=df_data, method=c("center", "scale"), pcaComp = nComponents)
  df_scaled <- predict(PreProcessor, df_data)

  # reduce dimensions of sclaed dataset using pca
  rslt_pca <- df_scaled %>%
    FactoMineR::PCA(ncp = nComponents, scale.unit = FALSE, graph = FALSE)
  df_pred <- as.data.frame(predict(rslt_pca, df_scaled)$coord)

  # run kmeans analysis
  km <- pguXAI::pca.KMeans$new(n=nCluster, seed = 42, verbose = TRUE)
  km$train(obj = df_pred)

  km$cluster_plot(obj = df_pred)

  km$silhouette_plot(obj = df_pred) %>%
    plot()

  print("Result of silhouette analysis:")
  km$df_silhouette %>%
    print()

  print("Average silhouette width:")
  km$av_sil_width %>%
    print()

  print("Centers of clusters:")
  km$df_centers %>%
    print()

  print("Probability of the class label assignment:")
  km$predProb %>%
    print()

  print("Majority vote of the class label assignment:")
  km$predClass %>%
    print()

  print("Within cluster sum of squares analysis:")
  km$df_withinss %>%
    print()

  km$tot_withinss %>%
    print()

  fin <- "done"
  fin
}

main()
