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
  maxCluster <- 8
  max_seed <- 20

  # pre-scale the data for pca
  PreProcessor <- caret::preProcess(x=df_data, method=c("center", "scale"), pcaComp = nComponents)
  df_scaled <- predict(PreProcessor, df_data)

  # reduce dimensions of sclaed dataset using pca
  rslt_pca <- df_scaled %>%
    FactoMineR::PCA(ncp = nComponents, scale.unit = FALSE, graph = FALSE)
  df_pred <- as.data.frame(predict(rslt_pca, df_scaled)$coord)

  km_screen <- pguXAI::screen.KMeans$new(n=8, seed=42, verbose = FALSE)
  km_screen$train(df_pred, n = 5)

  km_screen$wcss_component_plot()

  km_screen$silhouette_component_plot()

  fin <- "done"
  fin
}

main()
