library(tidyverse)
library(pguXAI)
library(FactoMineR)
library(caret)

main = function(){
  df_data <- iris %>%
    dplyr::select(-Species)

  classes_true <- iris$Species
  nComponents <- 2
  nCluster <- 5

  PreProcessor <- caret::preProcess(x=df_data, method=c("center", "scale"), pcaComp = nComponents)
  df_scaled <- predict(PreProcessor, df_data)

  rslt_pca <- df_scaled %>%
    FactoMineR::PCA(ncp = nComponents, scale.unit = FALSE, graph = FALSE)
  df_pred <- as.data.frame(predict(rslt_pca, df_scaled)$coord)

  km <- pguXAI::pca.KMeans$new(n=nCluster, seed = 42, verbose = TRUE)
  km$train(obj = df_pred, n = 100)

  km$probHist_plot() %>%
    plot()

  km$cluster_plot(obj = df_pred)

  km$silhouette_plot(obj = df_pred) %>%
    plot()
  #
  # km$df_centers %>%
  #   print()

  fin <- "done"
  fin
}

main()

library(tidyverse)
library(FactoMineR)
library(caret)
library(FeatureImpCluster)
library(flexclust)

df_data <- iris %>%
  dplyr::select(-Species)

classes_true <- iris$Species
nComponents <- 2

PreProcessor <- caret::preProcess(x=df_data, method=c("center", "scale"), pcaComp = nComponents)
df_scaled <- predict(PreProcessor, df_data)

rslt_pca <- df_scaled %>%
  FactoMineR::PCA(ncp = nComponents, scale.unit = FALSE, graph = FALSE)
df_pred <- as.data.frame(predict(rslt_pca, df_scaled)$coord)

set.seed(10)
res <- flexclust::kcca(df_pred, k=3, family=kccaFamily("kmeans"), simple = TRUE)
flexclust::clusters(res) %>%
  print()


