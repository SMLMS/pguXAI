library(tidyverse)
library(pguXAI)
library(caret)


main = function(){
  df_data <- iris %>%
    dplyr::select(-Species)

  classes_true <- iris$Species

  # ca <- pguXAI::componentAnalysis.pca$new(thr = 0.99 , verbose=TRUE)
  # ca$train(obj=df_data)
  # print(ca)
  # plot(ca)
  # nComponents <- ca$nComp

  nComponents <- 2
  PreProcessor <- caret::preProcess(x=df_data, method=c("center", "scale", "pca"), pcaComp = nComponents)
  df_pred <- predict(PreProcessor, df_data)

  km <- pguXAI::pca.KMeans$new(n=3, seed = 42, verbose = TRUE)
  km$train(obj = df_pred, n = 100)

  km$probHist_plot() %>%
    plot()
  km$cluster_plot(obj = df_pred)
  km$df_centers %>%
    print()
  km$silhouette_plot(obj = df_pred)
}

main()

library(cluster)
library(factoextra)
df_data <- iris %>%
  dplyr::select(-Species)
dis = stats::dist(df_data, method = "euclediean")
print(dis)
res = kmeans(df_data,3)
print(res$cluster)
sil = silhouette (res$cluster, dis)
print(mean(sil[,3]))
fviz_silhouette(sil)
# #windows()
# plot(sil)
