# library(tidyverse)
# library(pguXAI)
# library(FactoMineR)
# library(caret)
#
# main = function(){
#   # load data set and remove class labels
#   df_data <- iris %>%
#     dplyr::select(-Species)
#
#   # define true class labels
#   classes_true <- iris$Species
#
#   # define nuber of components for pca and number of clusters for kmeans
#   nComponents <- 2
#   maxCluster <- 8
#   max_seed <- 20
#
#   # pre-scale the data for pca
#   PreProcessor <- caret::preProcess(x=df_data, method=c("center", "scale"), pcaComp = nComponents)
#   df_scaled <- predict(PreProcessor, df_data)
#
#   # reduce dimensions of sclaed dataset using pca
#   rslt_pca <- df_scaled %>%
#     FactoMineR::PCA(ncp = nComponents, scale.unit = FALSE, graph = FALSE)
#   df_pred <- as.data.frame(predict(rslt_pca, df_scaled)$coord)
#
#   km_screen <- pguXAI::screen.KMeans$new(n=8, seed=42, verbose = FALSE)
#   km_screen$train(df_pred, n = 5)
#
#
#
#   # df_summary <- df_rslt %>%
#   #   dplyr::group_by(cluster) %>%
#   #   dplyr::summarise(
#   #     n = n(),
#   #     mean = mean(wcss, na.rm = TRUE),
#   #     sd = sd(wcss, na.rm = TRUE)
#   #   )
#   #
#   # df_summary %>%
#   #   ggplot2::ggplot(mapping =aes(x=cluster, y=mean)) +
#   #   ggplot2::geom_errorbar(mapping = aes(ymin=mean-sd, width=.1, ymax=mean+sd)) +
#   #   geom_line() +
#   #   geom_point()
#   #
#   # model <- lm(mean ~ poly(cluster,3), data = df_summary)
#   # y2  <- stats::predict(model, newdata = df_summary)
#   # print(y2)
#   # fx <- stats::splinefun(df_summary[["cluster"]], y= df_summary[["mean"]])
#   # fx(df_summary[["cluster"]], deriv = 2) %>%
#   #   print()
#
#
#   # df_summary %>%
#   #   dplyr::mutate(d1 = deriv(mean,n,1)) %>%
#   #   print()
#   # dy <- deriv(df_summary[[mean]], df_summary[[n]], 2)
#   # print(dy)
#   # df_rslt %>%
#   #   ggplot2::ggplot(mapping = aes(x= factor(n), y=wcss)) +
#   #   ggplot2::geom_boxplot()
#
#   #
#   # fin <- "done"
#   # fin
# }
#
# main()
