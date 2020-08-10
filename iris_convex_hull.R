library(tidyverse)
library(grDevices)
library(caret)
library(FactoMineR)


##########################
# modify iris data.frame #
##########################
modified_iris = function(){
  data(iris)
  df_data <- iris[,1:4] %>%
    as.data.frame()

  colnames_permutated <- colnames(df_data)
  for (i in seq(length(colnames_permutated))){
    colnames_permutated[i] <- sprintf("%s_permuted", colnames_permutated[i])
  }

  df_data_permutated <- apply(df_data, 2, function(x) jitter(sample(x))) %>%
    as.data.frame()

  colnames(df_data_permutated) <- colnames_permutated

  df_data <- df_data %>%
    dplyr::mutate(idx = seq(1,nrow(df_data),1))

  df_data_permutated <- df_data_permutated %>%
    dplyr::mutate(idx = seq(1,nrow(df_data),1))

  df_data <- df_data %>%
    dplyr::full_join(df_data_permutated, by="idx") %>%
    dplyr::select(-c("idx"))

  return(df_data)
}

data(iris)

#################
# main function #
#################
plot_iris_pca = function(){
  # set seed
  seed <- 42
  set.seed(seed)

  # define data.frame
  classes <- iris[,5]
  label <- labels(classes)
  df_data <- modified_iris()

  # perform pca pre-processing (sclae data)
  nFeatures <- ncol(df_data)
  PreProcessor <- caret::preProcess(x=df_data, method=c("center", "scale"), pcaComp = nFeatures)
  df_scaled <- predict(PreProcessor, df_data)

  # perform pca
  rslt_pca <- df_scaled %>%
    FactoMineR::PCA(ncp = nFeatures, scale.unit = FALSE, graph = FALSE)
  lst_pred <- predict(rslt_pca, df_scaled)

  df_components <- lst_pred$coord %>%
    as.data.frame()

  rslt_pca$eig %>%
    as.data.frame() %>%
    print()

  # create convex hull
  df_hull <- df_components %>%
    dplyr::select(c("Dim.1", "Dim.2")) %>%
    dplyr::mutate(class = classes) %>%
    dplyr::group_by(class) %>%
    dplyr::slice(grDevices::chull(x = Dim.1, y = Dim.2))

  # calculate means
  df_mean <- df_components %>%
    dplyr::select(c("Dim.1", "Dim.2")) %>%
    dplyr::mutate(class = classes) %>%
    dplyr::group_by(class) %>%
    dplyr::summarise(Dim.1 = mean(Dim.1),
                     Dim.2 = mean(Dim.2))


  # create compound plot
  iris_hull_plot <- df_components %>%
    dplyr::select(c("Dim.1", "Dim.2")) %>%
    dplyr::mutate(class = classes) %>%
    ggplot2::ggplot() +
    ggplot2::geom_point(mapping = aes(x=Dim.1, y=Dim.2, color = class, shape = class)) +
    geom_point(data = df_mean, mapping = aes(x=Dim.1, y=Dim.2, color = class, shape = class), size = 3.5) +
    ggplot2::geom_polygon(data = df_hull, mapping = aes(x=Dim.1, y=Dim.2, color = class, fill = class), alpha = 0.2) +
    ggplot2::theme_bw() +
    ggthemes::scale_color_colorblind() +
    ggthemes::scale_fill_colorblind() +
    ggplot2::labs(title = "Factor map") +
    ggplot2::theme(legend.position = c(1.0,.28),
                   legend.justification=c(1,1),
                   legend.key = element_rect(colour = NA, fill = "transparent"),
                   legend.box.background = element_rect(color=NA, fill="transparent"),
                   legend.background = element_rect(colour = NA, fill = "transparent")
                   ) +
    ggplot2::coord_cartesian(xlim =c(-3.5, 3.5), ylim = c(-3.5, 3.5))

  return(iris_hull_plot)
}

plot_iris_pca()
