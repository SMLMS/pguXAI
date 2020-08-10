library(tidyverse)
library(caret)
library(FactoMineR)
library(keras)
library(tensorflow)

data(iris)

modified_iris = function(){
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
main = function(session_seed = 42){
  set.seed(session_seed)


  label <- factor(iris[,5])
  # df_data <- iris[,1:4] %>%
  #   as.data.frame()
  df_data <- modified_iris()

  # standardise
  PreProcessor <- caret::preProcess(x=df_data, method=c("center", "scale"), pcaComp = ncol(df_data))
  df_scaled <- predict(PreProcessor, df_data)

  # # pca
  # pca_result <- df_scaled %>%
  #   FactoMineR::PCA(ncp = ncol(df_data), scale.unit = FALSE, graph = FALSE)

  # autoencoder
  mat_data <- as.matrix(df_scaled)
  ncol(mat_data)

  as.integer(ncol(mat_data)/2)

  # create model
  model <- keras::keras_model_sequential()
  layer_initializer <- keras::initializer_random_uniform(minval = -0.05, maxval = 0.05, seed = session_seed)

  model %>%
    layer_dense(units = as.integer(ncol(mat_data)/2), activation = "tanh", input_shape = ncol(mat_data), kernel_initializer=layer_initializer) %>%
    layer_dense(units = 2, activation = "tanh", name = "bottleneck", kernel_initializer=layer_initializer) %>%
    layer_dense(units = as.integer(ncol(mat_data)/2), activation = "tanh", kernel_initializer=layer_initializer) %>%
    layer_dense(units = ncol(mat_data), kernel_initializer=layer_initializer)

  # model %>%
  #   layer_dense(units = 2, activation = "tanh", input_shape = ncol(mat_data), name = "bottleneck", kernel_initializer=layer_initializer) %>%
  #   layer_dense(units = ncol(mat_data), kernel_initializer=layer_initializer )

  # view model layers
  summary(model)

  # compile model
  model %>% compile(
    loss = "mean_squared_error",
    optimizer = "adam"
  )

  # fit model
  model %>% fit(
    x = mat_data,
    y = mat_data,
    epochs = 2000,
    verbose = 0
  )


  # evaluate the performance of the model
  mse.ae2 <- evaluate(model, mat_data, mat_data)
  mse.ae2

  # extract the bottleneck layer
  encoder_model <- keras::keras_model(inputs = model$input, outputs = get_layer(model, "bottleneck")$output)
  encoder_result <- predict(encoder_model, mat_data) %>%
    as.data.frame()

  colnames(encoder_result) <- c("ae.1", "ae.2")

  # create convex hull
  df_hull <- encoder_result %>%
    dplyr::select(c("ae.1", "ae.2")) %>%
    dplyr::mutate(class = label) %>%
    dplyr::group_by(class) %>%
    dplyr::slice(grDevices::chull(x = ae.1, y = ae.2))

  # calculate means
  df_mean <- encoder_result %>%
    dplyr::select(c("ae.1", "ae.2")) %>%
    dplyr::mutate(class = label) %>%
    dplyr::group_by(class) %>%
    dplyr::summarise(ae.1 = mean(ae.1),
                     ae.2 = mean(ae.2))


  p <- encoder_result %>%
    dplyr::select(c("ae.1", "ae.2")) %>%
    dplyr::mutate(class = label) %>%
    ggplot2::ggplot() +
    ggplot2::geom_point(mapping = aes(x=ae.1, y=ae.2, color = class, shape = class)) +
    geom_point(data = df_mean, mapping = aes(x=ae.1, y=ae.2, color = class, shape = class), size = 3.5) +
    ggplot2::geom_polygon(data = df_hull, mapping = aes(x=ae.1, y=ae.2, color = class, fill = class), alpha = 0.2) +
    ggplot2::theme_bw() +
    ggthemes::scale_color_colorblind() +
    ggthemes::scale_fill_colorblind() +
    ggplot2::labs(title = sprintf("Factor map with seed: %i", session_seed))

  p %>%
    show()
}

main(3)




