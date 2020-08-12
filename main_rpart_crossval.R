library(tidyverse)
library(rpart)
library(rpart.plot)
library(caret)
library(gridExtra)
source(file = "rpart_predictor.R", local = TRUE)

fileName <- "~/Documents/Code/R/pguXAI/ActualDataForClassification.csv"
pain_data <- readr::read_csv(fileName) %>%
  dplyr::mutate(Clusters = as.factor(Clusters))

head(pain_data)

df_data <- pain_data %>%
  # dplyr::select(c("Clusters", "HPT_Z_Control", "CPT_Z_UVB", "CPT_Z_Control"))
  dplyr::select(c("Clusters","UVBcoldEff", "HPT_Z_Control", "CPT_Z_UVB")) #, "CPT_Z_Control"))

label <- df_data[["Clusters"]]
class_names <- levels(label)
nFeatures <- ncol(df_data)
feature_names <- colnames(df_data)[-1]

set.seed(42)
idx <- seq(nrow(df_data))
mt_sample <- idx %>%
  caret::createDataPartition(times = 100, p = 0.66, list = FALSE)


ctrl <- rpart::rpart.control(cp = 0, maxdepth = 30, minsplit = 5)
df_splits <- tibble::tibble(feature = character(),
                            splits = integer())
for (i in seq(ncol(mt_sample))){
  df_sample_data <- df_data %>%
    dplyr::slice(mt_sample[,i])

  rpart_model <- rpart::rpart(formula = Clusters ~ .,
                              data = df_sample_data,
                              method = "class",
                              xval = 1000,
                              parms = list(split = "information"),
                              control = ctrl)

  rp <- rpart.predictor$new(rpart_model = rpart_model, feature_names, class_names)

  df_splits <- df_splits %>%
    dplyr::bind_rows(rp$df_splits)
}

df_splits <- df_splits %>%
  dplyr::rename(variable = feature)



rpart_model <- rpart::rpart(formula = Clusters ~ .,
                            data = df_data,
                            method = "class",
                            xval = 1000,
                            parms = list(split = "information"),
                            control = ctrl)


rp <- rpart.predictor$new(rpart_model = rpart_model, feature_names, class_names)
rp$df_splits

df_model_splits <- tibble::tibble(variable= c("HPT_Z_Control", "CPT_Z_UVB", "CPT_Z_UVB", "UVBcoldEff", "UVBcoldEff"),
                                  splits = c(0.92, -0.177, 0.840, -0.767, 0.536),
                                  model = rep("GMM", 5)
                                  )

df_model_splits <- rp$df_splits %>%
  dplyr::mutate(model = rep("RPART", nrow(rp$df_splits))) %>%
  dplyr::rename(variable = feature) %>%
  dplyr::bind_rows(df_model_splits)

df_model_splits


#############
# plot figure
#############
p <- df_splits %>%
  ggplot2::ggplot() +
  ggplot2::geom_density(mapping = aes(x=splits, group = variable, color = variable, fill = variable), alpha = 0.3, show.legend = FALSE) +
  ggplot2::geom_vline(data = df_model_splits, mapping=aes(group = variable, xintercept=splits, linetype = model),show.legend = FALSE) +
  ggplot2::facet_wrap(~variable, ncol = 1, scales = "free_y") +
  ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 3)) +
  ggplot2::theme_bw() +
  ggthemes::scale_color_colorblind() +
  ggthemes::scale_fill_colorblind()

figure <- gridExtra::grid.arrange(p, p, p, ncol = 3)

save(p, file = "/home/malkusch/Dokumente/revisions/xiao-2019/rpart_splits.RData")
