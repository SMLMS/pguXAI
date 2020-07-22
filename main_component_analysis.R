library(tidyverse)
library(pguXAI)

main = function(){
  # load data set and remove class labels
  df_data <- iris %>%
    dplyr::select(-Species)

  # define true class labels
  classes_true <- iris$Species

  # run principle component analysis
  ca <- pguXAI::componentAnalysis.pca$new()
  ca$train(obj=df_data)

  # plot results
  ca$evr_overview_plot()

  ca$evr_detail_plot() %>%
    show()

  ca$cos2_corrplot() %>%
    show()

  ca$cos2_barplot()

  ca$contrib_corrplot() %>%
    show()

  ca$contrib_barplot()

  fin <- "done"
  fin
}

main()
