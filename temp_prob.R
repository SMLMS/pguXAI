

obj_kmeans <- flexclust::kcca(obj, k=self$nCenters, family=kccaFamily("kmeans"), simple = TRUE)
classes_init <- self$level[flexclust::clusters(obj_kmeans)] %>%
  factor(levels = self$level)

# iterative training
df_predict <- tibble::tibble(measurement = seq(nrow(obj)))

for (i in seq(n)){
  col_name <- sprintf("pred_%i", i)
  classes_pred <- self$cluster_kmeans(obj, classes_init, i)
  df_predict <- df_predict %>%
    dplyr::mutate(!!col_name := classes_pred)
}

df_predict <- df_predict %>%
  dplyr::select(-c("measurement"))

# log training results into instance variables
private$.predProb <- df_predict %>%
  apply(1, function(x) summary(factor(x, levels=self$level))/sum(summary(factor(x, levels=self$level)))) %>%
  t()

private$.predClass <- factor(self$level[apply(self$predProb, 1, which.max)], levels=self$level)

cross_cl_table <- table(cluster_rslt, classes_init)

maxIdx <- apply(cross_cl_table, 1, function(x) which.max(x))

colnames_rslt <- colnames(cross_cl_table)[maxIdx]

classes_pred <- factor(colnames_rslt[cluster_rslt], levels = self$level)

#' @description
#' Performs iterative kMeans step.
#' Not to run by the user.
#' @param obj
#' The data to be analyzed.
#' Needs to be the result of a pca analysis.
#' (tibble::tibble)
#' @param classes_init
#' Contains the true class labels of the data in obj
#' (factor)
#' @param n
#' Indicates the number of training iterations
#' (integer)
cluster_kmeans = function(obj = "tbl_df", classes_init = "factor", n = "integer"){
  set.seed(self$seed+n)
  # cluster_rslt<- obj %>%
  #   stats::kmeans(centers = self$nCenters, nstart = 1)
  cluster_obj <- flexclust::kcca(obj, k=self$nCenters, family=kccaFamily("kmeans"), simple = TRUE)

  cluster_rslt <- cluster_obj %>%
    flexclust::clusters()

  cross_cl_table <- table(cluster_rslt, classes_init)

  maxIdx <- apply(cross_cl_table, 1, function(x) which.max(x))

  colnames_rslt <- colnames(cross_cl_table)[maxIdx]

  classes_pred <- factor(colnames_rslt[cluster_rslt], levels = self$level)

  print("distsum")
  cluster_obj %>%
    flexclust::info("distsum") %>%
    print()

  print("av_dist")
  cluster_obj %>%
    flexclust::info("av_dist") %>%
    print()

  return(classes_pred)
}, #cluster_kmeans

#' @description
#' Plots a histogram of Class probability.
#' @return
#' (list)
probHist_plot = function(){
  self$predProb %>%
    as.data.frame() %>%
    tibble::as_tibble() %>%
    dplyr::mutate(id = seq(nrow(self$predProb))) %>%
    reshape2::melt(id.vars = "id") %>%
    ggplot2::ggplot() +
    ggplot2::facet_wrap(~variable,scales = "free_x", ncol = ceiling(sqrt(self$nCenters))) +
    ggplot2::geom_histogram(mapping = ggplot2::aes(x=value), binwidth = 0.05) +
    ggplot2::scale_x_continuous(n.breaks = 3) +
    ggplot2::scale_y_continuous(n.breaks = 3) +
    ggplot2::labs(x = "probability", y="occurrence", title = "Class Probability Distribution") +
    ggplot2::theme(plot.title = element_text(size=18),
                   axis.text=element_text(size=10),
                   axis.title=element_text(size=12)) %>%
    return()
},
