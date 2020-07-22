#' @title pca.KMeans
#'
#' @description
#' Performs KMeans clustering on a given pca dataset.
#'
#' @details
#' Performs KMeans clustering on a given pca dataset.
#'
#' @format [R6::R6Class] object.
#'
#' @import R6
#' @import tidyverse
#' @import reshape2
#' @import GGally
#' @import cluster
#' @import factoextra
#' @import flexclust
#'
#' @author Sebastian Malkusch, \email{malkusch@@med.uni-frankfurt.de}
#'
#' @examples
#' library(tidyverse)
#' library(pguXAI)
#' library(FactoMineR)
#' library(caret)
#'
#' main = function(){
#'   # load data set and remove class labels
#'   df_data <- iris %>%
#'     dplyr::select(-Species)
#'
#'   # define true class labels
#'   classes_true <- iris$Species
#'
#'   # define nuber of components for pca and number of clusters for kmeans
#'   nComponents <- 2
#'   nCluster <- 3
#'
#'   # pre-scale the data for pca
#'   PreProcessor <- caret::preProcess(x=df_data, method=c("center", "scale"), pcaComp = nComponents)
#'   df_scaled <- predict(PreProcessor, df_data)
#'
#'   # reduce dimensions of sclaed dataset using pca
#'   rslt_pca <- df_scaled %>%
#'     FactoMineR::PCA(ncp = nComponents, scale.unit = FALSE, graph = FALSE)
#'   df_pred <- as.data.frame(predict(rslt_pca, df_scaled)$coord)
#'
#'   # run kmeans analysis
#'   km <- pguXAI::pca.KMeans$new(n=nCluster, seed = 42, verbose = TRUE)
#'   km$train(obj = df_pred, n = 100)
#'
#'   # plot results
#'   km$probHist_plot() %>%
#'     plot()
#'
#'   km$cluster_plot(obj = df_pred)
#'
#'   km$silhouette_plot(obj = df_pred) %>%
#'     plot()
#'
#'   print("Result of silhouette analysis:")
#'   km$df_silhouette %>%
#'     print()
#'
#'   print("Average silhouette width:")
#'   km$av_sil_width %>%
#'     print()
#'
#'   print("Centers of clusters:")
#'   km$df_centers %>%
#'     print()
#'
#'   print("Probability of the class label assignment:")
#'   km$predProb %>%
#'     print()
#'
#'   print("Majority vote of the class label assignment:")
#'   km$predClass %>%
#'     print()
#'
#'   fin <- "done"
#'   fin
#' }
#'
#' main()
#'
#' @export
#'

pca.KMeans <- R6::R6Class("pca.KMeans",
                          ####################
                          # instance variables
                          ####################
                          private = list(
                            .seed = "integer",
                            .nCenters = "integer",
                            .level = "character",
                            .predProb = "matrix",
                            .predClass = "factor",
                            .df_centers = "tbl_df",
                            .df_silhouette = "tbl_df",
                            .av_sil_width = "numeric",
                            .verbose = "logical"
                          ), # private
                          ##################
                          # accessor methods
                          ##################
                          active = list(
                            #' @field seed
                            #' Returns the instance variable seed
                            #' (integer)
                            seed = function(){
                              return(as.integer(private$.seed))
                            },
                            #' @field setSeed
                            #' Sets the instance variable seed
                            #' (integer)
                            setSeed = function(value = "integer"){
                              if (!(is.numeric(value))){
                                warningMessage <- sprintf("Warning: Seed needs to be of type numeric. setSeed was given a value of type %s. Seed will be set to 42.", typeof(value))
                              print(warningMessage)
                              private$.seed <- as.integer(42)
                              } else if(length(value) > 1){
                                warningMessage <- sprintf("Warning: Seed needs to be of length 1. setSeed was given a value of length %i. Seed will be set to 42.", length(value))
                                print(warningMessage)
                                private$.seed <- as.integer(42)
                              }
                              else{
                                private$.seed <- as.integer(value)
                              }
                            },
                            #' @field nCenters
                            #' Returns the instance variable nCenters
                            #' (integer)
                            nCenters = function(){
                              return(private$.nCenters)
                            },
                            #' @field setNCenters
                            #' Sets the instance variable nCenters
                            #' (integer)
                            setNCenters = function(value = "integer"){
                              if (!(is.numeric(value))){
                                warningMessage <- sprintf("Warning: nCenters needs to be of type numeric. nCenters was given a value of type %s. nCenters will be set to 2.", typeof(value))
                                print(warningMessage)
                                private$.nCenters <- as.integer(2)
                              } else if(length(value) > 1){
                                warningMessage <- sprintf("Warning: nCenters needs to be of length 1. nCenters was given a value of length %i. nCenters will be set to 2.", length(value))
                                print(warningMessage)
                                private$.nCenters <- as.integer(2)
                              }
                              else{
                                private$.nCenters <- as.integer(value)
                              }
                              level_idx <- seq(self$nCenters)
                              private$.level <- sapply(level_idx, function(x) sprintf("Class_%i", x))
                            },
                            #' @field level
                            #' Returns the instancs variable level
                            #' (character)
                            level = function(){
                              return(private$.level)
                            },
                            #' @field predProb
                            #' Returns th instance variable predProb
                            #' (matrix)
                            predProb = function(){
                              return(private$.predProb)
                            },
                            #' @field predClass
                            #' Returns the instance variable predClass
                            #' (factor)
                            predClass = function(){
                              return(private$.predClass)
                            },
                            #' @field df_centers
                            #' Returns the instance variable df_centers
                            #' (tbl_df)
                            df_centers = function(){
                              return(private$.df_centers)
                            },
                            #' @field df_silhouette
                            #' Returns the instance variable df_silhouette
                            #' (tbl_df)
                            df_silhouette = function(){
                              return(private$.df_silhouette)
                            },
                            #' @field av_sil_width
                            #' Returns the instance variable av_sil_width
                            #' (numeric)
                            av_sil_width = function(){
                              return(private$.av_sil_width)
                            },
                            #' @field verbose
                            #' Returns the instance variable verbose
                            #' (logical)
                            verbose = function(){
                              return(private$.verbose)
                            }
                          ), #active
                          public =list(
                            ###################
                            # memory management
                            ###################
                            #' @description
                            #' Creates and returns a new pca.KMeans object.
                            #' @param n
                            #' Initial number of cluster
                            #' (integer)
                            #' @param seed
                            #' An initial seed.
                            #' Default is 42
                            #' (integer)
                            #' @param verbose
                            #' Makes the class chatty.
                            #' Default is FALSE.
                            #' (logical)
                            #' @return
                            #' A new R6 object of type pca.KMeans.
                            #' (pguXAI::pca.KMeans)
                            initialize = function(n=2, seed = 42, verbose = FALSE){
                              self$setNCenters <- n
                              self$setSeed <- seed
                              private$.verbose <- verbose
                              level_idx <- seq(self$nCenters)
                              private$.level <- sapply(level_idx, function(x) sprintf("Class_%i", x))
                            }, #initialize
                            #' @description
                            #' Clears the heap and
                            #' indicates that instance of pca.KMeans is removed from heap.
                            finalize = function() {
                              if(self$verbose){
                                print("Instance of pca.KMeans removed from heap")
                              }
                            }, #finalize
                            #############
                            # print class
                            #############
                            #' @description
                            #' Prints instance variables of a pca.KMeans object.
                            #' @return
                            #' string
                            print = function(){
                              rString <- sprintf("\npca.KMeans\n")
                              rString <- sprintf("%s\nseed: %i\n", rString, self$seed)
                              rString <- sprintf("%s\nnCenters: %i\n", rString, self$nCenters)
                              rString <- sprintf("%s\nverbose: %s\n", rString, self$verbose)
                              rString <- sprintf("%s\n\n", rString)
                              cat(rString)
                              invisible(self)
                            }, #function
                            #' @description
                            #' trains the model
                            #' @param obj
                            #' The data to be analyzed.
                            #' Needs to be the result of a pca analysis.
                            #' (tibble::tibble)
                            #' @param n
                            #' Indicates the number of training iterations
                            #' (integer)
                            train = function(obj = "tbl_df", n = 100){
                              # inital run to set up class names
                              set.seed(self$seed)
                              # cluster_rslt <- obj %>%
                              #   stats::kmeans(centers = self$nCenters, nstart = 20)
                              #
                              # classes_init <- self$level[cluster_rslt$cluster] %>%
                              #   factor(levels = self$level)
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

                              self$cluster_statistics(obj)
                              self$silhouette_analysis(obj)
                            }, #train
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
                              cluster_rslt <- flexclust::kcca(obj, k=self$nCenters, family=kccaFamily("kmeans"), simple = TRUE) %>%
                                flexclust::clusters()

                              cross_cl_table <- table(cluster_rslt, classes_init)

                              maxIdx <- apply(cross_cl_table, 1, function(x) which.max(x))

                              colnames_rslt <- colnames(cross_cl_table)[maxIdx]

                              classes_pred <- factor(colnames_rslt[cluster_rslt], levels = self$level)

                              return(classes_pred)
                            }, #cluster_kmeans
                            #' @description
                            #' Performs iterative kMeans step.
                            #' Not to run by the user.
                            #' @param obj
                            #' The data to be analyzed.
                            #' Needs to be the result of a pca analysis.
                            #' (tibble::tibble)
                            cluster_statistics = function(obj = "tbl_df"){
                              df_stat <- tibble::tibble(class = self$level)
                              df_data <- obj %>%
                                dplyr::mutate(class = self$predClass)

                              df_statistics <- NULL


                              for (className in self$level){
                                df_temp <- df_data %>%
                                  dplyr::filter(class == className) %>%
                                  dplyr::select(-c("class"))

                                df_statistics_temp <- do.call(cbind, lapply(df_temp, summary)) %>%
                                  t() %>%
                                  as.data.frame() %>%
                                  tibble::rownames_to_column("component") %>%
                                  tibble::as_tibble() %>%
                                  dplyr::mutate(class = className)


                                class_vector <- c()
                                comp_vector <- c()
                                low_bound_vector <- c()
                                high_bound_vector <- c()
                                if(nrow(df_temp) > 0){
                                  for (compName in colnames(df_temp)){
                                    test_rslt <- df_temp %>%
                                      dplyr::select(compName) %>%
                                      stats::t.test()
                                    class_vector <- append(class_vector,className)
                                    comp_vector <- append(comp_vector, compName)
                                    low_bound_vector <- append(low_bound_vector, test_rslt$conf.int[1])
                                    high_bound_vector <- append(high_bound_vector, test_rslt$conf.int[2])
                                  }
                                } else {
                                  for (compName in colnames(df_temp)){
                                    class_vector <- append(class_vector,className)
                                    comp_vector <- append(comp_vector, compName)
                                    low_bound_vector <- append(low_bound_vector, NA)
                                    high_bound_vector <- append(high_bound_vector, NA)
                                  }
                                }

                                df_conf <- tibble::tibble(class = class_vector,
                                                          component = comp_vector,
                                                          lBound_95 = low_bound_vector,
                                                          hBound_95 = high_bound_vector)

                                df_statistics_temp <- df_statistics_temp %>%
                                  merge(df_conf, by = c("class", "component"))


                                df_statistics <- df_statistics %>%
                                  dplyr::bind_rows(df_statistics_temp)
                              }
                              private$.df_centers <- df_statistics

                            }, #cluster_statistics
                            #' @description
                            #' Performs a silouette analysis.
                            #' Not to run by the user.
                            #' @param obj
                            #' The data to be analyzed.
                            #' Needs to be the result of a pca analysis.
                            #' (tibble::tibble)
                            silhouette_analysis = function(obj = "tbl_df"){
                              dist_mat <- stats::dist(obj, method = "euclidean")

                              sil <- self$predClass %>%
                                match(self$level) %>%
                                cluster::silhouette(dist_mat) %>%
                                abs()

                              sil_sum <- summary(sil)
                              class_names <- dimnames(sil_sum$clus.avg.widths) %>%
                                lapply( function(x) sprintf("Class_%s", x)) %>%
                                unlist()

                              private$.df_silhouette <- tibble::tibble(class = class_names) %>%
                                dplyr::mutate(size_abs = as.integer(sil_sum$clus.sizes)) %>%
                                dplyr::mutate(size_rel = size_abs / sum(size_abs)) %>%
                                dplyr::mutate(sil_width = sil_sum$clus.avg.widths)

                              private$.av_sil_width <- sil_sum$avg.width
                            },
                            ################
                            # plot functions
                            ################
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
                            #' @description
                            #' Plots Clustering Result in all pca dimensions
                            #' @param obj
                            #' The data to be analyzed.
                            #' Needs to be the result of a pca analysis.
                            #' (tibble::tibble)
                            #' @return
                            #' (list)
                            cluster_plot = function(obj = "tbl_df"){
                              p <- obj %>%
                                dplyr::mutate(class = self$predClass) %>%
                                GGally::ggpairs(mapping=ggplot2::aes(colour = class), title = "Cluster structure (kMeans)") %>%
                                GGally::print_if_interactive()
                              return(p)
                            }, #cluster_plot
                            #' @description
                            #' Plots Silhouette analysis
                            #' @param obj
                            #' The data to be analyzed.
                            #' Needs to be the result of a pca analysis.
                            #' (tibble::tibble)
                            #' @return
                            #' (list)
                            silhouette_plot = function(obj = "tbl_df"){
                              dist_mat <- stats::dist(obj, method = "euclidean")

                              sil <- self$predClass %>%
                                match(self$level) %>%
                                cluster::silhouette(dist_mat) %>%
                                abs()

                              factoextra::fviz_silhouette(sil) +
                                #ggplot2::scale_fill_discrete(name = "Cluster", labels = self$level) +
                                ggplot2::guides(col=FALSE) +
                                ggplot2::theme_minimal() +
                                ggplot2::theme(plot.title = element_text(size=18),
                                               axis.text.y=element_text(size=10),
                                               axis.title.y=element_text(size=12),
                                               axis.ticks = element_blank(),
                                               axis.text.x = element_blank(),
                                               axis.title.x = element_blank(),
                                               panel.grid.major = element_blank(),
                                               panel.grid.minor = element_blank()
                                )
                            }
                          )#public
)#pca.KMeans
