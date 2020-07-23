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
#'
#' @include pcaKMeans.R
#'
#' @author Sebastian Malkusch, \email{malkusch@@med.uni-frankfurt.de}
#'
#' @examples
#'
#' @export
#'

screen.KMeans <- R6::R6Class("screen.KMeans",
                             ####################
                             # instance variables
                             ####################
                             private = list(
                               .seed = "integer",
                               .maxCluster = "integer",
                               .df_data = "tbl_df",
                               .df_wcss = "tbl_df",
                               .df_silhouette = "tbl_df",
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
                               #' @field maxCluster
                               #' Returns the instance variable maxCluster
                               #' (integer)
                               maxCluster = function(){
                                 return(private$.maxCluster)
                               },
                               #' @field setMaxCluster
                               #' Sets the instance variable maxCluster
                               #' (integer)
                               setMaxCluster = function(value = "integer"){
                                 if (!(is.numeric(value))){
                                   warningMessage <- sprintf("Warning: maxClsuter needs to be of type numeric. maxCluster was given a value of type %s. maxCluster will be set to 2.", typeof(value))
                                   print(warningMessage)
                                   private$.maxCluster <- as.integer(2)
                                 } else if(length(value) > 1){
                                   warningMessage <- sprintf("Warning: maxCluster needs to be of length 1. maxCluster was given a value of length %i. maxCluster will be set to 2.", length(value))
                                   print(warningMessage)
                                   private$.maxCluster <- as.integer(2)
                                 } else if(value < 2){
                                   warningMessage <- sprintf("Warning: maxCluster needs at least 1. maxCluster was given a value of %i. maxCluster will be set to 2.", length(value))
                                   print(warningMessage)
                                   private$.maxCluster <- as.integer(2)
                                 }
                                 else{
                                   private$.maxCluster <- as.integer(value)
                                 }
                               },
                               #' @field df_data
                               #' Returns the instance variable df_data
                               #' (tbl_df)
                               df_data = function(){
                                 return(private$.df_data)
                               },
                               #' @field df_wcss
                               #' Returns the instance variable df_wcss
                               #' (tbl_df)
                               df_wcss = function(){
                                 return(private$.df_wcss)
                               },
                               #' @field df_silhouette
                               #' Returns the instance variable df_silhouette
                               #' (tbl_df)
                               df_silhouette = function(){
                                 return(private$.df_silhouette)
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
                               #' Creates and returns a new screen.KMeans object.
                               #' @param n
                               #' Initial number of maxCluster
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
                                 self$setMaxCluster <- n
                                 self$setSeed <- seed
                                 private$.verbose <- verbose
                                 private$.df_wcss <- tibble::tibble(cluster = seq(self$maxCluster)[c(-1)],
                                                                    mean = rep(0, self$maxCluster -1),
                                                                    sd = rep(0, self$maxCluster -1),
                                                                    spline = rep(0, self$maxCluster -1),
                                                                    d_spline = rep(0, self$maxCluster -1),
                                                                    dd_spline = rep(0, self$maxCluster -1))

                                 private$.df_silhouette <- tibble::tibble(cluster = seq(self$maxCluster)[c(-1)],
                                                                          mean = rep(0, self$maxCluster -1),
                                                                          sd = rep(0, self$maxCluster -1),
                                                                          spline = rep(0, self$maxCluster -1),
                                                                          d_spline = rep(0, self$maxCluster -1),
                                                                          dd_spline = rep(0, self$maxCluster -1))
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
                               #' Prints instance variables of screen.KMeans object.
                               #' @return
                               #' string
                               print = function(){
                                 rString <- sprintf("\nscreen.KMeans\n")
                                 rString <- sprintf("%s\nseed: %i\n", rString, self$seed)
                                 rString <- sprintf("%s\nmaxClusters: %i\n", rString, self$maxCluster)
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
                               #' number of iterations
                               #' (integer)
                               train = function(obj = "tbl_df", n = 10){
                                 private$.df_data = tibble::tibble()
                                 for (i in seq(n)){
                                   df_iter <- self$kmeansAnalysis(obj, i)
                                   private$.df_data <- rbind(self$df_data, df_iter)
                                 }
                                 self$wcssAnalysis()
                                 self$silhouetteAnalysis()

                                 print(self$df_wcss)
                                 print(self$df_silhouette)
                               },
                               #' @description
                               #' Performs an iterative KMeans step.
                               #' Not to run by the user.
                               #' @param obj
                               #' The data to be analyzed.
                               #' Needs to be the result of a pca analysis.
                               #' (tibble::tibble)
                               #' @param n
                               #' Seed additive
                               #' (integer)
                               #' @return
                               #' Result dataframe of the kmeans analysis.
                               #' (tbl_df)
                               kmeansAnalysis = function(obj, n){
                                 vec_cNumber <- rep(0, self$maxCluster-1)
                                 vec_seed <- rep(self$seed, self$maxCluster-1)
                                 vec_wcss <- rep(0, self$maxCluster-1)
                                 vec_silhouette <- rep(0, self$maxCluster-1)
                                 for (i in seq(self$maxCluster -1)){
                                   nCluster <- i+1
                                   km <- pguXAI::pca.KMeans$new(n=nCluster, seed = self$seed + n, verbose = FALSE)
                                   km$train(obj = obj)
                                   vec_cNumber[i] <- nCluster
                                   vec_wcss[i] <- km$tot_withinss
                                   vec_silhouette[i] <- km$av_sil_width
                                 }
                                 df_rslt <- tibble::tibble(cluster = vec_cNumber,
                                                           seed = vec_seed,
                                                           wcss = vec_wcss,
                                                           sil_width = vec_silhouette)
                                 return(df_rslt)
                               },
                               #' @description
                               #' Performs wcss analysis on df_data.
                               #' Not to run by the user.
                               wcssAnalysis = function(){
                                 df_summary <- self$df_data %>%
                                   dplyr::group_by(cluster) %>%
                                   dplyr::summarise(
                                     n = n(),
                                     mean = mean(wcss, na.rm = TRUE),
                                     sd = sd(wcss, na.rm = TRUE)
                                   )

                                 private$.df_wcss <- self$df_wcss %>%
                                   dplyr::mutate(cluster = df_summary[["cluster"]]) %>%
                                   dplyr::mutate(mean = df_summary[["mean"]]) %>%
                                   dplyr::mutate(sd = df_summary[["sd"]])

                                 fx <- stats::splinefun(df_summary[["cluster"]], y= df_summary[["mean"]])

                                 private$.df_wcss <- self$df_wcss %>%
                                   dplyr::mutate(spline = fx(df_summary[["cluster"]], deriv = 0)) %>%
                                   dplyr::mutate(d_spline = fx(df_summary[["cluster"]], deriv = 1)) %>%
                                   dplyr::mutate(dd_spline = fx(df_summary[["cluster"]], deriv = 2))
                               },
                               #' @description
                               #' Performs silhouette analysis on df_data.
                               #' Not to run by the user.
                               silhouetteAnalysis = function(){
                                 df_summary <- self$df_data %>%
                                   dplyr::group_by(cluster) %>%
                                   dplyr::summarise(
                                     n = n(),
                                     mean = mean(sil_width, na.rm = TRUE),
                                     sd = sd(sil_width, na.rm = TRUE)
                                   )

                                 private$.df_silhouette <- self$df_silhouette %>%
                                   dplyr::mutate(cluster = df_summary[["cluster"]]) %>%
                                   dplyr::mutate(mean = df_summary[["mean"]]) %>%
                                   dplyr::mutate(sd = df_summary[["sd"]])

                                 fx <- stats::splinefun(df_summary[["cluster"]], y= df_summary[["mean"]])

                                 private$.df_silhouette <- self$df_silhouette %>%
                                   dplyr::mutate(spline = fx(df_summary[["cluster"]], deriv = 0)) %>%
                                   dplyr::mutate(d_spline = fx(df_summary[["cluster"]], deriv = 1)) %>%
                                   dplyr::mutate(dd_spline = fx(df_summary[["cluster"]], deriv = 2))
                               }
                             )

) #screen.KMeans
