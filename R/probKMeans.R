#' @title prob.KMeans
#'
#' @description
#' Calculates the probability of clutser sorting on a given pca dataset.
#'
#' @details
#' Calculates the probability of clutser sorting on a given pca dataset.
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

prob.KMeans <- R6::R6Class("prob.KMeans",
                           ####################
                           # instance variables
                           ####################
                           private = list(
                             .seed = "integer",
                             .nCluster = "integer",
                             .maxIter = "integer",
                             .ref_classes = "factor",
                             .level = "character",
                             .df_prob = "tbl_df",
                             .pred_classes = "factor",
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
                             #' @field nCluster
                             #' Returns the instance variable nCluster
                             #' (integer)
                             nCluster = function(){
                               return(private$.nCluster)
                             },
                             #' @field setNCluster
                             #' Sets the instance variable nCluster
                             #' (integer)
                             setNCluster = function(value = "integer"){
                               if (!(is.numeric(value))){
                                 warningMessage <- sprintf("Warning: nClsuter needs to be of type numeric. nCluster was given a value of type %s. nCluster will be set to 2.", typeof(value))
                                 print(warningMessage)
                                 private$.nCluster <- as.integer(2)
                               } else if(length(value) > 1){
                                 warningMessage <- sprintf("Warning: nCluster needs to be of length 1. nCluster was given a value of length %i. nCluster will be set to 2.", length(value))
                                 print(warningMessage)
                                 private$.nCluster <- as.integer(2)
                               } else if(value < 2){
                                 warningMessage <- sprintf("Warning: nCluster needs at least 2. nCluster was given a value of %i. nCluster will be set to 2.", length(value))
                                 print(warningMessage)
                                 private$.nCluster <- as.integer(2)
                               }
                               else{
                                 private$.nCluster <- as.integer(value)
                               }
                             },
                             #' @field maxIter
                             #' Returns the instance variable maxIter
                             #' (integer)
                             maxIter = function(){
                               return(as.integer(private$.maxIter))
                             },
                             #' @field setMaxIter
                             #' Sets the instance variable maxIter
                             #' (integer)
                             setMaxIter = function(value = "integer"){
                               if (!(is.numeric(value))){
                                 warningMessage <- sprintf("Warning: maxIter needs to be of type numeric. maxIter was given a value of type %s. maxIter will be set to 2.", typeof(value))
                                 print(warningMessage)
                                 private$.maxIter <- as.integer(2)
                               } else if(length(value) > 1){
                                 warningMessage <- sprintf("Warning: maxIter needs to be of length 1. maxIter was given a value of length %i. maxIter will be set to 2.", length(value))
                                 print(warningMessage)
                                 private$.maxIter <- as.integer(2)
                               }
                               else{
                                 private$.maxIter <- as.integer(value)
                               }
                             },
                             #' @field ref_classes
                             #' Returns the instance variable ref_classes
                             #' (factor)
                             ref_classes = function(){
                               return(private$.ref_classes)
                             },
                             #' @field level
                             #' Returns the instance variable level.
                             #' (character)
                             level = function(){
                               return(private$.level)
                             },
                             #' @field df_prob
                             #' Returns the instance variable df_prob
                             #' (df_tbl)
                             df_prob = function(){
                               return(private$.df_prob)
                             },
                             #' @field pred_classes
                             #' Returns the instance variable pred_classes
                             #' (factor)
                             pred_classes = function(){
                               return(private$.pred_classes)
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
                             train = function(obj = "tbl_df"){

)# prob.KMeans
