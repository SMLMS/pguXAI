#' @title componentAnalysis.pca
#'
#' @description
#' Performs a pca anlysis on a given dataset.
#' The explained variance ratio is calculated
#' and used to indicate the optimal number of components for a pca.
#'
#' @details
#' Performs a pca anlysis on a given dataset.
#' The explained variance ratio is calculated
#' and used to indicate the optimal number of components for a pca.
#'
#' @format [R6::R6Class] object.
#'
#' @import R6
#' @import tidyverse
#' @import caret
#' @import gridExtra
#'
#' @author Sebastian Malkusch, \email{malkusch@@med.uni-frankfurt.de}
#'
#' @examples
#' data_df <- iris %>%
#'    dplyr::select(-Species)
#' ca <- pguXAI::componentAnalysis.pca$new(thr = 0.99, verbose=FALSE)
#' ca$train(obj=data_df)
#' print(ca)
#' plot(ca)
#'
#' @export
#'

componentAnalysis.pca <- R6::R6Class("componentAnalysis.pca",
                                     ####################
                                     # instance variables
                                     ####################
                                     private = list(
                                       .threshold = "numeric",
                                       .df_pca = "tbl_df",
                                       .nComp = "integer",
                                       .verbose = "logical"
                                     ),#private
                                     ##################
                                     # accessor methods
                                     ##################
                                     active = list(
                                       #' @field threshold
                                       #' Returns the instance variable threshold
                                       #' (numeric)
                                       threshold = function(){
                                         return(private$.threshold)
                                       },
                                       #' @field setThreshold
                                       #' Sets the instance variable threshold
                                       #' (numeric)
                                       setThreshold = function(value = "numeric"){
                                         if (!(is.numeric(value))){
                                           warningMessage <- sprintf("Warning: Threshold needs to be of type numeric. setThreshold was given a value of type %s. Threshold will be set to 0.95.", typeof(value))
                                           print(warningMessage)
                                           private$.threshold <- 0.95
                                         } else if(length(value) > 1){
                                          warningMessage <- sprintf("Warning: Threshold needs to be of length 1. setThreshold was given a value of length %i. Threshold will be set to 0.95.", length(value))
                                          print(warningMessage)
                                          private$.threshold <- 0.95
                                         } else if(!(value <= 1.0)){
                                           warningMessage <- sprintf("Warning: Threshold needs to be in range [0;1]. setThreshold was given a value of  %.3e. Threshold will be set to 0.95.", value)
                                           print(warningMessage)
                                           private$.threshold <- 0.95
                                         }
                                         else{
                                           private$.threshold <- value
                                         }
                                       },
                                       #' @field df_pca
                                       #' Returns the instance variable df_pca
                                       #' (tibble::tibble)
                                       df_pca = function(){
                                         return(private$.df_pca)
                                       },
                                       #' @field nComp
                                       #' Returns the instance varibale nComp
                                       #' (integer)
                                       nComp = function(){
                                         return(private$.nComp)
                                       },
                                       #' @field verbose
                                       #' Returns the instance variable verbose
                                       #' (logical)
                                       verbose = function(){
                                         return(private$.verbose)
                                       }
                                     ),#active
                                     public = list(
                                       ###################
                                       # memory management
                                       ###################
                                       #' @description
                                       #' Creates and returns a new componentAnalysis.pca object.
                                       #' @param thr
                                       #' An initial threshold.
                                       #' Default is 0.95
                                       #' (numeric)
                                       #' @param verbose
                                       #' Makes the class chatty.
                                       #' Default is FALSE.
                                       #' (logical)
                                       #' @return
                                       #' A new R6 object of type componentAnalysis.pca.
                                       #' (pguXAI::componentAnalysis.pca)
                                       initialize = function(thr = 0.95, verbose=FALSE){
                                         self$setThreshold <- thr
                                         private$.verbose <- verbose
                                       }, #initialize
                                       #' @description
                                       #' Clears the heap and
                                       #' indicates that instance of componentAnalysis.pca is removed from heap.
                                       finalize = function() {
                                         if(self$verbose){
                                           print("Instance of componentAnalysis.pca removed from heap")
                                         }
                                       }, #finalize
                                       #############
                                       # print class
                                       #############
                                       #' @description
                                       #' Prints instance variables of a componentAnalysis.pca object.
                                       #' @return
                                       #' string
                                       print = function(){
                                         rString <- sprintf("\ncomponentAnalysis.pca\n")
                                         rString <- sprintf("%s\nthreshold: %.3e\n", rString, self$threshold)
                                         rString <- sprintf("%s\nComp: %i\n", rString, self$nComp)
                                         rString <- sprintf("%s\nverbose: %s\n", rString, self$verbose)
                                         rString <- sprintf("%s\n\n", rString)
                                         cat(rString)
                                         invisible(self)
                                       }, #function
                                       #' @description
                                       #' trains the model
                                       #' @param obj
                                       #' The data to be analyzed.
                                       #' (tibble::tibble)
                                       train = function(obj = "tbl_df"){
                                         PreProcessor <- caret::preProcess(x=obj, method=c("center", "scale", "pca"), thresh = 1.0)
                                         df_pred <- predict(PreProcessor, obj)
                                         pc_seq <- colnames(df_pred)
                                         df_data <- df_pred %>%
                                           dplyr::summarise_if(is.numeric, sd) %>%
                                           t()

                                         colnames(df_data) <- c("sd")
                                         private$.df_pca <- df_data %>%
                                           tibble::as_tibble() %>%
                                           dplyr::mutate(pc = pc_seq) %>%
                                           dplyr::mutate(evr = sd^2 / sum(sd^2)) %>%
                                           dplyr::mutate(cumSumEvr = cumsum(evr)) %>%
                                           dplyr::select(c("pc", "sd", "evr", "cumSumEvr"))

                                         thr_comp <- sum(self$df_pca$cumSumEvr < self$threshold)
                                         if (thr_comp < 2){
                                           if(self$verbose){
                                             print("Optimal Threshold is < 2. It will be set to 2.")
                                           }
                                           thr_comp <- 2
                                         }
                                         private$.nComp <- thr_comp


                                         if(self$verbose){
                                           print("pca result:")
                                           print(head(df_pred))
                                         }
                                       }, #train
                                       #' @description
                                       #' Plots standard deviation of detected compenents.
                                       #' @return
                                       #' (list)
                                       sd_plot = function(){
                                         p <- self$df_pca %>%
                                           ggplot2::ggplot(
                                             mapping = aes(x = pc, y=sd)
                                           ) +
                                           ggplot2::geom_bar(stat = "identity") +
                                           # ggplot2::coord_cartesian(ylim = c(minY, 1.01)) +
                                           ggplot2::labs(x = "PC", y="value [a.u.]", title = "Standard deviation")
                                         return(p)
                                       }, #plot_sd
                                       #' @description
                                       #' Plots the explained variance ratio of the components
                                       #' @return
                                       #' (list)
                                       evr_plot = function(){
                                         p <- self$df_pca %>%
                                           ggplot2::ggplot(
                                             mapping = aes(x = pc, y=evr)
                                           ) +
                                           ggplot2::geom_bar(stat = "identity") +
                                           # ggplot2::coord_cartesian(ylim = c(minY, 1.01)) +
                                           ggplot2::labs(x = "PC", y="ratio", title = "Explained Variance")
                                         return(p)
                                       }, #plot evr
                                       #' @description
                                       #' Plots the cummulative sum of the explained variance ratio of the components
                                       #' @return
                                       #' (list)
                                       cumSumEvr_plot = function(){
                                         minY <- min(self$df_pca$cumSumEvr) - 0.01*min(self$df_pca$cumSumEvr)
                                         p <- self$df_pca %>%
                                           ggplot2::ggplot(
                                             mapping = aes(x = pc, y=cumSumEvr)
                                           ) +
                                           ggplot2::geom_bar(stat = "identity") +
                                           ggplot2::coord_cartesian(ylim = c(minY, 1.01)) +
                                           ggplot2::geom_hline(aes(yintercept=self$threshold, colour = "cutoff"), linetype="dashed", size=1) +
                                           ggplot2::scale_color_manual(name="legend", values=c(cutoff="blue")) +
                                           ggplot2::theme(legend.key = element_rect(fill = "white", colour = "white")) +
                                           ggplot2::labs(x = "PC", y="cummulative ratio", title = "Explained Variance")
                                         print(typeof(p))
                                         return(p)
                                       }, #cumSumEvr_plot
                                       #' @description
                                       #' Plots the analysis result.
                                       #' @return
                                       #' (list)
                                       plot = function(){
                                         p_blank <- ggplot2::ggplot() +
                                           ggplot2::geom_blank() +
                                           ggplot2::theme_bw() +
                                           ggplot2::theme(panel.border = element_blank())
                                         p1 <- self$sd_plot()
                                         p2 <- self$evr_plot()
                                         p3 <- self$cumSumEvr_plot()
                                         p4 <- p_blank
                                         p <- gridExtra::grid.arrange(p1,p2,p3,p4, layout_matrix = rbind(c(1,1,1,2,2,2), c(3,3,3,3,4,4)))
                                         invisible(self)
                                       }
                                     )#public
)#class
