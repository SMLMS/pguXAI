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
#' @import factoextra
#' @import FactoMineR
#' @import gridExtra
#' @import ggpubr
#'
#' @author Sebastian Malkusch, \email{malkusch@@med.uni-frankfurt.de}
#'
#' @examples
#' library(tidyverse)
#' library(pguXAI)
#'
#' main = function(){
#'   # load data set and remove class labels
#'   df_data <- iris %>%
#'     dplyr::select(-Species)
#'
#'   # define true class labels
#'   classes_true <- iris$Species
#'
#'   # run principle component analysis
#'   ca <- pguXAI::componentAnalysis.pca$new()
#'   ca$train(obj=df_data)
#'
#'   # plot results
#'   ca$evr_overview_plot()
#'
#'   ca$evr_detail_plot() %>%
#'     show()
#'
#'   ca$cos2_corrplot() %>%
#'     show()
#'
#'   ca$cos2_barplot()
#'
#'   ca$contrib_corrplot() %>%
#'     show()
#'
#'   ca$contrib_barplot()
#'
#'   fin <- "done"
#'   fin
#' }
#'
#' main()
#'
#' @export
#'

componentAnalysis.pca <- R6::R6Class("componentAnalysis.pca",
                                     ####################
                                     # instance variables
                                     ####################
                                     private = list(
                                       .rslt_pca = "list",
                                       .df_pca = "tbl_df",
                                       .nComp = "integer",
                                       .verbose = "logical"
                                     ),#private
                                     ##################
                                     # accessor methods
                                     ##################
                                     active = list(
                                       #' @field rslt_pca
                                       #' Returns the instance variable rslt.pca
                                       #' (list)
                                       rslt_pca = function(){
                                         return(private$.rslt_pca)
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
                                       #' @param verbose
                                       #' Makes the class chatty.
                                       #' Default is FALSE.
                                       #' (logical)
                                       #' @return
                                       #' A new R6 object of type componentAnalysis.pca.
                                       #' (pguXAI::componentAnalysis.pca)
                                       initialize = function(verbose=FALSE){
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
                                         private$.nComp <- ncol(obj)

                                         PreProcessor <- caret::preProcess(x=obj, method=c("center", "scale"), pcaComp = self$nComp)
                                         df_scaled <- predict(PreProcessor, obj)

                                         private$.rslt_pca <- df_scaled %>%
                                           FactoMineR::PCA(ncp = self$nComp, scale.unit = FALSE, graph = FALSE)
                                         lst_pred <- predict(self$rslt_pca, df_scaled)

                                         pc_seq <- seq(self$nComp) %>%
                                           lapply(function(x) sprintf("pc.%i", x)) %>%
                                           unlist()
                                         df_data <- lst_pred$coord %>%
                                           as.data.frame() %>%
                                           dplyr::summarise_if(is.numeric, sd) %>%
                                           t()

                                         colnames(df_data) <- c("sd")
                                         private$.df_pca <- df_data %>%
                                           tibble::as_tibble() %>%
                                           dplyr::mutate(pc = pc_seq) %>%
                                           dplyr::mutate(evr = sd^2 / sum(sd^2)) %>%
                                           dplyr::mutate(cumSumEvr = cumsum(evr)) %>%
                                           dplyr::mutate(eigenvalue = self$rslt_pca$eig %>%
                                                           as.data.frame() %>%
                                                           dplyr::select(c("eigenvalue")) %>%
                                                           unlist()) %>%
                                           dplyr::select(c("pc", "eigenvalue", "sd", "evr", "cumSumEvr"))

                                         #
                                         # thr_comp <- sum(self$df_pca$cumSumEvr < self$threshold)
                                         # if (thr_comp < 2){
                                         #   if(self$verbose){
                                         #     print("Optimal Threshold is < 2. It will be set to 2.")
                                         #   }
                                         #   thr_comp <- 2
                                         # }
                                         # private$.nComp <- thr_comp


                                         # if(self$verbose){
                                         #   print("pca result:")
                                         #   print(head(df_pred))
                                         # }
                                       }, #train
                                       #' @description
                                       #' Plots the explained variance ration of the analysis.
                                       #' @return
                                       #' (ggplot2::ggplot)
                                       evr_overview_plot = function(){
                                         p_eigen <- self$df_pca %>%
                                           ggplot2::ggplot(
                                             mapping = aes(x = pc, y=eigenvalue)
                                           ) +
                                           ggplot2::geom_bar(stat = "identity") +
                                           ggplot2::labs(x = "components", y="value [a.u.]", title = "Eigenvalues")

                                         p_sd <- self$df_pca %>%
                                           ggplot2::ggplot(
                                             mapping = aes(x = pc, y=sd)
                                           ) +
                                           ggplot2::geom_bar(stat = "identity") +
                                           ggplot2::labs(x = "components", y="value [a.u.]", title = "Standard deviation")

                                         p_evr <- self$df_pca %>%
                                           ggplot2::ggplot(
                                             mapping = aes(x = pc, y=evr)
                                           ) +
                                           ggplot2::geom_bar(stat = "identity") +
                                           ggplot2::labs(x = "components", y="ratio", title = "Explained Variance")

                                         minY <- min(self$df_pca$cumSumEvr) - 0.01*min(self$df_pca$cumSumEvr)
                                         p_cevr <- p <- self$df_pca %>%
                                           ggplot2::ggplot(
                                             mapping = aes(x = pc, y=cumSumEvr)
                                           ) +
                                           ggplot2::geom_bar(stat = "identity") +
                                           ggplot2::coord_cartesian(ylim = c(minY, 1.01)) +
                                           ggplot2::labs(x = "components", y="cummulative ratio", title = "Explained Variance")

                                         p <- gridExtra::grid.arrange(p_eigen,p_sd,p_evr,p_cevr,
                                                                      layout_matrix = rbind(c(1,2), c(3,4)),
                                                                      top = ggpubr::text_grob("Component analysis", size = 18))
                                         return(p)
                                       },
                                       #' @description
                                       #' Plots the eigenvalues of the analysis.
                                       #' @return
                                       #' (ggplot2::ggplot)
                                       evr_detail_plot = function(){
                                         original_labels <- as.character(seq(self$nComp))
                                         substitute_labels <- lapply(original_labels, function(x) sprintf("pc.%s", x))
                                         p <- factoextra::fviz_eig(self$rslt_pca, choice = "variance", addlabels = TRUE, ylim = c(0, 80), ggtheme = ggplot2::theme_gray()) +
                                           ggplot2::labs(x = "components", y="value [%]", title = "Explained Variance") +
                                           ggplot2::scale_x_discrete(breaks = original_labels, labels = substitute_labels) +
                                           theme(plot.title = element_text(size=18),
                                                 axis.text=element_text(size=10),
                                                 axis.title=element_text(size=12))
                                         return(p)
                                       },
                                       #' @description
                                       #' Plots the quality of representation of the variables on factor map.
                                       #' @return
                                       #' (ggplot2::ggplot)
                                       cos2_corrplot = function(){
                                         mat_cos2 <- factoextra::get_pca_var(self$rslt_pca)$cos2

                                         pc_seq <- seq(self$nComp) %>%
                                           lapply(function(x) sprintf("pc.%i", x)) %>%
                                           unlist()

                                         colnames(mat_cos2) <- pc_seq

                                         df_cos2 <- melt(mat_cos2)
                                         df_cos2 <- df_cos2 %>%
                                           dplyr::mutate(value_round = round(value, 2))
                                         p <- ggplot2::ggplot(df_cos2, aes(x=Var1, y=Var2, fill=value, label = value_round)) +
                                           ggplot2::geom_tile() +
                                           geom_text() +
                                           ggplot2::scale_fill_gradient(low="white", high="blue", limits=c(0, 1)) +
                                           ggplot2::labs(x = "features", y = "components", title = "Quality of feature representation", fill = "Cos2 [a.u.]") +
                                           theme_minimal() +
                                           theme(plot.title = element_text(size=18),
                                                 axis.text=element_text(size=10),
                                                 axis.title=element_text(size=12),
                                                 axis.ticks = element_blank(),
                                                 axis.text.x=element_text(angle=45, hjust=1),
                                                 panel.grid.major = element_blank(),
                                                 panel.grid.minor = element_blank(),
                                           )
                                         return(p)
                                       },
                                       #' @description
                                       #' Plots the quality of representation of the variables on factor map.
                                       #' @return
                                       #' (ggplot2::ggplot)
                                       cos2_barplot = function(){
                                         n <- seq(self$nComp)
                                         cos2_single = function(max_dim = 2){
                                           title_single <- "PC 1"
                                           if(max_dim > 1){
                                             title_single <- sprintf("%s to %i", title_single, max_dim)
                                           }
                                           factoextra::fviz_cos2(self$rslt_pca, choice = "var", axes = 1:max_dim)+
                                             ggplot2::theme_grey() +
                                             ggplot2::theme(plot.margin = margin(10, 10, 10, 10, "pt"),
                                                            axis.text.x=element_text(angle=45, hjust=1)) +
                                             ggplot2::labs(x = "features", y = "Cos2 [a.u.]", title = title_single)
                                         }

                                         gridExtra::grid.arrange(grobs = lapply(n, function(x) cos2_single(x)),
                                                                 ncol=2,
                                                                 top = ggpubr::text_grob("Quality of feature representation", size = 18)) %>%
                                           return()
                                       },
                                       #' @description
                                       #' Maps the attributes contribution to each component.
                                       #' @return
                                       #' (ggplot2::ggplot)
                                       contrib_corrplot = function(){
                                         var <- factoextra::get_pca_var(self$rslt_pca)
                                         mat_contrib <- var$contrib

                                         pc_seq <- seq(self$nComp) %>%
                                           lapply(function(x) sprintf("pc.%i", x)) %>%
                                           unlist()

                                         colnames(mat_contrib) <- pc_seq

                                         df_contrib <- melt(mat_contrib)
                                         df_contrib <- df_contrib %>%
                                           dplyr::mutate(value_int = round(value))
                                         p <- ggplot2::ggplot(df_contrib, aes(x=Var2, y=Var1, fill=value, label = value_int)) +
                                           ggplot2::geom_tile() +
                                           geom_text() +
                                           ggplot2::scale_fill_gradient(low="white", high="blue", limits=c(0, 100)) +
                                           ggplot2::labs(x = "components", y = "features", title = "Feature contribution to component", fill = "contribution [%]") +
                                           theme_minimal() +
                                           theme(plot.title = element_text(size=18),
                                                 axis.text=element_text(size=10),
                                                 axis.title=element_text(size=12),
                                                 axis.ticks = element_blank(),
                                                 panel.grid.major = element_blank(),
                                                 panel.grid.minor = element_blank()
                                                 )
                                         return(p)
                                       },
                                       #' @description
                                       #' Plots the quality of representation of the variables on factor map.
                                       #' @return
                                       #' (ggplot2::ggplot)
                                       contrib_barplot = function(){
                                         n <- seq(self$nComp)
                                         contrib_single = function(max_dim = 2){
                                           title_single <- "PC 1"
                                           if(max_dim > 1){
                                             title_single <- sprintf("%s to %i", title_single, max_dim)
                                           }
                                           factoextra::fviz_contrib(self$rslt_pca, choice = "var", axes = 1:max_dim)+
                                             ggplot2::theme_grey() +
                                             ggplot2::theme(plot.margin = margin(10, 10, 10, 10, "pt"),
                                                            axis.text.x=element_text(angle=45, hjust=1)) +
                                             ggplot2::labs(x = "features", title = title_single)
                                         }

                                         gridExtra::grid.arrange(grobs = lapply(n, function(x) contrib_single(x)),
                                                                 ncol=2,
                                                                 top = ggpubr::text_grob("Contribution of features to", size = 18)) %>%
                                           return()
                                       }
                                     )#public
)#class
