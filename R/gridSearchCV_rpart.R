#' @title gridSearchCV.rpart
#'
#' @description
#' A cross validation grid search for hyperparameters of the CART tree.
#'
#' @details
#' Some mor details will follow
#'
#' @format [R6::R6Class] object.
#'
#' @import R6
#' @import tidyverse
#' @import rpart
#'
#' @include crossVal_rpart.R
#'
#' @author Sebastian Malkusch, \email{malkusch@@med.uni-frankfurt.de}
#'
#' @examples
#' data("iris")
#' df_data = tibble::as_tibble(iris)
#' ctrl <- rpart::rpart.control(minsplit = 3, maxdepth = 3)
#' cv <- pguXAI::crossVal.cart$new(obj = df_data, label = "Species", ctrl = ctrl, k = 10)
#' plot(cv)
#'
#' @export
#'

gridSearchCV.rpart <- R6::R6Class("gridSearchCV.rpart",
                                  ####################
                                  # instance variables
                                  ####################
                                  private = list(
                                    .data_df = "tbl_df",
                                    .label = "characetr",
                                    .seed = "integer",
                                    .k = "integer",
                                    .repeats = "integer",
                                    .grid = "tbl_df",
                                    .scores = "tbl_df",
                                    .verbose = "logical"
                                  ), #private
                                  ##################
                                  # accessor methods
                                  ##################
                                  active = list(
                                    #' @field data_df
                                    #' Returns the instance variable data_df
                                    #' (tibble::tibble)
                                    data_df = function(){
                                      return(private$.data_df)
                                    },
                                    #' @field label
                                    #' Returns the instance variable label
                                    #' (character)
                                    label = function(){
                                      return(private$.label)
                                    },
                                    #' @field seed
                                    #' Returns the instance variable seed
                                    #' (integer)
                                    seed = function(){
                                      return(private$.seed)
                                    },
                                    #' @field k
                                    #' Returns the instance variable k
                                    #' (integer)
                                    k = function(){
                                      return(private$.k)
                                    },
                                    #' @field repeats
                                    #' Returns the instance variable repeats
                                    #' (integer)
                                    repeats = function(){
                                      return(private$.repeats)
                                    },
                                    #' @field grid
                                    #' Returns the instance variable grid
                                    #' (tibble::tibble)
                                    grid = function(){
                                      return(private$.grid)
                                    },
                                    #' @field scores
                                    #' Returns the instance variable scores
                                    #' (tbl_df)
                                    scores = function(){
                                      return(private$.scores)
                                    },
                                    #' @field verbose
                                    #' Returns the instance variable verbose
                                    #' (logical)
                                    verbose = function(){
                                      return(private$.verbose)
                                    }
                                  ), #active
                                  public = list(
                                    ###################
                                    # memory management
                                    ###################
                                    #' @description
                                    #' Creates and returns a new gridSearchCV.rpart object.
                                    #' @param obj
                                    #' The data to be analyzed.
                                    #' (tibble::tibble)
                                    #' @param label
                                    #' The column name of the labels within obj
                                    #' (character)
                                    #' @param grid
                                    #' A list comprising the hyperparameter variations.
                                    #' (list)
                                    #' @param k
                                    #' The corss validation parameter
                                    #' (integer)
                                    #' @param repeats
                                    #' The repeat parameter
                                    #' (integer)
                                    #' @param seed
                                    #' The seed for the cross validation
                                    #' (integer)
                                    #' @param verbose
                                    #' Makes the class print results
                                    #' (logical)
                                    #' @return
                                    #' A new gridSearchCV.rpart object.
                                    #' (pguXAI::gridSearchCV.rpart)
                                    initialize = function(obj = "tbl_df", label = "character", grid = "list", k = 10, repeats = 1, seed = 42, verbose = FALSE){
                                      private$.data_df <- obj
                                      private$.label <- label
                                      private$.grid <- grid
                                      private$.seed <- seed
                                      set.seed(self$seed)
                                      private$.k <- k
                                      private$.repeats <- repeats
                                      private$.verbose <- verbose
                                      if(self$verbose){self$print()}
                                    }, #function
                                    #' @description
                                    #' Clears the heap and
                                    #' indicates that instance of gridSearchCV.rpart is removed from heap.
                                    finalize = function() {
                                      print("Instance of gridSearchCV.rpart removed from heap")
                                    }, #function
                                    #############
                                    # print class
                                    #############
                                    #' @description
                                    #' Prints instance variables of a gridSearchCV.rpart object.
                                    #' @return
                                    #' string
                                    print = function(){
                                      rString <- sprintf("\ngridSearchCV.rpart\n")
                                      rString <- sprintf("%s\nlabel: %s\n", rString, self$label)
                                      rString <- sprintf("%s\nk: %i\n", rString, self$k)
                                      rString <- sprintf("%s\nseed: %i\n", rString, self$seed)
                                      rString <- sprintf("%s\n\n", rString)
                                      cat(rString)
                                      invisible(self)
                                    }, #function
                                    #######
                                    # model
                                    #######
                                    #' @description
                                    #' creates a crossVal.rpart model and returns its scores
                                    #' @param ...
                                    #' The ctrl parameter of the model.
                                    calc_model_scores = function(...){
                                      cv <- pguXAI::crossVal.rpart$new(obj = self$data_df,
                                                                       label = self$label,
                                                                       ctrl = rpart::rpart.control(...),
                                                                       k = self$k,
                                                                       seed = self$seed,
                                                                       verbose = FALSE)
                                      cv$train(repeats = self$repeats)

                                      cv$scores %>%
                                        dplyr::slice(1) %>%
                                        dplyr::select(-statistic) %>%
                                        dplyr::mutate(accuracy = cv$accuracy[1]) %>%
                                        dplyr::mutate(auc_roc = cv$auc_roc) %>%
                                        return()
                                    }, #function
                                    #' @description
                                    #' runs the grid search
                                    train = function(){
                                      private$.scores <- self$grid %>%
                                        dplyr::bind_cols(purrr::pmap_dfr(self$grid, self$calc_model_scores))
                                    }, #function
                                    #' @description
                                    #' Returns the parameters of the best model
                                    #' @param score
                                    #' The score parameter by which the optimum is defined
                                    #' (character)
                                    best_model_parameter = function(score = "character"){
                                      idx <- self$scores %>%
                                        dplyr::select(score) %>%
                                        unlist() %>%
                                        which.max()

                                      self$grid %>%
                                        dplyr::slice(idx) %>%
                                        unlist() %>%
                                        return()
                                    } #function
                                  ) #public
) #class
