#' @title crossVal.rpart
#'
#' @description
#' A Cross Validation for the CART tree.
#'
#' @details
#' Some mor details will follow
#'
#' @format [R6::R6Class] object.
#'
#' @import R6
#' @import tidyverse
#' @import rpart
#' @import rpart.plot
#' @import caret
#' @import pROC
#'
#' @author Sebastian Malkusch, \email{malkusch@@med.uni-frankfurt.de}
#'
#' @examples
#' data("iris")
#' df_data = tibble::as_tibble(iris)
#' ctrl <- rpart::rpart.control(minsplit = 3, maxdepth = 3)
#' cv <- pguXAI::crossVal.rpart$new(obj = df_data, label = "Species", ctrl = ctrl, k = 10)
#' plot(cv)
#'
#' @export
#'

crossVal.rpart <- R6::R6Class("crossVal.rpart",
                              ####################
                              # instance variables
                              ####################
                              private = list(
                                .df_data = "tbl_df",
                                .label = "character",
                                .seed = "integer",
                                .lvls= "character",
                                .k = "integer",
                                .ctrl = "rpart::rpart.control",
                                .auc_roc = "numeric",
                                .confusion_matrix = "numeric",
                                .scores = "tbl_df",
                                .accuracy = "numeric",
                                .verbose = "logical"
                              ),
                              ##################
                              # accessor methods
                              ##################
                              active = list(
                                #' @field df_data
                                #' Returns the instance variable df_data
                                #' (tibble::tibble)
                                df_data = function(){
                                  return(private$.df_data)
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
                                #' @field lvls
                                #' Returns the instance variable lvls
                                #' (character)
                                lvls = function(){
                                  return(private$.lvls)
                                },
                                #' @field k
                                #' Returns the instance variable k
                                #' (integer)
                                k = function(){
                                  return(private$.k)
                                },
                                #' @field ctrl
                                #' Returns the instance variable ctrl
                                #' (rpart::rpart.control)
                                ctrl = function(){
                                  return(private$.ctrl)
                                },
                                #' @field auc_roc
                                #' Returns the instance variable auc_roc
                                #' (numeric)
                                auc_roc = function(){
                                  return(private$.auc_roc)
                                },
                                #' @field confusion_matrix
                                #' Returns the instance variable confucion_matrix
                                confusion_matrix = function(){
                                  return(private$.confusion_matrix)
                                },
                                #' @field scores
                                #' Returns the cross validation scores
                                #' (tibble::tibble)
                                scores = function(){
                                  return(private$.scores)
                                },
                                #' @field accuracy
                                #' Returns the corss validation accuracy
                                #' (numeric)
                                accuracy = function(){
                                  return(private$.accuracy)
                                },
                                #' @field verbose
                                #' Returns the corss validation verbose
                                #' (logical)
                                verbose = function(){
                                  return(private$.verbose)
                                }
                              ),
                              public = list(
                                ###################
                                # memory management
                                ###################
                                #' @description
                                #' Creates and returns a new crossVal.rpart object.
                                #' @param obj
                                #' The data to be analyzed.
                                #' (tibble::tibble)
                                #' @param label
                                #' The column name of the labels within obj
                                #' (character)
                                #' @param ctrl
                                #' The control sequence for the CART tree
                                #' (rpart::rpart.control)
                                #' @param k
                                #' The corss validation parameter
                                #' (integer)
                                #' @param seed
                                #' The seed for the cross validation
                                #' (integer)
                                #' @param verbose
                                #' Makes the class print results
                                #' (logical)
                                #' @return
                                #' A new crossVal.rpart object.
                                #' (pguXAI::crossVal.rpart)
                                initialize = function(obj = "tbl_df", label = "character", ctrl = "rpart::rpart.control", k = 10, seed = 42, verbose = FALSE){
                                  private$.df_data = obj
                                  private$.label <- label
                                  private$.seed <- seed
                                  private$.k <- k
                                  private$.lvls <- obj %>%
                                    dplyr::select(self$label) %>%
                                    unlist() %>%
                                    as.character() %>%
                                    unique()
                                  private$.ctrl <- ctrl
                                  private$.verbose <- verbose
                                }, #function
                                #' @description
                                #' trains the model
                                #' @param repeats
                                #' The repeat parameter
                                #' (integer)
                                train = function(repeats = 1){
                                  list_result <- list()
                                  for (i in 1:repeats){
                                    set.seed(self$seed + i)
                                    df_cv <- self$df_data %>%
                                      dplyr::rename(label = self$label) %>%
                                      modelr::crossv_kfold(k = self$k, id = ".id")

                                    for (i in 1:nrow(df_cv)){
                                      df_train <- df_cv$train[[i]] %>%
                                        as.data.frame()
                                      df_test <- df_cv$test[[i]] %>%
                                        as.data.frame()
                                      model <- rpart(label~., data = df_train, control = self$ctrl, parms = list(split = "gini"))
                                      df_result_step <- stats::predict(model, df_test, type = "prob") %>%
                                        tibble::as_tibble()
                                      df_result_step$predicted <- apply(df_result_step, 1, which.max)
                                      df_result_step <- df_result_step %>%
                                        dplyr::mutate(predicted = self$lvls[predicted]) %>%
                                        dplyr::mutate(predicted = factor(predicted, levels = self$lvls)) %>%
                                        dplyr::mutate(reference = df_test$label)

                                      list_result <- append(list_result, list(df_result_step))
                                    }
                                  }
                                  df_result <- dplyr::bind_rows(list_result)


                                  cmat <- caret::confusionMatrix(data = df_result$predicted,
                                                                 reference = df_result$reference,
                                                                 mode = "everything")
                                  private$.accuracy <- cmat$overall
                                  private$.confusion_matrix <- cmat$table

                                  cmat_mean <- cmat$byClass %>%
                                    tibble::as_tibble() %>%
                                    dplyr::summarize_all(mean) %>%
                                    dplyr::mutate(statistic = c("mean")) %>%
                                    dplyr::select(statistic, everything())

                                  cmat_sd <- cmat$byClass %>%
                                    tibble::as_tibble() %>%
                                    dplyr::summarize_all(sd) %>%
                                    dplyr::mutate(statistic = c("sd")) %>%
                                    dplyr::select(statistic, everything())

                                  private$.scores <- rbind(cmat_mean, cmat_sd)
                                  pred_prob <- df_result %>%
                                    dplyr::select(-c("predicted", "reference")) %>%
                                    as.matrix()

                                  multi_roc <- df_result %>%
                                    dplyr::select(reference) %>%
                                    unlist() %>%
                                    pROC::multiclass.roc(pred_prob, levels = self$lvls, percent = FALSE)

                                  private$.auc_roc <- multi_roc$auc[1]

                                  if(self$verbose){self$print()}
                                }, #function
                                #' @description
                                #' Clears the heap and
                                #' indicates that instance of crossVal.rpart is removed from heap.
                                finalize = function() {
                                  if(self$verbose){
                                    print("Instance of crossVal.rpart removed from heap")
                                  }
                                }, #function
                                #############
                                # print class
                                #############
                                #' @description
                                #' Prints instance variables of a crossVal.rpart object.
                                #' @return
                                #' string
                                print = function(){
                                  rString <- sprintf("\ncrossVal.rpart\n")
                                  rString <- sprintf("%s\nk: %i\n", rString, self$k)
                                  rString <- sprintf("%s\nseed: %i\n", rString, self$seed)
                                  rString <- sprintf("%s\naccuracy: %.3f\n", rString, self$accuracy[1])
                                  rString <- sprintf("%s\nauc_roc: %.3f\n", rString, self$auc_roc)
                                  rString <- sprintf("%s\n\n", rString)
                                  cat(rString)
                                  invisible(self)
                                }, #function
                                #' @description
                                #' Plots the confusion matrix of the cross validation
                                plot = function(){
                                  table <- self$confusion_matrix %>%
                                    tibble::as_tibble()

                                  plotTable <- table %>%
                                    mutate(correctness = ifelse(table$Prediction == table$Reference, "true", "false")) %>%
                                    group_by(Reference) %>%
                                    mutate(propability = n/sum(n))

                                  ggplot(data = plotTable, mapping = aes(x = Reference, y = Prediction, fill = correctness, alpha = propability)) +
                                    geom_tile() +
                                    geom_text(aes(label = n), vjust = .5, fontface  = "bold", alpha = 1) +
                                    scale_fill_manual(values = c(true = "green", false = "red"))+
                                    theme_bw()  +
                                    xlim(rev(self$lvls))
                                } #function
                              )
)
