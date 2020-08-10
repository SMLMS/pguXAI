library(R6)
library(tidyverse)
library(rpart)
library(partykit)

rpart.predictor = R6::R6Class("rpart.predictor",
                              ####################
                              # instance variables
                              ####################
                              private = list(
                                .df_leaf_rules = "tbl_df",
                                .df_class_rules = "tbl_df",
                                .df_splits = "tbl_df",
                                .features = "character",
                                .classes = "character"
                              ),  #private
                              active = list(
                                df_leaf_rules = function() {
                                  return(private$.df_leaf_rules)
                                },
                                df_class_rules = function() {
                                  return(private$.df_class_rules)
                                },
                                df_splits = function() {
                                  return(private$.df_splits)
                                },
                                features = function() {
                                  return(private$.features)
                                },
                                classes = function() {
                                  return(private$.classes)
                                }
                              ), #active
                              public =list(
                                ###################
                                # memory management
                                ###################
                                initialize = function(rpart_model, feature_names = "character", class_names  = "character"){
                                  private$.features <- feature_names
                                  private$.classes <- class_names
                                  self$train(rpart_model)
                                },
                                ################
                                # print function
                                ################
                                print = function(){
                                  rString <- sprintf("\ncompound rules by lime:\n")
                                  for (level in self$classes){
                                    rString <- sprintf("%s\nRules for class %s:\n%s\n", rString, level, self$df_class_rules %>%
                                                         dplyr::filter(class == level) %>%
                                                         dplyr::select(rules) %>%
                                                         unlist() %>%
                                                         as.character())
                                    rString <- sprintf("%s\n\n", rString)
                                  }
                                  cat(rString)
                                  invisible(self)
                                },
                                #################################
                                # user functions (train, predict)
                                #################################
                                train = function(rpart_model) {
                                  self$define_class_rules(rpart_model)
                                  self$concenate_rules()
                                  self$concenate_splits()
                                },
                                predict = function(df_data = "tbl_df"){
                                  df_data %>%
                                    self$predict_classes_bool(self$df_class_rules, self$classes) %>%
                                    self$predict_classes_prob(self$classes) %>%
                                    self$predict_classes_string(self$classes) %>%
                                    return()
                                },
                                #####################################
                                # help functions (not to run by user)
                                #####################################
                                define_class_rules = function(rpart_model){
                                  party_model <- partykit::as.party.rpart(rpart_model, data = TRUE)

                                  number_on_nodes = length(party_model)
                                  node_classes <- rep("", number_on_nodes)

                                  for (i in seq(number_on_nodes)){
                                    df_node <- partykit::data_party(party_model, id=i)
                                    node_classes[i] <-  df_node %>%
                                      dplyr::count(Clusters,sort = TRUE) %>%
                                      dplyr::filter(n == max(n)) %>%
                                      dplyr::slice(1) %>%
                                      dplyr::select(Clusters) %>%
                                      unlist() %>%
                                      as.character()
                                  }
                                  leaf_nodes <- partykit::nodeids(party_model, terminal = TRUE)
                                  leaf_classes <- node_classes[leaf_nodes]

                                  df_leaf_classes <- tibble::tibble(class = leaf_classes,
                                                                    leaf_nodes = leaf_nodes)

                                  private$.df_leaf_rules <- partykit:::.list.rules.party(party_model) %>%
                                    tibble::enframe(name = "leaf_nodes", value = "rules") %>%
                                    dplyr::mutate(leaf_nodes = as.integer(leaf_nodes)) %>%
                                    dplyr::inner_join(df_leaf_classes, by = "leaf_nodes")
                                },
                                concenate_rules = function(){
                                  number_of_classes <- length(self$classes)
                                  class_rules <- c("", number_of_classes)

                                  for (i in seq(number_of_classes)){
                                    class_rules[i] <-self$df_leaf_rules %>%
                                      dplyr::filter(class == self$classes[i]) %>%
                                      dplyr::select(rules) %>%
                                      unlist() %>%
                                      as.character() %>%
                                      paste(collapse = " | ")
                                  }
                                  private$.df_class_rules <- tibble::tibble(class = self$classes,
                                                                            rules = class_rules)
                                },
                                concenate_splits = function(){
                                  feature_vector <- c()
                                  split_vector <- c()

                                  for ( i in seq(nrow(self$df_leaf_rules))){
                                    leaf_rule_vector <- self$df_leaf_rules[["rules"]][i] %>%
                                      stringr::str_split("&", simplify = TRUE) %>%
                                      as.character() %>%
                                      stringr::str_remove_all(" ")
                                    # print(i)
                                    # print(leaf_rule_vector)
                                    for (j in seq(length(leaf_rule_vector))){
                                      rule <- "unknown"
                                      if (stringr::str_count(leaf_rule_vector[j], "<") >0) rule <- "l"
                                      if (stringr::str_count(leaf_rule_vector[j], ">") >0) rule <- "g"
                                      if (stringr::str_count(leaf_rule_vector[j], "=") >0) rule <- "e"
                                      if (stringr::str_count(leaf_rule_vector[j], "<=") >0) rule <- "le"
                                      if (stringr::str_count(leaf_rule_vector[j], ">=") >0) rule <- "ge"

                                      switch (rule,
                                              l = {
                                                single_rule_vector <- leaf_rule_vector[j] %>%
                                                  stringr::str_split("<", simplify = FALSE) %>%
                                                  unlist()
                                              },
                                              g = {
                                                single_rule_vector <- leaf_rule_vector[j] %>%
                                                  stringr::str_split(">", simplify = FALSE) %>%
                                                  unlist()
                                              },
                                              le = {
                                                single_rule_vector <- leaf_rule_vector[j] %>%
                                                  stringr::str_split("<=", simplify = FALSE) %>%
                                                  unlist()
                                              },
                                              ge = {
                                                single_rule_vector <- leaf_rule_vector[j] %>%
                                                  stringr::str_split(">=", simplify = FALSE) %>%
                                                  unlist()
                                              },
                                              {
                                                break()
                                              }
                                      )
                                      feature_vector <- feature_vector %>%
                                        append(single_rule_vector[1])

                                      split_vector <- split_vector %>%
                                        append(as.numeric(single_rule_vector[2]))
                                    }
                                  }
                                  private$.df_splits <- tibble::tibble(feature = feature_vector,
                                                                       splits = split_vector)
                                },
                                predict_classes_bool = function(df_data, df_rules, class_names){
                                  rule_names <- class_names
                                  for (i in seq(length(class_names))){
                                    rule_names[i] <- sprintf("%s_rules", class_names[i])
                                  }
                                  for (i in seq(length(class_names))){
                                    # predict_name <- sprintf("%s_rules", class_names[i])
                                    df_data <- df_data %>%
                                      dplyr::mutate(!!rule_names[i] := eval(parse(text = df_rules[i,"rules"])))
                                  }
                                  df_data_corrected <- apply(df_data[rule_names], 1, function(x) if(!any(x)){replace(x, !isTRUE(x) , TRUE)} else{x}) %>%
                                    t() %>%
                                    as.data.frame() %>%
                                    dplyr::mutate(idx = seq(1,nrow(df_data),1))

                                  for (i in seq(length(class_names))){
                                    df_data[rule_names[i]] <- df_data_corrected[rule_names[i]]
                                  }
                                  return(df_data)
                                },
                                predict_classes_prob = function(df_data, class_names){
                                  df_data <- df_data %>%
                                    dplyr::mutate(sum_prob = rep(0, nrow(df_data)))
                                  for (i in seq(length(class_names))){
                                    class_prob <- rep(0, nrow(df_data))
                                    #predict_name <- sprintf("%s_prob", class_names[i])
                                    predict_name <- class_names[i]
                                    class_idx <- df_data %>%
                                      dplyr::select(c(sprintf("%s_rules", class_names[i]))) %>%
                                      unlist()
                                    class_prob[class_idx] = 1
                                    df_data <- df_data %>%
                                      dplyr::mutate(!!predict_name := class_prob) %>%
                                      dplyr::mutate(sum_prob = sum_prob + !!as.symbol(predict_name))
                                  }
                                  for (i in seq(length(class_names))){
                                    #predict_name <- sprintf("%s_prob", class_names[i])
                                    predict_name <- class_names[i]
                                    df_data <- df_data %>%
                                      dplyr::mutate(!!predict_name := !!as.symbol(predict_name)/sum_prob)
                                  }
                                  df_data <- df_data %>%
                                    dplyr::select(-c("sum_prob"))
                                  return(df_data)
                                },
                                predict_classes_string = function(df_data, class_names){
                                  classify_0R = function(x, occurrence){
                                    probs <- x %>%
                                      unlist() %>%
                                      as.numeric()
                                    max_prob <- max(probs)
                                    idx_list <- seq(1, length(x),1)
                                    idx <- idx_list[probs == max_prob]
                                    max_occ_idx <- idx[which.max(occurrence[idx])]
                                    # max_occ_idx <-sample(idx_list[occurrence == max_occurrence],1)
                                    return(self$classes[max_occ_idx])
                                  }
                                  global_occurrence <- rep(0, length(class_names))
                                  for (i in seq(length(class_names))){
                                    global_occurrence[i] <- df_data %>%
                                      dplyr::select(c(sprintf("%s_rules", class_names[i]))) %>%
                                      dplyr::filter_all(all_vars(. == TRUE)) %>%
                                      nrow()
                                  }
                                  df_data <- df_data %>%
                                    dplyr::mutate(predict = apply(df_data[class_names], 1, function(x) classify_0R(x, global_occurrence)))
                                  # dplyr::mutate(predict = apply(df_data[class_names], 1, function(x) define_max(x)))
                                  return(df_data)
                                }
                              ) #public
)
