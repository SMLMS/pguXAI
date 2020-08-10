library(R6)
library(tidyverse)
library(lime)

lime.predictor = R6::R6Class("lime.predictor",
                             ####################
                             # instance variables
                             ####################
                             private = list(
                               .df_explanation = "tbl_df",
                               .df_rules = "tbl_df",
                               .features = "character",
                               .classes = "character",
                               .filter_request = "logical",
                               .thr = "numeric"
                             ), # private
                             ##################
                             # accessor methods
                             ##################
                             active = list(
                               df_explanation = function(){
                                 return(private$.df_explanation)
                               },
                               df_rules = function(){
                                 return(private$.df_rules)
                               },
                               features = function(){
                                 return(private$.features)
                               },
                               classes = function(){
                                 return(private$.classes)
                               },
                               filter_request = function(){
                                 return(private$.filter_request)
                               },
                               thr = function(){
                                 return(private$.thr)
                               },
                               setThr = function(val = "numeric"){
                                 private$.thr <- val
                               }
                             ),
                             public =list(
                               ###################
                               # memory management
                               ###################
                              initialize = function(explanation = "tbl_df", feature_names = "character", class_names  = "character",  filter_request = FALSE, thr = 0.0){
                                private$.features <- feature_names
                                private$.classes <- class_names
                                self$train(explanation, filter_request, thr)
                              },
                              ################
                              # print function
                              ################
                              print = function(){
                                rString <- sprintf("\ncompound rules by lime:\n")
                                for (level in self$classes){
                                  rString <- sprintf("%s\nRules for class %s:\n%s\n", rString, level, self$df_rules %>%
                                                       dplyr::filter(class == level) %>%
                                                       dplyr::select(rules) %>%
                                                       unlist() %>%
                                                       as.character())
                                  rString <- sprintf("%s\n\n", rString)
                                }
                                cat(rString)
                                invisible(self)
                              },
                              ################
                              # user functions
                              ################
                              train = function(explanation = "tbl_df", filter_request = FALSE, thr = 0.0){
                                private$.filter_request <- filter_request
                                private$.thr <- thr
                                private$.df_explanation <- explanation
                                rules <- c()
                                for (level in self$classes){
                                  if(self$filter_request == TRUE){
                                    # weight of rules for filtering
                                    weight <- self$df_explanation %>%
                                      dplyr::filter(label == level) %>%
                                      dplyr::mutate(comp_weight = label_prob * feature_weight) %>%
                                      dplyr::select(c("comp_weight")) %>%
                                      unlist() %>%
                                      as.numeric()

                                    class_rules <- explanation %>%
                                      dplyr::select(c("label", "case", "feature_desc", "feature_weight")) %>%
                                      dplyr::filter(label == level) %>%
                                      # if feature_weight filtering step ist left out, the classification quality will suffer!!
                                      dplyr::filter(feature_weight > mean(weight) + self$thr * sd(weight)) %>%
                                      self$form_class_rule(self$features)
                                  } else{
                                    class_rules <- explanation %>%
                                      dplyr::select(c("label", "case", "feature_desc", "feature_weight")) %>%
                                      dplyr::filter(label == level) %>%
                                      self$form_class_rule(self$features)
                                  }
                                  if(stringr::str_length(class_rules) < 1){
                                    class_rules <- "FALSE"
                                  }
                                  rules <- append(rules, class_rules)
                                }

                                private$.df_rules <- tibble::tibble(class = self$classes,
                                                           rules = rules)
                              },
                              predict = function(df_data = "tbl_df"){
                                df_data %>%
                                  #dplyr::mutate(Species = label_positive) %>%
                                  self$predict_classes_bool(self$df_rules, self$classes) %>%
                                  self$predict_classes_prob(self$classes) %>%
                                  self$predict_classes_string(self$classes) %>%
                                  return()
                              },
                              ##################
                              # helper functions
                              ##################
                              form_class_rule  = function(class_rule_set, feature_names) {
                                rule <- ""
                                cases <- class_rule_set %>%
                                  dplyr::select(case) %>%
                                  unique() %>%
                                  unlist() %>%
                                  as.integer()
                                for (true_case in cases){
                                  sub_rule <- class_rule_set %>%
                                    dplyr::filter(case == true_case) %>%
                                    self$form_case_rule(feature_names)
                                  if(stringr::str_length(sub_rule) > 0){
                                    rule <- sprintf("%s %s |", rule, sub_rule)
                                  }
                                }
                                # clip
                                rule_length <- stringr::str_length(rule)
                                rule <- substr(rule,2, rule_length-2)
                                return(rule)
                              },
                              form_case_rule = function(case_rule_set, feature_names){
                                rule <- ""
                                for (name in feature_names){
                                  for (sub_rules in case_rule_set[["feature_desc"]]){
                                    sub_rule <- self$form_feature_subrule(sub_rules, name)
                                    if(stringr::str_length(sub_rule) > 0){
                                      rule <- sprintf("%s %s", rule, sub_rule)
                                    }
                                  }
                                }
                                # clip
                                rule_length <- stringr::str_length(rule)
                                rule <- substr(rule,2,rule_length-2)
                                return(rule)
                              },
                              form_feature_subrule = function(rule, feature){
                                feature_idx <- stringr::str_locate(rule, feature)
                                if(is.na(feature_idx[1])){
                                  return("")
                                } else if (feature_idx[1] == 1){
                                  sprintf("%s &", rule) %>%
                                    return()
                                } else if(feature_idx[2] == stringr::str_length(rule)){
                                  sprintf("%s &", rule) %>%
                                    return()
                                } else{
                                  sub_rules <- unlist(stringr::str_split(rule, feature))
                                  sprintf("%s%s & %s%s &", sub_rules[1], feature, feature, sub_rules[2]) %>%
                                    return()
                                }
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
                                # define_max= function(x){
                                #  probs <- x %>%
                                #     unlist() %>%
                                #     as.numeric()
                                #  max_prob <- max(probs)
                                #  idx_list <- seq(1, length(x),1)
                                #  idx <-sample(idx_list[probs == max_prob],1)
                                #  return(self$classes[idx])
                                # }
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
                             )
)
