####################################### Verschiedene Classifier anwenden: Feature selection und Classification  #######################################

library(caTools)
library(caret)
library(partykit)
library(rpart)
library(C50)
library(RWeka)
library(earth)
library(randomForest)
library(pROC)
library(parallel)
library(doParallel)
library(tidyverse)
library(lime)

#########################
# Modify Iris dataframe #
#########################
modified_iris = function(){
  data(iris)
  df_data <- iris[,1:4] %>%
    as.data.frame()

  colnames_permutated <- colnames(df_data)
  for (i in seq(length(colnames_permutated))){
    colnames_permutated[i] <- sprintf("%s_permuted", colnames_permutated[i])
  }

  df_data_permutated <- apply(df_data, 2, function(x) jitter(sample(x))) %>%
    as.data.frame()

  colnames(df_data_permutated) <- colnames_permutated

  df_data <- df_data %>%
    dplyr::mutate(idx = seq(1,nrow(df_data),1))

  df_data_permutated <- df_data_permutated %>%
    dplyr::mutate(idx = seq(1,nrow(df_data),1))

  df_data <- df_data %>%
    dplyr::full_join(df_data_permutated, by="idx") %>%
    dplyr::select(-c("idx"))

  return(df_data)
}

############################
# Setup Parallel computing #
############################
# nCores <- parallel::detectCores()
#
# cl <- parallel::makeCluster(nCores/2, setup_strategy = "sequential", setup_timeout = 0.5) #Kerne
# doParallel::registerDoParallel(cl) #besser cl?
# foreach(i=1:3) %dopar% sqrt(i)

##############################
# Define cluster algorithms  #
##############################
# erst ohne RFlime
ClassifierChoice <- c("RF", "CTREE",	"Rpart",	"C4.5",	"C5.0", "PART",	"RIPPER",	"C5.0rules")
# ClassifierChoice <- c("RF", "RFlime")#, "CTREE",	"Rpart",	"C4.5",	"C5.0", "PART",	"RIPPER",	"C5.0rules")

nIter=1 # 1000 @ end

#################################
# Create dataframe for analysis #
#################################
# iris Data_frame i. spalte numerische Klassen
ActualDataForClassification <- modified_iris()
#classes_iris <- iris[,5]
#level_iris <- levels(classes_iris)

ActualDataForClassification$Clusters <- as.numeric(iris[,5])

ActualDataForClassification

nCluster <- length(unique(ActualDataForClassification$Clusters))
if (nCluster > 2) {PerformanceActualClassifiersAll_ABCreduced <- data.frame(matrix(ncol = length(ClassifierChoice), nrow = nIter*nCluster))
} else {PerformanceActualClassifiersAll_ABCreduced <- data.frame(matrix(ncol = length(ClassifierChoice), nrow = nIter))}
if (nCluster > 2) {PerformanceActualClassifiersAll <- data.frame(matrix(ncol = length(ClassifierChoice), nrow = nIter*nCluster))
} else {PerformanceActualClassifiersAll <- data.frame(matrix(ncol = length(ClassifierChoice), nrow = nIter))}
names(PerformanceActualClassifiersAll_ABCreduced) <- ClassifierChoice
############# Feature selection

for (ii2 in 1:length(ClassifierChoice)) {
  VariableImportanceAktualClassifier <- data.frame(matrix(ncol = (length(ActualDataForClassification)-1), nrow = 0))
  names(VariableImportanceAktualClassifier) <- names(ActualDataForClassification[2:ncol(ActualDataForClassification)])
  if (ii2 == 1) {
    for (i in 1:nIter)
    {
      set.seed(42+i)
      sample <- sample.split(ActualDataForClassification$Clusters, SplitRatio = .67)
      TrainData <- subset(ActualDataForClassification, sample == TRUE)
      TestData <- subset(ActualDataForClassification, sample == FALSE)
      MatrixTrain  <- subset(TrainData, select = names(TrainData)[2:ncol(TrainData)])
      MatrixTest  <- subset(TestData, select = names(TestData)[2:ncol(TestData)])
      UrsachenTrain  <- TrainData[,1]
      UrsachenTest  <- TestData[,1]


      switch(ClassifierChoice[ii2],
             CTREE = {ActualClassifierObject <- ctree(as.factor(Clusters) ~ ., data = TrainData,
                                                      control = ctree_control(minbucket = 1, cores = 3, mincriterion = 0, maxdepth = 30, minsplit = 5))},
             Rpart = {ActualClassifierObject <- rpart(as.factor(Clusters) ~ ., data = TrainData,
                                                      method = "class", xval = 1000, parms = list(split = "information"), control = rpart.control(cp = 0, maxdepth = 30, minsplit = 5))},
             C4.5 = {ActualClassifierObject <- RWeka::J48(as.factor(Clusters) ~ ., data = TrainData)},
             C5.0 = {ActualClassifierObject <- C5.0(as.factor(Clusters) ~ ., data = TrainData)},
             MARS = {ActualClassifierObject <- earth(as.factor(Clusters) ~ ., data = TrainData)},
             PART = {ActualClassifierObject <- RWeka::PART(as.factor(Clusters) ~ ., data = TrainData)},
             RIPPER = {ActualClassifierObject <- RWeka::JRip(as.factor(Clusters) ~ ., data = TrainData)},
             C5.0rules = {ActualClassifierObject <- C5.0(as.factor(Clusters) ~ ., data = TrainData, rules =T)},
             # RF = {ActualClassifierObject <- randomForest(as.factor(Clusters) ~ ., data = TrainData,
             #                                              ntree = 1500,  mtry = 1*sqrt(length(names(TrainData))-1), na.action = na.roughfix,
             #                                              strata=TrainData$Clusters, replace = T)}
             RF = {ActualClassifierObject <- caret::train(x = MatrixTrain, y = as.factor(UrsachenTrain),
                                                          method = 'rf', ntree = 1500,
                                                          tuneGrid = data.frame(mtry = 1*sqrt(length(names(TrainData))-1)))},
             RFlime = {
               # define data and label sets for training and test
               level_temp <- c("class_1", "class_2", "class_3", "class_4", "class_5")
               df_train <- MatrixTrain
               df_test <- MatrixTest
               row.names(df_train) <- c()
               row.names(df_test) <- c()
               label_train <- as.factor(level_temp[UrsachenTrain])
               label_test <- as.factor(level_temp[UrsachenTest])
               feature_names <- colnames(df_train)
               classes <- levels(label_train)

               # train random forest by caret as randomForest library is not implemented in lime.
               model_caret <- caret::train(x = df_train,
                                           y = label_train,
                                           method = 'rf',
                                           ntree = 1500,
                                           tuneGrid = data.frame(mtry = 1*sqrt(length(names(df_train))-1)))

               label_predict <- predict(model_caret, newdata = df_test)

               # define positive classification events
               label_positive <- label_test[label_test == label_predict]

               df_positive = df_test[label_test == label_predict,]

               # Create an explainer object
               explainer <- lime::lime(df_train, model_caret)

               # Explain new observation
               explanation <- lime::explain(df_test, explainer, n_labels = 1, n_features = length(feature_names))

               # create instance of lime predictor
               source(file = "lime_predictor.R", local = TRUE)
               lp = lime.predictor$new(explanation, feature_names = feature_names, class_names = classes, filter_request = FALSE)
               # print un-filtered rules

               df_limePredict <-lp$predict(df_test)

               cm <- caret::confusionMatrix(factor(label_test, levels = classes), factor(df_limePredict[["predict"]], levels = classes))
               cm$byClass %>%
                 as.data.frame() %>%
                 dplyr::select(c("Balanced Accuracy")) %>%
                 tidyr::drop_na() %>%
                 unlist() %>%
                 as.numeric() %>%
                 na.omit() %>%
                 mean()

               #screen optimal filter parameter
               thr <- seq(-2, 1.5, 0.1)
               length(thr)
               balanced_accuracy <- rep(0.0, length(thr))

               for (i in seq(length(thr))){
                 lp$train(explanation, filter_request = TRUE, thr = thr[i])
                 df_limePredict <-lp$predict(df_test)
                 cm <- caret::confusionMatrix(factor(label_test, levels = classes), factor(df_limePredict[["predict"]], levels = classes))
                 balanced_accuracy[i] <- cm$byClass %>%
                   as.data.frame() %>%
                   dplyr::select(c("Balanced Accuracy")) %>%
                   tidyr::drop_na() %>%
                   unlist() %>%
                   as.numeric() %>%
                   na.omit() %>%
                   mean()
               }
             }
      )


      if (ClassifierChoice[ii2] != "RFlime") {
        Pred0 <- predict(ActualClassifierObject, TestData)
      } else {
        Pred0 <- df_limePredict$predict
        Pred0 <- readr::parse_number(df_limePredict$predict)

      }
      Pred <- Pred0
      if (length(table(Pred0)) > nCluster | isTRUE(ncol(Pred0) > 1)) {
        if (ncol(Pred0) == nCluster) Pred <- as.vector(apply(Pred0, 1, which.max))
        else Pred <- round(scales::rescale(Pred0, to = c(1,2)))
      }
      cTab <- table(factor(Pred, levels = 1:nCluster), factor(TestData$Clusters, levels = 1:nCluster))
      ifelse(nCluster > 2, cMat <- t(caret::confusionMatrix(cTab)$byClass), cMat <- caret::confusionMatrix(cTab)$byClass)
      #AccuracyAll <- cMat$overall[1]
      ifelse(nCluster > 2, AccuracyAll <- mean(na.omit(cMat[11,])), AccuracyAll <- mean(na.omit(cMat[11])))

      for (i1 in 2:ncol(TrainData))
      {
        TrainData1 <- TrainData[,-i1]
        TestData1 <- TestData[,-i1]
        MatrixTrain1  <- subset(TrainData1, select = names(TrainData1)[2:ncol(TrainData1)])
        MatrixTest1  <- subset(TestData1, select = names(TestData1)[2:ncol(TestData1)])
        UrsachenTrain1  <- TrainData1[,1]
        UrsachenTest1  <- TestData1[,1]


        switch(ClassifierChoice[ii2],
               CTREE = {ActualClassifierObject <- ctree(as.factor(Clusters) ~ ., data = TrainData1,
                                                        control = ctree_control(minbucket = 1, cores = 3, mincriterion = 0, maxdepth = 30, minsplit = 5))},
               Rpart = {ActualClassifierObject <- rpart(as.factor(Clusters) ~ ., data = TrainData1,
                                                        method = "class", xval = 1000, parms = list(split = "information"), control = rpart.control(cp = 0, maxdepth = 30, minsplit = 5))},
               C4.5 = {ActualClassifierObject <- RWeka::J48(as.factor(Clusters) ~ ., data = TrainData1)},
               C5.0 = {ActualClassifierObject <- C5.0(as.factor(Clusters) ~ ., data = TrainData1)},
               MARS = {ActualClassifierObject <- earth(as.factor(Clusters) ~ ., data = TrainData1)},
               PART = {ActualClassifierObject <- RWeka::PART(as.factor(Clusters) ~ ., data = TrainData1)},
               RIPPER = {ActualClassifierObject <- RWeka::JRip(as.factor(Clusters) ~ ., data = TrainData1)},
               C5.0rules = {ActualClassifierObject <- C5.0(as.factor(Clusters) ~ ., data = TrainData1, rules =T)},
               # RF = {ActualClassifierObject <- randomForest(as.factor(Clusters) ~ ., data = TrainData1,
               #                                              ntree = 1500,  mtry = 1*sqrt(length(names(TrainData1))-1), na.action = na.roughfix,
               #                                              strata=TrainData1$Clusters, replace = T)}
               RF = {ActualClassifierObject <- caret::train(x = MatrixTrain1, y = as.factor(UrsachenTrain1),
                                                            method = 'rf', ntree = 1500,
                                                            tuneGrid = data.frame(mtry = 1*sqrt(length(names(TrainData1))-1)))},
               RFlime = {
                 # define data and label sets for training and test
                 level_temp <- c("class_1", "class_2", "class_3", "class_4", "class_5")
                 df_train <- MatrixTrain1
                 df_test <- MatrixTest1
                 row.names(df_train) <- c()
                 row.names(df_test) <- c()
                 label_train <- as.factor(level_temp[UrsachenTrain1])
                 label_test <- as.factor(level_temp[UrsachenTest1])
                 feature_names <- colnames(df_train)
                 classes <- levels(label_train)

                 # train random forest by caret as randomForest library is not implemented in lime.
                 model_caret <- caret::train(x = df_train,
                                             y = label_train,
                                             method = 'rf',
                                             ntree = 1500,
                                             tuneGrid = data.frame(mtry = 1*sqrt(length(names(df_train))-1)))

                 label_predict <- predict(model_caret, newdata = df_test)

                 # define positive classification events
                 label_positive <- label_test[label_test == label_predict]

                 df_positive = df_test[label_test == label_predict,]

                 # Create an explainer object
                 explainer <- lime::lime(df_train, model_caret)

                 # Explain new observation
                 explanation <- lime::explain(df_test, explainer, n_labels = 1, n_features = length(feature_names))

                 # create instance of lime predictor
                 source(file = "lime_predictor.R", local = TRUE)
                 lp = lime.predictor$new(explanation, feature_names = feature_names, class_names = classes, filter_request = FALSE)
                 # print un-filtered rules

                 df_limePredict <-lp$predict(df_test)

                 cm <- caret::confusionMatrix(factor(label_test, levels = classes), factor(df_limePredict[["predict"]], levels = classes))
                 cm$byClass %>%
                   as.data.frame() %>%
                   dplyr::select(c("Balanced Accuracy")) %>%
                   tidyr::drop_na() %>%
                   unlist() %>%
                   as.numeric() %>%
                   na.omit() %>%
                   mean()

                 #screen optimal filter parameter
                 thr <- seq(-2, 1.5, 0.1)
                 length(thr)
                 balanced_accuracy <- rep(0.0, length(thr))

                 for (i in seq(length(thr))){
                   lp$train(explanation, filter_request = TRUE, thr = thr[i])
                   df_limePredict <-lp$predict(df_test)
                   cm <- caret::confusionMatrix(factor(label_test, levels = classes), factor(df_limePredict[["predict"]], levels = classes))
                   balanced_accuracy[i] <- cm$byClass %>%
                     as.data.frame() %>%
                     dplyr::select(c("Balanced Accuracy")) %>%
                     tidyr::drop_na() %>%
                     unlist() %>%
                     as.numeric() %>%
                     na.omit() %>%
                     mean()
                 }
               }
        )


        if (ClassifierChoice[ii2] != "RFlime") {
          Pred0 <- predict(ActualClassifierObject, TestData1)
        } else {
          Pred0 <- df_limePredict$predict
          Pred0 <- readr::parse_number(df_limePredict$predict)
        }
        Pred <- Pred0
        if (length(table(Pred0)) > nCluster | isTRUE(ncol(Pred0) > 1)) {
          if (ncol(Pred0) == nCluster) Pred <- as.vector(apply(Pred0, 1, which.max))
          else Pred <- round(scales::rescale(Pred0, to = c(1,2)))
        }
        cTab <- table(factor(Pred, levels = 1:nCluster), factor(TestData1$Clusters, levels = 1:nCluster))
        ifelse(nCluster > 2, cMat <- t(caret::confusionMatrix(cTab)$byClass), cMat <- caret::confusionMatrix(cTab)$byClass)
        #AccuracyAktual <- cMat$overall[1]
        ifelse(nCluster > 2, AccuracyAktual <- mean(na.omit(cMat[11,])), AccuracyAktual <- mean(na.omit(cMat[11])))
        VariableImportanceAktualClassifier[i,(i1-1)] <- AccuracyAll - AccuracyAktual
      }
    }

    ############# ABC analysis of the feature importance measure
    VariableImportanceAktualClassifierRanked <- (apply(VariableImportanceAktualClassifier, 1, rank))
    FeatureMeanRanks <- rowSums(VariableImportanceAktualClassifierRanked, na.rm = T)

    # VariableImportanceAktualClassifierRanked <- VariableImportanceAktualClassifier#apply(VariableImportanceAktualClassifier, 1, rank)
    # FeatureMeanRanks <- colMedians(as.matrix(VariableImportanceAktualClassifierRanked), na.rm = T)
    names(FeatureMeanRanks) <- names(VariableImportanceAktualClassifier)
    ABC_Features <- ABCanalysis(as.vector(FeatureMeanRanks), PlotIt = F)
    ABC_A <- c(ABC_Features$Aind,ABC_Features$Bind)
    ABC_A_names <- names(FeatureMeanRanks)[ABC_A]
    #ABC - Ergebnisse
    ABCResamplinA <- ABC_A_names
    ABCResamplinAcount <- length(ABC_A_names)
    FeatureMeanRanksDF <- data.frame(FeatureMeanRanks)
    FeatureMeanRanksDF$Features <- rownames(FeatureMeanRanksDF)
    FeatureMeanRanksDFo <- FeatureMeanRanksDF[order(-FeatureMeanRanksDF$FeatureMeanRanks),]
    FeatureMeanRanksDFo$Features <- factor(FeatureMeanRanksDFo$Features, levels = FeatureMeanRanksDFo$Features)

    library(ggplot2)
    library(gridExtra)
    assign(paste0("plotVarImp_",ClassifierChoice[ii2]),
           ggplot(data = FeatureMeanRanksDFo, aes (x = Features, y = FeatureMeanRanks)) +
             geom_bar(stat = "identity", fill = c(rep("steelblue", ABCResamplinAcount),rep("grey55", 6-ABCResamplinAcount) )) +
             labs(x = "Pain related parameters", y = "Decrease in classification accuray when omitted from forest [sum of ranks]") +
             theme(legend.position = "none") +   theme_linedraw()  +
             ggtitle(paste0("Results of ABC anaylsis of variable importance: ", ClassifierChoice[ii2])) +
             theme(axis.text.x = element_text(angle = 45, hjust = 1)) )
    assign(paste0("plotVarImpABC_",ClassifierChoice[ii2]),
           ABCplotGG(as.vector(FeatureMeanRanks)) +   theme_linedraw()  +
             theme(legend.position = c(.85,.4)) +  ggtitle(paste0("ABC anaylsis of variable importance: ", ClassifierChoice[ii2])) )
  }
  ############# Classifier performance testing

  for (ii3 in 1:2) {
    ifelse(ii3 == 2, ActualDataReduced <- subset(ActualDataForClassification, select = c("Clusters", ABCResamplinA)),
           ActualDataReduced <- ActualDataForClassification)
    library(caTools)
    library(matrixStats)
    library(pROC)
    for (ii1 in 1:2)
    {
      PerformanceActualClassifier <- matrix(NA, nrow = 11, ncol = 0)
      rocAUC_ActualClassifier <- vector()
      for (i in 1:nIter)
      {
        set.seed(42+i)
        sample <- sample.split(ActualDataReduced$Clusters, SplitRatio = .67)
        TrainData <- subset(ActualDataReduced, sample == TRUE)
        TestData <- subset(ActualDataReduced, sample == FALSE)
        if (ii1 == 2) {
          if (ii3 == 2) {
            TrainData[names(TrainData) %in% ABCResamplinA] <- apply(TrainData[names(TrainData) %in% ABCResamplinA], 2, sample)
            TrainData[names(TrainData) %in% ABCResamplinA] <- sapply(TrainData[names(TrainData) %in% ABCResamplinA],as.numeric)
            TestData[names(TestData) %in% ABCResamplinA] <- sapply(TestData[names(TestData) %in% ABCResamplinA],as.numeric)
          } else {
            TrainData[2:ncol(TrainData)] <- apply(TrainData[2:ncol(TrainData)], 2, sample)
            TrainData[2:ncol(TrainData)] <- sapply(TrainData[2:ncol(TrainData)],as.numeric)
            TestData[2:ncol(TrainData)] <- sapply(TestData[2:ncol(TrainData)],as.numeric)
          }
        }
        MatrixTrain  <- subset(TrainData, select = names(TrainData)[2:ncol(TrainData)])
        MatrixTest  <- subset(TestData, select = names(TestData)[2:ncol(TestData)])
        UrsachenTrain  <- TrainData[,1]
        UrsachenTest  <- TestData[,1]

        switch(ClassifierChoice[ii2],
               CTREE = {ActualClassifierObject <- ctree(as.factor(Clusters) ~ ., data = TrainData,
                                                        control = ctree_control(minbucket = 1, cores = 3, mincriterion = 0, maxdepth = 30, minsplit = 5))},
               Rpart = {ActualClassifierObject <- rpart(as.factor(Clusters) ~ ., data = TrainData,
                                                        method = "class", xval = 1000, parms = list(split = "information"), control = rpart.control(cp = 0, maxdepth = 30, minsplit = 5))},
               C4.5 = {ActualClassifierObject <- RWeka::J48(as.factor(Clusters) ~ ., data = TrainData)},
               C5.0 = {ActualClassifierObject <- C5.0(as.factor(Clusters) ~ ., data = TrainData)},
               MARS = {ActualClassifierObject <- earth(as.factor(Clusters) ~ ., data = TrainData, glm=list(family=binomial))},
               PART = {ActualClassifierObject <- RWeka::PART(as.factor(Clusters) ~ ., data = TrainData)},
               RIPPER = {ActualClassifierObject <- RWeka::JRip(as.factor(Clusters) ~ ., data = TrainData)},
               C5.0rules = {ActualClassifierObject <- C5.0(as.factor(Clusters) ~ ., data = TrainData, rules =T)},
               # RF = {ActualClassifierObject <- randomForest(as.factor(Clusters) ~ ., data = TrainData,
               #                                              ntree = 1500,  mtry = 1*sqrt(length(names(TrainData))-1), na.action = na.roughfix,
               #                                              strata=TrainData$Clusters, replace = T)}
               RF = {ActualClassifierObject <- caret::train(x = MatrixTrain, y = as.factor(UrsachenTrain),
                                                            method = 'rf', ntree = 1500,
                                                            tuneGrid = data.frame(mtry = 1*sqrt(length(names(TrainData))-1)))},
               RFlime = {
                 # define data and label sets for training and test
                 level_temp <- c("class_1", "class_2", "class_3", "class_4", "class_5")
                 df_train <- MatrixTrain
                 df_test <- MatrixTest
                 row.names(df_train) <- c()
                 row.names(df_test) <- c()
                 label_train <- as.factor(level_temp[UrsachenTrain])
                 label_test <- as.factor(level_temp[UrsachenTest])
                 feature_names <- colnames(df_train)
                 classes <- levels(label_train)

                 # train random forest by caret as randomForest library is not implemented in lime.
                 model_caret <- caret::train(x = df_train,
                                             y = label_train,
                                             method = 'rf',
                                             ntree = 1500,
                                             tuneGrid = data.frame(mtry = 1*sqrt(length(names(df_train))-1)))

                 label_predict <- predict(model_caret, newdata = df_test)

                 # define positive classification events
                 label_positive <- label_test[label_test == label_predict]

                 df_positive = df_test[label_test == label_predict,]

                 # Create an explainer object
                 explainer <- lime::lime(df_train, model_caret)

                 # Explain new observation
                 explanation <- lime::explain(df_test, explainer, n_labels = 1, n_features = length(feature_names))

                 # create instance of lime predictor
                 source(file = "lime_predictor.R", local = TRUE)
                 lp = lime.predictor$new(explanation, feature_names = feature_names, class_names = classes, filter_request = FALSE)
                 # print un-filtered rules

                 df_limePredict <-lp$predict(df_test)

                 cm <- caret::confusionMatrix(factor(label_test, levels = classes), factor(df_limePredict[["predict"]], levels = classes))
                 cm$byClass %>%
                   as.data.frame() %>%
                   dplyr::select(c("Balanced Accuracy")) %>%
                   tidyr::drop_na() %>%
                   unlist() %>%
                   as.numeric() %>%
                   na.omit() %>%
                   mean()

                 #screen optimal filter parameter
                 thr <- seq(-2, 1.5, 0.1)
                 length(thr)
                 balanced_accuracy <- rep(0.0, length(thr))

                 for (i in seq(length(thr))){
                   lp$train(explanation, filter_request = TRUE, thr = thr[i])
                   df_limePredict <-lp$predict(df_test)
                   cm <- caret::confusionMatrix(factor(label_test, levels = classes), factor(df_limePredict[["predict"]], levels = classes))
                   balanced_accuracy[i] <- cm$byClass %>%
                     as.data.frame() %>%
                     dplyr::select(c("Balanced Accuracy")) %>%
                     tidyr::drop_na() %>%
                     unlist() %>%
                     as.numeric() %>%
                     na.omit() %>%
                     mean()
                 }
               }
        )

        if (ClassifierChoice[ii2] != "RFlime") {
          Pred0 <- predict(ActualClassifierObject, TestData)
        } else {
          Pred0 <- df_limePredict$predict
          Pred0 <- readr::parse_number(df_limePredict$predict)
        }
        Pred <- Pred0
        if (length(table(Pred0)) > nCluster | isTRUE(ncol(Pred0) > 1)) {
          if (ncol(Pred0) == nCluster) Pred <- as.vector(apply(Pred0, 1, which.max))
          else Pred <- round(scales::rescale(Pred0, to = c(1,2)))
        }
        cTab <- table(factor(Pred, levels = 1:nCluster), factor(TestData$Clusters, levels = 1:nCluster))
        ifelse(nCluster > 2, cMat <- t(caret::confusionMatrix(cTab)$byClass), cMat <- caret::confusionMatrix(cTab)$byClass)
        PerformanceActualClassifier <- cbind(PerformanceActualClassifier,cMat)
        if(ClassifierChoice[ii2] != "RFlime") {
          Pred0 <- Pred2 <- try(predict(ActualClassifierObject, TestData, type = "prob"), TRUE)
          } else {
            Pred2 <- as.matrix(subset(df_limePredict, select = c("class_1", "class_2", "class_3", "class_4", "class_5")))
            colnames(Pred2) <- c(1:nCluster)
          }
        if (length(setdiff(names(table(Pred0)),as.character(unique(UrsachenTest)))) > 0) {
          if (ncol(Pred0) == nCluster) Pred2 <- Pred0
          else {
            Pred0resc <- scales::rescale(Pred0, to = c(0,1))
            Pred2 <- data.frame(cbind(Pred0resc, Pred0resc))
            names(Pred2) <- c(1,2)
            Pred2[,2] <- 1- Pred2[,1]
          }
        }
        rocAUC_ActualClassifier <-  append(rocAUC_ActualClassifier, multiclass.roc(TestData$Clusters,Pred2, quiet=T)$auc)
      }
      print(paste(ClassifierChoice[ii2], c("Original data", "Permuted data")[ii1]))
      print(paste("Data set", c("Full", "ABC reduced")[ii3]))
      print(rowQuantiles(PerformanceActualClassifier, probs=c(0.025,0.5,0.975), na.rm = T)*100)
      print(quantile(rocAUC_ActualClassifier, probs=c(0.025,0.5,0.975), na.rm = T)*100)
      if (ii1 == 1 & ii3 == 1) PerformanceActualClassifiersAll[,ii2] <- PerformanceActualClassifier[11,]
      if (ii1 == 1 & ii3 == 2) PerformanceActualClassifiersAll_ABCreduced[,ii2] <- PerformanceActualClassifier[11,]
    }
  }
}
stopCluster(cl)


library(reshape2)
library(ggthemes)
library(scales)

PerformanceActualClassifiersAll_ABCreduced_long <- melt(PerformanceActualClassifiersAll_ABCreduced)
PerformanceActualClassifiersAll_long <- melt(PerformanceActualClassifiersAll)
PerformanceActualClassifiersAll_ABCreduced_long$valueAll <- PerformanceActualClassifiersAll_long$value
PerformanceActualClassifiersAll_ABCreduced_long$DiffAllReduced <- PerformanceActualClassifiersAll_ABCreduced_long$valueAll - PerformanceActualClassifiersAll_ABCreduced_long$value
PerformanceActualClassifiersAll_ABCreduced_long$DiffAllReducedWeighted <- PerformanceActualClassifiersAll_ABCreduced_long$value * (1 - PerformanceActualClassifiersAll_ABCreduced_long$DiffAllReduced)
plotPerformanceActualClassifiersAll_ABCall <-
  ggplot(data = PerformanceActualClassifiersAll_ABCreduced_long, aes(x = reorder(variable, -valueAll, FUN = median), y = valueAll, fill = variable)) +
  geom_boxplot(outlier.shape = NA) + stat_summary(fun=mean, geom="point", shape=20, size=4, color="yellow", fill="yellow") +  theme_linedraw() + scale_fill_manual(values = colorblind_pal()(length(ClassifierChoice))) +
  theme(legend.position = "none") + ggtitle(paste0("Classifier balanced accuracy with all features")) +
  labs(x = "Classifier",  y = "Balanced accuracy") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + theme(plot.title = element_text(size=8))
plotPerformanceActualClassifiersAll_ABCreduced <-
  ggplot(data = PerformanceActualClassifiersAll_ABCreduced_long, aes(x = reorder(variable, -value, FUN = median), y = value, fill = variable)) +
  geom_boxplot(outlier.shape = NA) + stat_summary(fun=mean, geom="point", shape=20, size=4, color="yellow", fill="yellow")  +  theme_linedraw() + scale_fill_manual(values = colorblind_pal()(length(ClassifierChoice))) +
  theme(legend.position = "none") + ggtitle(paste0("Classifier balanced accuracy with selected features")) +
  labs(x = "Classifier",  y = "Balanced accuracy") + theme(axis.text.x = element_text(angle = 45, hjust = 1))  + theme(plot.title = element_text(size=8))
plotPerformanceActualClassifiersAll_ABCreducedDifftoAll <-
  ggplot(data = PerformanceActualClassifiersAll_ABCreduced_long, aes(x = reorder(variable, DiffAllReduced, FUN = median), y = DiffAllReduced, fill = variable)) +
  geom_boxplot(outlier.shape = NA) + stat_summary(fun=mean, geom="point", shape=20, size=4, color="yellow", fill="yellow") +  theme_linedraw() + scale_fill_manual(values = colorblind_pal()(length(ClassifierChoice))) +
  theme(legend.position = "none") + ggtitle(paste0("Classifier change in balanced accuracy,\nall features minus only selected features "))  +
  labs(x = "Classifier",  y = "Differecne in balanced accuracy")+ theme(axis.text.x = element_text(angle = 45, hjust = 1))  + theme(plot.title = element_text(size=8))
plotPerformanceActualClassifiersAll_ABCreducedDiffWeightedtoAll <-
  ggplot(data = PerformanceActualClassifiersAll_ABCreduced_long, aes(x = reorder(variable, -DiffAllReducedWeighted, FUN = median), y = DiffAllReducedWeighted, fill = variable)) +
  geom_boxplot(outlier.shape = NA) + stat_summary(fun=mean, geom="point", shape=20, size=4, color="yellow", fill="yellow") +  theme_linedraw() + scale_fill_manual(values = colorblind_pal()(length(ClassifierChoice))) +
  theme(legend.position = "none") + ggtitle(paste0("Classifier balanced accuracy with selected features *\n (1 - reduction from balanced accuracy obtained with all features)"))  +
  labs(x = "Classifier",  y = "Weigted balanced accuracy")+ theme(axis.text.x = element_text(angle = 45, hjust = 1))  + theme(plot.title = element_text(size=8))
#geom_bar(stat = "summary", fun = median) + theme(legend.position = "none") + ggtitle(paste0("Classifier change in balanced accuracy with selected features, reduced by the reduction from the accuracy obtained with all features"))

# gs <- list(plotPerformanceActualClassifiersAll_ABCreduced, plotPerformanceActualClassifiersAll_ABCreducedDifftoAll, plotPerformanceActualClassifiersAll_ABCreducedDiffWeightedtoAll)
# lay <- rbind(c(1), c(2), c(3))
# grid.arrange(grobs = gs, layout_matrix = lay)

# i = 1
# Liste <- list()
# while (i < 2*length(ClassifierChoice)) {
#   Liste[[i]] <- get(paste(paste0("plotVarImp_",ClassifierChoice[ceiling(i/2)])))
#   Liste[[i+1]] <- get(paste(paste0("plotVarImpABC_",ClassifierChoice[ceiling(i/2)])))
#   i = i+2
# }
# Liste[[i+1]] <- plotPerformanceActualClassifiersAll_ABCreduced
# do.call(grid.arrange, Liste)

# gs <- list(plotVarImp_RF,plotVarImpABC_RF,
#            plotVarImp_CTREE, plotVarImpABC_CTREE,
#            plotVarImp_Rpart, plotVarImpABC_Rpart,
#            plotVarImp_C4.5,plotVarImpABC_C4.5,
#            plotVarImp_C5.0,plotVarImpABC_C5.0,
#            plotVarImp_MARS,plotVarImpABC_MARS,
#            plotVarImp_PART,plotVarImpABC_PART,
#            plotVarImp_RIPPER,plotVarImpABC_RIPPER,
#            plotVarImp_C5.0rules,plotVarImpABC_C5.0rules,
#            plotPerformanceActualClassifiersAll_ABCreducedDiffWeightedtoAll)
# lay <- rbind(c(1,2,3,4),
#              c(5,6,7,8),
#              c(9,10,11,12),
#              c(13,14,15,16),
#              c(17,18,19,19))
# grid.arrange(grobs = gs, layout_matrix = lay)


gs <- list(plotVarImp_RF,plotVarImpABC_RF,
           #plotPerformanceActualClassifiersAll_ABCall,
           plotPerformanceActualClassifiersAll_ABCreduced,
           plotPerformanceActualClassifiersAll_ABCreducedDifftoAll,
           plotPerformanceActualClassifiersAll_ABCreducedDiffWeightedtoAll)
lay <- rbind(c(1,1,1,2,2,2),
             c(3,3,4,4,5,5))
# lay <- rbind(c(1,1,1,1,2,2,2,2),
#              c(3,3,4,4,5,5,6,6))
grid.arrange(grobs = gs, layout_matrix = lay)

# gs <- list(plotPerformanceActualClassifiersAll_ABCall,
#            plotPerformanceActualClassifiersAll_ABCreduced,
#            plotPerformanceActualClassifiersAll_ABCreducedDifftoAll,
#            plotPerformanceActualClassifiersAll_ABCreducedDiffWeightedtoAll)
# lay <- rbind(c(1,2,3,4))
# grid.arrange(grobs = gs, layout_matrix = lay)

#save.image(file = "PART_CART_7_GMM_RFbasedFeatureSelectionAndAIfits.RData")
#load("/home/joern/Aktuell/TRP_UVB/01Transformierte/PART_CART_7_GMM_RFbasedFeatureSelectionAndAIfits.RData")


###################### Rpart plotten

#Tree plot
library(rpart)
set.seed(42)
tree.model.PainClustersGMMcombined <- rpart(
  as.factor(Clusters) ~ .,
  data = ActualDataForClassification,
  method = "class", xval = 1000,
  parms = list(split = "gini"),
  control = rpart.control(cp = 0, maxdepth = 30, minsplit = 5)
)
library(rpart.plot)
rpart.plot(tree.model.PainClustersGMMcombined, type = 5,
           box.palette = list("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442"), col = "white", extra = 1)
