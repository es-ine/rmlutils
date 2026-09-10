# -*- coding: utf-8 -*-
#------------------------------------------------------------------------------#
# Copyright (C) [2026] Instituto Nacional de Estadística
#
# Este archivo forma parte del proyecto MLutils.
#
# Licenciado bajo la Licencia Pública de la Unión Europea (EUPL) v.1.2.
# Puede obtener una copia de la licencia en la raiz de este proyecto o en:
# https://eupl.eu/1.2/es/
#
# A menos que se indique lo contrario, este software se distribuye
# "TAL CUAL", SIN GARANTÍAS NI CONDICIONES DE NINGÚN TIPO.
# Consulte la licencia para conocer los términos específicos.
#------------------------------------------------------------------------------#
# Copyright (C) [2026] National Institute of Statistics
#
# This file is part of the MLutils project.
#
# Licensed under the European Union Public License (EUPL) v.1.2.
# You can obtain a copy of the license at the root of this project or at:
# https://eupl.eu/1.2/es/
#
# Unless otherwise indicated, this software is distributed
# "AS IS", WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND.
# See the license for specific terms.
#------------------------------------------------------------------------------#

#' @name classifierModeler
#' @title Class constructors for classification modeler classes.
#'
#' @description Creates a modeler object as specified.
#'
#' The modeler classes contain different types of machine learning models to
#' provide a common interface for all of them.
#'
#' @section Current implementations:
#'
#' \strong{Simple modelers:}
#' \itemize{
#' \item \code{\link{H2OClassifierModeler}}
#' \item \code{\link{LGBClassifierModeler}}
#' \item \code{\link{rangerClassifierModeler}}
#' \item \code{\link{catboostClassifierModeler}}
#' \item \code{\link{miceClassifierModeler}}
#' }
#'
#' \strong{Complex modelers:}
#' \itemize{
#' \item \code{\link{preproClassifierModeler}}
#' }
#'
#' @return A classifierModeler object.
NULL

#' @rdname H2OClassifierModeler
#' @title Class constructor for H2OClassifierModeler
#'
#' @description H2OModeler implementation for classification tasks.
#' This modeler offers access to the H2O library, but requires the user to run an H2O cluster for it to work.
#'
#' @param model Name of H2O model to be used. The following models are available:
#' \itemize{
#'   \item adaBoost (Binary classification)
#'   \item decision_tree (Binary classification)
#'   \item deeplearning
#'   \item gam (General additive model)
#'   \item gbm (Gradient boosting model)
#'   \item glm (Generalized linear model)
#'   \item naiveBayes
#'   \item psvm (Support vector machine, Binary classification)
#'   \item randomForest
#'   \item rulefit
#'   \item xgboost (Only works on linux machines)
#' }
#' @param params List of parameters to be used for training an H2O model.
#' @param target String indicating the name of the target variable in the data.
#' @param regressors List or vector of strings indicating the regressor column names in the data.
#' @param DF Alternate H2O modeler definition.
#'
#' @return An H2OClassifierModeler object.
#'
#' @seealso \code{\link{classifierModeler}}
#'
#' @examples
#' #NOTE: Running this tutorial may take a few seconds, since the
#' #H2O cloud needs to start
#' #Generate data
#' data.dt <- generateBinaryClassificationData(n_samples = 1000, generate_random_weights = FALSE)
#'
#' #Define train/test partitions
#' set.seed(273)
#' data.keys <- 1:nrow(data.dt)
#' train_size = round(0.8 * nrow(data.dt))
#' smplr <- srsSampler(key = data.keys, ssize = train_size)
#' train.keys <- getSample(smplr)
#' test.keys <- setdiff(data.keys, train.keys)
#'
#' target <- "classification_target"
#' regressors <- setdiff(colnames(data.dt), target)
#'
#' #Start H2O cloud (change port if that one is not available)
#' h2o::h2o.init(port = 54321, startH2O = TRUE)
#'
#' h2o_model <- "adaBoost"
#' params <- list()
#'
#' #Define and train modeler
#' modeler <- H2OClassifierModeler(model = h2o_model,
#'                                 params = params,
#'                                 target = target,
#'                                 regressors = regressors)
#'
#' modeler <- train(object = modeler,
#'                  data = data.dt,
#'                  subSet = train.keys)
#'
#' #Predict
#' #preds <- getPredictions(object = modeler,
#' #                        data = data.dt,
#' #                        subSet = train.keys)
#'
#' #clss <- getClasses(object = modeler,
#' #                   data = data.dt,
#' #                   subSet = train.keys,
#' #                   class_mapping = maxScoreClass())
#'
#' #Evaluate
#' cf <- accuracyCostFunction()
#'
#' getCost(object = cf,
#'         data = data.dt,
#'         modeler = modeler,
#'         fSample = 1:nrow(data.dt),
#'         cSample = test.keys)
#'
#' #evaluate(object = modeler,
#' #         data = data.dt,
#' #         cost_function = cf,
#' #         subSet = test.keys)
#'
#' h2o::h2o.shutdown(prompt = FALSE)
#'
#'
#' @export
H2OClassifierModeler <- function(model, params, target, regressors, DF) {
  if (!requireNamespace("h2o", quietly = TRUE)){
    stop("Package 'h2o' required but not installed")
  }
  suppressWarnings(library(h2o))
  UseMethod("H2OClassifierModeler")
}

#' @rdname H2OClassifierModeler
#' @export
H2OClassifierModeler.character <- function(model, params, target, regressors) {
  object <- list(model = model,
                 params = params,
                 target = target,
                 regressors = regressors,
                 class_names = NA,
                 weightsColumn = params$weights_column,
                 isTrained = FALSE)
  class(object) <- c("H2OClassifierModeler",
                     "H2OModeler",
                     "classifierModeler",
                     "modeler")
  object <- RMLutils:::.validate_H2OModeler(object)
  return(object)
}

#' @rdname H2OClassifierModeler
#' @export
H2OClassifierModeler.data.frame <- function(DF, regressors) {
  object <- lapply(split(DF, 1:nrow(DF)), function(x) {
    x <- as.list(x)
    x <- x[!sapply(x, is.na)]
    x <- lapply(x, function(p) {
      p <- as.character(p)
      if (is.na(as.numeric(p)) && !startsWith(p, "[")) {
        return(p) } else if (!startsWith(p, "[")) {
          return(as.numeric(p))} else {
            p <- paste0("c(", substr(p, 2, nchar(p) - 1), ")")
            return(eval(parse(text = p)))}})
    model <- x$model
    target <- x$target
    params <- x[setdiff(names(x), c("model", "target"))]
    return(H2OClassifierModeler(model, params, target, regressors))
  })
  return(unname(object))
}

#' @rdname preproClassifierModeler
#' @title Class constructor for preproClassifierModeler
#'
#' @description preproModeler implementation for classification tasks.
#'
#' A preproModeler applies a preprocessing function to the data before sending it to another modeler.
#' This pre-processing step will be applied right before training or predicting with the model.
#'
#' @param modeler A classifierModeler object.
#' @param preproObject A \link{preproObject} structure, containing a preprocesing function to apply before training (or predicting) with the modeler.
#'
#' @return A preproClassifierModeler object.
#'
#' @seealso \code{\link{classifierModeler}}
#' @seealso \code{\link{preproRegressorModeler}}
#'
#' @examples
#' #data.dt <- generateBinaryClassificationData(n_samples = 1000, generate_random_weights = FALSE)
#' data.dt <- generateMulticlassClassificationData(n_samples = 1000, generate_random_weights = FALSE)
#'
#' #Define train/test partitions
#' set.seed(273)
#' data.keys <- 1:nrow(data.dt)
#' train_size = round(0.8 * nrow(data.dt))
#' smplr <- srsSampler(key = data.keys, ssize = train_size)
#' train.keys <- getSample(smplr)
#' test.keys <- setdiff(data.keys, train.keys)
#'
#' target <- "classification_target"
#' regressors <- setdiff(colnames(data.dt), target)
#'
#' #Define and train modeler
#' params <- list(
#'   "num.trees" = 50,
#'   "max.depth" = 20
#' )
#'
#' #Internal modeler
#' modeler <- rangerClassifierModeler(params = params,
#'                                    target = target,
#'                                    regressors = regressors)
#'
#' #Preprocessing parameters
#' prepro_params <- list(modeler_type = NULL, pretrained_min_max = NULL)
#'
#' #Define preproObject
#' prepro_object <- preproObject(preproFunction = minMaxScalerPreproFunction,
#'                               params = prepro_params)
#'
#'
#' p_modeler <- preproClassifierModeler(modeler = modeler,
#'                                      preproObject = prepro_object)
#'
#' p_modeler <- train(object = p_modeler, data = data.dt, subSet = train.keys)
#'
#' #Get predictions and classes
#' preds <- getPredictions(object = p_modeler,
#'                         data = data.dt,
#'                         subSet = train.keys)
#'
#' clss <- getClasses(object = p_modeler,
#'                    data = data.dt,
#'                    subSet = train.keys,
#'                    class_mapping = maxScoreClass())
#'
#' #cf <- confusionMatrixCostFunction()
#' cf <- accuracyCostFunction()
#'
#' #Obtain cost with getCost (this way we see the confusion matrix)
#' getCost(object = cf,
#'         data = data.dt,
#'         modeler = p_modeler,
#'         fSample = 1:nrow(data.dt),
#'         cSample = train.keys)
#'
#' #Obtain cost with evaluate
#' evaluate(object = p_modeler,
#'          data = data.dt,
#'          cost_function = cf,
#'          subSet = test.keys)
#'
#'
#' @export
preproClassifierModeler <- function(modeler, preproObject){
  UseMethod("preproClassifierModeler")
}

#' @rdname preproClassifierModeler
#' @export
preproClassifierModeler.modeler <- function(modeler, preproObject){

  #preproObject gets added later
  object <- list(modeler = modeler,
                 target = getTarget(modeler),
                 regressors = getRegressors(modeler),
                 preproObject = preproObject,
                 isTrained = FALSE)

  class(object) <- c("preproClassifierModeler",
                     "preproModeler",
                     "classifierModeler",
                     "modeler")
  object <- RMLutils:::.validate_preproModeler(object)

  #Add important metadata to preproObject
  #and add it to preproModeler
  object$preproObject <- assignExternalAttributes(modeler = modeler,
                                                  object = preproObject)
  return(object)
}

#' @rdname LGBClassifierModeler
#' @title Class constructor for LGBClassifierModeler
#'
#' @description LightGBM implementation in rmlutils for classification tasks.
#'
#' @param params List of parameters to be used for training a LightGBM model.
#' Tipically, the user may want to train a binary classifier or a multiclass
#' classifier for a binary variable, depending on their requirements
#' (as some parameters work only with one or the other). In that case, the
#' objective parameter should be either "binary" or "multiclass", respectively.
#' @param target String indicating the name of the target variable in the data.
#' @param regressors List or vector of strings indicating the regressor column names in the data.
#' @param positive_class String indicating the name of the positive class, only
#' used when params$objective is binary. Note that if this parameter is not
#' provided, it will be assumed that the target variable levels are the negative
#' class and the positive class, in that order.
#'
#' @return A LGBClassifierModeler object.
#'
#' @seealso \code{\link{classifierModeler}}
#'
#' @examples
#' #Generate data
#' data.dt <- generateBinaryClassificationData(n_samples = 1000,
#'                                             generate_random_weights = FALSE)
#'
#' #Define train/test partitions
#' set.seed(273)
#' data.keys <- 1:nrow(data.dt)
#' train_size = round(0.8 * nrow(data.dt))
#' smplr <- srsSampler(key = data.keys, ssize = train_size)
#' train.keys <- getSample(smplr)
#' test.keys <- setdiff(data.keys, train.keys)
#'
#' #Multiclass model
#' target <- "classification_target"
#' regressors <- setdiff(colnames(data.dt), target)
#' params <- list(
#'   "learning_rate" = 0.07,
#'   "nrounds" = 10,
#'   "objective" = "multiclass",
#'   "num_class" = length(unique(data.dt[, classification_target]))
#' )
#'
#' #Define and train modeler
#' modeler <- LGBClassifierModeler(params = params,
#'                                 target = target,
#'                                 regressors = regressors)
#'
#' modeler <- train(object = modeler,
#'                  data = data.dt,
#'                  subSet = train.keys)
#'
#' #Get predictions and classes
#' preds <- getPredictions(object = modeler,
#'                         data = data.dt,
#'                         subSet = train.keys)
#'
#' clss <- getClasses(object = modeler,
#'                    data = data.dt,
#'                    subSet = train.keys,
#'                    class_mapping = maxScoreClass())
#'
#' #cf <- confusionMatrixCostFunction()
#' cf <- accuracyCostFunction()
#'
#' #Obtain cost with getCost (this way we see the confusion matrix)
#' getCost(object = cf,
#'         data = data.dt,
#'         modeler = modeler,
#'         fSample = 1:nrow(data.dt),
#'         cSample = train.keys)
#'
#' #Obtain cost with evaluate
#' evaluate(object = modeler,
#'          data = data.dt,
#'          cost_function = cf,
#'          subSet = test.keys)
#'
#' #Binary model
#' target <- "classification_target"
#' regressors <- setdiff(colnames(data.dt), target)
#' params <- list(
#'   "learning_rate" = 0.07,
#'   "nrounds" = 10,
#'   "objective" = "binary"
#' )
#'
#' #Specify positive class, otherwise, if target levels are not
#' #(negative, positive), it may not work properly
#' modeler2 <- LGBClassifierModeler(params = params,
#'                                  target = target,
#'                                  regressors = regressors,
#'                                  positive_class = "positive")
#'
#' #Train and predict
#' modeler2 <- train(object = modeler2,
#'                   data = data.dt,
#'                   subSet = train.keys)
#'
#' preds <- getPredictions(object = modeler2,
#'                         data = data.dt,
#'                         subSet = train.keys)
#'
#' clss <- getClasses(object = modeler2,
#'                    data = data.dt,
#'                    subSet = train.keys,
#'                    class_mapping = maxScoreClass())
#'
#' cf <- accuracyCostFunction()
#' #cf <- confusionMatrixCostFunction()
#'
#' getCost(object = cf,
#'         data = data.dt,
#'         modeler = modeler2,
#'         fSample = 1:nrow(data.dt),
#'         cSample = train.keys)
#'
#' evaluate(object = modeler2,
#'          data = data.dt,
#'          cost_function = cf,
#'          subSet = train.keys)
#'
#'
#' @export
LGBClassifierModeler <- function(params, target, regressors, positive_class = NULL) {
  if (!requireNamespace("lightgbm", quietly = TRUE)){
    stop("Package 'lightgbm' required but not installed")
  }
  UseMethod("LGBClassifierModeler")
}

#' @rdname LGBClassifierModeler
#' @export
LGBClassifierModeler.list <- function(params, target, regressors, positive_class = NULL) {# Al quitar model del primer parámetro, cambiamos a LGBModeler.list (los parámetros se meten como lista)

  object <- list(model= "lightgbm",
                 params = params,
                 target = target,
                 regressors = regressors,
                 class_names = NA,
                 positive_class = positive_class,
                 weightsColumn = params$weight_column,
                 isTrained = FALSE)
  class(object) <- c("LGBClassifierModeler",
                     "LGBModeler",
                     "classifierModeler",
                     "modeler")

  object <- RMLutils:::.validate_LGBModeler(object)

  return(object)
}

#' @rdname rangerClassifierModeler
#' @title Class constructor for rangerClassifierModeler
#'
#' @description ranger implementation in rmlutils for classification tasks.
#'
#' Wright, M. N. & Ziegler, A. (2017). ranger: A fast implementation of random
#' forests for high dimensional data in C++ and R. J Stat Softw
#' 77:1-17. \url{https://doi.org/10.18637/jss.v077.i01}.
#'
#' @param params List of parameters to be used for training a ranger model.
#' @param target String indicating the name of the target variable in the data.
#' @param regressors List or vector of strings indicating the regressor column names in the data.
#'
#' @return A rangerClassifierModeler object.
#'
#' @seealso \code{\link{classifierModeler}}
#'
#' @examples
#' #Generate data
#' #data.dt <- generateBinaryClassificationData(n_samples = 1000, generate_random_weights = FALSE)
#' data.dt <- generateMulticlassClassificationData(n_samples = 1000, generate_random_weights = FALSE)
#'
#' #Define train/test partitions
#' set.seed(273)
#' data.keys <- 1:nrow(data.dt)
#' train_size = round(0.8 * nrow(data.dt))
#' smplr <- srsSampler(key = data.keys, ssize = train_size)
#' train.keys <- getSample(smplr)
#' test.keys <- setdiff(data.keys, train.keys)
#'
#' target <- "classification_target"
#' regressors <- setdiff(colnames(data.dt), target)
#'
#' #Define and train modeler
#' params <- list(
#'   "num.trees" = 50,
#'   "max.depth" = 20
#' )
#'
#' modeler <- rangerClassifierModeler(params = params,
#'                                    target = target,
#'                                    regressors = regressors)
#'
#' modeler <- train(object = modeler, data = data.dt, subSet = train.keys)
#'
#' #Get predictions and classes
#' preds <- getPredictions(object = modeler,
#'                         data = data.dt,
#'                         subSet = train.keys)
#'
#' clss <- getClasses(object = modeler,
#'                    data = data.dt,
#'                    subSet = train.keys,
#'                    class_mapping = maxScoreClass())
#'
#' #cf <- confusionMatrixCostFunction()
#' cf <- accuracyCostFunction()
#'
#' #Obtain cost with getCost (this way we see the confusion matrix)
#' getCost(object = cf,
#'         data = data.dt,
#'         modeler = modeler,
#'         fSample = 1:nrow(data.dt),
#'         cSample = train.keys)
#'
#' #Obtain cost with evaluate
#' evaluate(object = modeler,
#'          data = data.dt,
#'          cost_function = cf,
#'          subSet = test.keys)
#'
#'
#' @export
rangerClassifierModeler <- function(params, target, regressors) {
  if (!requireNamespace("ranger", quietly = TRUE)){
    stop("Package 'ranger' required but not installed")
  }
  UseMethod("rangerClassifierModeler")
}

#' @rdname rangerClassifierModeler
#' @export
rangerClassifierModeler.list <- function(params, target, regressors) {

  object <- list(model= "ranger",
                 params = params,
                 target = target,
                 regressors = regressors,
                 class_names = NA,
                 weightsColumn = params$case.weights,
                 isTrained = FALSE)
  class(object) <- c("rangerClassifierModeler",
                     "rangerModeler",
                     "classifierModeler",
                     "modeler")
  object <- RMLutils:::.validate_rangerModeler(object)
  return(object)
}

#' @rdname catboostClassifierModeler
#' @title Class constructor for catboostClassifierModeler
#'
#' @description Catboost implementation in rmlutils for classification tasks.
#'
#' @param params List of parameters to be used for training a catboost model.
#' For classification, the available loss_function parameters are:
#' \itemize{
#'   \item Logloss (Binary)
#'   \item CrossEntropy (Binary)
#'   \item MultiClass
#'   \item MultiClassOneVsAll
#' }
#' @param target String indicating the name of the target variable in the data.
#' @param regressors List or vector of strings indicating the regressor column names in the data.
#' @param val_data Validation dataset to be used when training.
#' @param positive_class String indicating the name of the positive class, only
#' used when params$loss_function is Logloss or CrossEntropy. Note that if this
#' parameter is not provided, it will be assumed that the target variable levels
#' are the negative class and the positive class, in that order.
#' @param ntree_start Additional parameter for catboost's predict method.
#' @param ntree_end Additional parameter for catboost's predict method.
#' @param thread_count Additional parameter for catboost's predict method.
#'
#' @return A catboostClassifierModeler object.
#'
#' @seealso \code{\link{classifierModeler}}
#'
#' @examples
#' #Generate data
#' data.dt <- generateBinaryClassificationData(n_samples = 1000, generate_random_weights = FALSE)
#' #data.dt <- generateMulticlassClassificationData(n_samples = 1000, generate_random_weights = FALSE)
#'
#' #Define train/test partitions
#' set.seed(273)
#' data.keys <- 1:nrow(data.dt)
#' train_size = round(0.8 * nrow(data.dt))
#' smplr <- srsSampler(key = data.keys, ssize = train_size)
#' train.keys <- getSample(smplr)
#' test.keys <- setdiff(data.keys, train.keys)
#'
#' target <- "classification_target"
#' regressors <- setdiff(colnames(data.dt), target)
#'
#' #Define and train modeler
#' params <- list(
#'   #loss_function = 'MultiClass',
#'   #loss_function = 'MultiClassOneVsAll',
#'   loss_function = 'Logloss',#Binary
#'   #loss_function = 'CrossEntropy',#Binary
#'   iterations = 100,
#'   metric_period=10)
#'
#' modeler <- catboostClassifierModeler(params = params,
#'                                      target = target,
#'                                      regressors = regressors,
#'                                      positive_class = "positive")
#'
#' modeler <- train(object = modeler, data = data.dt, subSet = train.keys)
#'
#' #Get predictions and classes
#' preds <- getPredictions(object = modeler,
#'                         data = data.dt,
#'                         subSet = train.keys)
#'
#' clss <- getClasses(object = modeler,
#'                    data = data.dt,
#'                    subSet = train.keys,
#'                    class_mapping = maxScoreClass())
#'
#' #cf <- confusionMatrixCostFunction()
#' cf <- accuracyCostFunction()
#'
#' #Obtain cost with getCost (this way we see the confusion matrix)
#' getCost(object = cf,
#'         data = data.dt,
#'         modeler = modeler,
#'         fSample = 1:nrow(data.dt),
#'         cSample = train.keys)
#'
#' #Obtain cost with evaluate
#' evaluate(object = modeler,
#'          data = data.dt,
#'          cost_function = cf,
#'          subSet = test.keys)
#'
#'
#' @export
catboostClassifierModeler <- function(params,
                                      target,
                                      regressors,
                                      val_data,
                                      positive_class = NULL,
                                      ntree_start = 0,
                                      ntree_end = 0,
                                      thread_count = -1) {
  if (!requireNamespace("catboost", quietly = TRUE)){
    stop("Package 'catboost' required but not installed")
  }
  UseMethod("catboostClassifierModeler")
}

#' @rdname catboostClassifierModeler
#' @export
catboostClassifierModeler.list <- function(params,
                                           target,
                                           regressors,
                                           val_data = NULL,
                                           positive_class = NULL,
                                           ntree_start = 0,
                                           ntree_end = 0,
                                           thread_count = -1) {

  object <- list(model = "catboost",
                 params = params,
                 target = target,
                 regressors = regressors,
                 class_names = NA,
                 positive_class = positive_class,
                 weightsColumn = params$weight_column,
                 isTrained = FALSE,
                 prediction_type = "Probability",
                 val_data = val_data,
                 ntree_start = ntree_start,
                 ntree_end = ntree_end,
                 thread_count = thread_count)
  class(object) <- c("catboostClassifierModeler",
                     "catboostModeler",
                     "classifierModeler",
                     "modeler")
  object <- RMLutils:::.validate_catboostModeler(object)
  return(object)
}


#' @rdname miceClassifierModeler
#' @title Class constructor for miceClassifierModeler
#'
#' @description mice implementation in rmlutils for classification tasks.
#' Note that due to the way that mice works, its results are stochastic,
#' meaning that predicting over the same dataset multiple times will
#' usually provide different results. The user should use it with caution.
#'
#' @param params List of parameters to be used for training a mice model.
#' @param target String indicating the name of the target variable in the data.
#' @param regressors List or vector of strings indicating the regressor column names in the data.
#'
#' @return A miceClassifierModeler object.
#'
#' @seealso \code{\link{classifierModeler}}
#'
#' @examples
#' #Generate data
#' #data.dt <- generateBinaryClassificationData(n_samples = 1000, generate_random_weights = FALSE)
#' data.dt <- generateMulticlassClassificationData(n_samples = 1000, generate_random_weights = FALSE)
#'
#' #Define train/test partitions
#' set.seed(273)
#' data.keys <- 1:nrow(data.dt)
#' train_size = round(0.8 * nrow(data.dt))
#' smplr <- srsSampler(key = data.keys, ssize = train_size)
#' train.keys <- getSample(smplr)
#' test.keys <- setdiff(data.keys, train.keys)
#'
#' target <- "classification_target"
#' regressors <- setdiff(colnames(data.dt), target)
#'
#' #Define and train modeler
#' params <- list(m = 1,
#'                method = "rf",
#'                print = FALSE)
#'
#' modeler <- miceClassifierModeler(params = params,
#'                                  target = target,
#'                                  regressors = regressors)
#'
#' modeler <- train(object = modeler, data = data.dt, subSet = train.keys)
#'
#' #Get predictions and classes
#' preds <- getPredictions(object = modeler,
#'                         data = data.dt,
#'                         subSet = train.keys)
#'
#' clss <- getClasses(object = modeler,
#'                    data = data.dt,
#'                    subSet = train.keys,
#'                    class_mapping = maxScoreClass())
#'
#' #cf <- confusionMatrixCostFunction()
#' cf <- accuracyCostFunction()
#'
#' #Obtain cost with getCost (this way we see the confusion matrix)
#' getCost(object = cf,
#'         data = data.dt,
#'         modeler = modeler,
#'         fSample = 1:nrow(data.dt),
#'         cSample = train.keys)
#'
#' #Obtain cost with evaluate
#' evaluate(object = modeler,
#'          data = data.dt,
#'          cost_function = cf,
#'          subSet = test.keys)
#'
#'
#' @export
miceClassifierModeler <- function(params, target, regressors) {
  if (!requireNamespace("mice", quietly = TRUE)){
    stop("Package 'mice' required but not installed")
  }
  UseMethod("miceClassifierModeler")
}

#' @rdname miceClassifierModeler
#' @export
miceClassifierModeler.list <- function(params, target, regressors = NULL) {

  object <- list(model= "mice",
                 params = params,
                 target = target,
                 regressors = regressors,
                 class_names = NA,
                 weightsColumn = NULL,
                 isTrained = FALSE)
  class(object) <- c("miceClassifierModeler",
                     "miceModeler",
                     "classifierModeler",
                     "modeler")
  object <- RMLutils:::.validate_miceModeler(object)
  return(object)
}
