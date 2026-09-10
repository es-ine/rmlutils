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

# Sampling utilities ####

#' @title Alternate sample function
#'
#' @description
#' A custom variant to the base R function sample that allows samples from single-element vectors.
#'
#' @param ... Common arguments to send to \code{\link{sample}}.
#'   \itemize{
#'     \item \code{x}: vector of elements to sample from.
#'     \item \code{size}: amount of elements to sample.
#'     \item \code{replace}: logical indicating whether to allow for replacement when sampling.
#'     \item \code{prob}: vector with sampling probabilities.
#'   }
#'
#' @return A vector of length \code{size} with the sampled elements.
#'   If \code{x} is of length 1, returns \code{rep(x, size)}.
#'   Otherwise, it is identical to \code{sample}.
#' @seealso \code{\link{sample}}
#'
#' @examples
#' set.seed(273)
#' # Single value vector to sample from
#' sample_alt(x = c(1), size = 3)
#'
#' #Normal case
#' sample_alt(x = 1:10, size = 5)
#'
#'
#' @export
sample_alt <- function(...) {

  alt_call <- match.call(sample)
  alt_call[[1]] <- as.name("sample")

  if (length(eval.parent(alt_call$x)) == 1)
    return(rep(eval.parent(alt_call$x), eval.parent(alt_call$size)))
  else return(eval.parent(alt_call))
}

# Training utilities ####

#' @title Expand hyper parameters
#'
#' @description A function to expand a list of hyperparameters into all its possible combinations, generally useful for cross-validation.
#'
#' @param hyperParamSet Named list of hyperparameters.
#'
#' @return List of lists. Where each sublist is a possible combination of hyperparameters.
#'
#' @seealso \code{\link{crossValidate}}
#' @seealso \code{\link{hyperParamCVH2O}}
#'
#' @examples
#' hyperparams <- list(
#' family = "gaussian",
#' lambda = 10**(-3:3),
#' alpha = 0:10 / 10)
#'
#' combinations <- expandHyperParams(hyperparams)
#'
#' print(combinations)
#'
#' @export
expandHyperParams <- function(hyperParamsSet) {
  if (is.null(hyperParamsSet) || !is.list(hyperParamsSet))
    return(NULL)
  combination <- integer(length(hyperParamsSet))
  output <- list()
  for (i in 1:prod(sapply(hyperParamsSet, length)) - 1) {
    icopy <- i
    newelement <- list()
    for (j in 1:length(combination)) {
      combination[j] <- icopy %% length(hyperParamsSet[[j]])
      icopy <- icopy %/% length(hyperParamsSet[[j]])
      newelement <- c(newelement, hyperParamsSet[[j]][combination[j] + 1])
    }
    names(newelement) <- names(hyperParamsSet)
    output[[i + 1]] <- newelement
  }
  return(output)
}

#' @title Hyperparameter Cross-validation H2O
#'
#' @description H2o grid searches hyperparameters by grid to tune the best models for
#' fitting the dataset to the target variables.
#'
#'
#' @param data Dataset to work with
#' @param targets Name or list of names of the target variables
#' @param regressors Name or list of names of the regressor variables.
#' @param nfolds Number of folds to use for cross validation.
#' @param weights Name or list of names for weight values each entry. (Ask)
#' @param models Name or list of names of models to study.
#' @param search_criteria List of search parameters to follow.
#' @param result_metric Type of metric to order the grid models with.
#'        Valid values include: "r2", "logloss", "residual_deviance", "mse", "auc", "accuracy", "precision", "recall", "f1", etc.
#'
#' @seealso \code{\link{expandHyperParams}}
#'
#' @examples
#' models <- hyperParamCVH2O(DFH2O, "d", c("a", "b", "c"), 5, "w",
#'    models = c("glm", "randomForest")
#'    search_criteria = list(strategy = "RandomDiscrete", max_runtime_secs = 3600)
#'    result_metric = "r2")
#'
#' @return return a data.frame with the cost function metric specified of each model. In order to obtain the best model it would
#' have to be trained separately.
#'
#' @noRd
hyperParamCVH2O <- function(data, targets, regressors, nfolds = 5, weights = "Weights",
                            models = c("glm", "randomForest", "xgboost", "deeplearning"),
                            results_metric = "r2",
                            search_criteria = list(strategy = "RandomDiscrete",max_runtime_secs = 3600, ...)) {

  extraArgs <- list(...)
  output <- data.table()

  for (targetVar in targets) {
    cat(paste("Processing variable", (1:length(targets))[targetVar == targets],"out of", length(targets), "...\n"))
    for (model in models) {
      cat(paste("Tuning algorithm", model, "...\n"))
      IND <- (1:nrow(data))[as.logical(!is.na(data[[targetVar]]))]
      if (!is.null(weights))
        gridML <- h2o.grid(model, grid_id = paste0(targetVar,"-",model, "-grid"), x = regressors, y = targetVar,
                           search_criteria = search_criteria, training_frame = data[IND,], nfolds = nfolds,
                           weights_column = weights, hyper_params = extraArgs[[model]])
      else gridML <- h2o.grid(model, grid_id = paste0(targetVar,"-",model, "-grid"), x = regressors, y = targetVar,
                              search_criteria = search_criteria, training_frame = data[IND,], nfolds = nfolds,
                              hyper_params = extraArgs[[model]])
      output <- rbind(output,
                      data.table(h2o.getGrid(paste0(targetVar,"-",model, "-grid"),
                                             results_metric, TRUE)@summary_table), fill = TRUE)
      h2o.rm(paste0(targetVar,"-",model, "-grid"))
      write.csv(output, file = "output.csv")
    }
  }

  output[, (results_metric) := as.numeric(get(results_metric))]
  output[, target := sapply(strsplit(output$model_ids, "-"), "[", i = 1)]
  output[, model := sapply(strsplit(output$model_ids, "-"), "[", i = 2)]
  output[, model_ids := NULL]
  setorderv(output, results_metric, order = -1L)

  return(output)
}

# PreproModeler utilities ####

#' @name preproObject
#' @title Class constructor for preproObject class.
#'
#' @description Creates a preproObject as specified.
#'
#' A preproObject is used to apply preprocessing steps before calling the train and getPredictions methods.
#' Once built, it can be used by calling the \link{getPreproData} method.
#' The user should generally use it mostly with preproModeler objects,
#' although it is possible to use it to preprocess data separately.
#'
#' @param preproFunction A custom preprocessing function, such as \code{\link{minMaxScalerPreproFunction}} or any other function defined by the user (external to RMLutils).
#' These preproFunctions should only take a preproObject as their unique argument.
#' @param params A list of parameters to be used when calling preproFunction. The user is free to use them however they see fit.
#'
#' @return A preproObject.
#'
#' @seealso \code{\link{preproRegressorModeler}}
#' @seealso \code{\link{preproClassifierModeler}}
#' @seealso \code{\link{assignExternalAttributes}}
#' @seealso \code{\link{getPreproData}}
#'
#' @examples
#' #Generate data
#' data.dt <- generateRegressionData(n_samples = 1000,
#'                                   generate_random_weights = TRUE)
#' head(data.dt)
#'
#' #Define relevant variables
#' target <- "regression_target"
#' weightsColumn <- "weights"
#' regressors <- setdiff(colnames(data.dt), c(target, weightsColumn))
#'
#' modeler <- list("target" = target,
#'                 "regressors" = regressors,
#'                 "weightsColumn" = weightsColumn)
#'
#' #Define preproObject
#' #Preprocessing parameters
#' prepro_params <- list(modeler_type = NULL, pretrained_min_max = NULL)
#'
#' #Define preproObject
#' prepro_object <- preproObject(preproFunction = minMaxScalerPreproFunction,
#'                               params = prepro_params)
#'
#' #Assign relevant metadata to preproObject
#' prepro_object <- assignExternalAttributes(modeler = modeler,
#'                                           object = prepro_object)
#'
#' #Call getPreproData to obtain preproObject with preprocessed data
#' prepro_object <- getPreproData(object = prepro_object, data = data.dt)
#'
#' #data is saved within prepro_object
#' head(prepro_object$data)
#'
#' @export
preproObject <- function(preproFunction, params) {
  UseMethod("preproObject")
}

#' @rdname preproObject
#' @export
preproObject.function <- function(preproFunction, params){

  object <- list(preproFunction = preproFunction, params = params, data = NULL, new_regressors = NULL)

  class(object) <- c("preproObject")

  object <- RMLutils:::.validate_preproObject(object)

  return(object)
}

#' @name assignExternalAttributes
#' @title Method to assign required attributes to a preproObject.
#'
#' @description
#' Assigns target, regressors and weightsColumn to a preproObject.
#' Additionally, defines which columns to filter when training and predicting.
#'
#' This method runs automatically when instantiating a preproModeler. The user
#' should avoid running it unless they want to preprocess data outside a
#' preproModeler.
#'
#' @param modeler A modeler object with attributes relevant to preprocessing,
#' such as target(s), regressors and weightsColumn. It can also be a list with
#' target, regressors and weightsColumn components, in case the user wants to
#' preprocess data independently from any specific modeler.
#' @param object A preproObject.
#'
#' @return preproObject with target, regressors and weightsColumn attributes.
#'
#' @seealso \code{\link{preproRegressorModeler}}
#' @seealso \code{\link{preproClassifierModeler}}
#' @seealso \code{\link{preproObject}}
#' @seealso \code{\link{getPreproData}}
#'
#' @examples
#' #Generate data
#' data.dt <- generateRegressionData(n_samples = 1000,
#'                                   generate_random_weights = TRUE)
#' head(data.dt)
#'
#' #Define relevant variables
#' target <- "regression_target"
#' weightsColumn <- "weights"
#' regressors <- setdiff(colnames(data.dt), c(target, weightsColumn))
#'
#' modeler <- list("target" = target,
#'                 "regressors" = regressors,
#'                 "weightsColumn" = weightsColumn)
#'
#' #Define preproObject
#' #Preprocessing parameters
#' prepro_params <- list(pretrained_min_max = NULL)
#'
#' #Define preproObject
#' prepro_object <- preproObject(preproFunction = minMaxScalerPreproFunction,
#'                               params = prepro_params)
#'
#' #Assign relevant metadata to preproObject
#' prepro_object <- assignExternalAttributes(modeler = modeler,
#'                                           object = prepro_object)
#'
#' #Call getPreproData to obtain preproObject with preprocessed data
#' prepro_object <- getPreproData(object = prepro_object, data = data.dt)
#'
#' #data is saved within prepro_object
#' head(prepro_object$data)
#'
#'
#' #Alternatively, assignExternalAttributes can work with any modeler
#' params <- list(
#'   "learning_rate" = 0.07,
#'   "nrounds" = 10,
#'   "objective" = "regression",
#'   "weight_column" = "weights"
#' )
#'
#' modeler <- LGBRegressorModeler(params = params,
#'                                target = target,
#'                                regressors = regressors)
#'
#' #Define preproObject
#' prepro_object <- preproObject(preproFunction = minMaxScalerPreproFunction,
#'                               params = prepro_params)
#'
#' #Assign relevant metadata to preproObject
#' prepro_object <- assignExternalAttributes(modeler = modeler,
#'                                           object = prepro_object)
#'
#' #As it can be seen, preproObject now has the matching attributes provided
#' #by the modeler (note that the weightsColumn attribute is now NULL)
#' print(prepro_object)
#'
#' #Call getPreproData to obtain preproObject with preprocessed data
#' prepro_object <- getPreproData(object = prepro_object, data = data.dt)
#'
#' #data is saved within prepro_object
#' #note that our data has a weights column, and the previous modeler does
#' #have a weightsColumn attribute (since it was defined in its parameters),
#' #so the weights column was transformed.
#' head(prepro_object$data)
#'
#' @export
assignExternalAttributes <- function(modeler, object){
  UseMethod("assignExternalAttributes")
}

#' @rdname assignExternalAttributes
#' @export
assignExternalAttributes.modeler <- function(modeler, object){
  object$target <- getTarget(modeler)
  object$regressors <- getRegressors(modeler)
  object$weightsColumn <- getWeightsColumn(modeler)

  if(is.null(object$weightsColumn)){
    object$weightsColumn <- ""
  }

  all_regressors <- object$regressors

  if(object$weightsColumn != ""){
    object$filterColumnsTrain <- c(object$target, all_regressors, object$weightsColumn)
  }else{
    object$filterColumnsTrain <- c(object$target, all_regressors)
  }
  object$filterColumnsPredict <- all_regressors
  return(object)
}

#' @rdname assignExternalAttributes
#' @export
assignExternalAttributes.prePredRegressorModeler <- function(modeler, object){
  #TODO: Consider naming the object$target vector
  object$target <- c(getTarget(modeler$predModeler),
                     getTarget(modeler$mainModeler))
  object$regressors <- getRegressors(modeler)
  object$weightsColumn <- getWeightsColumn(modeler)

  if(is.null(object$weightsColumn)){
    object$weightsColumn <- ""
  }

  all_regressors <- setdiff(unique(unlist(getRegressors(modeler), use.names = FALSE)),
                            getTarget(modeler$predModeler))

  if(object$weightsColumn != ""){
    object$filterColumnsTrain <- c(object$target,
                                   all_regressors,
                                   object$weightsColumn)
  }else{
    object$filterColumnsTrain <- c(object$target,
                                   all_regressors)
  }
  object$filterColumnsPredict <- unique(unlist(getRegressors(modeler)), use.names = FALSE)
  return(object)
}

#' @rdname assignExternalAttributes
#' @export
assignExternalAttributes.classifRegRegressorModeler <- function(modeler, object){
  #TODO: Consider naming the object$target vector
  object$target <- c(getTarget(modeler$classifModeler),
                     getTarget(modeler$regModeler))
  object$regressors <- getRegressors(modeler)
  object$weightsColumn <- getWeightsColumn(modeler)

  if(is.null(object$weightsColumn)){
    object$weightsColumn <- ""
  }

  all_regressors <- unique(unlist(object$regressors, use.names = FALSE))

  if(object$weightsColumn != ""){
    object$filterColumnsTrain <- c(object$target, all_regressors, object$weightsColumn)
  }else{
    object$filterColumnsTrain <- c(object$target, all_regressors)
  }
  object$filterColumnsPredict <- all_regressors
  return(object)
}

#' @rdname assignExternalAttributes
#' @export
assignExternalAttributes.benchModeler <- function(modeler, object){
  object$target <- getTarget(modeler)
  object$regressors <- getRegressors(modeler)
  object$weightsColumn <- getWeightsColumn(modeler)

  if(is.null(object$weightsColumn)){
    object$weightsColumn <- ""
  }

  all_regressors <- unique(unlist(object$regressors, use.names = FALSE))

  if(object$weightsColumn != ""){
    object$filterColumnsTrain <- c(object$target, all_regressors, object$weightsColumn)
  }else{
    object$filterColumnsTrain <- c(object$target, all_regressors)
  }
  object$filterColumnsPredict <- all_regressors
  return(object)
}

#' @rdname assignExternalAttributes
#' @export
assignExternalAttributes.list <- function(modeler, object){
  object$target <- modeler$target
  object$regressors <- modeler$regressors
  object$weightsColumn <- modeler$weightsColumn

  if(is.null(object$weightsColumn)){
    object$weightsColumn <- ""
  }

  if(object$weightsColumn != ""){
    object$filterColumnsTrain <- c(object$target, object$regressors, object$weightsColumn)
  }else{
    object$filterColumnsTrain <- c(object$target, object$regressors)
  }
  object$filterColumnsPredict <- object$regressors
  return(object)
}

#' @name getPreproData
#' @title Method to apply preprocessing to data, given a preproObject
#'
#' @description Applies preprocessing to the given data, saving the data inside the object. Additionally, saves the new regressors in case some were added during the preprocessing.
#'
#' @param object preproObject to be used. It should contain a preprocessing function and a set of parameters.
#' @param data dataset to be preprocessed.
#' @return preproObject with preprocessed data.
#'
#' @seealso \code{\link{preproRegressorModeler}}
#' @seealso \code{\link{preproClassifierModeler}}
#' @seealso \code{\link{assignExternalAttributes}}
#' @seealso \code{\link{preproObject}}
#'
#' @examples
#' #Generate data
#' data.dt <- generateRegressionData(n_samples = 1000,
#'                                   generate_random_weights = TRUE)
#' head(data.dt)
#'
#' #Define relevant variables
#' target <- "regression_target"
#' weightsColumn <- "weights"
#' regressors <- setdiff(colnames(data.dt), c(target, weightsColumn))
#'
#' modeler <- list("target" = target,
#'                 "regressors" = regressors,
#'                 "weightsColumn" = weightsColumn)
#'
#' #Define preproObject
#' #Preprocessing parameters
#' prepro_params <- list(modeler_type = NULL, pretrained_min_max = NULL)
#'
#' #Define preproObject
#' prepro_object <- preproObject(preproFunction = minMaxScalerPreproFunction,
#'                               params = prepro_params)
#'
#' #Assign relevant metadata to preproObject
#' prepro_object <- assignExternalAttributes(modeler = modeler,
#'                                           object = prepro_object)
#'
#' #Call getPreproData to obtain preproObject with preprocessed data
#' prepro_object <- getPreproData(object = prepro_object, data = data.dt)
#'
#' #data is saved within prepro_object
#' head(prepro_object$data)
#'
#' @export
getPreproData <- function(object, data) {
  UseMethod("getPreproData")
}

#' @rdname getPreproData
#' @export
getPreproData.preproObject <- function(object, data){

  #Convert to data.table
  data.dt <- as.data.table(data)

  #Select only relevant columns for preprocessing
  if(any(object$target %in% colnames(data.dt))){
    filter_columns <- object$filterColumnsTrain
  }else{
    filter_columns <- object$filterColumnsPredict
  }
  data.dt <- data.dt[, ..filter_columns]

  #Put data into preproObject
  object$data <- data.dt

  #Call prepro function (outputted object contains preprocessed data)
  #Can be accessed like: object$data (i.e. it overwrites original data)
  object <- object$preproFunction(object)

  #Last consistency check to make sure the new_regressors attribute was set.
  if(is.null(object$new_regressors)){
    warning("The current prepro_function implementation did not update the new_regressors attribute in the PreproObject. Attempting to detect new_regressors automatically...")
    column_name_candidates <- setdiff(colnames(object$data), object$regressors)
    #Separate target and weightsColumn from new_regressors
    if(object$weightsColumn != ""){
      no_preprocessing <- c(object$target, object$weightsColumn)

    }else{
      no_preprocessing <- c(object$target)
    }
    object$new_regressors <- column_name_candidates[!column_name_candidates %in% no_preprocessing]
  }

  return(object)
}

# Prepro functions ####

#' @name minMaxScalerPreproFunction
#' @title A numeric min-max scaling preprocessing function
#'
#' @description
#' Given a \code{\link{preproObject}}, applies a Min-Max transformation to its
#' numeric data, so that it always ranges from 0 to 1.
#' Note that it saves the found minimum and maximum in the preproObject,
#' so that future executions will take those statistics into account,
#' instead of calculating them each time.
#' In general, the user should use the \code{\link{getPreproData}} function
#' to interact with it. Although it is recommended to use either a
#' \code{\link{preproRegressorModeler}} or \code{\link{preproClassifierModeler}}
#' instead of directly calling the previous method.
#'
#' Prepro functions should only take the preproObject as an argument.
#'
#' @param object \code{\link{preproObject}} to be used.
#' It should contain a preprocessing function and a set of parameters.
#'
#' For this preprocessing function in particular, its parameters should be:
#'
#' \itemize{
#'   \item modeler_type: String indicating the type of modeler that will
#'   be trained with preprocessed data. This is specifically to calculate
#'   the correct format for new_regressors. Its possible values are:
#'   \itemize{
#'      \item "simple" or NULL: A string vector will be assigned to new_regressors.
#'      \item "classifRegModeler": A named list with new_regressors for each
#'      internal modeler will be assigned.
#'      \item "prePredModeler": A named list with new_regressors for each
#'      internal modeler will be assigned.
#'      \item "benchRegressorModeler": An unnamed list with new_regressors for each
#'      internal modeler will be assigned.
#'   }
#'   \item pretrained_min_max (optional): A named list of lists, containing
#'   the minimum and maximum value for each numeric regressor. If left empty,
#'   it will be filled the first time that this function is called.
#' }
#'
#' Note that this preprocessing function assumes that all internal modelers (for
#' complex modelers only) will be assigned the same regressors, except
#' prePredModeler, where its mainModeler will also have the predModeler target
#' in its new_regressors.
#'
#' @return \code{\link{preproObject}} with preprocessed data.
#'
#' @seealso \code{\link{preproObject}}
#' @seealso \code{\link{assignExternalAttributes}}
#' @seealso \code{\link{getPreproData}}
#' @seealso \code{\link{preproClassifierModeler}}
#' @seealso \code{\link{preproRegressorModeler}}
#'
#' @examples
#' #Generate data
#' data.dt <- generateRegressionData(n_samples = 1000,
#'                                   generate_random_weights = TRUE)
#' head(data.dt)
#'
#' #Define relevant variables
#' target <- "regression_target"
#' weightsColumn <- "weights"
#' regressors <- setdiff(colnames(data.dt), c(target, weightsColumn))
#'
#' modeler <- list("target" = target,
#'                 "regressors" = regressors,
#'                 "weightsColumn" = weightsColumn)
#'
#' #Define preproObject
#' #Preprocessing parameters
#' prepro_params <- list(modeler_type = NULL, pretrained_min_max = NULL)
#'
#' #Define preproObject
#' prepro_object <- preproObject(preproFunction = minMaxScalerPreproFunction,
#'                               params = prepro_params)
#'
#' #Assign relevant metadata to preproObject
#' prepro_object <- assignExternalAttributes(modeler = modeler,
#'                                           object = prepro_object)
#'
#' #Call getPreproData to obtain preproObject with preprocessed data
#' prepro_object <- getPreproData(object = prepro_object, data = data.dt)
#'
#' #data is saved within prepro_object
#' head(prepro_object$data)
#'
#' @export
minMaxScalerPreproFunction <- function(object = NULL){

  #Extract necessary variables from object
  data <- object$data
  target <- object$target
  regressors <- object$regressors
  weightsColumn <- object$weightsColumn
  modeler_type <- object$params$modeler_type
  pretrained_min_max <- object$params$pretrained_min_max
  if(is.null(modeler_type)){
    modeler_type <- "simple"
  }

  dt <- as.data.table(data)

  #Get numeric variables
  num_vars <- colnames(dt)[sapply(dt, is.numeric)]

  #Define which columns should not be preprocessed
  if(any(target %in% colnames(data))){
    if(!is.null(weightsColumn)){
      if(weightsColumn %in% colnames(data)){
        no_preprocessing <- c(target, weightsColumn)
      }else{
        no_preprocessing <- c(target)
      }
    }else{
      no_preprocessing <- c(target)
    }

  }else{
    if(!is.null(weightsColumn)){
      if(weightsColumn %in% colnames(data)){
        no_preprocessing <- c(weightsColumn)
      }else{
        no_preprocessing <- c()
      }
    }else{
      no_preprocessing <- c()
    }
  }

  #Remove numeric variables that are in no_preprocessing
  #(i.e. weightsColumn and target)
  num_vars <- num_vars[!num_vars %in% no_preprocessing]

  #Calculate min and max for each numeric variable
  #(this will be saved in params for preprocessing future datasets)
  if(is.null(pretrained_min_max)){
    pretrained_min_max <- lapply(num_vars, function(var){
      return(list("min" = min(dt[[var]]),
                  "max" = max(dt[[var]])))
    })
    names(pretrained_min_max) <- num_vars

    #Save into object params
    object$params$pretrained_min_max <- pretrained_min_max
  }else{
    pretrained_min_max <- object$params$pretrained_min_max
  }

  #MinMax scale numeric variables (range from 0 to 1)
  for (var in num_vars){
    dt[, paste0("st_", var) := (get(var) - pretrained_min_max[[var]]$min) / (pretrained_min_max[[var]]$max - pretrained_min_max[[var]]$min)]
    dt[, (var) := NULL]
  }

  #Overwrite data (with preprocessed data)
  object$data <- dt

  #Define the name of the new regressors and add them to object
  prepro_regressors <- colnames(dt)
  prepro_regressors<- prepro_regressors[! prepro_regressors %in% no_preprocessing]
  switch (modeler_type,
    "simple" = {#For all other modelers
      object$new_regressors <- prepro_regressors
      },
    "classifRegModeler" = {
      object$new_regressors <- list(
                                  "regModeler" = prepro_regressors,
                                  "classifModeler" = prepro_regressors)
    },
    "prePredModeler" = {
      object$new_regressors <- list(
                                  "predModeler" = prepro_regressors,
                                  #Add target of predModeler to regressors
                                  "mainModeler" = c(target[1], prepro_regressors))
    },
    "benchModeler" = {
      object$new_regressors <- lapply(1:length(regressors), function(x){
                                  return(prepro_regressors)
                                })
    },
    {stop(paste0("The provided modeler_type parameter: ", modeler_type, " was not recognized. Available types are \n NULL, classifRegModeler, prePredModeler and benchRegressorModeler"))}
  )

  return(object)
}

# Cost function utilities ####

#' @title Get Variance Estimates of Bias Estimates of MCSubRB
#'
#' @param cSampler the conditioned sampler object
#' @param errorsKeys the keys of the s2 sample, this is necessary for the ssSampler case
#' @param errors the differences (e1) between the real and predicted values
#'
#' @return A list with estimates of the bias variance and of the squared bias variance.
#'
#' @seealso \code{\link{conditionedSampler}}
#' @seealso \code{\link{estimator}}
#' @seealso \code{\link{MCSubRBEstimate}}
#' @seealso \code{\link{MCSubRBDomainEstimate}}
#'
#' @import data.table
#'
#' @examples
#' #Generate data
#' set.seed(273)
#' data.dt <- generateStratifiedDomainModelAssistedEstimateData(n_samples = 1000, generate_random_weights = FALSE)
#'
#' #Get relevant variables from data
#' data.keys <- 1:nrow(data.dt)
#' target <- "target"# Regression
#' regressors <- setdiff(names(data.dt), target)
#'
#' #Define main sampler and sub-sampler
#' train_size = round(0.8 * nrow(data.dt))
#' main_sampler <- srsSampler(key = data.keys, ssize = train_size)
#' main_sample <- getSample(main_sampler)
#' sub_sampler <- srsSampler(key = main_sample, ssize = train_size*0.5)
#' sub_sample <- getSample(sub_sampler)
#'
#' #Define modeler
#' params <- list(
#'   "num.trees" = 50,
#'   "max.depth" = 20
#' )
#' modeler <- rangerRegressorModeler(params = params, target = target, regressors = regressors)
#'
#' #Train modeler with sub-sample
#' modeler <- train(modeler, data = data.dt, subSet = sub_sample)
#'
#' #Define conditionedSampler
#' cSampler <- conditionedSampler(object = main_sampler, sampleKeys = sub_sample)
#'
#' #Get model predictions over s2
#' errorsKeys <- setdiff(sub_sampler$key, sub_sample)
#'
#' preds <- getPredictions(object = modeler, data = data.dt, subSet = errorsKeys)
#'
#' y_true <- data.dt[errorsKeys, target]
#'
#' errors <- y_true - preds
#'
#' biasVarianceEstimate(cSampler = cSampler, errorsKeys = errorsKeys, errors = errors)
#'
#' @noRd
biasVarianceEstimate <- function(cSampler, errorsKeys, errors) {
  RMLutils:::.validate_biasVarianceEstimate(cSampler, errorsKeys, errors)
  UseMethod("biasVarianceEstimate")
}

#' @rdname biasVarianceEstimate
#' @noRd
biasVarianceEstimate.srsSampler <- function(cSampler, errorsKeys, errors) {
  N <- length(cSampler$key)
  n <- length(errorsKeys)
  S <- var(errors) # quasi variance of the errors
  vb <- (N - n) ^ 2 * (1 - (n / N)) * (S / n)
  vb2 <- (1 - (n / N)) * n * S
  bias <- (N / n - 1) * sum(errors)
  output <- list(bias = bias, vb = vb, vb2 = vb2)
  return(output)
}

#' @rdname biasVarianceEstimate
#' @noRd
biasVarianceEstimate.ssSampler <- function(cSampler, errorsKeys, errors) {
  if (is.null(dim(errors)))
    dim(errors) <- c(length(errors), 1)
  strata <- table(cSampler$strata)

  auxDT <- data.table(okeys = c(errorsKeys), errors)
  auxDT[, ostrata := cSampler$strata[match(okeys, cSampler$key)]]
  auxDT[, ostrataN := as.numeric(strata[ostrata])]
  auxDT[, okeys := NULL]

  vbs <- auxDT[, lapply(.SD, function(x) (ostrataN - .N) ^ 2 *
                          (1 - (.N / ostrataN)) * (var(x) / .N)),
               by = c("ostrata", "ostrataN")][,-"ostrataN"]
  vbs <- as.matrix(vbs[, -"ostrata", with = F])
  vb <- sum(vbs)

  vb2s <- auxDT[, lapply(.SD, function(x) ((ostrataN - .N) / ostrataN) *
                           .N * var(x)),
                by = c("ostrata", "ostrataN")][,-"ostrataN"]
  vb2s <- as.matrix(vb2s[, -"ostrata"])
  vb2 <- sum(vb2s)

  bias <- auxDT[, lapply(.SD, function(x) (ostrataN / .N - 1)*sum(x)),
                by = c("ostrata", "ostrataN")][, -"ostrataN"]
  bias <- as.matrix(bias[, -"ostrata"])
  bias <- sum(bias)

  output <- list(bias = bias, vb = vb, vb2 = vb2)
  return(output)
}

# Memory modeler utilities ####

#' @title add new data into memoryModeler
#'
#' @description Add new data to memoryModeler and remove oldest data if necessary
#'
#' @param object \code{\link{modeler}} object to fit to the data.
#' @param data Dataframe containing the data to use for the training
#'
#' @return memoryModeler object with new data loaded into list of data sets.
#'
#' @seealso \code{\link{memoryRegressorModeler}}
#' @seealso \code{\link{cleanMemory}}
#' @seealso \code{\link{updateMemory}}
#'
#' @examples
#' #Generate data
#' set.seed(273)
#' data.dt <- generateStratifiedDomainModelAssistedEstimateData(n_samples = 1000, generate_random_weights = FALSE)
#' data2.dt <- generateStratifiedDomainModelAssistedEstimateData(n_samples = 100, generate_random_weights = FALSE)
#' data3.dt <- generateStratifiedDomainModelAssistedEstimateData(n_samples = 50, generate_random_weights = FALSE)
#'
#' #Define train/test partitions
#' data.keys <- 1:nrow(data.dt)
#' train_size = round(0.8 * nrow(data.dt))
#' smplr <- srsSampler(key = data.keys, ssize = train_size)
#' train.keys <- getSample(smplr)
#' test.keys <- setdiff(data.keys, train.keys)
#'
#' #Define base modeler
#' target <- "target"# Regression
#' regressors <- setdiff(names(data.dt), target)
#' params <- list(
#'   "num.trees" = 50,
#'   "max.depth" = 20
#' )
#' base_modeler <- rangerRegressorModeler(params = params,
#'                                        target = target,
#'                                        regressors = regressors)
#'
#' #Instantiate memoryModeler
#' #memoryModeler parameters
#' data <- list(data2.dt)
#' maxData <- 3L
#' modeler <- memoryRegressorModeler(data = data,
#'                                   modeler = base_modeler,
#'                                   maxData = maxData)
#'
#' #Add new data to modeler
#' length(modeler$data)
#' modeler <- addMemory(object = modeler,
#' data = data3.dt)
#' length(modeler$data)
#'
#' #If data list exceeds maxData length, the oldest dataset is deleted
#' modeler <- addMemory(object = modeler,
#' data = data3.dt)
#' modeler <- addMemory(object = modeler,
#' data = data3.dt)
#'
#' length(modeler$data)
#'
#' @export
addMemory <- function(object, data) {
  UseMethod("addMemory")
}

#' @rdname addMemory
#' @export
addMemory.memoryModeler <- function(object, data){
  if(!"data.frame" %in% class(data)){
    stop(paste0("data must be a data.frame or data.table, but got a ", paste0(class(data), collapse = " ")))
  }
  if(class(object)[2] != "memoryModeler"){
    stop(paste0("object must be a memoryModeler, but got a ", paste0(class(object), collapse = " "), " instead."))
  }
  object$data <- append(list(data), object$data)
  if (length(object$data) > object$maxData){
    object$data <- object$data[-length(object$data)]
  }
  return(object)
}

#' @title clean data from memoryModeler
#'
#' @description clean a certain number of data sets from memoryModeler. By default all data sets are removed
#'
#' @param object \code{\link{modeler}} object to fit to the data.
#' @param index vector containing the positions of the data sets to be removed.
#'
#' @return memoryModeler object without all or some data sets.
#'
#' @seealso \code{\link{memoryRegressorModeler}}
#' @seealso \code{\link{addMemory}}
#' @seealso \code{\link{updateMemory}}
#'
#' @examples
#' #Generate data
#' set.seed(273)
#' data.dt <- generateStratifiedDomainModelAssistedEstimateData(n_samples = 1000, generate_random_weights = FALSE)
#' data2.dt <- generateStratifiedDomainModelAssistedEstimateData(n_samples = 100, generate_random_weights = FALSE)
#' data3.dt <- generateStratifiedDomainModelAssistedEstimateData(n_samples = 50, generate_random_weights = FALSE)
#'
#' #Define train/test partitions
#' data.keys <- 1:nrow(data.dt)
#' train_size = round(0.8 * nrow(data.dt))
#' smplr <- srsSampler(key = data.keys, ssize = train_size)
#' train.keys <- getSample(smplr)
#' test.keys <- setdiff(data.keys, train.keys)
#'
#' #Define base modeler
#' target <- "target"# Regression
#' regressors <- setdiff(names(data.dt), target)
#' params <- list(
#'   "num.trees" = 50,
#'   "max.depth" = 20
#' )
#' base_modeler <- rangerRegressorModeler(params = params,
#'                                        target = target,
#'                                        regressors = regressors)
#'
#' #Instantiate memoryModeler
#' #memoryModeler parameters
#' data <- list(data2.dt)
#' maxData <- 3L
#' modeler <- memoryRegressorModeler(data = data,
#'                                   modeler = base_modeler,
#'                                   maxData = maxData)
#' #cleanMemory can delete all data (if index is left empty) or any specific dataset
#' #Let us delete all data in this example
#' length(modeler$data)
#' modeler <- cleanMemory(object = modeler)
#' length(modeler$data)
#'
#' @export
cleanMemory <- function(object, index = 1:object$maxData) {
  UseMethod("cleanMemory")
}

#' @rdname cleanMemory
#' @export
cleanMemory.memoryModeler <- function(object, index = 1:object$maxData){
  if(class(object)[2] != "memoryModeler"){
    stop(paste0("object must be a memoryModeler, but got a ", paste0(class(object), collapse = " "), " instead."))
  }
  object$data <- object$data[-index]
  return(object)
}

#' @title update new data into memoryModeler
#'
#' @description update new data to memoryModeler into the specified position, substituting the original data
#'
#' @param object \code{\link{modeler}} object to fit to the data.
#' @param data Dataframe containing the data to use for the training
#' @param index Optional value, select the position of the data to be updated.
#' By default, index = 1 (newest data)
#'
#'
#' @return memoryModeler object with updated data.
#'
#' @seealso \code{\link{memoryRegressorModeler}}
#' @seealso \code{\link{addMemory}}
#' @seealso \code{\link{cleanMemory}}
#'
#' @examples
#' #Generate data
#' set.seed(273)
#' data.dt <- generateStratifiedDomainModelAssistedEstimateData(n_samples = 1000, generate_random_weights = FALSE)
#' data2.dt <- generateStratifiedDomainModelAssistedEstimateData(n_samples = 100, generate_random_weights = FALSE)
#' data3.dt <- generateStratifiedDomainModelAssistedEstimateData(n_samples = 50, generate_random_weights = FALSE)
#'
#' #Define train/test partitions
#' data.keys <- 1:nrow(data.dt)
#' train_size = round(0.8 * nrow(data.dt))
#' smplr <- srsSampler(key = data.keys, ssize = train_size)
#' train.keys <- getSample(smplr)
#' test.keys <- setdiff(data.keys, train.keys)
#'
#' #Define base modeler
#' target <- "target"# Regression
#' regressors <- setdiff(names(data.dt), target)
#' params <- list(
#'   "num.trees" = 50,
#'   "max.depth" = 20
#' )
#' base_modeler <- rangerRegressorModeler(params = params,
#'                                        target = target,
#'                                        regressors = regressors)
#'
#' #Instantiate memoryModeler
#' #memoryModeler parameters
#' data <- list(data2.dt)
#' maxData <- 3L
#' modeler <- memoryRegressorModeler(data = data,
#'                                   modeler = base_modeler,
#'                                   maxData = maxData)
#' #Update stored data
#' length(modeler$data)
#' modeler <- updateMemory(object = modeler,
#'                         data = data3.dt,
#'                         index = 1)
#' length(modeler$data)#Length has not changed
#'
#' @export
updateMemory <- function(object, data, index = 1) {
  UseMethod("updateMemory")
}

#' @rdname updateMemory
#' @export
updateMemory.memoryModeler <- function(object, data, index = 1){
  if(!"data.frame" %in% class(data)){
    stop(paste0("data must be a data.frame or data.table, but got a ", paste0(class(data), collapse = " ")))
  }
  if(class(object)[2] != "memoryModeler"){
    stop(paste0("object must be a memoryModeler, but got a ", paste0(class(object), collapse = " "), " instead."))
  }
  if(!(index <= object$maxData) & index > 0){
    stop(paste0("index must be at most, like maxData: ", object$maxData, " but got: ", index, " instead."))
  }
  object$data[[index]] <- data

  return(object)
}
