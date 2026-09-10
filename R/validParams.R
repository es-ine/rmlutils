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

# classMapping validation ####

#' @param object classMapping object
#' @noRd
.validate_maxScoreClass <- function(object){
  if(!object$ties.method %in% c("first", "random", "last")){
    stop(paste0("Provided ties.method parameter must be either first, random or last, but got: ", object$ties.method, " instead."))
  }
  return(object)
}

#' @param object classMapping object
#' @noRd
.validate_binaryThresholdClass <- function(object){
  if(object$threshold <= 0 | object$threshold >=1){
    stop("threshold must be a value between 0 and 1 (exclusive).")
  }
  if(!is.null(object$positive_class)){
    if(!"character" %in% class(object$positive_class)){
      stop(paste0("positive_class must be either NULL or a string indicating the column name of the positive class, but got a ", class(object$positive_class), " instead."))
    }
  }
  return(object)
}

# Estimator validation ####

#' @param object sampler object to sample from
#' @param sampleKeys vector of indices of the sample
#' @param sampleData Data sampled from the full population
#' @noRd
.validate_HTEstimate <- function(object, sampleKeys, sampleData){
  if(!"sampler" %in% class(object)){
    stop(paste0("object must be a sampler, but got a ", class(object), " instead."))
  }
}

#' @param object sampler object to sample from
#' @param sampleKeys vector of indices of the sample
#' @param sampleData Data sampled from the full population
#' @param domains List, matrix or data.table of domains in the sampled data
#' @noRd
.validate_HTDomainEstimate <- function(object, sampleKeys, sampleData, domains){
  RMLutils:::.validate_HTEstimate(object, sampleKeys, sampleData)
  #TODO: Validate domains?
}

#' @param data Dataset of information and variables of the sampled Data.
#' @param strata_colummn String indicating the column name that identifies strata.
#' @param weights_column String indicating the column name that contains sampling weights.
#' @noRd
.validate_REstimate <- function(data, strata_colummn, weights_column){
  #Check that strata_column and weights_column are provided
  if(!is.character(strata_colummn)){
    stop(paste0("strata_colummn must be a string, got ", class(strata_colummn), " instead."))
  }
  if(!is.character(weights_column)){
    stop(paste0("weights_column must be a string, got ", class(weights_column), " instead."))
  }
}

#' @param data Dataset of information and variables of the sampled Data.
#' @param strata_colummn String indicating the column name that identifies strata.
#' @param weights_column String indicating the column name that contains sampling weights.
#' @noRd
.validate_RDomainEstimate <- function(data, strata_colummn, domain_column, weights_column){
  RMLutils:::.validate_REstimate(data, strata_colummn, weights_column)
  #Check that domain_column is provided
  if(!is.character(domain_column)){
    stop(paste0("domain_column must be a string, got ", class(domain_column), " instead."))
  }
}

#' @noRd
.validate_MCSubRBEstimate <- function(auxModeler, data, mainSampler,
                                      subSampler, targetVar, ssNumber){
  if(!"regressorModeler" %in% class(auxModeler)){
    stop(paste0("auxModeler must be a regressorModeler, but got a ", class(auxModeler), " instead."))
  }
  if(!"sampler" %in% class(mainSampler)){
    stop(paste0("mainSampler must be a sampler, but got a ", class(mainSampler), " instead."))
  }
  if(!"sampler" %in% class(subSampler)){
    stop(paste0("subSampler must be a sampler, but got a ", class(subSampler), " instead."))
  }
}

#' @noRd
.validate_MCSubRBDomainEstimate <- function(auxModeler, data, mainSampler,
                                            subSampler, targetVar, domains,
                                            ssNumber){
  RMLutils:::.validate_MCSubRBEstimate(auxModeler, data, mainSampler,
                                       subSampler, targetVar, ssNumber)
  #TODO: Validate domains?
}

#' @noRd
.validate_biasVarianceEstimate <- function(cSampler, errorsKeys, errors){
  if(!"sampler" %in% class(cSampler)){
    stop(paste0("cSampler must be a sampler, but got a ", class(cSampler), " instead."))
  }
}

#' @noRd
.validate_predictionEstimate <- function(modeler, data, mainSampler,
                                         subSampler, ssNumber){
  if(!"regressorModeler" %in% class(modeler)){
    stop(paste0("modeler must be a regressorModeler, but got a ", class(modeler), " instead."))
  }
  if(!"sampler" %in% class(mainSampler)){
    stop(paste0("mainSampler must be a sampler, but got a ", class(mainSampler), " instead."))
  }
  if(!"sampler" %in% class(subSampler)){
    stop(paste0("subSampler must be a sampler, but got a ", class(subSampler), " instead."))
  }
}

#' @noRd
.validate_predictionDomainEstimate <- function(modeler, data, mainSampler,
                                               subSampler, domains, ssNumber){
  RMLutils:::.validate_predictionEstimate(modeler, data, mainSampler,
                                          subSampler, ssNumber)
  #Validate domains?
}

#' @noRd
.validate_preTrainedEstimate <- function(object, data){
  if(!"modeler" %in% class(object)){
    stop(paste0("object must be a modeler, but got a ", class(modeler), " instead."))
  }
  if(!object$isTrained){
    stop("Provided modeler is not trained. Please review your inputs.")
  }
}

# Common modeler validation ####

#' @title Validate target for H2OModelers and data
#' @noRd
.validate_target_H2OModeler <- function(object, data){

  if(!inherits(data, "H2OFrame")){
    .validate_target(object, data)
  }else{
    model_task <- class(object)[3]
    tgt <- getTarget(object)

    for(t in tgt){
      switch (model_task,
              "regressorModeler" = {
                if(h2o::h2o.getTypes(data[t])[[1]] == "real"){
                  stop(paste0("Target variable must be numeric (real), but its class is: ", h2o.getTypes(data[t])[[1]], "\n"))
                }
              },
              "classifierModeler" = {
                if(h2o::h2o.getTypes(data[t])[[1]] != "enum"){
                  stop(paste0("Target variable must be categorical (enum), but its class is: ", h2o.getTypes(data[t])[[1]], "\n"))
                }
              }
      )
    }
  }
}

#' @title Validate target for generic modelers and data
#' @noRd
.validate_target <- function(object, data){
  #Force conversion to data.table (with normal modelers that do not use H2O frames)
  data <- as.data.table(data)

  model_task <- class(object)[3]
  tgt <- getTarget(object)
  for(t in tgt){
    switch (model_task,
            "regressorModeler" = {
              if(!is.numeric(data[[t]])){
                stop(paste0("Target variable must be numeric, but its class is: ", class(data[[t]]), "\n"))
              }
            },
            "classifierModeler" = {
              if(!is.factor(data[[t]])){
                stop(paste0("Target variable must be categorical, but its class is: ", class(data[[t]]), "\n"))
              }
            }
    )
  }

}

#' @rdname .validate_target
#' @noRd
.validate_target.benchModeler <- function(object, data){
  #Force conversion to data.table (with normal modelers that do not use H2O frames)
  data <- as.data.table(data)

    model_task <- class(mdlr)[[3]]
    tgt <- getTarget(object)
    for(t in tgt){
      switch (model_task,
              "regressorModeler" = {
                if(!is.numeric(data[[t]])){
                  stop(paste0("Target variable must be numeric, but its class is: ", class(data[[t]]), "\n"))
                }
              },
              "classifierModeler" = {
                if(!is.factor(data[[t]])){
                  stop(paste0("Target variable must be categorical, but its class is: ", class(data[[t]]), "\n"))
                }
              }
      )
    }
}

#' @title Validate data and subset for H2OModelers
#' @noRd
.validate_H2O_data_subset <- function(data, subSet, target = NULL){
  if(!inherits(data, "H2OFrame")){
    return(RMLutils:::.validate_generic_data_subset(data, subSet, target))
  }else{
    if (is.null(subSet)){
      subSet <- 1:nrow(data)
    }else{
      if(!is.null(target)){#Only if necessary, generally for train(), but not for getPredictions()
        l1 <- length(subSet)
        #Filter out missing data from target variable
        subSet <- subSet[as.logical(!is.na(data[sort(subSet), target]))]
        l2 <- length(subSet)
        if(l2 < l1){
          warning("Missing data was found among the target variable, but was removed to output a proper result. Please, make sure to review your data if this was unexpected.")
        }
      }
    }
    data <- data[sort(subSet), ]
    return(data)
  }
}

#' @title Validate data and subset
#'
#' @param data dataset to check
#' @param subSet subSet to check and use for filtering the data
#' @param target optional string indicating the name of the target variable in
#' the data. Should be provided in train() but must be left as NULL in
#' getPredictions().
#' @noRd
.validate_generic_data_subset <- function(data, subSet, target = NULL){
  #Force conversion to data.table (with normal modelers that do not use H2O frames)
  data <- as.data.table(data)
  if (is.null(subSet)){
    subSet <- 1:nrow(data)
  }else{
    if(!is.null(target)){#Only if necessary, generally for train(), but not for getPredictions()
      l1 <- length(subSet)
      #Filter out missing data from target variable
      subSet <- subSet[as.logical(!is.na(data[subSet, c(target)]))]
      l2 <- length(subSet)
      if(l2 < l1){
        warning("Missing data was found among the target variable, but was removed to output a proper result. Please, make sure to review your data if this was unexpected.")
      }
    }
  }
  data <- data[subSet]
  return(data)
}

#' @title Validate trained model
#'
#' @description
#' A simple method to check whether a modeler has been trained or not.
#' Outputs an error if it was not trained.
#'
#' This function should be used at the beginning of any getPredictions method.
#'
#' @param modeler modeler to check if it has been trained
#'
#' @noRd
.validate_trained_modeler <- function(modeler){
  if(!("modeler" %in% class(modeler))){
    stop("validate_trained_modeler only accepts a modeler object as its argument.")
  }
  if(!modeler$isTrained){
    stop("Modeler is not trained.")
  }
}

#' @title Validate complex modeler internal modeler
#'
#' @description
#' A validation method to check that a memoryModeler is not a submodeler inside
#' any complex modeler.
#'
#' @param object Modeler object
#' @noRd
.validate_complex_modeler <- function(object){
  #Since object is a modeler, check wheter it is a memoryModeler or not.
  if(class(object)[2] == "memoryModeler"){
    stop("A memoryModeler, by design, can not be inside another modeler.")
  }
}

#' @title Validate save and load arguments
#'
#' @param path Path to directory where the modeler will be saved or loaded.
#' @param fileName Name for the file where the modeler will be saved.
#' @param action String indicating whether this method is being used for saving
#' or loading a model ("save" and "load" respectively).
#' @noRd
.validate_modeler_save_load_arguments <- function(path, fileName, action = "save"){
  if(class(path) != "character"){
    stop(paste0("path must be a character string, but got a ", class(path), " instead."))
  }
  if(class(fileName) != "character"){
    stop(paste0("fileName must be a character string, but got a ", class(fileName), " instead."))
  }

  if(endsWith(fileName, ".mlu") | action == "load"){#Allow older modelers (which do not end with .mlu) to be loaded
    file_path <- paste0(path, "/", fileName)
  }else{
    file_path <- paste0(path, "/", fileName, ".mlu")
  }
  return(file_path)
}

# Modeler constructor validation ####

#' @param object Modeler object
#' @noRd
.validate_common_modeler_args <- function(object){
  if(class(object$target) != "character"){
    stop(paste0("target parameter must be a character, but got a ",
                class(object$target), " instead."))
  }
  if(class(object$regressors) != "character"){
    stop(paste0("regressors parameter must be a character vector, but got a ",
                class(object$regressors), " instead."))
  }
  if(class(object$params) != "list"){
    stop(paste0("params parameter must be a list, but got a ",
                class(object$params), " instead."))
  }
}

#' @param object Modeler object
#' @noRd
.validate_direct_modeler_args <- function(object){
  if(class(object$target) != "character"){
    stop(paste0("target parameter must be a character, but got a ",
                class(object$target), " instead."))
  }
  if(class(object$protoTarget) != "character"){
    stop(paste0("protoTarget parameter must be a character, but got a ",
                class(object$protoTarget), " instead."))
  }
  if(class(object$scaleFactor) != "numeric"){
    stop(paste0("scaleFactor parameter must be a number, but got a ",
                class(object$scaleFactor), " instead."))
  }
  return(object)
}

#' @param object Modeler object
#' @noRd
.validate_HT_modeler_args <- function(object){
  if(class(object$target) != "character"){
    stop(paste0("target parameter must be a character, but got a ",
                class(object$target), " instead."))
  }
  if(class(object$weightsColumn) != "character"){
    stop(paste0("weightsColumn parameter must be a character, but got a ",
                class(object$weightsColumn), " instead."))
  }
  return(object)
}

#' @param object Modeler object
#' @noRd
.validate_strataHT_modeler_args <- function(object){
  if(class(object$target) != "character"){
    stop(paste0("target parameter must be a character, but got a ",
                class(object$target), " instead."))
  }
  if(class(object$weightsColumn) != "character"){
    stop(paste0("weightsColumn parameter must be a character, but got a ",
                class(object$weightsColumn), " instead."))
  }
  if(class(object$strata) != "character"){
    stop(paste0("strata parameter must be a character, but got a ",
                class(object$strata), " instead."))
  }
  return(object)
}

#' @param object Modeler object
#' @noRd
.validate_strataMean_modeler_args <- function(object){
  if(class(object$target) != "character"){
    stop(paste0("target parameter must be a character, but got a ",
                class(object$target), " instead."))
  }
  if(class(object$regressors) != "character"){
    stop(paste0("regressors parameter must be a character vector, but got a ",
                class(object$regressors), " instead."))
  }
  if(class(object$weightsColumn) != "character"){
    stop(paste0("weightsColumn parameter must be a character, but got a ",
                class(object$weightsColumn), " instead."))
  }
  return(object)
}

#' @param object Modeler object
#' @noRd
.validate_H2OModeler <- function(object){
  .validate_common_modeler_args(object)
  if(class(object)[3] == "regressorModeler"){
    available_models <- c("deeplearning", "gam", "glm", "randomForest", "rulefit", "xgboost")
  }else{#Classifier modeler
    available_models <- c("adaBoost", "decision_tree", "deeplearning", "gam", "glm", "naiveBayes", "psvm", "randomForest", "rulefit", "xgboost")
  }

  if(!object$model %in% available_models){
    stop(paste0("model parameter not found in available modelers\n",
                "Provided model: ", object$model, "\n",
                "Available models: ", paste0(available_models, collapse = ", ")))
  }
  return(object)
}

#' @param object Modeler object
#' @noRd
.validate_LGBModeler <- function(object){
  .validate_common_modeler_args(object)
  if (is.null(object$params$nrounds)){
    object$params$nrounds <- 100
    warning("Nrounds was not provided. Defaulting to 100")
  }
  #Throw error if it is a classifier modeler and its objective param was not set to binary or multiclass
  if(!(object$params$objective %in% c("binary", "multiclass")) & ("classifierModeler" %in% class(object))){
    stop(paste0("The objective parameter must either be binary or multiclass for LGB"))
  }
  #Also throw error if its objective is not regression when it is a regressorModeler
  if(!(object$params$objective %in% c("regression")) & ("regressorModeler" %in% class(object))){
    stop(paste0("The objective parameter must either be binary or multiclass for LGB"))
  }
  return(object)
}

#' @param object Modeler object
#' @noRd
.validate_rangerModeler <- function(object){
  .validate_common_modeler_args(object)
  return(object)
}

#' @param object Modeler object
#' @noRd
.validate_catboostModeler <- function(object){
  .validate_common_modeler_args(object)

  #Validate val_data
  if(!is.null(object$val_data)){
    if(!"data.frame" %in% class(object$val_data)){
      stop(paste0("The provided val_data must inherit from data.frame, but got a ",
                  paste0(class(val_data), collapse = " "), " instead."))
    }
  }

  if(class(object)[3] == "classifierModeler"){
    available_loss_functions <- c("Logloss","CrossEntropy","MultiClass","MultiClassOneVsAll")
    if(!object$params$loss_function %in% available_loss_functions){
      stop(paste0("The provided loss_function in params is not available for classification tasks\n",
                  "Provided loss_function: ",
                  object$params$loss_function, "\n",
                  "Available loss functions for classification: ", paste0(available_loss_functions, collapse = ", ")))
    }
    if(!is.null(object$positive_class)){
      if(class(object$positive_class) != "character"){
        stop(paste0("positive_class must be a character, but got a ",
                    class(object$positive_class), " instead."))
      }
    }
  }
  return(object)
}

#' @param object Modeler object
#' @noRd
.validate_miceModeler <- function(object){
  .validate_common_modeler_args(object)
  return(object)
}

#' @param object classifRegRegressorModeler object
#' @noRd
.validate_classifRegRegressorModeler <- function(object){

  #Validate modeler inputs
  #.validate_complex_modeler(object = object$classifModeler)#MemoryModeler can only be a regressorModeler (review this)
  .validate_complex_modeler(object = object$regModeler)
  if("benchModeler" %in% class(object$regModeler)){
    stop("A benchModeler can not be inside a classifReg modeler, please, change regModeler into another regressor modeler type.")
  }

  #Various sanity checks
  if(!"classifierModeler" %in% class(object$classifModeler)){
    stop("The classifModeler parameter must be a classifierModeler. Please review your inputs.")
  } else if(!"regressorModeler" %in% class(object$regModeler)){
    stop("The regModeler parameter must be a regressorModeler. Please review your inputs.")
  }else if (getTarget(object$classifModeler) != getTarget(object$regModeler)) {
    stop("Both modelers must have the same target variables")
  } else if (object$epsilon < 0) {
    stop("The tolerance parameter 'epsilon' must be non-negative. Please review your inputs.")
  } else if (length(object$specialCats) > 1) {
    if (object$epsilon >= round(min(apply(combn(x = object$specialCats, m = 2), 2, function(x){abs(x[1]-x[2])}))/2, digits = 15)) {
      stop("The tolerance parameter 'epsilon' must be smaller than half the minimum distance between special categories. Please review your inputs.")
    }
  }

  return(object)
}

#' @param object A BenchRegressorModeler
#' @noRd
.validate_benchRegressorModeler <- function(object){


  for(mdlr in object$submodels){
    .validate_complex_modeler(object = mdlr)
  }

  #Check that none of the submodel targets are in any of the
  #other submodeler regressors
  targets <- getTarget(object)
  unique_regressors <- unique(unlist(getRegressors(object)))
  if(any(targets %in% unique_regressors)){
    stop(paste0("One or more submodel targets were found in the submodel regressors:\n",
                "Submodeler targets: ",
                paste0(targets, collapse = " "),
                "\n",
                "Unique submodeler regressors: ",
                paste0(unique_regressors, collapse = " "),
                "\n",
                "Please review your inputs, as submodelers should not see other submodeler targets for training or predicting."))
  }


  return(object)
}

#' @param object preproModeler object
#' @noRd
.validate_preproModeler <- function(object){
  if(class(object)[3] == "classifierModeler"){
    if(!"classifierModeler" %in% class(object$modeler)){
      stop("The modeler parameter must be a classifierModeler. Please review your inputs.")
    }
  }else if(class(object)[3] == "regressorModeler"){
    if(!"regressorModeler" %in% class(object$modeler)[3]){
      stop("The modeler parameter must be a regressorModeler. Please review your inputs.")
    }
  }

  if(!"preproObject" %in% class(object$preproObject)){
    stop(paste0("The preproObject argument must be a preproObject, but got a ", class(object$preproObject), " instead."))
  }

  return(object)
}

#' @param object prePredRegressorModeler object
#' @noRd
.validate_prePredRegressorModeler <- function(object){
  .validate_complex_modeler(object$predModeler)
  .validate_complex_modeler(object$mainModeler)
  predTargets <- RMLutils::getTarget(object$predModeler)
  mainRegressors <- RMLutils::getRegressors(object$mainModeler)
  if(class(mainRegressors) == "list"){
    mainRegressors <- unlist(unique(mainRegressors))
  }
  if(!all(predTargets %in% mainRegressors)){
    stop("There are target variables in the prediction modeler that are not included as regressors in the main modeler. Please, review your inputs.")
  }
  return(object)
}

#' @param object memoryRegressorModeler object
#' @noRd
.validate_memoryRegressorModeler <- function(object){
  #Check that modeler is not another memoryModeler
  .validate_complex_modeler(object$modeler)
  if(!"regressorModeler" %in% class(object$modeler)){
    stop("modeler must be a regressorModeler. Please, review your inputs.")
  }
  if(object$maxData < 1){
    stop("maxData must be a positive number")
  }
  if(length(object$data) > object$maxData){
    stop("Number of data sets is greater than maximum length specified.")
  }
  return(object)
}

# Sampler costructor validation ####

#' @param object sampler object
#' @noRd
.validate_cvSampler <- function(object){
  if(object$folds < 2){
    stop("The folds parameter must be strictly larger than 1")
  }
  return(object)
}

# Utilities validation ####

#' @param object preproObject object
#' @noRd
.validate_preproObject <- function(object){
  if(class(object$params) != "list"){
    stop(paste0("params argument must be a list, but got a ", class(object$params), " instead."))
  }
  if(class(object$preproFunction) != "function"){
    stop(paste0("preproFunction argument must be a function, but got a ", class(object$preproFunction), " instead."))
  }
  #Confirm that preproFunction only accepts "object" as an argument:
  arguments <- names(formals(object$preproFunction))
  if(length(arguments) != 1){
    stop(paste0("The preproFunction can only have a single argument, but has a total of ", length(arguments), " arguments."))
  }
  if(arguments != "object"){
    stop(paste0("The preproFunction must have an argument named object, which must be a preproObject, but its argument was: ", arguments))
  }
  return(object)
}

#' @param rest character vector
#' @noRd
.validateRestrictions <- function(rest) {
  # Auxiliary functions
  is_number <- function(x) {
    return(is.numeric(x) ||
             (is.call(x) &&
                identical(x[[1]], as.name("-")) &&
                length(x) == 2 &&
                is.numeric(x[[2]])))
  }

  is_term <- function(x) {
    if (is.symbol(x))
      return(TRUE)

    if (!is.call(x))
      return(FALSE)

    if (identical(x[[1]], as.name("-")))
      return(length(x) == 2 && is.symbol(x[[2]]))

    identical(x[[1]], as.name("*")) &&
      length(x) == 3 &&
      is_number(x[[2]]) &&
      is.symbol(x[[3]])
  }

  is_linear_expr <- function(lhs) {
    # Base step
    if (is_term(lhs)) {
      return(TRUE)
    }
    # Recursive step
    if (!is.call(lhs) || length(lhs) != 3) {
      return(FALSE)
    }
    if (!(as.character(lhs[[1]]) %in% c("+", "-")) || !is_term(lhs[[3]])) {
      return(FALSE)
    }
    return(is_linear_expr(lhs[[2]]))
  }

  # Convert the string to an expression and check validity
  expr <- tryCatch(
    parse(text = rest),
    error = function(e) NULL
  )
  # Check there is exactly one expression
  if (is.null(expr) || length(expr) != 1) {
    return(FALSE)
  }
  # Check the expression has length 3
  if (length(expr[[1]]) != 3) {
    return(FALSE)
  }
  # Check it is equality or inequality
  if (!(as.character(expr[[1]][[1]]) %in% c("==", ">=", "<="))) {
    return(FALSE)
  }
  # Check rhs is a number
  if (!is_number(expr[[1]][[3]])) {
    return(FALSE)
  }
  # Check lhs is a linear combination of R identifiers
  return(is_linear_expr(expr[[1]][[2]]))
}




