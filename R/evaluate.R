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

#' @name evaluate
#' @title Built-in modeler evaluation.
#'
#' @description Evaluates a trained modeler object (either a \code{\link{regressorModeler}} or a \code{\link{classifierModeler}}) over a provided dataset and cost function.
#'
#' @param object Modeler  object to predict with.
#' @param data Dataset to do the prediction with.
#' @param subSet List of indices to subset form the dataframe.
#' @param cost_function A costFunction object or a named list of cost functions,
#' for evaluating complex modelers, such as prePredModeler and classifRegModeler.
#' @param cSample An optional argument for compatibility in some specific cases.
#' Generally, the user should use subSet instead of cSample to define the data subset to evaluate.
#'
#' @return Performance metric for a given modeler, dataset and cost function.
#'
#' @examples
#' # Generate data
#' data.dt <- generateRegressionData(n_samples = 1000, generate_random_weights = FALSE)
#'
#' #Define train and test partitions
#' data.keys <- 1:nrow(data.dt)
#' train_size = round(0.8 * nrow(data.dt))
#'
#' #Split train/test ####
#' sampler <- srsSampler(data.keys, ssize = train_size)#Simple random sampler
#'
#' train.keys <- getSample(sampler)
#' test.keys <- setdiff(data.keys, train.keys)
#'
#' train.dt <- data.dt[train.keys]
#' test.dt <- data.dt[test.keys]
#'
#' #Define modeler parameters and modeler itself
#' target <- "regression_target"# Regression
#' regressors <- setdiff(names(data.dt), target)
#'
#' params <- list(
#'   "learning_rate" = 0.07,
#'   "nrounds" = 10,
#'   "objective" = "regression"
#' )
#'
#' regressor <- LGBRegressorModeler(params = params, target = target, regressors = regressors)
#' regressor <- train(regressor, data = train.dt)
#'
#' #Evaluate R^2 cost function over train and test
#' cf <- r2CostFunction()
#' evaluate(object = regressor, data = train.dt, cost_function = cf)
#' evaluate(object = regressor, data = test.dt, cost_function = cf)
#'
#' @export
evaluate <- function(object, data, cost_function = NULL, subSet = NULL, csample = NULL) {
  UseMethod("evaluate")
}

#' @rdname evaluate
#' @export
evaluate.modeler <- function(object, data, cost_function = NULL, subSet = NULL, csample = NULL){

  data <- RMLutils:::.validate_generic_data_subset(data = data,
                                        subSet = subSet)

  cost <- getCost(object = cost_function, data = data, modeler = object, fSample = 1:nrow(data), cSample = 1:nrow(data))
  targets <- getTarget(object)
  cost <- lapply(targets, function(x) cost[[x]][["cost"]])
  names(cost) <- targets
  return(cost)
}

#' @rdname evaluate
#' @export
evaluate.prePredModeler <- function(object, data, cost_function = NULL, subSet = NULL, csample = NULL){

  data <- RMLutils:::.validate_generic_data_subset(data = data,
                                        subSet = subSet)

  #Proceed differently depending on the cost_function being a list of cost_functions
  #or a single cost_function.
  if(any("list" == class(cost_function))){
    if(!any(c("predModeler", "mainModeler") %in% names(cost_function))){
      stop(paste0("cost_function must be a named list, with its names matching the internal modeler names (predModeler and mainModeler), but the provided list names were: ", names(cost_function)))
    }

    pre_ev <- evaluate(object = object$predModeler,
                         data = data,
                         cost_function = cost_function$predModeler,
                         subSet = NULL,
                         csample = csample)

    main_ev <- evaluate(object = object$mainModeler,
                          data = data,
                          cost_function = cost_function$mainModeler,
                          subSet = NULL,
                          csample = csample)
    cost <- list("predModeler" = pre_ev,
                   "mainModeler" = main_ev)
    return(cost)

  }else if(any("costFunction" == class(cost_function))){
    cost <- getCost(object = cost_function, data = data, modeler = object, fSample = 1:nrow(data), cSample = 1:nrow(data))
    targets <- getTarget(object)
    cost <- lapply(targets, function(x) cost[[x]][["cost"]])
    names(cost) <- targets
    return(cost)
  }else{
    stop(paste0("cost_function must be a costFunction or a named list of costFunctions, but got a ", class(cost_function), " instead."))
  }
}

#' @rdname evaluate
#' @export
evaluate.classifRegModeler <- function(object, data, cost_function = NULL, subSet = NULL, csample = NULL){

  data <- RMLutils:::.validate_generic_data_subset(data = data,
                                        subSet = subSet)

  #Proceed differently depending on the cost_function being a list of cost_functions
  #or a single cost_function.
  if(any("list" == class(cost_function))){
    if(!any(c("classifModeler", "regModeler") %in% names(cost_function))){
      stop(paste0("cost_function must be a named list, with its names matching the internal modeler names (classifModeler and regModeler), but the provided list names were: ", names(cost_function)))
    }

    #Map special classes and NOSP from target column
    data[, (paste0(getTarget(object), "CAT")) := factor(ifelse(get(getTarget(object)) == object$specialCats, as.character(get(getTarget(object))), "NOSP"), levels = object$classifModeler$class_names)]

    classif_ev <- evaluate(object = object$classifModeler,
                           data = data,
                           cost_function = cost_function$classifModeler,
                           subSet = NULL,
                           csample = csample)

    reg_ev <- evaluate(object = object$regModeler,
                       data = data,
                       cost_function = cost_function$regModeler,
                       subSet = NULL,
                       csample = csample)

    cost <- list("classifModeler" = classif_ev,
                 "regModeler" = reg_ev)
    return(cost)

  }else if(any("costFunction" == class(cost_function))){
    cost <- getCost(object = cost_function, data = data, modeler = object, fSample = 1:nrow(data), cSample = 1:nrow(data))
    targets <- getTarget(object)
    cost <- lapply(targets, function(x) cost[[x]][["cost"]])
    names(cost) <- targets
    return(cost)
  }else{
    stop(paste0("cost_function must be a costFunction or a named list of costFunctions, but got a ", class(cost_function), " instead."))
  }
}

#' @rdname evaluate
#' @export
evaluate.preproModeler <- function(object, data, cost_function = NULL, subSet = NULL, csample = NULL){
  #Preprocess data before evaluating internal modeler
  prepro_object <- getPreproData(object = object$preproObject, data = data)
  data <- prepro_object$data
  cost <- evaluate(object$modeler, data, cost_function, subSet, csample)
  return(cost)
}

#' @rdname evaluate
#' @export
evaluate.memoryModeler <- function(object, data, cost_function = NULL, subSet = NULL, csample = NULL){
  cost <- evaluate(object$modeler, data, cost_function, subSet, csample)
  return(cost)
}
