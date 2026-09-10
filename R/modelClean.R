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

#' @title Cleans a modeler object.
#'
#' @description Removes any fit model from the modeler.
#'
#' @param object Modeler to clean.
#'
#' @return The baseline model.
#'
#' @seealso \code{\link{regressorModeler}}
#' @seealso \code{\link{classifierModeler}}
#' @seealso \code{\link{modelSave}}
#' @seealso \code{\link{modelLoad}}
#'
#' @examples
#' #Generate data
#' data.dt <- generateRegressionData(n_samples = 1000,
#'                                   generate_random_weights = FALSE)
#'
#' #Define train/test partitions
#' set.seed(273)
#' data.keys <- 1:nrow(data.dt)
#' train_size = round(0.8 * nrow(data.dt))
#' smplr <- srsSampler(key = data.keys, ssize = train_size)
#' train.keys <- getSample(smplr)
#' test.keys <- setdiff(data.keys, train.keys)
#'
#' #Define modeler
#' params <- list(
#'   "num.trees" = 50,
#'   "max.depth" = 20
#' )
#' target <- "regression_target"
#' regressors <- setdiff(colnames(data.dt), target)
#'
#' modeler <- rangerRegressorModeler(params = params, target = target, regressors = regressors)
#'
#' #Train, predict and evaluate
#' modeler <- train(object = modeler, data = data.dt, subSet = train.keys)
#'
#' print(modeler$isTrained)#Model is now trained
#'
#' modeler <- modelClean(modeler)
#'
#' print(modeler$isTrained)#Model is now clean
#'
#' @export
modelClean <- function(object) {
  UseMethod("modelClean")
}

#' @rdname modelClean
#' @export
modelClean.default <- function(object){
  stop("This object does not have an implemented modelClean function.")
}

#' @rdname modelClean
#' @export
modelClean.modeler <- function(object){
  if (!is.null(object$trainedModel)){
    object$trainedModel <- NULL
    #Set boolean trained flag
    object$isTrained <- FALSE
  }
  return(object)
}

#' @rdname modelClean
#' @export
modelClean.directRegressorModeler <- function(object){
  warning("The directModeler does not need to be trained by default. modelClean will do nothing.")
  return(object)
}

#' @rdname modelClean
#' @export
modelClean.H2OModeler <- function(object) {
  if (!is.null(object$trainedModel)){
    h2o::h2o.rm(object$trainedModel, cascade = TRUE)
    object$trainedModel <- NULL
    #Set boolean trained flag
    object$isTrained <- FALSE
  }
  return(object)
}

#' @rdname modelClean
#' @export
modelClean.preproModeler <- function(object){

  object$modeler <- modelClean(object$modeler)
  #Set boolean trained flag
  object$isTrained <- FALSE

  return(object)
}

#' @rdname modelClean
#' @export
modelClean.classifRegModeler <- function(object) {
  object$classifModeler <- modelClean(object$classifModeler)
  object$classifModeler$target <- object$regModeler$target
  object$regModeler <- modelClean(object$regModeler)
  object$trainedModel <- NULL
  #Set boolean trained flag
  object$isTrained <- FALSE
  return(object)
}

#' @rdname modelClean
#' @export
modelClean.prePredModeler <- function(object) {
  object$predModeler <- modelClean(object$predModeler)
  object$mainModeler <- modelClean(object$mainModeler)
  #Set boolean trained flag
  object$isTrained <- FALSE
  return(object)
}

#' @rdname modelClean
#' @export
modelClean.memoryModeler <- function(object){

  object$modeler <- modelClean(object$modeler)
  #Set boolean trained flag
  object$isTrained <- FALSE

  return(object)
}

#' @rdname modelClean
#' @export
modelClean.benchModeler <- function(object){

  for(i in 1:length(object$submodels)){
    object$submodels[[i]] <- modelClean(object$submodels[[i]])
  }
  #Set boolean trained flag
  object$isTrained <- FALSE

  return(object)
}
