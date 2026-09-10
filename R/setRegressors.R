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

#' @title Sets regressors attribute in a modeler
#'
#' @description Sets regressors into a modeler.
#'
#' @param object A modeler object.
#' @param newRegressors String vector or list of string vectors.
#'
#' @return A modeler object with its regressors attribute changed.
#'
#' @examples
#' mdl <- H2ORegressorModeler("glm", list(lambda=0.0),"d" , c("a", "b", "c"))
#' mdl <- setRegressors(mdl, c("e", "f", "g"))
#' getRegressors(mdl)
#'
#' @export
setRegressors <- function(object, newRegressors) {
  UseMethod("setRegressors")
}

#' @rdname setRegressors
#' @export
setRegressors.default <- function(object, newRegressors){
  stop("This object does not have an implemented setRegressors method.")
}

#' @rdname setRegressors
#' @export
setRegressors.modeler <- function(object, newRegressors){
  if(class(newRegressors) != "character"){
    stop(paste0("newRegressors must be a character vector, got a ", class(newRegressors), " instead."))
  }
  object$regressors <- newRegressors
  return(object)
}

#' @rdname setRegressors
#' @export
setRegressors.preproModeler <- function(object, newRegressors){

  object$regressors <- newRegressors
  object$modeler <- setRegressors(object$modeler, newRegressors)

  #Fix preproObject, as regressors are still the previous ones
  object$preproObject$regressors <- newRegressors
  return(object)
}

#' @rdname setRegressors
#' @export
setRegressors.classifRegModeler <- function(object, newRegressors){
  if(class(newRegressors) != "list"){
    stop(paste0("newRegressors must be a list, got a ", class(newRegressors), " instead."))
  }
  if(length(newRegressors) != 2){
    stop(paste0("newRegressors must be a named list of length 2 (one component for classifModeler and another one for regModeler), but its length was: ", length(newRegressors)))
  }
  if(!any(c("regModeler", "classifModeler") %in% names(newRegressors))){
    stop(paste0("newRegressors must be a named list, with its names matching the internal modeler names (regModeler and classifModeler), but the provided list names were: ", names(newRegressors)))
  }
  object$regModeler <- setRegressors(object$regModeler, newRegressors$regModeler)
  object$classifModeler <- setRegressors(object$classifModeler, newRegressors$classifModeler)
  return(object)
}

#' @rdname setRegressors
#' @export
setRegressors.memoryRegressorModeler <- function(object, newRegressors){

  object$modeler <- setRegressors(object$modeler, newRegressors)
  return(object)
}

#TODO: Review this implementation (maybe also use a named list for each of them?)
#' @rdname setRegressors
#' @export
setRegressors.prePredModeler <- function(object, newRegressors){
  if(class(newRegressors) != "list"){
    stop(paste0("newRegressors must be a list, got a ", class(newRegressors), " instead."))
  }
  if(length(newRegressors) != 2){
    stop(paste0("newRegressors must be a named list of length 2 (one component for predModeler and another one for mainModeler), but its length was: ", length(newRegressors)))
  }
  if(!any(c("predModeler", "mainModeler") %in% names(newRegressors))){
    stop(paste0("newRegressors must be a named list, with its names matching the internal modeler names (predModeler and mainModeler), but the provided list names were: ", names(newRegressors)))
  }
  #Assign regressors (mainModeler gets the predModeler target(s) as regressors as well)
  object$predModeler <- setRegressors(object$predModeler, newRegressors$predModeler)
  object$mainModeler <- setRegressors(object$mainModeler, unique(c(newRegressors$mainModeler, getTarget(object$predModeler))))

  #Validate that predModeler target is in mainModeler regressors
  object <- RMLutils:::.validate_prePredRegressorModeler(object)
  return(object)
}

#' @rdname setRegressors
#' @export
setRegressors.benchRegressorModeler <- function(object, newRegressors){
  if(class(newRegressors) != "list"){
    stop(paste0("newRegressors must be a list, got a ", class(newRegressors), " instead."))
  }
  if(length(newRegressors) != length(object$submodels)){
    stop(paste0("newRegressors must be a list of the same length as the submodels list ", length(object$submodels), " , but its length was: ", length(newRegressors)))
  }

  #Assume that newRegressors and submodels are in the same order, as submodels is an unnamed list
  for (i in 1:length(newRegressors)){
    object$submodels[[i]] <- setRegressors(object$submodels[[i]], newRegressors[[i]])
  }

  return(object)
}

