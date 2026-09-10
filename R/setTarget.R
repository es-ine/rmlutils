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

#' @title Sets target attribute in a modeler
#'
#' @description Sets one or multiple target variables into a modeler.
#'
#' @param object A modeler object.
#' @param newTarget String or list with the new target name(s).
#'
#' @return A modeler object with its target attribute changed.
#'
#' @examples
#' mdl <- H2ORegressorModeler("glm", list(lambda=0.0),"d" , c("a", "b", "c"))
#' mdl <- setTarget(mdl, "e")
#' getTarget(mdl)
#'
#' @export
setTarget <- function(object, newTarget) {
  UseMethod("setTarget")
}

#' @rdname setTarget
#' @export
setTarget.default <- function(object, newTarget){
  stop("This object does not have an implemented setTarget method.")
}

#' @rdname setTarget
#' @export
setTarget.modeler <- function(object, newTarget){
  if(class(newTarget) != "character"){
    stop(paste0("newTarget must be a character, got a ", class(newTarget), " instead."))
  }
  object$target <- newTarget
  return(object)
}

#' @rdname setTarget
#' @export
setTarget.preproModeler <- function(object, newTarget){

  object$target <- newTarget
  object$modeler <- setTarget(object$modeler, newTarget)

  #Fix preproObject, as target is still the previous one
  object$preproObject$target <- newTarget
  return(object)
}

#' @rdname setTarget
#' @export
setTarget.classifRegModeler <- function(object, newTarget){

  object$target <- newTarget
  object$regModeler <- setTarget(object$regModeler, newTarget)
  object$classifModeler <- setTarget(object$classifModeler, newTarget)
  return(object)
}

#' @rdname setTarget
#' @export
setTarget.memoryRegressorModeler <- function(object, newTarget){

  object$modeler <- setTarget(object$modeler, newTarget)
  return(object)
}

#' @rdname setTarget
#' @export
setTarget.prePredModeler <- function(object, newTarget){

  #Assign target only to main modeler
  object$mainModeler <- setTarget(object$mainModeler, newTarget)

  return(object)
}

#' @rdname setTarget
#' @export
setTarget.benchRegressorModeler <- function(object, newTarget){
  if(length(newTarget) != length(object$submodels)){
    stop(paste0("newTarget must be a list of the same length as the submodels list ", length(object$submodels), " , but its length was: ", length(newTarget)))
  }
  if(class(newTarget) != "list"){
    stop(paste0("newTarget must be a list, got a ", class(newTarget), " instead."))
  }

  #Assume that newTarget and submodels are in the same order, as submodels is an unnamed list
  for (i in 1:length(newTarget)){
    object$submodels[[i]] <- setTarget(object$submodels[[i]], newTarget[[i]])
  }

  return(object)
}
