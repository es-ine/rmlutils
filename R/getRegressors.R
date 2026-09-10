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

#' @title Returns regressors
#'
#' @description Returns the regressors of a modeler object
#'
#' @param object  Modeler object
#'
#' @return A list with the regressors of the \code{\link{regressorModeler}} or \code{\link{classifierModeler}} object
#'
#' @examples
#' mdl <- H2OModeler("glm", list(lambda=0.0),"d" , c("a", "b", "c"))
#' getRegressors(mdl)
#'
#' @export
getRegressors <- function(object){
  UseMethod("getRegressors")
}

#' @rdname getRegressors
#' @export
getRegressors.default <- function(object){
  return(object$regressors)
}

#' @rdname getRegressors
#' @export
getRegressors.preproModeler <- function(object){
  return(getRegressors(object$modeler))
}

#' @rdname getRegressors
#' @export
getRegressors.classifRegModeler <- function(object){
  return(list("classifModeler" = getRegressors(object$classifModeler),
              "regModeler" = getRegressors(object$regModeler)))
}

#' @rdname getRegressors
#' @export
getRegressors.benchModeler <- function(object){
  return(lapply(object$submodels, getRegressors))
}


#' @rdname getRegressors
#' @export
getRegressors.prePredModeler <- function(object){
  return(list("predModeler" = getRegressors(object$predModeler),
              "mainModeler" = getRegressors(object$mainModeler)))
}

#' @rdname getRegressors
#' @export
getRegressors.memoryModeler <- function(object){
  return(getRegressors(object$modeler))
}
