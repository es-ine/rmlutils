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

#' @title Returns weightsColumn
#'
#' @description Returns a string or a vector of weightsColumn attributes from a modeler.
#' Generally, it will return a single string (or a NULL value), but since some
#' complex modelers contain multiple modelers internally
#' (which may have a different weights column each), a vector would be returned
#' in that case.
#'
#' Note that if some of the multiple weightColumns match, only unique values
#' would be returned.
#'
#' Besides other getters such as getTarget and getRegressors, which have a
#' paired setter method, this one does not have it, because it is not a good
#' practice to change a weightsColumn attribute after instantiating a modeler.
#'
#' @param object A modeler object.
#'
#' @return The weightsColumn attribute (or attributes) of the modeler object.
#'
#' @examples
#' modeler <- HTRegressorModeler(target = "income",
#'                               weightsColumn = "weights")
#' getWeightsColumn(modeler)
#'
#' @export
getWeightsColumn <- function(object) {
  UseMethod("getWeightsColumn")
}

#' @rdname getWeightsColumn
#' @export
getWeightsColumn.default <- function(object){
  return(object$weightsColumn)
}

#' @rdname getWeightsColumn
#' @export
getWeightsColumn.preproModeler <- function(object){
  return(getWeightsColumn(object$modeler))
}

#' @rdname getWeightsColumn
#' @export
getWeightsColumn.classifRegModeler <- function(object){
  return(unique(c(getWeightsColumn(object$classifModeler), getWeightsColumn(object$regModeler))))
}

#' @rdname getWeightsColumn
#' @export
getWeightsColumn.prePredRegressorModeler <- function(object){
  return(unique(c(getWeightsColumn(object$classifModeler), getWeightsColumn(object$regModeler))))
}

#' @rdname getWeightsColumn
#' @export
getWeightsColumn.memoryRegressorModeler <- function(object){
  return(getWeightsColumn(object$modeler))
}

#' @rdname getWeightsColumn
#' @export
getWeightsColumn.benchModeler <- function(object) {
  return(unlist(unique(sapply(object$submodels, getWeightsColumn))))
}
