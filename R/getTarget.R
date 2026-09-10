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

#' @title Returns target variables
#'
#' @description Returns a vector of targets variables from a modeler.
#' Note that modelers with intermediate target variables such as
#' \code{\link{prePredRegressorModeler}}and
#' \code{\link{classifRegRegressorModeler}} output their main target variable.
#'
#' On the other hand, benchRegressorModeler returns a vector of targets (with
#' each component being the main target of its internal modelers).
#'
#' @param object \code{\link{modeler}} object.
#'
#' @return The target attribute of the modeler object.
#'
#' @examples
#' mdl <- H2ORegressorModeler("glm", list(lambda=0.0),"d" , c("a", "b", "c"))
#' getTarget(mdl)
#'
#' @export
getTarget <- function(object) {
  UseMethod("getTarget")
}
#' @rdname getTarget
#' @export
getTarget.benchModeler <- function(object) {
  return(sapply(object$submodels, getTarget))
}
#' @rdname getTarget
#' @export
getTarget.default <- function(object) {
  return(object$target)
}
#' @rdname getTarget
#' @export
getTarget.prePredModeler <- function(object) {
  return(RMLutils::getTarget(object$mainModeler))
}
