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

#' @title Class mapping from model scores
#'
#' @description A collection of methods that map a set of \code{\link{classifierModeler}} prediction scores to a set of classes.
#'
#' @name classMapping
#' @section Current implementations:
#'
#' \itemize{
#' \item \code{\link{maxScoreClass}}
#' \item \code{\link{binaryThresholdClass}}
#'}
#'
NULL

#' @title Maximum score class mapping.
#'
#' @description Implementation of maximum score mapping. This serves as the default choice unless specified otherwise, by the user.
#' When given a matrix of scores for a set of samples, sending this object to the \code{\link{getClassMapping}} function will output an array indicating the position of the class with the highest score, for each sample.
#'
#' @rdname maxScoreClass
#' @param ties.method A string indicating the ties.method to use with max.col. Set to "first" by default. Additional options include "random" and "last".
#' @return A classMapping object
#'
#' @seealso \code{\link{getClassMapping}}
#' @seealso \code{\link{getClasses}}
#' @seealso \code{\link{binaryThresholdClass}}
#'
#' @examples
#'
#' #Define class mapping (check getClassMapping and getClasses documentation to see a more complete example)
#' cls_mapping <- maxScoreClass(ties.method = "first")
#'
#' @export
maxScoreClass <- function(ties.method = "first"){
  object <- list(ties.method = ties.method)
  class(object) <- c("maxScoreClass", "classMapping")
  object <- RMLutils:::.validate_maxScoreClass(object)
  return(object)
}

#' @title Binary threshold class mapping.
#'
#' @description Implementation of a binary threshold class mapping algorithm.
#' When given a score matrix with two columns (a positive and a negative class), the \code{\link{getClassMapping}} function will output an index array indicating which class was chosen.
#' The expected use of this mapping involves situations in which we require a higher (or lower) score threshold for the positive class to be chosen, rather than simply being larger than 0.5.
#'
#' @rdname binaryThresholdClass
#' @param threshold A threshold used to pick the positive class (if it's below that score, the negative class is returned instead).
#' @param positive_class The column name of the positive class.
#' @return A classMapping object
#'
#' @seealso \code{\link{getClassMapping}}
#' @seealso \code{\link{getClasses}}
#' @seealso \code{\link{maxScoreClass}}
#'
#' @examples
#'
#' #Define class mapping (check getClassMapping and getClasses documentation to see a more complete example)
#' cls_mapping <- binaryThresholdClass(threshold = 0.6, positive_class = NULL)
#'
#'
#' @export
binaryThresholdClass <- function(threshold = 0.5, positive_class = NULL){

  object <- list(threshold = threshold, positive_class = positive_class)
  class(object) <- c("binaryThresholdClass", "classMapping")
  object <- RMLutils:::.validate_binaryThresholdClass(object)
  return(object)
}
