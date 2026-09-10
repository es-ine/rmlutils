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

#' @title Get class mapping method
#'
#' @description Defines different ways of assigning a class (or group of classes) when given an array of scores/probabilities, outputted from a modeler object.
#' The user should take into account that while this method may be used to map prediction scores to classes, it is a better practice to use a higher level function such as \code{\link{getClasses}}.
#'
#' @param object A \code{\link{classMapping}} object
#' @param scores A named scores matrix, like the ones outputted via \code{\link{getPredictions}} by a \code{\link{classifierModeler}}.
#'
#' @return A vector or list of indices, indicating the classes that have been selected for each sample.
#'
#' @seealso \code{\link{classMapping}}
#' @seealso \code{\link{maxScoreClass}}
#' @seealso \code{\link{binaryThresholdClass}}
#' @seealso \code{\link{getClasses}}
#'
#' @examples
#' #Define data and model, then train it.
#' data.dt <- generateBinaryClassificationData(n_samples = 10000, generate_random_weights = FALSE)
#' target <- "classification_target"
#' regressors <- setdiff(names(data.dt), target)
#' params <- list(
#'   "learning_rate" = 0.07,
#'   "nrounds" = 10,
#'   "objective" = "multiclass",#Works better this way with RMLutils
#'   "num_class" = length(unique(data.dt[, classification_target]))
#' )
#'
#' classifier <- LGBClassifierModeler(params = params, target = target, regressors = regressors)
#' classifier <- train(classifier, data = data.dt)
#'
#' #Get prediction scores
#' pred_scores <- getPredictions(classifier, data)
#' head(pred_scores)
#'
#' #Define max score class mapping
#' cls_mapping <- maxScoreClass(ties.method = "first")
#'
#' #Get class indices given a class mapping
#' output <- getClassMapping(cls_mapping, pred_scores)
#' head(output)#As it can be seen, only the max score classes are selected
#'
#' #Define binary threshold class mapping
#' cls_mapping <- binaryThresholdClass(threshold = 0.78, positive_class = "Target_1")
#'
#' #Get class indices given a class mapping
#' output <- getClassMapping(cls_mapping, pred_scores)
#' #Since we demand for a very high threshold in order for Target_1 to be selected,
#' #in this case all mapped classes correspond to Target_2
#' head(output)
#'
#' @export
getClassMapping <- function(object, scores) {
  UseMethod("getClassMapping")
}

#' @rdname getClassMapping
#' @export
getClassMapping.maxScoreClass <- function(object, scores){
  # Returns indices of max scores in every row (1-based indexing in R)
  max_indices <- max.col(scores, ties.method = object$ties.method)
  return(max_indices)
}

#' @rdname getClassMapping
#' @export
getClassMapping.binaryThresholdClass <- function(object, scores){
  if(!(dim(scores)[2] == 2)){
    stop("The binaryThresholdClass mapping requires the score matrix to have two columns.")
  }
  positive_class <- if (!is.null(object$positive_class)) object$positive_class else colnames(scores)[2]
  #Get index of positive and negative classes:
  pos_idx <- which(colnames(scores) == positive_class)
  neg_idx <- which(colnames(scores) != positive_class)
  # Returns a list of indices using vectorized operations
  positive_scores <- scores[, positive_class]
  result <- integer(nrow(scores))
  result[positive_scores > object$threshold] <- pos_idx
  result[positive_scores <= object$threshold] <- neg_idx

  return(result)
}
