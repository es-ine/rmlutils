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

#' @title Get class predictions with a classifierModeler object
#'
#' @description Computes class predictions from a trained \code{\link{classifierModeler}} object and a frame of (new)
#' regressors.
#'
#' @param object \code{\link{classifierModeler}} object to predict with.
#' @param data Dataset to do the prediction with.
#' @param subSet List of indices to subset form the dataframe.
#' @param class_mapping A \code{\link{classMapping}} object, indicating how to map scores to classes
#'
#' @return A vector of class predictions
#'
#' @seealso \code{\link{classMapping}}
#' @seealso \code{\link{maxScoreClass}}
#' @seealso \code{\link{binaryThresholdClass}}
#' @seealso \code{\link{getClassMapping}}
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
#' #Define max score class mapping
#' cls_mapping_1 <- maxScoreClass(ties.method = "first")
#'
#' #Get classes given a class mapping
#' pred_classes1 <- getClasses(object = classifier, data = data.dt, class_mapping = cls_mapping_1)
#' head(pred_classes1)
#'
#' #Define binary threshold class mapping
#' cls_mapping_2 <- binaryThresholdClass(threshold = 0.5, positive_class = "Target_1")
#'
#' #Get classes given a class mapping
#' pred_classes2 <- getClasses(object = classifier, data = data.dt, class_mapping = cls_mapping_2)
#' head(pred_classes2)
#'
#' @export
getClasses <- function(object, data, class_mapping = maxScoreClass(), subSet = NULL) {
  UseMethod("getClasses")
}

#' @rdname getClasses
#' @export
getClasses.default <- function(object, data, class_mapping = maxScoreClass(), subSet = NULL){
  stop("This modeler does not have a getClasses attribute. Please make sure that you are using a classifierModeler object.")
}



#' @rdname getClasses
#' @export
getClasses.classifierModeler <- function(object, data, class_mapping = maxScoreClass(), subSet = NULL){
  if (is.null(subSet)) subSet <- 1:nrow(data)

  pred_scores <- getPredictions(object, data, subSet)
  output <- getClassMapping(class_mapping, pred_scores)
  #Handle the output depending on the class_mapping argument
  if (class(class_mapping)[1] == "maxScoreClass"){
    output <- factor(colnames(pred_scores)[output], levels = object$class_names)
    output <- data.table(output)
    setnames(output, new = object$target)
  }else if(class(class_mapping)[1] == "binaryThresholdClass"){
    output <- factor(colnames(pred_scores)[output], levels = object$class_names)
    #output <- unlist(lapply(output, function(out){
    #  return(factor(colnames(pred_scores)[out], levels = object$class_names))
    #}))
    output <- data.table(output)
    setnames(output, new = object$target)
  }else{
    stop("The only class mapping methods that work with getClasses currently are maxScoreClass and binaryThresholdClass.")
  }
  return(output)
}

#' @rdname getClasses
#' @export
getClasses.preproClassifierModeler <- function(object, data, class_mapping = maxScoreClass(), subSet = NULL){
  if (is.null(subSet)) subSet <- 1:nrow(data)

  pred_scores <- getPredictions(object, data, subSet)
  output <- getClassMapping(class_mapping, pred_scores)
  #Handle the output depending on the class_mapping argument
  if (class(class_mapping)[1] == "maxScoreClass"){
    output <- factor(colnames(pred_scores)[output], levels = object$modeler$class_names)
    output <- data.table(output)
    setnames(output, new = object$target)
  }else if(class(class_mapping)[1] == "binaryThresholdClass"){
    output <- unlist(lapply(output, function(out){
      return(factor(colnames(pred_scores)[out], levels = object$modeler$class_names))
    }))
    output <- data.table(output)
    setnames(output, new = object$target)
  }else{
    stop("The only class mapping methods that work with getClasses currently are maxScoreClass and binaryThresholdClass.")
  }
  return(output)
}
