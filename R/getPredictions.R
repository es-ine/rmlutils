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

#' @title Predicts with a modeler object
#'
#' @description Computes predictions from a trained \code{\link{modeler}} object and a frame of (new)
#' regressors.
#'
#' @param object Model object to predict with.
#' @param data Dataset to do the prediction with.
#' @param subSet List of indices to subset form the dataframe.
#'
#' @return A vector of predictions
#'
#' @import data.table
#'
#' @examples
#' #Generate data
#' data.dt <- generateRegressionData(n_samples = 1000, generate_random_weights = FALSE)
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
#' preds <- getPredictions(modeler, data = data.dt, subSet = train.keys)
#'
#' @export
getPredictions <- function(object, data, subSet = NULL) {
  UseMethod("getPredictions")
}
#' @rdname getPredictions
#' @export
getPredictions.directModeler <- function(object, data, subSet = NULL) {
  data <- RMLutils:::.validate_generic_data_subset(data = data,
                                        subSet = subSet)
  output <- as.matrix(data[, get(object$protoTarget)])[,1, drop=FALSE]
  colnames(output) <- object$target
  output <- object$scaleFactor * output
  output[is.na(output)] <- 0
  return(output)
}
#' @rdname getPredictions
#' @export
getPredictions.HTModeler <- function(object, data, subSet = NULL) {
  RMLutils:::.validate_trained_modeler(object)
  weightsColumn <- object$weightsColumn
  #Validate subSet and filter data
  data <- RMLutils:::.validate_generic_data_subset(data = data,
                                        subSet = subSet)

  output <- as.matrix(data[, get(weightsColumn)])[,1, drop=FALSE]
  colnames(output) <- getTarget(object)
  output <- object$trainedModel$factorMean / output
  return(output)
}

#' @rdname getPredictions
#' @export
getPredictions.strataHTModeler <- function(object, data, subSet = NULL) {
  RMLutils:::.validate_trained_modeler(object)
  tgt <- getTarget(object)
  weightsColumn <- object$weightsColumn
  st <- object$strata
  #Validate subSet and filter data
  data <- RMLutils:::.validate_generic_data_subset(data = data,
                                        subSet = subSet)
  output <- merge(data[, st, with = F],
                  object$trainedModel$factorMeanStrata,
                  all.x = T, sort = F, by = st)[, tgt, with = F]
  output <- as.matrix(output / data[, weightsColumn, with = F])
  colnames(output) <- tgt
  output[is.na(output),] <- object$trainedModel$factorMeanGeneral
  return(output)
}

#' @rdname getPredictions
#' @export
getPredictions.strataMeanModeler <- function(object, data, subSet = NULL) {
  RMLutils:::.validate_trained_modeler(object)
  data <- RMLutils:::.validate_generic_data_subset(data = data,
                                        subSet = subSet)
  output <- merge(data.table(data)[subSet, c(object$regressors), with = F],
                  object$trainedModel$strata,
                  all.x = T, by = object$regressors)[, object$target, with = F]
  output <- as.matrix(output)
  colnames(output) <- object$target
  output[is.na(output),] <- object$trainedModel$general
  return(output)
}

#' @rdname getPredictions
#' @export
getPredictions.H2OModeler <- function(object, data, subSet = NULL) {
  RMLutils:::.validate_trained_modeler(object)
  movedData <- FALSE
  data <- RMLutils:::.validate_H2O_data_subset(data = data,
                                               subSet = subSet)

  is.fact <- sapply(data, is.factor)
  for(nm in names(is.fact)){
    if(is.fact[nm]){
      data[, (nm) := factor(get(nm), ordered = FALSE)]
    }
  }
  if (!inherits(data, "H2OFrame")) {
    data <- h2o::as.h2o(data)
    movedData <- TRUE
  }

  arguments <- list(object = object$trainedModel, newdata = data)

  #predictions <- do.call("h2o::h2o.predict", arguments)
  predictions <-do.call(getFromNamespace("h2o.predict", "h2o"), arguments)
  switch (class(object)[3],
          "classifierModeler" = {
            output <- as.matrix(predictions[,-1])#Remove first colum
            colnames(output) <- object$class_names
          },
          "regressorModeler" = {
            output <- as.matrix(predictions)[,1, drop=FALSE]
            colnames(output) <- getTarget(object)
          }
  )
  h2o::h2o.rm(predictions)
  if (movedData) {
    h2o::h2o.rm(list(data))
  }
  return(output)
}


#' @rdname getPredictions
#' @export
getPredictions.benchModeler <- function(object, data, subSet = NULL) {

  RMLutils:::.validate_trained_modeler(object)

  yt <- sapply(object$submodels, getPredictions, data, subSet)
  colnames(yt) <- object$target

  # Settings for the first pass
  settings <- list(max_iter = 100000L,
                   eps_abs = 1e-6,
                   eps_rel = 1e-8,
                   polishing = TRUE,
                   verbose = FALSE)

  solver <- osqp::osqp(
    P = object$P,
    q = rep(0, ncol(yt)),
    A = object$A,
    l = object$l,
    u = object$u,
    pars = settings
  )

  output <- matrix(NA_real_, nrow = nrow(yt), ncol = ncol(yt))
  colnames(output) <- object$target

  max_viol <- numeric(nrow(yt))

  # First pass of the optimizer
  for(i in seq_len(nrow(yt))) {
    q <- as.numeric(-object$P %*% yt[i,])
    solver@Update(q = q)
    res <- solver@Solve()
    output[i,] <- res$x
    # Compute violations of the restrictions
    Ax <- object$A %*% res$x
    viol <- pmax(
      pmax(object$l - Ax, 0),
      pmax(Ax - object$u, 0)
    )
    max_viol[i] <- max(viol)
  }

  # Select bad samples
  badSamples <- which(max_viol > 1e-6)

  settings <- list(max_iter = 100000L,
                   eps_abs = 1e-8,
                   eps_rel = 1e-14,
                   polishing = TRUE,
                   verbose = FALSE)

  solver <- osqp::osqp(
    P = object$P,
    q = rep(0, ncol(yt)),
    A = object$A,
    l = object$l,
    u = object$u,
    pars = settings
  )

  # Second pass of the optimizer

  for(i in badSamples) {
    q <- as.numeric(-object$P %*% yt[i,])
    solver@Update(q = q)
    res <- solver@Solve()
    output[i,] <- res$x
    # Compute violations of the restrictions
    Ax <- object$A %*% res$x
    viol <- pmax(
      pmax(object$l - Ax, 0),
      pmax(Ax - object$u, 0)
    )
    max_viol[i] <- max(viol)
  }

  # If there are still samples violating restrictions show a warning
  if (max(max_viol)>1e-6) {
    warning("Restrictions are violated for some samples.")
  }


  return(output)
}

#' @rdname getPredictions
#' @export
getPredictions.preproModeler <- function(object, data, subSet = NULL){
  RMLutils:::.validate_trained_modeler(object)

  data <- RMLutils:::.validate_generic_data_subset(data = data,
                                        subSet = subSet)

  object$preproObject <- getPreproData(object = object$preproObject,
                                       data = data)
  output <- getPredictions(object$modeler, data = object$preproObject$data)

  return(output)

}

#' @rdname getPredictions
#' @export
getPredictions.LGBModeler <- function(object, data, subSet = NULL){
  RMLutils:::.validate_trained_modeler(object)
  data <- RMLutils:::.validate_generic_data_subset(data = data,
                                        subSet = subSet)

  arguments <- c(list(object = object$trainedModel,
                      newdata = data))
  # Se filtran las columnas usadas en el entrenamiento
  dataPre <- arguments$newdata[, object$regressors, with=FALSE]

  # Codificamos los niveles de los factores de la misma forma que en train()
  #Automatically find categorical variables
  is.fact <- sapply(dataPre, is.factor)
  if(sum(is.fact) > 0){
    ifCategVars <- TRUE
    categVars <- names(is.fact)[is.fact]

    #Then, convert categorical columns into integer, starting from 0 (necessary for lightgbm)
    dataPre[, (categVars) := lapply(.SD, function(x) as.integer(x) - 1), .SDcols = categVars]
  }else{
    ifCategVars <- FALSE
  }

  # incluir predicciones lgbm
  predictions <- predict(object$trainedModel, data.matrix(dataPre))
  switch (class(object)[3],
          "classifierModeler" = {
            if(object$params$objective == "binary"){
              output <- cbind(1-predictions, predictions)#(negative score, positive score)
              #Properly map each class name
              t.levels <- object$class_names
              negative_class <- t.levels[t.levels != object$positive_class]
              colnames(output) <- c(negative_class, object$positive_class)
            }else{
              output <- as.matrix(predictions)
              colnames(output) <- object$class_names
            }

          },
          "regressorModeler" = {
            output <- as.matrix(predictions)[,1, drop=FALSE]
            colnames(output) <- object$target
          }
  )

  rm(predictions)
  return(output)
}

#' @rdname getPredictions
#' @export
getPredictions.rangerModeler <- function(object, data, subSet = NULL){
  RMLutils:::.validate_trained_modeler(object)

  data <- RMLutils:::.validate_generic_data_subset(data = data,
                                        subSet = subSet)

  # Se filtran las columnas usadas en el entrenamiento
  #if (is.data.table(data)) {
  #  dataPre <- data[subSet, object$regressors, with=FALSE]
  #} else {
  #  dataPre <- data[subSet, object$regressors, drop = FALSE]
  #}


  switch (class(object)[3],
          "classifierModeler" = {
            predictions <- predict(object$trainedModel, data)
            output <- as.matrix(predictions$predictions)
            colnames(output) <- object$class_names
          },
          "regressorModeler" = {
            predictions <- predict(object$trainedModel, data)
            output <- as.matrix(predictions$predictions)[,1, drop=FALSE]
            colnames(output) <- object$target
          }
  )

  rm(predictions)
  return(output)
}

#' @rdname getPredictions
#' @export
getPredictions.catboostModeler <- function(object, data, subSet = NULL){
  RMLutils:::.validate_trained_modeler(object)

  data <- RMLutils:::.validate_generic_data_subset(data = data,
                                        subSet = subSet)

  arguments <- c(list(object = object$trainedModel,
                      newdata = data))
  # Se filtran las columnas usadas en el entrenamiento
  dataPre <- arguments$newdata[, object$regressors, with=FALSE]
  dataPre_pool <- catboost::catboost.load_pool(dataPre)
  # incluir predicciones lgbm
  predictions <- catboost::catboost.predict(object$trainedModel,
                                            dataPre_pool,
                                            prediction_type = object$prediction_type,
                                            ntree_start = object$ntree_start,
                                            ntree_end = object$ntree_end,
                                            thread_count = object$thread_count)


  switch (class(object)[3],
          "classifierModeler" = {
            if(object$params$loss_function %in% c("Logloss","CrossEntropy")){#Binary
              output <- cbind(1-predictions, predictions)#(negative score, positive score)
              #Properly map each class name
              t.levels <- object$class_names
              negative_class <- t.levels[t.levels != object$positive_class]
              colnames(output) <- c(negative_class, object$positive_class)
            }else{#Multiclass
              output <- as.matrix(predictions)
              colnames(output) <- object$class_names
            }
          },
          "regressorModeler" = {
            output <- as.matrix(predictions)[,1, drop=FALSE]
            colnames(output) <- object$target
          }
  )
  return(output)
}


#' @rdname getPredictions
#' @export
getPredictions.miceModeler <- function(object, data = NULL, subSet = NULL){
  RMLutils:::.validate_trained_modeler(object)

  data <- RMLutils:::.validate_generic_data_subset(data = data,
                                        subSet = subSet)

  data_orig <- object$trainedModel
  #Join train and prediction data
  data_pred.dt <- copy(data[, setdiff(names(data), object$target), with = FALSE])

  object$params$data <- rbind(object$trainedModel,
                              data_pred.dt, fill = TRUE)
  #TODO: Why is this print here? Commenting it for now.
  #print(paste("m=",object$params$m))


  #Call mice and extract predictions
  object$trainedModel <- do.call(mice::mice, args = object$params)

  datatablePred <- mice::complete(object$trainedModel, object$params$m)
  datatablePred <- as.data.table(datatablePred)
  datatablePred <- datatablePred[(nrow(data_orig) + 1):nrow(datatablePred)]
  predictions <- datatablePred[, get(object$target)]

  switch (class(object)[3],
          "classifierModeler" = {

            output <- matrix(0,
                             nrow = length(predictions),
                             ncol = length(object$class_names),
                             dimnames = list(NULL, object$class_names))

            # Assign 1s using factor integer codes
            output[cbind(seq_along(predictions), as.integer(predictions))] <- 1
            colnames(output) <- object$class_names
          },
          "regressorModeler" = {
            output <- as.matrix(predictions)[,1, drop=FALSE]
            colnames(output) <- object$target
          }
  )

  rm(predictions)
  return(output)
}

#' @rdname getPredictions
#' @export
getPredictions.classifRegModeler <- function(object, data, subSet=NULL) {

  RMLutils:::.validate_trained_modeler(object)
  #This function already selects the specified subSet
  data <- RMLutils:::.validate_generic_data_subset(data = data,
                                        subSet = subSet)

  #classifPred <- getPredictions(object$classifModeler, data, classifSubset)
  classifPred <- getClasses(object = object$classifModeler, data = data, class_mapping = object$class_mapping)
  # select the predicted non special elements
  regSubset <- as.vector(classifPred[, 1] == "NOSP")
  #regSubset <- classifSubset[which(classifPred == "NOSP")]

  # predict non special elements with the regression model
  # Check for nonCatsMean first
  reg_data <- data[regSubset, ]
  if (!is.null(object$trainedModel$nonCatsMean)) {
    regPred <- object$trainedModel$nonCatsMean
  } else {
    regPred <- getPredictions(object$regModeler, data[regSubset, ])
  }
  #print(as.numeric(as.character(classifPred[, object$classifModeler$target, with = FALSE])))
  # retrieve the original numeric vector from the factor
  setnames(classifPred, new = c(getTarget(object$regModeler)))
  pred <- suppressWarnings(classifPred[, (getTarget(object$regModeler)) := as.numeric(as.character(get(getTarget(object$regModeler))))])
  pred[regSubset, 1] <- regPred
  #Create bool mask to assign regression results

  output <- as.matrix(pred, dimnames = list(NULL, object$target))[,1, drop=FALSE]
  #colnames(output) <- c(object$target)
  #print(output)
  return(output)
}

#' @rdname getPredictions
#' @export
getPredictions.prePredModeler <- function(object, data, subSet = NULL){
  RMLutils:::.validate_trained_modeler(object)
  data <- RMLutils:::.validate_generic_data_subset(data = data,
                                        subSet = subSet)
  # Get initial models predictions
  preds <- RMLutils::getPredictions(object$predModeler, data)
  # Add predictions to data copy
  dataCopy <- as.data.table(data)
  dataCopy[, colnames(preds) := as.data.table(preds)]
  # Get predictions
  output <- RMLutils::getPredictions(object$mainModeler, dataCopy)
  # Clean data copy
  rm(dataCopy)

  return(output)
}

#' @rdname getPredictions
#' @export
getPredictions.memoryModeler <- function(object, data, subSet = NULL){
  RMLutils:::.validate_trained_modeler(object)
  if (is.null(subSet)) {subSet <- 1:nrow(data)}
  # Get predictions
  output <- RMLutils::getPredictions(object$modeler, data, subSet)

  return(output)
}
