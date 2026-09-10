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

#' @title Evaluates cost function
#'
#' @description Returns the evaluation of the cost function in the given complementary
#' sample.
#'
#' @param object \code{\link{costFunction}} object.
#' @param data Datatable containing dataset to work with.
#' @param modeler Model object used whose performance is to be studied.
#' @param fSample All indices of entries obtained by \code{\link{sampler}}.
#' @param cSample Complementary indices to the ones obtained by sampler.
#'
#' @seealso \code{\link{costFunction}}
#' @seealso \code{\link{getReducedCost}}
#' @seealso \code{\link{r2CostFunction}}
#' @seealso \code{\link{confusionMatrixCostFunction}}
#'
#' @return A named list containing cost and additional metadata, useful for getReducedCost.
#'
#' @import data.table
#'
#' @examples
#' # Regression example ####
#' #Generate data
#' data.dt <- generateRegressionData(n_samples = 1000, generate_random_weights = FALSE)
#'
#' #Instantiate cost function
#' cf <- r2CostFunction(weightsColumn = "")
#'
#' target <- "regression_target"# Regression
#'
#' regressors <- setdiff(names(data.dt), target)
#'
#' #Define modeler
#' params <- list(
#'   "learning_rate" = 0.07,
#'   "nrounds" = 10,
#'   "objective" = "regression"
#' )
#'
#' regressor <- LGBRegressorModeler(params = params, target = target, regressors = regressors)
#' regressor <- train(regressor, data = data.dt)
#'
#' cost <- getCost(object = cf,
#'                 data = data.dt,
#'                 modeler = regressor,
#'                 fSample = 1:nrow(data.dt),
#'                 cSample = 1:nrow(data.dt))
#' print(cost)
#'
#' # Classification example ####
#' #Generate data
#' data.dt <- generateMulticlassClassificationData(n_samples = 1000)
#' #Instantiate cost function
#' cf <- confusionMatrixCostFunction(weightsColumn = "",
#'                                   class_map = maxScoreClass(),
#'                                   normalize = NULL,
#'                                   class_names = levels(data.dt[["classification_target"]]))
#'
#' #Train simple modeler
#' target <- "classification_target"
#' regressors <- setdiff(names(data.dt), target)
#' params <- list(
#'   "learning_rate" = 0.07,
#'   "nrounds" = 10,
#'   "objective" = "multiclass",
#'   "num_class" = length(unique(data.dt[, classification_target]))
#' )
#'
#' classifier <- LGBClassifierModeler(params = params, target = target, regressors = regressors)
#' classifier <- train(classifier, data = data.dt)
#'
#' #Evaluate with getCost
#' cost <- getCost(object = cf, data = data.dt, modeler = classifier, cSample = 1:nrow(data.dt))
#' print(cost)
#'
#' @export
getCost <- function(object, data, modeler, fSample, cSample) {
  UseMethod("getCost")
}


#' @rdname getCost
#' @export
getCost.r2CostFunction <- function(object, data, modeler, fSample, cSample) {

  #Force conversion to data.table
  data <- data.table::as.data.table(data)

  if (is.data.table(data))
    eMatrix <- getPredictions(modeler, data, cSample) -
      as.matrix(data[cSample, getTarget(modeler), with = F])
  else
    eMatrix <- getPredictions(modeler, data, cSample) -
      as.matrix(data[cSample, getTarget(modeler), with = F])
  output <- lapply(getTarget(modeler), function(x) {
    if (object$weightsColumn != "")
      weights <- as.matrix(data[fSample, object$weightsColumn, with = F])[, 1]
    else weights <- rep(1, length(fSample))

    partial_weights <- weights[fSample %in% cSample]

    full_mean <- weighted.mean(as.matrix(data[fSample, x, with = F])[, 1], weights)
    partial_mean <- weighted.mean(as.matrix(data[cSample, x, with = F])[, 1], partial_weights)

    full_variance <- sum((as.matrix(data[fSample, x, with = F])[, 1] - full_mean) ** 2 * weights)
    partial_variance <- sum((as.matrix(data[cSample, x, with = F])[, 1] - partial_mean) ** 2 * partial_weights)
    sme <- sum(eMatrix[, x]**2 * partial_weights)
    return(list("cost" = 1 - sme / partial_variance, "weights" = partial_variance/full_variance))
  })
  names(output) <- getTarget(modeler)
  return(output)
}
#' @rdname getCost
#' @export
getCost.squareBiasCostFunction <- function(object, data, modeler, fSample, cSample) {

  #Force conversion to data.table
  data <- data.table::as.data.table(data)

  if (is.data.table(data))
    eMatrix <- getPredictions(modeler, data, cSample) -
      as.matrix(data[cSample, getTarget(modeler), with = F])
  else
    eMatrix <- getPredictions(modeler, data, cSample) -
      as.matrix(data[cSample, getTarget(modeler)])

  if (object$weightsColumn != "")
    weights <- as.matrix(data[cSample, object$weightsColumn, with = F])[, 1]
  else weights <- rep(1, length(cSample))

  output <- lapply(getTarget(modeler), function(x) {
    #Revisar esto, creo que faltaba un **2
    return(list("cost" = (sum(eMatrix[,x] * weights) / sum(weights))**2, "weights" = sum(weights)))
  })
  names(output) <- getTarget(modeler)
  return(output)
}

#' @rdname getCost
#' @export
getCost.MSECostFunction <- function(object, data, modeler, fSample, cSample) {

  #Force conversion to data.table
  data <- data.table::as.data.table(data)

  if (is.data.table(data)){
    eMatrix <- getPredictions(modeler, data, cSample) -
      as.matrix(data[cSample, getTarget(modeler), with = F])
  }
  else{
    eMatrix <- getPredictions(modeler, data, cSample) -
      as.matrix(data[cSample, getTarget(modeler)])
  }

  eMatrix <- t(eMatrix)
  eMatrixAlCuadrado <- eMatrix^2
  if (object$weightsColumn != "") {
    if (is.data.table(data)) {
      weights <- as.matrix(data[cSample, object$weightsColumn, with = F])[, 1]
    } else weights <- as.matrix(data[cSample, object$weightsColumn])[, 1]
  }
  else weights <- rep(1, length(cSample))
  output <- lapply(getTarget(modeler), function(x) {
    sme <- eMatrixAlCuadrado[x, ] %*% weights/(sum(weights))
    return(list("cost" = sme, "weights" = weights/(sum(weights)), "residuals" = eMatrixAlCuadrado[x, ]))
  })
  names(output) <- getTarget(modeler)
  return(output)
}

#' @rdname getCost
#' @export
getCost.groupTotalErrorCostFunction <- function(object, data, modeler, fSample, cSample){

  data.dt <- copy(data)

  if (!is.data.table(data.dt)){
    data.dt <- as.data.table(data.dt)
  }

  if (object$weightsColumn != "") {
    weights <- data.dt[cSample, object$weightsColumn, with = F]
  } else{
    weights <- rep(1, length(cSample))
  }

  targets <- getTarget(modeler)
  prednames <- paste0("pred_", targets)

  for (target in targets) {
    data.dt[cSample, (target) := get(target)]
  }

  predsMatrix <- getPredictions(modeler, data.dt, cSample)

  for (i in seq_along(prednames)) {
    data.dt[cSample, (prednames[i]) := predsMatrix[, i]]
  }

  eTable <- data.dt[cSample, c(
    lapply(targets, function(target) sum(get(target) * weights)),
    lapply(prednames, function(pred) sum(get(pred) * weights))
  ), by = eval(object$groupVars)]

  setnames(eTable,
           old = paste0("V", seq_along(targets)),
           new = paste0("totalTarget_", targets))

  setnames(eTable,
           old = paste0("V", seq_along(prednames) + length(targets)),
           new = paste0("totalPred_", targets))

  for (target in targets) {
    eTable[, paste0("difa_", target) := abs(get(paste0("totalTarget_", target)) - get(paste0("totalPred_", target)))]
  }

  error_total <- lapply(targets, function(target) {
    sum(eTable[[paste0("difa_", target)]])
  })

  output <- lapply(seq_along(targets), function(i) {
    list("cost" = error_total[[i]], "weights" = weights/(sum(weights)), "residuals" = eTable[[paste0("difa_", targets[i])]])
    #list(cost = error_total[[i]])
  })

  names(output) <- targets
  return(output)
}


#' @rdname getCost
#' @export
getCost.groupMSECostFunction <- function(object, data, modeler, fSample, cSample) {

  #Force conversion to data.table
  data <- data.table::as.data.table(data)

  data.dt <- copy(data[cSample])
  targets <- getTarget(modeler)

  prednames <- paste0("pred_", targets)
  targets_col <- paste0("group_target_", targets)
  names(targets_col) <- targets
  pred_col <- paste0("group_pred_", targets)
  names(pred_col) <- targets

  data.dt[, (prednames) := getPredictions(modeler, data, cSample)]
  data.dt[, (targets_col) := lapply(.SD, object$groupFunction), by = c(object$groupColumn), .SDcols = targets]
  data.dt[, (pred_col) := lapply(.SD, object$groupFunction), by = c(object$groupColumn), .SDcols = prednames]

  lapply(targets, function(t) {
    dif2_col <- paste0("dif2_", t)
    data.dt[, (dif2_col) := (get(targets_col[t]) - get(pred_col[t]))^2]
  })

  if (object$weightsColumn != ""){
    data.dt[, group_weights := object$groupWeightsFunction(get(object$weightsColumn)), by = c(object$groupColumn)]
    weights <- as.matrix(data.dt[, group_weights])[, 1]
  }
  else{weights <- rep(1, length(cSample))}

  output <- lapply(targets, function(t) {
    dif2_col <- paste0("dif2_", t)
    sme <- (data.dt[, get(dif2_col)] %*% weights)/(sum(weights))
    list2 <- list("cost" = sme, "size_fold" = (data[,.N] - length(cSample)))
    return(list2)
  })

  names(output) <- targets

  return(output)
}

#' @rdname getCost
#' @export
getCost.groupTotalErrorCostFunction <- function(object, data, modeler, fSample, cSample){

  data.dt <- data.table::copy(data)

  if (!is.data.table(data.dt)){
    data.dt <- as.data.table(data.dt)
  }

  if (object$weightsColumn != "") {
    weights <- data.dt[cSample, object$weightsColumn, with = F]
  } else{
    weights <- rep(1, length(cSample))
  }

  targets <- getTarget(modeler)
  prednames <- paste0("pred_", targets)

  for (t in targets) {
    data.dt[cSample, (t) := get(t)]
  }

  predsMatrix <- getPredictions(modeler, data.dt, cSample)

  for (i in seq_along(prednames)) {
    data.dt[cSample, (prednames[i]) := predsMatrix[, i]]
  }

  eTable <- data.dt[cSample, c(
    lapply(targets, function(t) sum(get(t) * weights)),
    lapply(prednames, function(p) sum(get(p) * weights))
  ), by = eval(object$groupVars)]

  setnames(eTable,
           old = paste0("V", seq_along(targets)),
           new = paste0("totalTarget_", targets))

  setnames(eTable,
           old = paste0("V", seq_along(prednames) + length(targets)),
           new = paste0("totalPred_", targets))

  for (target in targets) {
    eTable[, paste0("difa_", target) := abs(get(paste0("totalTarget_", target)) - get(paste0("totalPred_", target)))]
  }

  error_total <- lapply(targets, function(target) {
    sum(eTable[[paste0("difa_", target)]])
  })

  output <- lapply(seq_along(targets), function(i) {
    list("cost" = error_total[[i]], "weights" = weights/(sum(weights)), "residuals" = eTable[[paste0("difa_", targets[i])]])
  })

  names(output) <- targets
  return(output)
}


#' @rdname getCost
#' @export
getCost.MCSubRBMSECostFunction <- function(object, data, modeler, fSample, cSample) {

  #Force conversion to data.table
  data <- data.table::as.data.table(data)

  # auxiliary variables
  cSample2 <- sort(object$sampleKeys[cSample])
  s1 <- sort(setdiff(object$sampleKeys, cSample2))

  # the model is already trained over s1
  predictionsNSD <- getPredictions(modeler, object$nonSampleData)
  predictionss2 <- getPredictions(modeler, data, cSample)

  # bias estimate
  cSampler <- conditionedSampler(object$sampler, s1)
  outputs <- lapply(getTarget(modeler), function(x){
    if (is.data.table(data)) {
      errors <- predictionss2 -
        as.matrix(data[cSample, x, with = F])[, 1]
    } else {
      errors <- predictionss2 - as.matrix(data[cSample, x])[, 1]
    }

    # variance estimate of bias estimate and b2
    varianceBias <- RMLutils:::biasVarianceEstimate(cSampler, cSample2, errors)
    vb <- varianceBias$vb
    vb2 <- varianceBias$vb2
    bias <- varianceBias$bias
    biasSquared <- bias^2

    # single sample estimate
    if (is.data.table(data)) {
      ysSum <- sum(as.matrix(data[, x, with = F])[, 1])
    } else {
      ysSum <- sum(as.matrix(data[, x])[, 1])
    }
    y1Star <- ysSum + sum(predictionsNSD)

    output <- c(biasSquared=biasSquared, bias = bias, vb=vb, vb2=vb2, y1Star=y1Star)
    return(list("cost" = output))
  })
  names(outputs) <- getTarget(modeler)
  return(outputs)
}

#' @rdname getCost
#' @export
getCost.holdoutTemporalCostFunction <- function(object, data, modeler, fSample, cSample) {

  #Force conversion to data.table
  data <- data.table::as.data.table(data)

  data.dt <- copy(data[cSample])

  all_cSamplePeriods <- data.dt[, get(object$cvTemporalSampler$identifier)]
  periods <- object$cvTemporalSampler$orderPeriods
  all_valPer <- periods[which(periods %in% all_cSamplePeriods)][1:object$cvTemporalSampler$n_valPeriods]

  data.dt[, idx := cSample]
  cSampleTemp <- data.dt[get(object$cvTemporalSampler$identifier) %in% all_valPer, idx]

  output <- getCost(object$costFunction, data, modeler, fSample, cSampleTemp)

  return(output)
}

# Classification cost functions ####

#' @rdname getCost
#' @export
getCost.confusionMatrixCostFunction <- function(object, data, modeler, fSample, cSample){

  #Force conversion to data.table
  data <- data.table::as.data.table(data)

  if(!is.null(cSample)){
    data.dt <- data.table::copy(data[cSample])
  }else{
    data.dt <- data
  }

  if (object$weightsColumn != ""){
    weights <- as.matrix(data[cSample, get(object$weightsColumn)])[, 1]
  }else{
    weights <- rep(1, length(cSample))
  }

  total_weights <- sum(weights)
  output <- lapply(getTarget(modeler), function(x) {
    y_true <- data.dt[[c(x)]]

    y_pred <- unlist(getClasses(object = modeler, data = data.dt, class_mapping = object$class_map), use.names = FALSE)
    #Get all levels from each vector
    common_levels <- union(levels(y_true), levels(y_pred))
    #Re-order factor levels to prevent sorting issues, in case their levels are different
    y_true <- factor(y_true, levels = common_levels)
    y_pred <- factor(y_pred, levels = common_levels)
    confusion_matrix <- xtabs(weights ~ y_true + y_pred)
    cm <- confusion_matrix

    #Apply normalization, if specified
    if(!is.null(object$normalize)){
      switch (object$normalize,
              "true" = {
                confusion_matrix <- prop.table(confusion_matrix, margin = 1)
              },
              "pred" = {
                confusion_matrix <- prop.table(confusion_matrix, margin = 2)
              },
              "all" = {
                confusion_matrix <- prop.table(confusion_matrix)
              },
              {
                stop("The provided normalization parameter does not exist. Please use one of the folloring:\n (true, pred, all)")
              }
      )
    }
    confusion_matrix[is.nan(confusion_matrix)] <- 0
    return(list("cost" = confusion_matrix, "weights" = sum(total_weights), "cm" = cm))
  })
  names(output) <- getTarget(modeler)
  return(output)
}

#' @rdname getCost
#'
#' @export
getCost.accuracyCostFunction <- function(object, data, modeler, fSample, cSample){

  #Force conversion to data.table
  data <- data.table::as.data.table(data)

  #Define non-normalized confusion matrix
  cm.cf <- confusionMatrixCostFunction(weightsColumn = object$weightsColumn, class_map = object$class_map)

  #Calculate confusion matrix (The output is a list of length = number of target variables)
  confusion_matrices <- getCost(object = cm.cf, data = data, modeler = modeler, fSample = fSample, cSample = cSample)

  output <- lapply(getTarget(modeler), function(x) {

    confusion_matrix <- confusion_matrices[[x]][["cost"]]
    cm_weights <- confusion_matrices[[x]][["weights"]]

    #Calculate accuracy (TP+TN)/(TP + TN + FP + FN)
    support <- rowSums(confusion_matrix)  #Real frequency of each class
    total_samples <- sum(confusion_matrix)
    class_names <- rownames(confusion_matrix)
    n_classes <- nrow(confusion_matrix)

    class_accuracy <- sapply(1:n_classes, function(i){
      TP <- diag(confusion_matrix)[i]
      FN <- sum(confusion_matrix[i, -i])
      FP <- sum(confusion_matrix[-i, i])
      TN <- sum(confusion_matrix[-i, -i])
      denom <- TP + TN + FP + FN
      acc_per_class <- ifelse(denom == 0, 0, (TP + TN) / denom)
      return(acc_per_class)
    })

    switch (object$average,
            "micro" = {
              score <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
            },
            "macro" = {
              score <- mean(class_accuracy, na.rm = TRUE)
            },
            "weighted" = {
              weights <- support / total_samples
              score <- sum(class_accuracy * weights, na.rm = TRUE)
            }
    )
    return(list("cost" = score, "weights" = cm_weights, "cm" = confusion_matrix))
  })
  names(output) <- getTarget(modeler)
  return(output)
}

#' @rdname getCost
#'
#' @export
getCost.precisionCostFunction <- function(object, data, modeler, fSample, cSample){

  #Force conversion to data.table
  data <- data.table::as.data.table(data)

  #Define non-normalized confusion matrix
  cm.cf <- confusionMatrixCostFunction(weightsColumn = object$weightsColumn, class_map = object$class_map)

  #Calculate confusion matrix
  confusion_matrices <- getCost(object = cm.cf, data = data, modeler = modeler, fSample = fSample, cSample = cSample)

  output <- lapply(getTarget(modeler), function(x) {

    confusion_matrix <- confusion_matrices[[x]][["cost"]]
    cm_weights <- confusion_matrices[[x]][["weights"]]

    #Calculate precision TP/(TP+FP)
    support <- rowSums(confusion_matrix)  #Real frequency of each class
    total_samples <- sum(confusion_matrix)
    class_names <- rownames(confusion_matrix)
    n_classes <- nrow(confusion_matrix)

    class_precision <- sapply(1:n_classes, function(i){
      TP <- diag(confusion_matrix)[i]
      FN <- sum(confusion_matrix[i, -i])
      FP <- sum(confusion_matrix[-i, i])
      TN <- sum(confusion_matrix[-i, -i])
      denom <- TP + FP
      prec_per_class <- ifelse(denom == 0, 0, TP / denom)
      return(prec_per_class)
    })
    switch (object$average,
            "micro" = {
              TP_total <- sum(diag(confusion_matrix))
              FP_total <- sum(colSums(confusion_matrix)) - TP_total
              denom <- TP_total + FP_total
              score <- ifelse(denom == 0, 0, TP_total / denom)
            },
            "macro" = {
              per_class_precision <- diag(confusion_matrix) / colSums(confusion_matrix)
              score <- mean(class_precision, na.rm = TRUE)
            },
            "weighted" = {
              weights <- support / total_samples
              score <- sum(class_precision * weights, na.rm = TRUE)
            }
    )
    return(list("cost" = score, "weights" = cm_weights, "cm" = confusion_matrix))
  })
  names(output) <- getTarget(modeler)
  return(output)
}

#' @rdname getCost
#'
#' @export
getCost.recallCostFunction <- function(object, data, modeler, fSample, cSample){

  #Force conversion to data.table
  data <- data.table::as.data.table(data)

  #Define non-normalized confusion matrix
  cm.cf <- confusionMatrixCostFunction(weightsColumn = object$weightsColumn, class_map = object$class_map)

  #Calculate confusion matrix
  confusion_matrices <- getCost(object = cm.cf, data = data, modeler = modeler, fSample = fSample, cSample = cSample)

  output <- lapply(getTarget(modeler), function(x) {

    confusion_matrix <- confusion_matrices[[x]][["cost"]]
    cm_weights <- confusion_matrices[[x]][["weights"]]

    #Calculate recall TP/(TP+FN)
    support <- rowSums(confusion_matrix)  #Real frequency of each class
    total_samples <- sum(confusion_matrix)
    class_names <- rownames(confusion_matrix)
    n_classes <- nrow(confusion_matrix)

    class_recall <- sapply(1:n_classes, function(i){
      TP <- diag(confusion_matrix)[i]
      FN <- sum(confusion_matrix[i, -i])
      FP <- sum(confusion_matrix[-i, i])
      TN <- sum(confusion_matrix[-i, -i])
      denom <- TP + FN
      rec_per_class <- ifelse(denom == 0, 0, TP / denom)
      return(rec_per_class)
    })
    switch (object$average,
            "micro" = {
              TP_total <- sum(diag(confusion_matrix))
              FN_total <- sum(rowSums(confusion_matrix)) - TP_total
              denom <- TP_total + FN_total
              score <- ifelse(denom == 0, 0, TP_total / denom)
            },
            "macro" = {
              score <- mean(class_recall, na.rm = TRUE)
            },
            "weighted" = {
              weights <- support / total_samples
              score <- sum(class_recall * weights, na.rm = TRUE)
            }
    )
    return(list("cost" = score, "weights" = cm_weights, "cm" = confusion_matrix))
  })
  names(output) <- getTarget(modeler)
  return(output)
}

#' @rdname getCost
#'
#' @export
getCost.f1ScoreCostFunction <- function(object, data, modeler, fSample, cSample){

  #Force conversion to data.table
  data <- data.table::as.data.table(data)

  #Define non-normalized confusion matrix
  cm.cf <- confusionMatrixCostFunction(weightsColumn = object$weightsColumn, class_map = object$class_map)

  #Calculate confusion matrix
  confusion_matrices <- getCost(object = cm.cf, data = data, modeler = modeler, fSample = fSample, cSample = cSample)

  output <- lapply(getTarget(modeler), function(x) {

    confusion_matrix <- confusion_matrices[[x]][["cost"]]
    cm_weights <- confusion_matrices[[x]][["weights"]]

    #Calculate F1 score 2·(precision·recall)/(precision+recall)
    support <- rowSums(confusion_matrix)  #Real frequency of each class
    total_samples <- sum(confusion_matrix)
    class_names <- rownames(confusion_matrix)
    n_classes <- nrow(confusion_matrix)

    class_precision <- sapply(1:n_classes, function(i) {
      TP <- confusion_matrix[i, i]
      FP <- sum(confusion_matrix[-i, i])
      denom <- TP + FP
      ifelse(denom == 0, 0, TP / denom)
    })

    class_recall <- sapply(1:n_classes, function(i) {
      TP <- confusion_matrix[i, i]
      FN <- sum(confusion_matrix[i, -i])
      denom <- TP + FN
      ifelse(denom == 0, 0, TP / denom)
    })

    class_f1 <- mapply(function(p, r) {
      if (is.na(p) || is.na(r) || (p + r) == 0) {
        return(0)
      } else {
        return(2 * p * r / (p + r))
      }
    }, class_precision, class_recall)

    switch (object$average,
            "micro" = {
              TP_total <- sum(diag(confusion_matrix))
              FP_total <- sum(colSums(confusion_matrix)) - TP_total
              FN_total <- sum(rowSums(confusion_matrix)) - TP_total
              precision_micro <- ifelse((TP_total + FP_total) == 0, NA, TP_total / (TP_total + FP_total))
              recall_micro <- ifelse((TP_total + FN_total) == 0, NA, TP_total / (TP_total + FN_total))
              if (is.na(precision_micro) || is.na(recall_micro) || (precision_micro + recall_micro) == 0) {
                score <- 0
              } else {
                score <- 2 * precision_micro * recall_micro / (precision_micro + recall_micro)
              }
            },
            "macro" = {
              score <- mean(class_f1, na.rm = TRUE)
            },
            "weighted" = {
              weights <- support / total_samples
              score <- sum(class_f1 * weights)
            }
    )
    return(list("cost" = score, "weights" = cm_weights, "cm" = confusion_matrix))
  })
  names(output) <- getTarget(modeler)
  return(output)
}

#' @rdname getCost
#'
#' @export
getCost.ROCAUCCostFunction <- function(object, data, modeler, fSample, cSample){

  #Force conversion to data.table
  data <- data.table::as.data.table(data)

  if(!is.null(cSample)){
    data.dt <- copy(data[cSample])
  }else{
    data.dt <- data
    cSample <- 1:nrow(data.dt)
  }

  if(object$weightsColumn != ""){
    weights <- as.matrix(data[cSample, get(object$weightsColumn)])[, 1]
  }else{
    weights <- rep(1, length(cSample))
  }

  total_weights <- sum(weights)
  output <- lapply(getTarget(modeler), function(x) {
    #Get necessary parameters
    true_classes <- data.table(data.dt[[c(x)]])
    setnames(true_classes, new = x)
    #TODO: If multivariate classifier modelers are added this has to be changed (select only current variable predictions)
    y_predicted_scores <- getPredictions(object = modeler, data = data.dt)
    #Get category names, then proceed differently for multiclass or binary classification
    categories <- colnames(y_predicted_scores)

    if(object$average != "micro"){
      #If average is macro or weighted:
      #if(length(categories) > 2){#Multiclass
        positive_scores <- lapply(categories, function(x) y_predicted_scores[, x])
        negative_scores <- lapply(categories, function(x) 1-y_predicted_scores[, x])#This assumes scores are normalized
      #}else if(length(categories) == 2){#Binary
      #  positive_scores <- list(y_predicted_scores[, categories[[1]]])
      #  negative_scores <- list(y_predicted_scores[, categories[[2]]])
      #  names(positive_scores) <- categories[[1]]
      #  names(negative_scores) <- categories[[2]]
      #  categories <- categories[[1]]#Assume first category to be the positive one (irrelevant for ROCAUC calculation)
      #}

      #Binarized scores (for each category):
      binarized_scores <- lapply(1:length(categories), function(y){
        c_scores <- matrix(c(positive_scores[[y]], negative_scores[[y]]), ncol = 2)
        colnames(c_scores) <- c(categories[[y]], paste0("Negative_",categories[[y]]))
        return(c_scores)
      })
      names(binarized_scores) <- categories

      #Binarized true classes (for each category)
      binarized_true_classes <- lapply(categories, function(y){
        #Define binarized_category as a factor, specifying the factor order to be first positive class and next, negative class
        true_classes[, binarized_category := factor(ifelse(get(x) == y, y, paste0("Negative_",y)), levels = c(y, paste0("Negative_",y)))]
        return(true_classes[["binarized_category"]])
      })
      names(binarized_true_classes) <- categories

      #Binary class thresholds (for each category): Percentile thresholds method
      binary_class_thresholds <- lapply(1:length(categories), function(y){
        #Get 100 points to make it more efficient
        thresholds <- sort(quantile(unique(positive_scores[[y]], decreasing = TRUE),
                                    probs = seq(0,1,0.01), names = FALSE), decreasing = TRUE)
        return(thresholds)
      })
      names(binary_class_thresholds) <- categories

      #For each class and threshold, obtain true positive and false positive rate
      tpr_fpr <- lapply(categories, function(y){

        #Define current positive class
        object$class_map$positive_class <- y
        output <- lapply(binary_class_thresholds[[y]], function(z){
          #Define current threshold
          object$class_map$threshold <- z
          #Get true binarized classes
          y_true <- binarized_true_classes[[y]]
          #Get index of predicted classes according to previously defined threshold
          predicted_idx <- RMLutils::getClassMapping(object$class_map, binarized_scores[[y]])
          #Get predicted classes
          class_names <- colnames(binarized_scores[[y]])
          #Important, levels must match the y_true levels
          y_pred <- factor(class_names[predicted_idx], levels = levels(y_true))

          #Calculate confusion matrix (2x2, since everything is binarized)
          confusion_matrix <- xtabs(weights ~ y_true + y_pred)

          #Get metrics of interest
          TP <- confusion_matrix[1,1]
          FN <- sum(confusion_matrix[1, 2])
          FP <- sum(confusion_matrix[2, 1])
          TN <- confusion_matrix[2,2]

          tpr <- ifelse(TP+FN > 0, TP/(TP+FN), 0)
          fpr <- ifelse(FP+TN > 0, FP/(FP+TN), 0)

          return(c("tpr" = tpr, "fpr" = fpr))
        })

        tpr <- sapply(1:length(output), function(i) output[[i]][["tpr"]])
        fpr <- sapply(1:length(output), function(i) output[[i]][["fpr"]])

        #Calculate support as additional metric
        #TODO: (Should it take into account the data weights?)
        support <- sum((binarized_true_classes[[y]] == y) * weights)

        #Add start and end points if needed
        if(fpr[1] > 0){
          fpr <- c(0, fpr)
          tpr <- c(0, tpr)
        }else if(fpr[length(fpr)] < 1){
          fpr <- c(fpr, 1)
          tpr <- c(tpr, 1)
        }

        n <- length(fpr)
        widths <- diff(fpr)
        mean_heights <- (tpr[-1] + tpr[-length(tpr)]) / 2
        auc <- sum(widths * mean_heights)

        #Return all relevant statistics (binarized scores, true classes and weights are used in get_reduced_cost)
        return(list("tpr" = tpr,
                    "fpr" = fpr,
                    "auc" = auc,
                    "support" = support,
                    "binarized_scores" = binarized_scores[[y]],
                    "binarized_true_classes" = binarized_true_classes[[y]],
                    "binarized_weights" = weights))
      })
      names(tpr_fpr) <- categories

    }else{
      #If average is "micro", proceed differently, as all classes are treated as the same
      y_true_bin_micro <- matrix(nrow = length(categories)*dim(true_classes)[1])
      score_micro <- matrix(nrow = length(categories)*dim(true_classes)[1])
      weights_micro <- matrix(nrow = length(categories)*dim(true_classes)[1])
      #Concatenate all classes, scores and weights into a single column
      for(i in 1:length(categories)){
        boolean_mask <- true_classes == categories[i]
        idxs <- seq(from = (i-1)*dim(true_classes)[1]+1, to = (i)*dim(true_classes)[1])
        y_true_bin_micro[idxs] <- as.integer(boolean_mask)
        score_micro[idxs] <- y_predicted_scores[, categories[i]]
        weights_micro[idxs] <- weights
      }

      #Calculate thresholds over this data (get only 101 points to increase its efficiency)
      binary_class_thresholds <- sort(quantile(unique(score_micro, decreasing = TRUE),
                                  probs = seq(0,1.0,0.01), names = FALSE), decreasing = FALSE)
      #Define arrays to store tpr and fpr
      tpr_matrix <- matrix(nrow = length(binary_class_thresholds))
      fpr_matrix <- matrix(nrow = length(binary_class_thresholds))
      #Then iterate over each threshold to calculate its tpr and fpr
      for (i in 1:length(binary_class_thresholds)){
        threshold <- binary_class_thresholds[i]
        y_pred_micro <- as.integer(score_micro >= threshold)
        confusion_matrix <- xtabs(weights_micro ~ factor(y_true_bin_micro, levels = 0:1) + factor(y_pred_micro, levels = 0:1))

        #Get metrics of interest
        TP <- confusion_matrix[1,1]
        FN <- sum(confusion_matrix[1, 2])
        FP <- sum(confusion_matrix[2, 1])
        TN <- confusion_matrix[2,2]

        tpr <- ifelse(TP+FN > 0, TP/(TP+FN), 0)
        fpr <- ifelse(FP+TN > 0, FP/(FP+TN), 0)
        tpr_matrix[i] <- tpr
        fpr_matrix[i] <- fpr
      }
      #Class support makes no sense in micro average. Simply sum weights_micro
      support <- sum(weights_micro)

      #Add start and end points if needed
      if(fpr_matrix[1] > 0){
        fpr_matrix <- as.matrix(c(0, fpr_matrix), ncol = 1)
        tpr_matrix <- as.matrix(c(0, tpr_matrix), ncol = 1)
      }else if(fpr_matrix[length(fpr_matrix)] < 1){
        fpr_matrix <- as.matrix(c(fpr_matrix, 1), ncol = 1)
        tpr_matrix <- as.matrix(c(tpr_matrix, 1), ncol = 1)
      }

      #Calculate AUC
      n <- length(fpr_matrix)
      widths <- diff(fpr_matrix)
      mean_heights <- (tpr_matrix[-1] + tpr_matrix[-length(tpr)]) / 2
      auc <- sum(widths * mean_heights)

      tpr_fpr <- list("tpr" = tpr_matrix,
                      "fpr" = fpr_matrix,
                      "auc" = auc,
                      "support" = support,
                      "y_true_bin_micro" = y_true_bin_micro,
                      "score_micro" = score_micro,
                      "weights_micro" = weights_micro)
    }

    #Store metrics accordingly
    switch (object$average,
            "micro" = {
              score <- tpr_fpr[["auc"]]
            },
            "macro" = {
              aucs <- sapply(1:length(tpr_fpr), function(i) tpr_fpr[[i]][["auc"]])
              score <- mean(aucs, na.rm = TRUE)
            },
            "weighted" = {
              aucs <- sapply(1:length(tpr_fpr), function(i) tpr_fpr[[i]][["auc"]])
              supports <- sapply(1:length(tpr_fpr), function(i) tpr_fpr[[i]][["support"]])
              score <- sum(aucs * supports, na.rm = TRUE) / sum(supports)
            }
    )

    return(list("cost" = score, "class_metrics" = tpr_fpr))
  })
  names(output) <- getTarget(modeler)
  return(output)
}
