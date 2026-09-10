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

#' @title Reduction operator for cost function classes.
#'
#' @description Summarizes the cost over the different cross validation subsets.
#'
#' @param object \code{\link{costFunction}} object.
#' @param data List of cross-validation cost results.
#'
#' @import data.table
#'
#' @seealso \code{\link{costFunction}}
#' @seealso \code{\link{getCost}}
#'
#' @examples
#' #Usage outside crossValidate:
#' #Generate data
#' data.dt <- generateRegressionData(n_samples = 1000, generate_random_weights = FALSE)
#' #Get relevant variables from data
#' data.keys <- 1:nrow(data.dt)
#' target <- "regression_target"# Regression
#' regressors <- setdiff(names(data.dt), target)
#' #Define modeler
#' params <- list(
#'   "num.trees" = 50,
#'   "max.depth" = 20
#' )
#' target <- "regression_target"
#' regressors <- setdiff(colnames(data.dt), target)
#'
#' modeler <- rangerRegressorModeler(params = params, target = target, regressors = regressors)
#' #Instantiate cost function
#' cf <- MSECostFunction()
#' #Instantiate sampler
#' set.seed(273)
#' cv_sampler <- cvSampler(key = data.keys, folds = 3)
#' #Define folds to train the modeler
#' data_folds <- getSample(cv_sampler)
#'
#' #Train modeler with different folds and save cost for each
#' cost_list <- list()
#' for (i in 1:dim(data_folds)[2]){
#'   iSample <- data_folds[,i]
#'   iSample <- sort(iSample[!is.na(iSample)])
#'   cSample <- sort(setdiff(cv_sampler$key, iSample))
#'   modeler <- train(modeler, data.dt, iSample)
#'   cost_list[[i]] <- getCost(cf, data.dt, modeler,
#'                             cv_sampler$key, cSample)
#'   modeler <- modelClean(modeler)
#' }
#'
#' #Obtain averaged cost
#' getReducedCost(cf, cost_list)
#'
#'
#' #Usage with crossValidate
#' set.seed(273)
#' crossValidate(modeler = modeler,
#'               data = data.dt,
#'               CVSampler = cv_sampler,
#'               costFunction = cf)
#'
#' @export
getReducedCost <- function(object, data) {
  UseMethod("getReducedCost")
}
#' @rdname getReducedCost
#' @export
getReducedCost.r2CostFunction <- function(object, data) {
  targets <- names(data[[1]])
  output <- lapply(targets, function(x) {
    y <- lapply(data, function(y) y[[x]])
    costs <- sapply(y, function(z) z[["cost"]])
    weights <- sapply(y, function(z) z[["weights"]])
    return(1 - sum((1 - costs) * weights)/sum(weights))
  })
  names(output) <- targets
  return(output)
}

#' @rdname getReducedCost
#' @export
getReducedCost.squareBiasCostFunction <- function(object, data) {
  targets <- names(data[[1]])
  output <- lapply(targets, function(x) {
    y <- lapply(data, function(y) y[[x]])
    costs <- sapply(y, function(z) z[["cost"]])
    #weights <- sapply(y, function(z) z[["weights"]])
    #return(weighted.mean(x = costs**2, w = weights))
    return(mean(costs))
  })
  names(output) <- targets
  return(output)
}

#' @rdname getReducedCost
#' @export
getReducedCost.domainMSECostFunction <- function(object, data) {
  data <- data[["cost"]]
  if (is.null(dim(data)))
    return(mean(data, na.rm = T))
  return(apply(data, 1, mean, na.rm = T))
}

#' @rdname getReducedCost
#' @export
getReducedCost.MSECostFunction <- function(object, data){
  targets <- names(data[[1]])
  output <- lapply(targets, function(x) {
    y <- lapply(data, function(y) y[[x]])
    weights <- lapply(y, function(z) z[["weights"]])
    residuals <- lapply(y, function(z) z[["residuals"]])

    residuals <- Reduce(c, residuals)
    weights <- Reduce(c, weights)

    sme <- weighted.mean(x = residuals, w = weights)

    return(sme)
    #reduced <- sapply(data, function(y) y[[x]])
    #return(mean(reduced))
  })
  names(output) <- targets
  return(output)
}

#' @rdname getReducedCost
#' @export
getReducedCost.groupMSECostFunction <- function(object, data){

  dt <- rbindlist(lapply(1:length(data), function(x) {
    rbindlist(lapply(names(data[[x]]), function(y) {
      data.table(
        fold = x,
        target = y,
        nombre_vector = names(data[[x]][[y]]),
        valor = as.numeric(data[[x]][[y]])
      )
    }))
  }))

  dt_dcasted.dt <- dcast(dt, formula = fold + target ~ nombre_vector, value.var = "valor")
  output.dt <- dt_dcasted.dt[, list(wm = weighted.mean(x = cost, w = size_fold)), by = target]
  output <- as.list(output.dt[, wm])
  names(output) <- output.dt[, target]

  return(output)
}

#' @rdname getReducedCost
#' @export
getReducedCost.groupTotalErrorCostFunction <- function(object, data){
  targets <- names(data[[1]])
  output <- lapply(targets, function(x) {
    y <- lapply(data, function(y) y[[x]])
    cost <- lapply(y, function(z) z[["cost"]])

    costs <- Reduce(c, cost)

    meanTotalError <- mean(costs)

    return(meanTotalError)

  })
  names(output) <- targets
  return(output)
}

#' @rdname getReducedCost
#' @export
getReducedCost.MCSubRBMSECostFunction <- function(object, data){
  targets <- names(data[[1]])
  output <- lapply(targets, function(x) {
    #y <- lapply(data, function(y) y[[x]])
    aggregates <- sapply(data, function(y) y[[x]][["cost"]])
    yRbMC <- mean(aggregates["y1Star", ])
    mseComponents <- aggregates["biasSquared", ] - aggregates["vb", ] +
      aggregates["vb2", ] - (aggregates["y1Star", ] - yRbMC)^2
    bias <- mean(aggregates["bias",])
    mse <- mean(mseComponents)
    mcerror <- sd(mseComponents) / sqrt(length(mseComponents))
    out <- list(yRbMC, bias, mse, mcerror)
    names(out) <- c("estimate", "bias", "mse", "mcerror")
    return(out)
  })
  names(output) <- targets
  return(output)
}

#' @rdname getReducedCost
#' @export
getReducedCost.holdoutTemporalCostFunction <- function(object, data){

  output <- getReducedCost(object$costFunction, data)

  return(output)

}

# Classification cost functions ####
#' @rdname getReducedCost
#'
#' @export
getReducedCost.confusionMatrixCostFunction <- function(object, data){
  #cm_dim <- sqrt(nrow(data[["cost"]]))
  ##data has to be reshaped, as its values are flattened in crossValidate
  #data <- lapply(1:ncol(data), function(i) {
  #  conf_matrix <- matrix(data[, i],
  #                        nrow = cm_dim,
  #                        ncol = cm_dim,
  #                        byrow = TRUE)  # Ensure correct reshaping
  #  if(!is.null(object$class_names)){
  #    rownames(conf_matrix) <- object$class_names
  #    colnames(conf_matrix) <- object$class_names
  #  }
  #  return(conf_matrix)
  #})
  targets <- names(data[[1]])
  output <- lapply(targets, function(x) {
    CMs <- lapply(data, function(y) y[[x]][["cm"]])
    weights <- lapply(data, function(y) y[[x]][["weights"]])
    #Calculate the average confusion matrix maintaining its normalization (if a normalization parameter was provided)
    confusion_matrix <- Reduce(`+`, CMs) / sum(unlist(weights))
    if(!is.null(object$normalize)){
      switch (object$normalize,
              "true" = {confusion_matrix <- prop.table(confusion_matrix, margin = 1)},
              "pred" = {confusion_matrix <- prop.table(confusion_matrix, margin = 2)},
              "all" = {confusion_matrix <- prop.table(confusion_matrix)},
              {stop("The provided normalization parameter does not exist. Please use one of the folloring:\n (true, pred, all)")}
      )
    }
    confusion_matrix[is.nan(confusion_matrix)] <- 0 #Remove NaNs if there were any divisions by zero
    return(confusion_matrix)
  })
  names(output) <- targets
  return(output)
}

#' @rdname getReducedCost
#'
#' @export
#'
getReducedCost.accuracyCostFunction <- function(object, data){
  targets <- names(data[[1]])
  output <- lapply(targets, function(x) {
    CMs <- lapply(data, function(y) y[[x]][["cm"]])
    confusion_matrix <- Reduce(`+`, CMs)

    #Calculate accuracy for total folds (TP+TN)/(TP + TN + FP + FN)
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
    return(score)
  })
  names(output) <- targets
  return(output)
}

#' @rdname getReducedCost
#'
#' @export
#'
getReducedCost.precisionCostFunction <- function(object, data){
  targets <- names(data[[1]])
  output <- lapply(targets, function(x) {
    CMs <- lapply(data, function(y) y[[x]][["cm"]])
    confusion_matrix <- Reduce(`+`, CMs)

    #Calculate precision for total folds TP/(TP+FP)
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
    return(score)
  })
  names(output) <- targets
  return(output)
}

#' @rdname getReducedCost
#'
#' @export
#'
getReducedCost.recallCostFunction <- function(object, data){
  targets <- names(data[[1]])
  output <- lapply(targets, function(x) {
    CMs <- lapply(data, function(y) y[[x]][["cm"]])
    confusion_matrix <- Reduce(`+`, CMs)

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
    return(score)
  })
  names(output) <- targets
  return(output)
}

#' @rdname getReducedCost
#'
#' @export
#'
getReducedCost.f1ScoreCostFunction <- function(object, data){
  targets <- names(data[[1]])
  output <- lapply(targets, function(x) {
    CMs <- lapply(data, function(y) y[[x]][["cm"]])
    confusion_matrix <- Reduce(`+`, CMs)

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
    return(score)
  })
  names(output) <- targets
  return(output)
}

#' @rdname getReducedCost
#'
#' @export
#'
getReducedCost.ROCAUCCostFunction <- function(object, data){
  targets <- names(data[[1]])
  output <- lapply(targets, function(x) {
    #Extract metadata from all folds
    #List of length = n_folds, containing the tpr_fpr metric list for each fold.
    tpr_fprs <- lapply(data, function(y) y[[x]][["class_metrics"]])

    if(object$average != "micro"){
      #Get category levels for current target variable
      categories <- names(tpr_fprs[[1]])

      #We must get the same structure of metrics like in get_cost()
      binarized_scores <- lapply(categories, function(category){
        return(do.call(rbind,lapply(1:length(tpr_fprs), function(fold){
          tpr_fprs[[fold]][[category]][["binarized_scores"]]
        })))
      })
      names(binarized_scores) <- categories

      binarized_true_classes <- lapply(categories, function(category){
        return(unlist(as.vector(sapply(1:length(tpr_fprs), function(fold){
          tpr_fprs[[fold]][[category]][["binarized_true_classes"]]
        }))))
      })
      names(binarized_true_classes) <- categories

      binarized_weights <- lapply(categories, function(category){
        return(unlist(as.vector(sapply(1:length(tpr_fprs), function(fold){
          tpr_fprs[[fold]][[category]][["binarized_weights"]]
        }))))
      })
      names(binarized_weights) <- categories

      #Get thresholds for each class (all folds combined)
      #Binary class thresholds (for each category): Percentile thresholds method
      binary_class_thresholds <- lapply(1:length(categories), function(y){
        #Get 100 points to make it more efficient
        thresholds <- sort(quantile(unique(binarized_scores[[y]], decreasing = TRUE),
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
          y_pred <- factor(class_names[predicted_idx], levels = class_names)

          #Calculate confusion matrix (2x2, since everything is binarized)
          confusion_matrix <- xtabs(binarized_weights[[y]] ~ y_true + y_pred)

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
        support <- sum((binarized_true_classes[[y]] == y) * binarized_weights[[y]])

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
      #Proceed differently for micro average (alike getCost)
      y_true_bin_micro <- do.call(rbind, lapply(1:length(tpr_fprs), function(fold){
        tpr_fprs[[fold]][["y_true_bin_micro"]]
      }))
      score_micro <- do.call(rbind, lapply(1:length(tpr_fprs), function(fold){
        tpr_fprs[[fold]][["score_micro"]]
      }))
      weights_micro <- do.call(rbind, lapply(1:length(tpr_fprs), function(fold){
        tpr_fprs[[fold]][["weights_micro"]]
      }))

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

    return(score)
  })
  names(output) <- targets
  return(output)
}


#' @rdname getReducedCost
#' @export
getReducedCost.groupTotalErrorCostFunction <- function(object, data){
  targets <- names(data[[1]])
  output <- lapply(targets, function(x) {
    y <- lapply(data, function(y) y[[x]])
    cost <- lapply(y, function(z) z[["cost"]])

    costs <- Reduce(c, cost)

    meanTotalError <- mean(costs)

    return(meanTotalError)

  })
  names(output) <- targets
  return(output)
}
