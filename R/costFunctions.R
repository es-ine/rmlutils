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

#' @name costFunction
#' @title A collection of class constructors for several cost functions.
#'
#' @description Creates a costFunction object as specified.
#'
#' The cost function classes contain different types of cost functions
#' to provide a commom interface for all of them.
#'
#'
#' @section Current implementations:
#'
#' \strong{Regression:}
#' \itemize{
#' \item \code{\link{r2CostFunction}}
#' \item \code{\link{squareBiasCostFunction}}
#' \item \code{\link{MSECostFunction}}
#' \item \code{\link{groupMSECostFunction}}
#' \item \code{\link{groupTotalErrorCostFunction}}
#' \item \code{\link{MCSubRBMSECostFunction}}
#' \item \code{\link{holdoutTemporalCostFunction}}
#' }
#'
#' \strong{Classification:}
#' \itemize{
#' \item \code{\link{confusionMatrixCostFunction}}
#' \item \code{\link{accuracyCostFunction}}
#' \item \code{\link{precisionCostFunction}}
#' \item \code{\link{recallCostFunction}}
#' \item \code{\link{f1ScoreCostFunction}}
#' \item \code{\link{ROCAUCCostFunction}}
#' }
#'
#'
NULL

# Regression cost functions ####

#' @title R^2 cost function
#'
#' @name r2CostFunction
#'
#' @description A costFunction object that outputs R^2 as a performance metric for a \code{\link{regressorModeler}}.
#' Like any other cost function, it must be used together with the \code{\link{getCost}} function to output its metric.
#'
#' @param weightsColumn Name of the weights column in the data. Set as "" by default.
#'
#' @export
#'
#' @seealso \code{\link{costFunction}}
#' @seealso \code{\link{getCost}}
#'
#' @examples
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
#'   "shrinkage_rate" = 0.12,
#'   "nrounds" = 10,#Avoid warning
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
r2CostFunction <- function(weightsColumn = "") {
  object <- list(weightsColumn = weightsColumn)
  class(object) <- c("r2CostFunction", "costFunction")
  return(object)
}

#' @title Squared bias cost function
#'
#' @name squareBiasCostFunction
#'
#' @description A costFunction object that outputs the squared bias as a performance metric for a \code{\link{regressorModeler}}.
#' Like any other cost function, it must be used together with the \code{\link{getCost}} function to output its metric.
#'
#' @param weightsColumn Name of the weights column in the data. Set as "" by default
#'
#' @export
#'
#' @seealso \code{\link{costFunction}}
#' @seealso \code{\link{getCost}}
#'
#' @examples
#' #Generate data
#' data.dt <- generateRegressionData(n_samples = 1000, generate_random_weights = FALSE)
#'
#' #Instantiate cost function
#' cf <- squareBiasCostFunction(weightsColumn = "")
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
squareBiasCostFunction <- function(weightsColumn = "") {
  object <- list(weightsColumn = weightsColumn)
  class(object) <- c("squareBiasCostFunction", "costFunction")
  return(object)
}

#' @title MSE cost function
#'
#' @name MSECostFunction
#'
#' @description A costFunction object that outputs the mean squared error as a performance metric for a \code{\link{regressorModeler}}.
#' Like any other cost function, it must be used together with the \code{\link{getCost}} function to output its metric.
#'
#' @param weightsColumn Name of the weights column in the data. Set as "" by default.
#'
#' @export
#'
#' @seealso \code{\link{costFunction}}
#' @seealso \code{\link{getCost}}
#'
#' @param weightsColumn Name of the weights column in the data. Set as "" by default.
#'
#' @examples
#' #Generate data
#' data.dt <- generateRegressionData(n_samples = 1000, generate_random_weights = FALSE)
#'
#' #Instantiate cost function
#' cf <- MSECostFunction(weightsColumn = "")
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
MSECostFunction <- function(weightsColumn = "") {
  object <- list(weightsColumn = weightsColumn)
  class(object) <- c("MSECostFunction", "costFunction")
  return(object)
}

#' @title Group MSE cost function
#'
#' @name groupMSECostFunction
#'
#' @description
#' A cost function that calculates the weighted MSE of a function of each group element.
#' That is, after predicting, each group is converted into a single metric,
#' such as a mean or a sum, and the MSE is calculated comparing the average/sum/other transform
#' of the predictions and original values.
#'
#' Afterwards, all MSEs are averaged with the data weights (if they exist).
#'
#' @param weightsColumn Name of the weights column in the data. "" by default.
#' @param groupWeightsFunction Function to average/combine weights for an entire group.
#' @param groupFunction Function to average/combine target values for an entire group.
#' @param groupColumn Name of the group-defining column in the data, sush as a strata or a domain.
#'
#' @seealso \code{\link{costFunction}}
#' @seealso \code{\link{getCost}}
#'
#' @examples
#' #Generate data
#' data.dt <- generateRegressionData(n_samples = 1000)
#'
#' data.keys <- 1:nrow(data.dt)
#' train_size = round(0.8 * nrow(data.dt))
#'
#' #Split train/test #
#' sampler <- srsSampler(data.keys, ssize = train_size)
#'
#' train.keys <- getSample(sampler)
#' test.keys <- setdiff(data.keys, train.keys)
#'
#' train.dt <- data.dt[train.keys]
#' test.dt <- data.dt[test.keys]
#'
#' #Define modeler
#' target <- "regression_target"
#' regressors <- setdiff(names(data.dt), target)
#' params <- list(
#'   "learning_rate" = 0.07,
#'   "nrounds" = 10,
#'   "objective" = "regression"
#' )
#'
#' regressor <- LGBRegressorModeler(params = params, target = target, regressors = regressors)
#' regressor <- train(regressor, data = train.dt)
#'
#' #Define cost function and get cost
#'
#' cf <- groupMSECostFunction(weightsColumn = "weights",
#'                            groupWeightsFunction = sum,
#'                            groupColumn = "cat_1",
#'                            groupFunction = mean)
#'
#' cost <- getCost(object = cf, data = data.dt, modeler = regressor, cSample = test.keys)
#' print(cost)
#'
#' @export
groupMSECostFunction <- function(weightsColumn, groupWeightsFunction, groupFunction, groupColumn) {
  UseMethod("groupMSECostFunction")
}

#' @rdname groupMSECostFunction
#' @export
groupMSECostFunction.character <- function(weightsColumn, groupWeightsFunction, groupFunction, groupColumn) {
  object <- list(weightsColumn = weightsColumn,
                 groupWeightsFunction = groupWeightsFunction,
                 groupFunction = groupFunction,
                 groupColumn = groupColumn)
  class(object) <- c("groupMSECostFunction", "costFunction")
  return(object)
}

#' @title Group absolute error cost function
#'
#' @name groupTotalErrorCostFunction
#'
#' @description
#' A cost function that calculates the weighted absolute error by group, for each
#' group element, and then combines it to obtain the total absolute prediction error.
#'
#' @param weightsColumn Name of the weights column in the data. "" by default.
#' @param groupVars Vector containing the variables to group.
#'
#' @seealso \code{\link{costFunction}}
#' @seealso \code{\link{getCost}}
#'
#' @examples
#' #Generate data
#' data.dt <- generateStratifiedDomainModelAssistedEstimateData(n_samples = 1000,
#'                                                              generate_random_weights = TRUE)
#' data.keys <- 1:nrow(data.dt)
#' set.seed(273)
#'
#' data.keys <- 1:nrow(data.dt)
#' train_size = round(0.8 * nrow(data.dt))
#'
#' #Split train/test
#' sampler <- srsSampler(data.keys, ssize = train_size)
#'
#' train.keys <- getSample(sampler)
#' test.keys <- setdiff(data.keys, train.keys)
#'
#' train.dt <- data.dt[train.keys]
#' test.dt <- data.dt[test.keys]
#'
#' #Define modeler
#' target <- "target"
#' regressors <- setdiff(names(data.dt), target)
#' params <- list(
#'   "num.trees" = 50,
#'   "max.depth" = 20
#' )
#'
#' modeler <- rangerRegressorModeler(params = params, target = target, regressors = regressors)
#' modeler <- train(modeler, data = train.dt)
#'
#' cf <- groupTotalErrorCostFunction(weightsColumn = "weights",
#'                                   groupVars = c("strata_class", "domain_class"))
#'
#' cost <- getCost(object = cf,
#'                 data = data.dt,
#'                 modeler = modeler,
#'                 cSample = test.keys)
#' print(cost)
#'
#' @export
groupTotalErrorCostFunction <- function(weightsColumn, groupVars) {
  UseMethod("groupTotalErrorCostFunction")
}


#' @rdname groupTotalErrorCostFunction
#' @export
groupTotalErrorCostFunction.character <- function(weightsColumn, groupVars) {
  object <- list(weightsColumn = weightsColumn,
                 groupVars = groupVars)
  class(object) <- c("groupTotalErrorCostFunction", "costFunction")
  return(object)
}

#' @title Mean squared error of the MCSubRBEstimator
#'
#' @name MCSubRBMSECostFunction
#'
#' @description
#' A cost function that outputs the estimator, bias MSE and Monte Carlo error of
#' the estimator based on Zhang, L.-C., Sanguiao-Sande, L., & Lee, D.
#' (2025). Design-Based Predictive Inference. Journal of Official Statistics,
#' 41(1), 404-432. \url{https://journals.sagepub.com/doi/10.1177/0282423X241277719}
#' It is the implementation of theorem 1.
#'
#'
#' @param sampler A sampler that has been used to generate a sample over the data.
#' @param sampleKeys Set of keys that was used to define the train data for a model.
#' @param nonSampleData Complementary data (test) that the model has not seen.
#'
#' @seealso \code{\link{costFunction}}
#' @seealso \code{\link{getCost}}
#'
#' @examples
#' data.dt <- generateStratifiedDomainModelAssistedEstimateData(n_samples = 1000)
#'
#' set.seed(123)
#'
#' data.keys <- 1:nrow(data.dt)
#' train_size = round(0.8 * nrow(data.dt))
#'
#' #Split train
#' sampler <- srsSampler(data.keys, ssize = train_size)
#'
#' SKeys <- getSample(sampler)
#'
#' train.dt <- data.dt[SKeys]
#'
#' #Define modeler
#' target <- "target"
#' regressors <- setdiff(names(data.dt), target)
#' params <- list(
#'   "num.trees" = 50,
#'   "max.depth" = 20
#' )
#'
#' regressor <- rangerRegressorModeler(params = params, target = target, regressors = regressors)
#' regressor <- train(regressor, data = train.dt)
#'
#' #Define cost function and get cost
#' cf <- MCSubRBMSECostFunction(sampler, SKeys, nonSampleData = data.dt[-SKeys,])
#'
#' crossValidate(regressor,
#'               data.dt[SKeys,],
#'               srsSampler(1:length(SKeys),150),#150 subsamples
#'               cf,
#'               times = 100)
#'
#'
#' @export
MCSubRBMSECostFunction <- function(sampler, sampleKeys, nonSampleData) {
  UseMethod("MCSubRBMSECostFunction")
}

#' @rdname MCSubRBMSECostFunction
#' @export
MCSubRBMSECostFunction.sampler <- function(sampler, sampleKeys, nonSampleData) {
  object <- list(sampler=sampler, sampleKeys = sampleKeys,
                 nonSampleData=nonSampleData)
  class(object) <- c("MCSubRBMSECostFunction", "costFunction")
  return(object)
}

#' @title Holdout temporal cost function
#'
#' @name holdoutTemporalCostFunction
#'
#' @description
#' A complex costFunction that takes a simpler costFunction and combines it with
#' a \code{\link{cvTemporalSampler}}, useful for evaluating data with some sort
#' of temporal dependence.
#'
#' @param costFunction A costFunction object.
#' @param cvTemporalSampler A cvTemporalSampler object.
#'
#' @seealso \code{\link{costFunction}}
#' @seealso \code{\link{getCost}}
#'
#' @examples
#' #Define a dataset with 36 periods (months)
#' years <- c(2023, 2024, 2025)
#' total_periods <- c(sapply(years, function(y){
#'   months <- 1:12
#'   months[1:9] <- paste0("0", months[1:9])
#'   return(paste0("MM", months, y))
#' }, USE.NAMES = FALSE))
#'
#' #Generate data for each period
#' data.dt <- rbindlist(lapply(total_periods, function(p){
#'   dt <- generateRegressionData(n_samples = 100,
#'                                generate_random_weights = FALSE)
#'   dt[, period := p]
#'   return(dt)
#' }))
#' head(data.dt)
#'
#' smplr <- cvTemporalSampler(key = 1:nrow(data.dt),
#'                            data = data.dt,
#'                            identifier = "period",
#'                            folds = 5,
#'                            n_valPeriods = 6,#Require 2 months for validation
#'                            orderPeriods = total_periods)
#'
#' #Define modeler and train it
#' params <- list(
#'   "num.trees" = 50,
#'   "max.depth" = 20
#' )
#' target <- "regression_target"
#' regressors <- setdiff(colnames(data.dt), target)
#'
#' modeler <- rangerRegressorModeler(params = params,
#'                                   target = target,
#'                                   regressors = regressors)
#'
#' modeler <- train(modeler, data = data.dt)
#'
#' #Define holdoutTemporalCostFunction
#' cf <- holdoutTemporalCostFunction(costFunction = MSECostFunction(),
#'                                   cvTemporalSampler = smplr)
#'
#' cost <- getCost(object = cf,
#'                 data = data.dt,
#'                 modeler = modeler,
#'                 cSample = 1:nrow(data.dt))
#' print(cost$regression_target$cost)
#'
#' cost <- crossValidate(modeler = modeler,
#'                       data = data.dt,
#'                       CVSampler = cvSampler(key = data.keys, folds = 3),
#'                       costFunction = cf)
#' print(cost)
#'
#'
#' @export
holdoutTemporalCostFunction <- function(costFunction, cvTemporalSampler) {
  UseMethod("holdoutTemporalCostFunction")
}

#' @rdname holdoutTemporalCostFunction
#' @export
holdoutTemporalCostFunction.costFunction <- function(costFunction, cvTemporalSampler){

  object <- list(costFunction = costFunction,
                 cvTemporalSampler = cvTemporalSampler)

  class(object) <- c("holdoutTemporalCostFunction", "costFunction")
  return(object)
}

#If it ever gets developed, remember to replace @noRd with @export
#' @title Rolling window temporal cost function
#'
#' @name rollingWindowTemporalCostFunction
#'
#' @description
#' Placeholder text
#' @seealso \code{\link{costFunction}}
#' @seealso \code{\link{getCost}}
#' @noRd
rollingWindowTemporalCostFunction <- function(costFunction, cvTemporalSampler) {
  UseMethod("rollingWindowTemporalCostFunction")
}


#' @rdname rollingWindowTemporalCostFunction
#' @noRd
rollingWindowTemporalCostFunction.costFunction <- function(costFunction, cvTemporalSampler){

  object <- list(costFunction = costFunction,
                 cvTemporalSampler = cvTemporalSampler)

  class(object) <- c("rollingWindowTemporalCostFunction", "costFunction")
  return(object)
}

# Classification cost functions ####

#' @title Confusion matrix cost function
#'
#' @description A costFunction object that outputs a confusion matrix as a performance metric for a \code{\link{classifierModeler}}.
#' Like any other cost function, it must be used together with the \code{\link{getCost}} function to output its metric.
#'
#' @param weightsColumn Name of the weights column in the data
#' @param class_map \code{\link{classMapping}} object to use (\code{\link{maxScoreClass}} by default)
#' @param normalize String indicating how to normalize the matrix. Possible values are:
#'
#' \itemize{
#'   \item "true": normalize by true labels (rows)
#'   \item "pred" : normalize by predicted labels (columns)
#'   \item "all": normalize by total samples
#'   \item NULL: no normalization (default)
#' }
#'
#' @param class_names Vector containing the levels of the categorical variable. This only affects the output of the getReducedCost method.
#'
#' @return Named list with all relevant cost metrics.
#'
#' @export
#'
#' @seealso \code{\link{costFunction}}
#' @seealso \code{\link{getCost}}
#'
#' @examples
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
#'   "nrounds" = 10,#Avoid warning
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
confusionMatrixCostFunction <- function(weightsColumn = "", class_map = maxScoreClass(), normalize = NULL, class_names = NULL){
  object <- list(weightsColumn = weightsColumn, class_map = class_map, normalize = normalize, class_names = class_names)
  class(object) <- c("confusionMatrixCostFunction", "costFunction")
  return(object)
}

#' @title Accuracy cost function
#'
#' @description A costFunction object that outputs accuracy \eqn{(TP+TN)/(TP+FP+TN+FN)} as a performance metric for a \code{\link{classifierModeler}}.
#' Like any other cost function, it must be used together with the \code{\link{getCost}} function to output its metric.
#'
#' @param weightsColumn Name of the weights column in the data
#' @param class_map \code{\link{classMapping}} object to use (\code{\link{maxScoreClass}} by default)
#' @param average String indicating the averaging method. This will generally only affect the multiclass case. Possible values are:
#'
#' \itemize{
#'  \item "micro": Global metric average (weighted by sample weights, if provided).
#'  \item "macro": Average class metric (weighted by sample weights, if provided).
#'  \item "weighted": Average class metric (weighted by sample weights, if provided, and number of times that each class appears in the dataset).
#' }
#'
#' @export
#'
#' @seealso \code{\link{costFunction}}
#' @seealso \code{\link{getCost}}
#'
#' @examples
#' #Generate data
#' data.dt <- generateMulticlassClassificationData(n_samples = 1000)
#' #Instantiate cost function
#' cf <- accuracyCostFunction(weightsColumn = "", class_map = maxScoreClass(), average = "micro")
#'
#' #Train simple modeler
#' target <- "classification_target"
#' regressors <- setdiff(names(data.dt), target)
#' params <- list(
#'   "learning_rate" = 0.07,
#'   "shrinkage_rate" = 0.12,
#'   "nrounds" = 10,#Avoid warning
#'   "objective" = "multiclass",#Works better this way with RMLutils
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
accuracyCostFunction <- function(weightsColumn = "", class_map = maxScoreClass(), average = "micro"){
  match.arg(average, choices = c("micro","macro","weighted"))
  object <- list(weightsColumn = weightsColumn, class_map = class_map, average = average)
  class(object) <- c("accuracyCostFunction", "costFunction")
  return(object)
}

#' @title Precision cost function
#'
#' @description A costFunction object that outputs precision \eqn{TP/(TP+FP)} as a performance metric for a \code{\link{classifierModeler}}.
#' Like any other cost function, it must be used together with the \code{\link{getCost}} function to output its metric.
#'
#' @param weightsColumn Name of the weights column in the data.
#' @param class_map \code{\link{classMapping}} object to use (\code{\link{maxScoreClass}} by default).
#' @param average String indicating the averaging method. This will generally only affect the multiclass case. Possible values are:
#'
#' \itemize{
#'  \item "micro": Global metric average (weighted by sample weights, if provided).
#'  \item "macro": Average class metric (weighted by sample weights, if provided).
#'  \item "weighted": Average class metric (weighted by sample weights, if provided, and number of times that each class appears in the dataset).
#' }
#'
#' @export
#'
#' @seealso \code{\link{costFunction}}
#' @seealso \code{\link{getCost}}
#'
#' @examples
#' #Generate data
#' data.dt <- generateMulticlassClassificationData(n_samples = 1000)
#' #Instantiate cost function
#' cf <- precisionCostFunction(weightsColumn = "", class_map = maxScoreClass(), average = "micro")
#'
#' #Train simple modeler
#' target <- "classification_target"
#' regressors <- setdiff(names(data.dt), target)
#' params <- list(
#'   "learning_rate" = 0.07,
#'   "shrinkage_rate" = 0.12,
#'   "nrounds" = 10,#Avoid warning
#'   "objective" = "multiclass",#Works better this way with RMLutils
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
precisionCostFunction <- function(weightsColumn = "", class_map = maxScoreClass(), average = "micro"){
  match.arg(average, choices = c("micro","macro","weighted"))
  object <- list(weightsColumn = weightsColumn, class_map = class_map, average = average)
  class(object) <- c("precisionCostFunction", "costFunction")
  return(object)
}

#' @title Recall cost function
#'
#' @description A costFunction object that outputs recall \eqn{TP/(TP+FN)} as a performance metric for a \code{\link{classifierModeler}}.
#' Like any other cost function, it must be used together with the \code{\link{getCost}} function to output its metric.
#'
#' @param weightsColumn Name of the weights column in the data.
#' @param class_map \code{\link{classMapping}} object to use (\code{\link{maxScoreClass}} by default).
#' @param average String indicating the averaging method. This will generally only affect the multiclass case. Possible values are:
#'
#' \itemize{
#'  \item "micro": Global metric average (weighted by sample weights, if provided).
#'  \item "macro": Average class metric (weighted by sample weights, if provided).
#'  \item "weighted": Average class metric (weighted by sample weights, if provided, and number of times that each class appears in the dataset).
#' }
#'
#' @export
#'
#' @seealso \code{\link{costFunction}}
#' @seealso \code{\link{getCost}}
#'
#' @examples
#' #Generate data
#' data.dt <- generateMulticlassClassificationData(n_samples = 1000)
#' #Instantiate cost function
#' cf <- recallCostFunction(weightsColumn = "", class_map = maxScoreClass(), average = "micro")
#'
#' #Train simple modeler
#' target <- "classification_target"
#' regressors <- setdiff(names(data.dt), target)
#' params <- list(
#'   "learning_rate" = 0.07,
#'   "shrinkage_rate" = 0.12,
#'   "nrounds" = 10,#Avoid warning
#'   "objective" = "multiclass",#Works better this way with RMLutils
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
recallCostFunction <- function(weightsColumn = "", class_map = maxScoreClass(), average = "micro"){
  match.arg(average, choices = c("micro","macro","weighted"))
  object <- list(weightsColumn = weightsColumn, class_map = class_map, average = average)
  class(object) <- c("recallCostFunction", "costFunction")
  return(object)
}

#' @title F1 score cost function
#'
#' @description A costFunction object that outputs the F1-score as a performance metric for a \code{\link{classifierModeler}}.
#' Like any other cost function, it must be used together with the \code{\link{getCost}} function to output its metric.
#'
#' @param weightsColumn Name of the weights column in the data
#' @param class_map \code{\link{classMapping}} object to use (\code{\link{maxScoreClass}} by default).
#' @param average String indicating the averaging method. This will generally only affect the multiclass case. Possible values are:
#'
#' \itemize{
#'  \item "micro": Global metric average (weighted by sample weights, if provided).
#'  \item "macro": Average class metric (weighted by sample weights, if provided).
#'  \item "weighted": Average class metric (weighted by sample weights, if provided, and number of times that each class appears in the dataset).
#' }
#'
#' @export
#'
#' @seealso \code{\link{costFunction}}
#' @seealso \code{\link{getCost}}
#'
#' @examples
#' #Generate data
#' data.dt <- generateMulticlassClassificationData(n_samples = 1000)
#' #Instantiate cost function
#' cf <- f1ScoreCostFunction(weightsColumn = "", class_map = maxScoreClass(), average = "micro")
#'
#' #Train simple modeler
#' target <- "classification_target"
#' regressors <- setdiff(names(data.dt), target)
#' params <- list(
#'   "learning_rate" = 0.07,
#'   "shrinkage_rate" = 0.12,
#'   "nrounds" = 10,#Avoid warning
#'   "objective" = "multiclass",#Works better this way with RMLutils
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
f1ScoreCostFunction <- function(weightsColumn = "", class_map = maxScoreClass(), average = "micro"){
  match.arg(average, choices = c("micro","macro","weighted"))
  object <- list(weightsColumn = weightsColumn, class_map = class_map, average = average)
  class(object) <- c("f1ScoreCostFunction", "costFunction")
  return(object)
}

#' @title Receiver Operating Characteristic Area Under Curve (ROC AUC) cost function
#'
#' @description A costFunction object that outputs the ROC-AUC score as a performance metric for a \code{\link{classifierModeler}}.
#' Like any other cost function, it must be used together with the \code{\link{getCost}} function to output its metric.
#'
#'
#' @param weightsColumn Name of the weights column in the data
#' @param average String indicating the averaging method. This will generally only affect the multiclass case. Possible values are:
#'
#' \itemize{
#'  \item "micro": Global metric average (weighted by sample weights, if provided).
#'  \item "macro": Average class metric (weighted by sample weights, if provided).
#'  \item "weighted": Average class metric (weighted by sample weights, if provided, and number of times that each class appears in the dataset).
#' }
#'
#' @export
#'
#' @seealso \code{\link{costFunction}}
#' @seealso \code{\link{getCost}}
#'
#' @examples
#' #Generate data
#' data.dt <- generateMulticlassClassificationData(n_samples = 1000)
#' #Instantiate cost function
#' cf <- ROCAUCCostFunction(weightsColumn = "", average = "micro")
#'
#' #Train simple modeler
#' target <- "classification_target"
#' regressors <- setdiff(names(data.dt), target)
#' params <- list(
#'   "learning_rate" = 0.07,
#'   "shrinkage_rate" = 0.12,
#'   "nrounds" = 10,#Avoid warning
#'   "objective" = "multiclass",#Works better this way with RMLutils
#'   "num_class" = length(unique(data.dt[, classification_target]))
#' )
#'
#' classifier <- LGBClassifierModeler(params = params, target = target, regressors = regressors)
#' classifier <- train(classifier, data = data.dt)
#'
#' #Evaluate with getCost
#' cost <- getCost(object = cf, data = data.dt, modeler = classifier, cSample = 1:nrow(data.dt))
#' print(cost$classification_target$cost)
#' head(cost$classification_target$class_metrics$tpr)
#' head(cost$classification_target$class_metrics$fpr)
#'
ROCAUCCostFunction <- function(weightsColumn = "", average = "micro"){
  match.arg(average, choices = c("micro","macro","weighted"))
  object <- list(weightsColumn = weightsColumn, class_map = RMLutils::binaryThresholdClass(), average = average)
  class(object) <- c("ROCAUCCostFunction", "costFunction")
  return(object)
}
