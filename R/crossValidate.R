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

#' @title Model cross validation.
#'
#' @description Computes the cross validation from a modeler.
#' Given a cross-validation sample it trains an independent model for each of the
#' k-folds. It then computes its corresponding cost function and stores it in a list.
#' The list is then used to compute the reduced cost and therefore returns a simple scalar
#' instead of a list.
#'
#' @param modeler \code{\link{modeler}} object with the untrained model.
#' @param data Datatable with the dataset to use during cross-validation.
#' @param CVSampler A cross-validation type \code{\link{sampler}}.
#' @param costFunction A costFunction object from \code{\link{costFunction}}
#' @param times Amount of times to sample (1 by default)
#'
#' @return The reduced cost function computed for the cross-validation.
#'
#' @seealso \code{\link{expandHyperParams}}
#'
#' @examples
#' #Generate data
#' data.dt <- generateRegressionData(n_samples = 10000, generate_random_weights = FALSE)
#'
#' #Get relevant variables from data
#' data.keys <- 1:nrow(data.dt)
#' target <- "regression_target"# Regression
#' regressors <- setdiff(names(data.dt), target)
#'
#' #Define modeler
#' params <- list(
#'   "learning_rate" = 0.07,
#'   "nrounds" = 10,
#'   "objective" = "regression"
#' )
#'
#' modeler <- LGBRegressorModeler(params = params, target = target, regressors = regressors)
#'
#' #Instantiate cost function
#' cf <- MSECostFunction()
#'
#' #Instantiate sampler
#' cv_sampler <- cvSampler(key = data.keys, folds = 3)
#'
#' set.seed(273)
#' reduced_cost <- crossValidate(modeler = modeler,
#'                              data = data.dt,
#'                              CVSampler = cv_sampler,
#'                              costFunction = cf)
#' print(reduced_cost)
#'
#' @export
crossValidate <- function(modeler, data, CVSampler, costFunction, times = 1) {
  UseMethod("crossValidate")
}
#' @rdname crossValidate
#' @export
crossValidate.modeler <- function(modeler, data, CVSampler, costFunction,
                                  times = 1) {
  samples <- getSample(CVSampler, times = times)
  output <- apply(samples, 2, function(x) {
                                iSample <- x
                                iSample <- sort(iSample[!is.na(iSample)])
                                cSample <- sort(setdiff(CVSampler$key, iSample))
                                modeler <- train(modeler, data, iSample)
                                output <- getCost(costFunction, data, modeler,
                                                   CVSampler$key, cSample)
                                modeler <- modelClean(modeler)
                                return(output)
  })

  return(getReducedCost(costFunction, output))
}
