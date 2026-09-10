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

#' @name sampler
#' @title A collection of class constructors for several sampler classes.
#'
#' @description Creates a sampler object as specified.
#'
#' The sampler classes contain different types of sampling schemes to
#' provide a common interface for all of them.
#'
#' @return A sampler object.
#'
#' @section Current implementations:
#'
#' \itemize{
#' \item \code{\link{srsSampler}}
#' \item \code{\link{ssSampler}}
#' \item \code{\link{bootstrapSampler}}
#' \item \code{\link{fullSampler}}
#' \item \code{\link{cvSampler}}
#' \item \code{\link{cvGroupSampler}}
#' \item \code{\link{cvMixedSampler}}
#' \item \code{\link{cvTemporalSampler}}
#' }
#'
#'
NULL

#' @name srsSampler
#'
#' @title Simple random sampler
#'
#' @description
#' Implementation of simple random sampling without replacement.
#'
#' @param key Vector of indices of the data entries (rows) to sample from.
#' @param ssize Amount of samples to be taken.
#'
#' @return A srsSampler object.
#'
#' @seealso \code{\link{sampler}}
#' @seealso \code{\link{ssSampler}}
#' @seealso \code{\link{bootstrapSampler}}
#' @seealso \code{\link{cvSampler}}
#' @seealso \code{\link{fullSampler}}
#'
#' @examples
#' set.seed(273)
#' #Define data keys
#' sample.keys <- 1:15
#' #Define sampler
#' sampler <- srsSampler(key = sample.keys, ssize = 10)
#' #Obtain a sample
#' getSample(sampler)
#'
#' @export
srsSampler <- function(key, ssize) {
  object <- list(key = key, ssize = ssize)
  class(object) <- c("srsSampler", "sampler")
  return(object)
}

#' @name ssSampler
#'
#' @title Stratified simple random sampler
#'
#' @description
#' Implementation of stratified random sampling without replacement. The user must manually specify how many units to sample from each strata.
#'
#' @param key Vector of indices of the data entries (rows) to sample from.
#' @param strata List of strata corresponding to each of the entries to sample from.
#' @param ssize Named vector indicating the amount of samples to take from each strata.
#'
#' @return A ssSampler object.
#'
#' @seealso \code{\link{sampler}}
#' @seealso \code{\link{srsSampler}}
#' @seealso \code{\link{bootstrapSampler}}
#' @seealso \code{\link{cvSampler}}
#' @seealso \code{\link{fullSampler}}
#'
#' @examples
#' set.seed(273)
#' #Generate data and define keys
#' #This data has two strata: Urban and Rural
#' data.dt <- generateStratifiedDomainEstimateData(n_samples = 100)
#' key <- 1:nrow(data.dt)
#' head(data.dt)
#' smplr <- ssSampler(key = key,
#'                    strata = data.dt[, strata_class],
#'                    ssize = c("Urban" = 20, "Rural" = 15))
#' s1 <- getSample(smplr)
#' print(table(data.dt[s1, strata_class]))
#' print(data.dt[s1])
#'
#'
#' @export
ssSampler <- function(key, strata, ssize) {
  object <- list(key = key, strata = strata, ssize = ssize)
  class(object) <- c("ssSampler", "sampler")
  return(object)
}

#' @name bootstrapSampler
#'
#' @title Bootstrap sampler
#'
#' @description
#' Bootstrap sampler implementation. Given a list or array of indices,
#' a sample with replacement is performed (its size is set to match the key size parameter).
#'
#' @param key Vector of indices of the data entries (rows) to sample from.
#'
#' @return A bootstrapSampler object.
#'
#' @seealso \code{\link{sampler}}
#' @seealso \code{\link{srsSampler}}
#' @seealso \code{\link{ssSampler}}
#' @seealso \code{\link{cvSampler}}
#' @seealso \code{\link{fullSampler}}
#'
#' @examples
#' set.seed(273)
#' #Define data and key
#' data.dt <- generateStratifiedDomainEstimateData(n_samples = 10)
#' key <- 1:nrow(data.dt)
#' head(data.dt)
#' smplr <- bootstrapSampler(key)
#' s1 <- getSample(smplr)
#' print(data.dt[s1])
#'
#' @export
bootstrapSampler <- function(key) {
  object <- list(key = key)
  class(object) <- c("bootstrapSampler", "sampler")
  return(object)
}

#' @name fullSampler
#'
#' @title Full sampler
#'
#' @description
#' A toy sampler that returns the full set of keys or all possible combinations
#' (by pairs) of such keys, as long as the ssize parameter is set to 2.
#'
#' @param key Vector of indices of the data entries (rows) to sample from.
#' @param ssize Integer indicating whether to return all possible combinations
#' of indices (2) or simply the total original indices (ssize != 2).
#'
#' @return A fullSampler object.
#'
#' @seealso \code{\link{sampler}}
#' @seealso \code{\link{srsSampler}}
#' @seealso \code{\link{ssSampler}}
#' @seealso \code{\link{cvSampler}}
#' @seealso \code{\link{bootstrapSampler}}
#'
#' @examples
#' key <- 1:6
#'
#' smplr1 <- fullSampler(key = key, ssize = 2)
#'
#' s1 <- getSample(smplr1)
#' print(s1)
#'
#' smplr2 <- fullSampler(key = key)
#'
#' s2 <- getSample(smplr2)
#' print(s2)
#'
#' @export
fullSampler <- function(key, ssize = 1) {
  object <- list(key = key, ssize = ssize)
  class(object) <- c("fullSampler", "sampler")
  return(object)
}

#' @name cvSampler
#'
#' @title Cross-Validation sampler
#'
#' @description Implementation of cross-validation sampling without replacement.
#' This sampler generates a total of k_fold samples, each one of them excluding one of the defined folds.
#' If the key vector is not a multiple of the folds parameter, some partitions will contain NA.
#'
#' @param key Vector of indices of the data entries (rows) to sample from.
#' @param folds Number of k-folds to apply.
#'
#' @return A cvSampler object.
#'
#' @seealso \code{\link{sampler}}
#' @seealso \code{\link{cvGroupSampler}}
#' @seealso \code{\link{cvMixedSampler}}
#' @seealso \code{\link{cvTemporalSampler}}
#'
#' @examples
#' set.seed(273)
#' #Define key vector
#' data.dt <- generateStratifiedDomainEstimateData(n_samples = 50)
#' key <- 1:nrow(data.dt)
#' smplr <- cvSampler(key = key, folds = 5)
#' s1 <- getSample(smplr)
#'
#' print(s1)
#'
#' print(data.dt[s1[,1]])
#'
#' @export
cvSampler <- function(key, folds) {
  object <- list(key = key, folds = folds)
  class(object) <- c("cvSampler", "sampler")
  object <- RMLutils:::.validate_cvSampler(object)
  return(object)
}

#' @name cvGroupSampler
#'
#' @title Grouped cross-validation sampler
#'
#' @description
#' A cross-validation sampler that instead of randomly partitioning the data,
#' defines its folds according to a grouping variable, where each fold will
#' exclude a certain amount of groups.
#'
#' @param key Vector of indices of the data entries (rows) to sample from.
#' @param data Dataset containing the identifier-group column.
#' @param identifier String indicating the name of the group column.
#' @param folds Number of k-folds to apply.
#'
#' @return A cvGroupSampler object.
#'
#' @seealso \code{\link{sampler}}
#' @seealso \code{\link{cvSampler}}
#' @seealso \code{\link{cvMixedSampler}}
#' @seealso \code{\link{cvTemporalSampler}}
#'
#' @examples
#' set.seed(273)
#' #Define data (10 groups, 10 rows for each group)
#' id_var <- c(sapply(paste0("Group_", 1:10), function(x){
#' return(rep(x, 10))
#' }, USE.NAMES = FALSE))
#' measure <- rnorm(n = length(id_var), mean = 10, sd = 1)
#' data.dt <- data.table(measure = measure, id_var = id_var)
#'
#' #Instantiate sampler
#' key <- 1:nrow(data.dt)
#' identifier <- "id_var"
#' smplr <- cvGroupSampler(key = key,
#'                         data = data.dt,
#'                         identifier = identifier,
#'                         folds = 3)
#' #As it can be seen, the data partitions contain all rows from the selected
#' #groups
#' s1 <- getSample(smplr)
#' print(s1)
#' table(data.dt[s1[, 1], id_var])
#' table(data.dt[s1[, 2], id_var])
#' table(data.dt[s1[, 3], id_var])
#'
#' @export
cvGroupSampler <- function(key, data, identifier, folds) {
  object <- list(key = key, data = data, identifier = identifier, folds = folds)
  class(object) <- c("cvGroupSampler", "sampler")
  object <- RMLutils:::.validate_cvSampler(object)
  return(object)
}

#' @name cvMixedSampler
#'
#' @title Mixed data cross-validation sampler
#'
#' @description
#' A cross-validation sampler designed to always include a fixed set of rows
#' in its samples, alongside a shuffled mixture of other samples that do not
#' always belong to each sample.
#'
#' @param key Vector of indices for all data, including the fixed data keys
#' and the remaining (non-fixed) data.
#' @param fixedKey Vector of indices indicating which samples should always be
#' included in every sample. Naturally, \code{length(fixedKey) < length(key)}.
#' @param folds Number of k-folds to apply.
#'
#' @return A cvMixedSampler object.
#'
#' @seealso \code{\link{sampler}}
#' @seealso \code{\link{cvSampler}}
#' @seealso \code{\link{cvGroupSampler}}
#' @seealso \code{\link{cvTemporalSampler}}
#'
#' @examples
#' set.seed(273)
#' #Define dataset
#' data.dt <- generateStratifiedDomainEstimateData(n_samples = 200)
#'
#' #Define fixed data keys
#' fixed_keys <- 1:50#These will always be included
#' all_keys <- 1:nrow(data.dt)
#'
#' smplr <- cvMixedSampler(key = all_keys,
#' fixedKey = fixed_keys,
#' folds = 5)
#' #As we can see, the fixed keys are always included
#' print(getSample(smplr))
#'
#' @export
cvMixedSampler <- function(key, fixedKey, folds) {
  object <- list(key = key, fixedKey = fixedKey, folds = folds)
  class(object) <- c("cvMixedSampler", "sampler")
  object <- RMLutils:::.validate_cvSampler(object)
  return(object)
}

#' @name cvTemporalSampler
#'
#' @title Temporal cross-validation sampler
#'
#' @description
#' This sampler implements an expanding window strategy for defining training
#' folds for a modeler that requires chronologically sorted data, rather than
#' randomly split data.
#'
#' For each fold (j), a data threshold is defined as:
#'
#' \code{thres <- total_periods - (n_valPeriods * folds) + (n_valPeriods * (j - 1)) + 1}
#'
#' Splitting the sorted train and validation data accordingly. The user should
#' note that the total amount of periods (\code{length(orderPeriods)}) must be at
#' least (\code{n_valPeriods * folds + 1}).
#'
#' @param key Vector of indices of the data entries (rows) to sample from.
#' @param data Complete dataset (train+validation) containing the identifier-period column.
#' @param identifier String indicating the name of the period column.
#' @param folds Number of k-folds to apply.
#' @param n_valPeriods Total amount of periods in validation set.
#' @param orderPeriods Chronologically sorted period vector.
#'
#' @return A cvTemporalSampler object.
#'
#' @seealso \code{\link{sampler}}
#' @seealso \code{\link{cvSampler}}
#' @seealso \code{\link{cvMixedSampler}}
#' @seealso \code{\link{cvGroupSampler}}
#'
#' @examples
#' #Define a dataset with 12 periods (months)
#' years <- c(2023)
#' total_periods <- c(sapply(years, function(y){
#'   months <- 1:12
#'   months[1:9] <- paste0("0", months[1:9])
#'   return(paste0("MM", months, y))
#' }, USE.NAMES = FALSE))
#' target <- rnorm(n = length(total_periods), mean = 10, sd = 1)
#' data.dt <- data.table::data.table(measure = target, period = total_periods)
#' print(data.dt)
#'
#' smplr <- cvTemporalSampler(key = 1:nrow(data.dt),
#'                            data = data.dt,
#'                            identifier = "period",
#'                            folds = 3,
#'                            n_valPeriods = 2,#Require 2 months for validation
#'                            orderPeriods = total_periods)
#'
#' getSample(smplr)
#'
#' @export
cvTemporalSampler <- function(key, data, identifier, folds, n_valPeriods, orderPeriods) {
  #Object validation
  nPeriods <- length(orderPeriods)
  min_required <- n_valPeriods * folds + 1

  if(nPeriods < min_required) {
    msg <- paste0("Not enough available periods (",
                  nPeriods,
                  ")\n",
                  "At least ",
                  min_required,
                  " periods are required.\n",
                  "Please, consider reducing either the total amount of folds (",
                  folds,
                  ") or validation periods (",
                  n_valPeriods, ")\n")
    stop(msg)
  }
  object <- list(key = key,
                 data = data,
                 identifier = identifier,
                 folds = folds,
                 n_valPeriods = n_valPeriods,
                 orderPeriods = orderPeriods)
  class(object) <- c("cvTemporalSampler", "sampler")
  #object <- RMLutils:::.validate_cvSampler(object)
  return(object)
}
