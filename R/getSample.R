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

#' @title Gets a matrix with samples.
#'
#' @description Obtains a sample according the sampling scheme from a sampler object.
#'
#' @param object \code{\link{sampler}} object
#' @param times Number of times to sample from data.
#'
#' @return A matrix with the samples. Each column of the data being the n-th
#' sample taken and the rows the sample results of each sample taken.
#'  When using stratified sampling, the rows are returned ordered by the strates.
#'
#' @seealso \code{\link{srsSampler}}
#' @seealso \code{\link{cvSampler}}
#'
#' @examples
#' set.seed(273)
#' #Define simple random sampler and get a sample
#' sampler1 <- srsSampler(key = 1:15, ssize = 10)
#' getSample(sampler1, times = 2)
#'
#' #Define cross-validation sampler and get a sample
#' sampler2 <- cvSampler(key = 1:25, folds = 5)
#' getSample(sampler2, times = 1)
#'
#' @export
getSample <- function(object, times=1) {
  UseMethod("getSample")
}

# simple samplers ####

#' @rdname getSample
#' @export
getSample.srsSampler <- function(object, times=1) {
  output <- sapply(rep(object$ssize,times), function(x)
              sample_alt(object$key, x))
  if (is.vector(output)) output <- matrix(output, nrow = 1)
  return(output)
}
#' @rdname getSample
#' @import data.table
#' @export
getSample.ssSampler <- function(object, times=1) {
  if (!setequal(object$strata, names(object$ssize)))
    stop("[RMLUtils::getSample] Sample sizes names don't match strata.")
  output <- matrix(data = 0, ncol = times, nrow = sum(object$ssize))
  auxDT <- data.table(okey = object$key, ostrata = object$strata)
  for (i in 1:times)
    output[, i] <- as.matrix(auxDT[,t(sample_alt(.SD$okey, object$ssize[ostrata])),
                                   by = "ostrata"]$V1)
  return(output)
}

#' @rdname getSample
#' @export
getSample.bootstrapSampler <- function(object, times = 1){
  output <- sapply(1:times, function(t){
    sample(x = object$key,
           size = length(object$key),
           replace = TRUE)})
  if (is.vector(output)) output <- matrix(output, nrow = 1)
  return(output)
}

#'@rdname getSample
#'@export
getSample.fullSampler <- function(object, times = 1) {
  if (object$ssize == 2) {
    output <- matrix(0, nrow = 2,
                     ncol = length(object$key) * (length(object$key) - 1) / 2)
    k <- 1
    for (i in 1:(length(object$key) - 1))
      for (j in (i + 1):length(object$key)) {
        output[, k] <- c(i, j)
        k <- k + 1
      }
    return(output)
  }
  return(matrix(object$key, nrow = 1))
}

# cv samplers ####

#' @rdname getSample
#' @export
getSample.cvSampler <- function(object, times=1) {
  output <- matrix(0, ncol = object$folds * times,
                   nrow = length(object$key) -
                     floor(length(object$key) / object$folds))
  lfold <- ceiling(length(object$key) / object$folds)
  for (i in 1:times) {
    skeys <- sample_alt(object$key, length(object$key))
    modulo <- length(object$key) %% object$folds
    starts <- 1
    for (j in 1:object$folds) {
      if ((modulo == 0) || (j <= modulo)) {
        ends <- starts + lfold - 1
      }
      else {
        ends <- starts + lfold - 2
      }
      newdata <- setdiff(object$key, skeys[starts:ends])
      starts <- ends + 1
      if (length(newdata) < nrow(output))
        output[, (i - 1) * object$folds + j] <- c(newdata, NA)
      else
        output[, (i - 1) * object$folds + j] <- newdata
    }
  }
  return(output)
}

#' @rdname getSample
#' @export
getSample.cvMixedSampler <- function(object, times=1) {

  output <- getSample(cvSampler(setdiff(object$key, object$fixedKey),
                                folds = object$folds))
  fixedM <- matrix(rep(object$fixedKey, times = object$folds),
                   nrow = length(object$fixedKey), ncol = object$folds)
  return(rbind(fixedM, output))
}

#' @rdname getSample
#' @import data.table
#' @export
getSample.cvGroupSampler <- function(object, times=1) {

  unique_groups <- unique(object$data[, get(object$identifier)])
  lfold <- ceiling(length(unique_groups) / object$folds)
  output.list <- list()

  for (i in 1:times) {
    skeys <- sample_alt(unique_groups, length(unique_groups))##groups ordered
    modulo <- length(unique_groups) %% object$folds
    starts <- 1

    for (j in 1:object$folds) {
      if ((modulo == 0) || (j <= modulo)) {
        ends <- starts + lfold - 1
      }
      else {
        ends <- starts + lfold - 2
      }

      complementario <- object$data[get(object$identifier)%in%skeys[starts:ends], which=TRUE]
      output.list[[(i-1)*object$folds+j]] <- setdiff(object$key, complementario)
      starts <- ends + 1
    }
  }

  longitud_maxima <- max(sapply(output.list, length))
  output.listconNAs <- lapply(output.list, function(x) c(x, rep(NA, longitud_maxima - length(x))))
  output <- as.matrix(do.call(cbind, output.listconNAs))

  return(output)
}

#' @rdname getSample
#' @import data.table
#' @export
getSample.cvTemporalSampler <- function(object, times = 1) {

  if(times > 1){stop("Times must be 1 for cvTemporalSampler")}
  # key, data, identifier, folds, n_valPeriods, orderPeriods
  # key el vector de indices de fila
  # data el conjunto completo de training (training+validacion)
  # identifier el nombre de columna que contiene los periodos
  # folds el numero de folds temporales
  # n_valPeriods el numero de periodos en el conjunto de validacion
  # orderPeriods el vector de periodos ordenado temporalmente
  orderPeriods <- object$orderPeriods

  nPeriods <- length(orderPeriods)

  dt <- copy(object$data)
  dt[, index := object$key]

  output.list <- list()

  for (j in 1:object$folds) {

    thres <- nPeriods - (object$n_valPeriods * object$folds) +
      (object$n_valPeriods * (j - 1)) + 1
    select_trainPeriods <- orderPeriods[1:(thres - 1)]
    output.list[[j]] <- dt[get(object$identifier) %in% select_trainPeriods, index]

  }

  longitud_maxima <- max(sapply(output.list, length))
  output.listconNAs <- lapply(output.list, function(x) c(x, rep(NA, longitud_maxima - length(x))))
  output <- as.matrix(do.call(cbind, output.listconNAs))

  return(output)
}
