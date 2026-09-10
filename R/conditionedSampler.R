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

#' @title Conditioned sampler.
#'
#' @description Generates the conditioned sampler from a sampler and a subsample.
#'
#' The subsampler is assumed to be independent on the sample. Otherwise the
#' subsampler would be needed to estimate the conditioned sampler.
#'
#'
#' @param object sampler object
#' @param sampleKeys list of keys obtained from a sample.
#'
#' @return A sampler.
#'
#' @examples
#' smplr1 <- srsSampler(1:20, 5)
#' sampleKeys1 <- c(2,7,8,10)
#' cSampler1 <- conditionedSampler(object = smplr1, sampleKeys = sampleKeys1)
#' cSampler1
#' #returns a smplr1 object with keys 1:20 except 2,7,8,10
#' #notice that it changes the sample size to 5 - lenght(sampleKeys)
#'
#' smplr2 <- ssSampler(1:20, rep(c("ODDS", "EVENS"), 10), c(EVENS = 8, ODDS = 2))
#' sampleKeys2 <- c(2,4,6,8,10,11)
#' cSampler2 <- conditionedSampler(object = smplr2, sampleKeys = sampleKeys2)
#' cSampler2
#' #returns a smplr2 object with keys 1:20 except 2,4,6,8,10,11
#' #notice that it changes the sample size to size
#' #EVENS = 8 - length(2,4,6,8,10)
#' #ODDS = 2 - length(11)
#'
#'
#'
#' @export
conditionedSampler <- function(object, sampleKeys) {
  UseMethod("conditionedSampler")
}
#' @rdname conditionedSampler
#' @export
conditionedSampler.srsSampler <- function(object, sampleKeys)
  return(srsSampler(setdiff(object$key, sampleKeys), object$ssize - length(sampleKeys)))


#' @rdname conditionedSampler
#' @import data.table
#' @export
conditionedSampler.ssSampler <- function(object, sampleKeys) {
  auxDT <- data.table(okey = object$key, ostrata = object$strata)
  auxDT[, ossize := object$ssize[ostrata]]
  auxDT[, inSubSample := okey %in% sampleKeys]
  auxDT[, ossize := ossize - sum(inSubSample), by = "ostrata"]
  auxDT <- auxDT[inSubSample == FALSE]
  auxSsize <- auxDT[,.SD[1, .(ossize = ossize)],by = "ostrata"]
  newSsize <- auxSsize$ossize
  names(newSsize) <- auxSsize$ostrata

  return(ssSampler(auxDT$okey, auxDT$ostrata, newSsize))
}
