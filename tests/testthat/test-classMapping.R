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

#Load all RMLutils functions
#devtools::load_all()

test_that("maxScoreClass behaves as expected", {
  #Define simple dataset
  scores.mat <- matrix(data = c(seq(0,1, 0.1), sort(seq(0,1, 0.1), decreasing = TRUE)), ncol = 2)
  colnames(scores.mat) <- c("negative_class", "positive_class")

  #Instantiate with expected errors
  expect_error(maxScoreClass(ties.method = "wrong_method"))

  #Define classMapping object
  cls_map <- maxScoreClass(ties.method = "first")

  result <- getClassMapping(object = cls_map, scores = scores.mat)
  expect_equal(result, c(rep(2, 5), rep(1, 6)))
})


test_that("binaryThresholdClass behaves as expected", {
  #Define simple dataset
  scores.mat <- matrix(data = c(seq(0,1, 0.1), sort(seq(0,1, 0.1), decreasing = TRUE)), ncol = 2)
  colnames(scores.mat) <- c("negative_class", "positive_class")

  #Instantiate with expected errors
  expect_error(binaryThresholdClass(threshold = 1.6, positive_class = "positive_class"))
  expect_error(binaryThresholdClass(threshold = 0.6, positive_class = 1))

  #Define classMapping object
  cls_map <- binaryThresholdClass(threshold = 0.61, positive_class = "positive_class")

  result <- getClassMapping(object = cls_map, scores = scores.mat)
  expect_equal(result, c(rep(2, 4), rep(1, 7)))

  #Call getClassMapping with expected error
  expect_error(getClassMapping(object = cls_map, scores = cbind(scores.mat, scores.mat)))
})
