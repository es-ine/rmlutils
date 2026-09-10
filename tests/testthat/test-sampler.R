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

test_that("srsSampler behaves as expected", {
  set.seed(273)
  #Define data keys
  sample.keys <- 1:15
  #Define sampler
  sampler <- srsSampler(key = sample.keys, ssize = 10)
  #Obtain a sample
  sample <- expect_no_error(getSample(sampler))
  print(sample)
})

test_that("ssSampler behaves as expected", {
  set.seed(273)
  #Generate data and define keys
  #This data has two strata: Urban and Rural
  data.dt <- generateStratifiedDomainEstimateData(n_samples = 100)
  key <- 1:nrow(data.dt)
  head(data.dt)
  smplr <- ssSampler(key = key,
                     strata = data.dt[, strata_class],
                     ssize = c("Urban" = 20, "Rural" = 15))
  s1 <- expect_no_error(getSample(smplr))
  print(table(data.dt[s1, strata_class]))
  print(head(data.dt[s1]))
})

test_that("bootstrapSampler behaves as expected", {
  set.seed(273)
  #Define data and key
  data.dt <- generateStratifiedDomainEstimateData(n_samples = 10)
  key <- 1:nrow(data.dt)
  head(data.dt)
  smplr <- bootstrapSampler(key)
  s1 <- expect_no_error(getSample(smplr))
  print(dim(data.dt[s1]))
})

test_that("cvSampler behaves as expected", {
  set.seed(273)
  #Define key vector
  data.dt <- generateStratifiedDomainEstimateData(n_samples = 50)
  key <- 1:nrow(data.dt)
  smplr <- cvSampler(key = key, folds = 5)
  s1 <- expect_no_error(getSample(smplr))

  print(s1)

  #print(data.dt[s1[,1]])
})

test_that("cvGroupSampler behaves as expected", {
  set.seed(273)
  #Define data (10 groups, 10 rows for each group)
  id_var <- c(sapply(paste0("Group_", 1:10), function(x){
    return(rep(x, 10))
  }, USE.NAMES = FALSE))
  measure <- rnorm(n = length(id_var), mean = 10, sd = 1)
  data.dt <- data.table::data.table(measure = measure, id_var = id_var)

  #Instantiate sampler
  key <- 1:nrow(data.dt)
  identifier <- "id_var"
  smplr <- cvGroupSampler(key = key,
                          data = data.dt,
                          identifier = identifier,
                          folds = 3)
  #As it can be seen, the data partitions contain all rows from the selected
  #groups
  s1 <- expect_no_error(getSample(smplr))
  print(s1)
  print(table(data.dt[s1[, 1], id_var]))
  print(table(data.dt[s1[, 2], id_var]))
  print(table(data.dt[s1[, 3], id_var]))
})

test_that("cvMixedSampler behaves as expected", {
  set.seed(273)
  #Define dataset
  data.dt <- generateStratifiedDomainEstimateData(n_samples = 200)

  #Define fixed data keys
  fixed_keys <- 1:50#These will always be included
  all_keys <- 1:nrow(data.dt)

  smplr <- cvMixedSampler(key = all_keys,
                          fixedKey = fixed_keys,
                          folds = 5)
  #As we can see, the fixed keys are always included
  print(expect_no_error(getSample(smplr)))
})

test_that("fullSampler behaves as expected", {
  key <- 1:6

  smplr1 <- fullSampler(key = key, ssize = 2)

  s1 <- expect_no_error(getSample(smplr1))
  print(s1)

  smplr2 <- fullSampler(key = key)

  s2 <- expect_no_error(getSample(smplr1))
  print(s2)
})

test_that("cvTemporalSampler behaves as expected", {
  #Define a dataset with 12 periods (months)
  years <- c(2023)
  total_periods <- c(sapply(years, function(y){
    months <- 1:12
    months[1:9] <- paste0("0", months[1:9])
    return(paste0("MM", months, y))
  }, USE.NAMES = FALSE))
  target <- rnorm(n = length(total_periods), mean = 10, sd = 1)
  data.dt <- data.table::data.table(measure = target, period = total_periods)
  print(data.dt)

  smplr <- cvTemporalSampler(key = 1:nrow(data.dt),
                             data = data.dt,
                             identifier = "period",
                             folds = 3,
                             n_valPeriods = 2,#Require 2 months for validation
                             orderPeriods = total_periods)

  print(expect_no_error(getSample(smplr)))
})

