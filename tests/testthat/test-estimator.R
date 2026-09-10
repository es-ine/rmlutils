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

test_that("HTEstimate behaves as expected", {
  data.dt <- generateEstimateData()
  head(data.dt)
  key <- 1:nrow(data.dt)
  srsmplr <- srsSampler(key = key, ssize = 80)
  sample_1 <- getSample(srsmplr)

  #Call with expected errors
  expect_error(HTEstimate(object = "srsmplr", sampleKeys = sample_1, sampleData = data.dt[sample_1]))

  #HT estimate for simple random sampling
  HTEstimate(object = srsmplr, sampleKeys = sample_1, sampleData = data.dt[sample_1])

  data2.dt <- generateStratifiedEstimateData()
  head(data2.dt)
  key = 1:nrow(data2.dt)
  sssmplr <- ssSampler(key = key,
                       strata = data2.dt[, strata_class],
                       ssize = c("Class_1" = 20, "Class_2" = 15, "Class_3" = 10))
  sample_2 <- getSample(sssmplr)
  #Define target columns
  target_cols <- c("Target_1", "Target_2", "Target_3")
  #HT estimate for stratified random sampling
  HTEstimate(object = sssmplr, sampleKeys = sample_2, sampleData = data2.dt[sample_2, ..target_cols])
})

test_that("HTDomainEstimator behaves as expected", {
  set.seed(273)
  #Define data to use
  data.dt <- generateStratifiedDomainEstimateData()
  head(data.dt)
  key <- 1:nrow(data.dt)
  srsmplr <- srsSampler(key = key, ssize = 80)
  sample_1 <- getSample(srsmplr)
  sample_data.dt <- data.dt[sample_1]
  domains <- sample_data.dt[, domain_class]
  target_cols <- c("income", "cost_of_living")

  #Instantiate with expected errors
  expect_error(HTDomainEstimate(object = "srsmplr",
                                sampleKeys = sample_1,
                                sampleData = sample_data.dt[, ..target_cols],
                                domains = domains))

  #HT estimate for simple random sampling
  HTDomainEstimate(object = srsmplr,
                   sampleKeys = sample_1,
                   sampleData = sample_data.dt[, ..target_cols],
                   domains = domains)

  #Use same data with ssSampler
  sssmplr <- ssSampler(key = key, strata = data.dt[, strata_class], ssize = c("Urban" = 25, "Rural" = 30))
  sample_2 <- getSample(srsmplr)
  sample_data_2.dt <- data.dt[sample_2]
  domains2 <- sample_data_2.dt[, domain_class]
  target_cols <- c("income", "cost_of_living")

  #HT domain estimate for stratified random sampling
  HTDomainEstimate(object = sssmplr,
                   sampleKeys = sample_2,
                   sampleData = sample_data_2.dt[, ..target_cols],
                   domains = domains2)
})

test_that("REstimate behaves as expected", {
  data.dt <- generateStratifiedDomainModelAssistedEstimateData(generate_random_weights = TRUE)
  head(data.dt)
  strata_colummn <- "strata_class"
  weights_column <- "weights"

  #Call with expected errors
  expect_error(REstimate(data = data.dt,
                         strata_colummn = 1,
                         weights_column = weights_column))
  expect_error(REstimate(data = data.dt,
                         strata_colummn = strata_colummn,
                         weights_column = 1))

  #Properly call function
  out <- REstimate(data = data.dt,
                   strata_colummn = strata_colummn,
                   weights_column = weights_column)
})

test_that("RDomainEstimate behaves as expected", {
  data.dt <- generateStratifiedDomainModelAssistedEstimateData(generate_random_weights = TRUE)
  head(data.dt)
  strata_colummn <- "strata_class"
  domain_column <- "domain_class"
  weights_column <- "weights"

  #Call with expected exceptions
  expect_error(RDomainEstimate(data = data.dt,
                               strata_colummn = 1,
                               domain_column = domain_column,
                               weights_column = weights_column))
  expect_error(RDomainEstimate(data = data.dt,
                               strata_colummn = strata_colummn,
                               domain_column = 1,
                               weights_column = weights_column))
  expect_error(RDomainEstimate(data = data.dt,
                               strata_colummn = strata_colummn,
                               domain_column = domain_column,
                               weights_column = 1))

  #Properly call function
  out <- RDomainEstimate(data = data.dt,
                         strata_colummn = strata_colummn,
                         domain_column = domain_column,
                         weights_column = weights_column)
})

test_that("MCSubRBEstimate behaves as expected", {
  #Generate data
  set.seed(273)
  data.dt <- generateStratifiedDomainModelAssistedEstimateData(n_samples = 1000, generate_random_weights = FALSE)
  head(data.dt)

  #Get relevant variables from data
  data.keys <- 1:nrow(data.dt)
  target <- "target"# Regression
  regressors <- setdiff(names(data.dt), target)

  #Define main sampler and sub-sampler
  train_size = round(0.8 * nrow(data.dt))
  main_sampler <- srsSampler(key = data.keys, ssize = train_size)
  main_sample <- getSample(main_sampler)
  sub_sampler <- srsSampler(key = main_sample, ssize = train_size*0.5)

  #Define modeler
  params <- list(
    "num.trees" = 50,
    "max.depth" = 20
  )

  modeler <- rangerRegressorModeler(params = params, target = target, regressors = regressors)

  #Call with expected errors
  expect_error(MCSubRBEstimate(auxModeler = "modeler",
                               data = data.dt,
                               mainSampler = main_sampler,
                               subSampler = sub_sampler,
                               targetVar = target,
                               ssNumber = 100))
  expect_error(MCSubRBEstimate(auxModeler = modeler,
                               data = data.dt,
                               mainSampler = "main_sampler",
                               subSampler = sub_sampler,
                               targetVar = target,
                               ssNumber = 100))
  expect_error(MCSubRBEstimate(auxModeler = modeler,
                               data = data.dt,
                               mainSampler = main_sampler,
                               subSampler = "sub_sampler",
                               targetVar = target,
                               ssNumber = 100))

  #Properly call function
  MCSubRBEstimate(auxModeler = modeler,
                  data = data.dt,
                  mainSampler = main_sampler,
                  subSampler = sub_sampler,
                  targetVar = target,
                  ssNumber = 100)
})

test_that("MCSubRBDomainEstimate behaves as expected", {
  #Generate data
  set.seed(273)
  data.dt <- generateStratifiedDomainModelAssistedEstimateData(n_samples = 1000, generate_random_weights = FALSE)
  head(data.dt)

  #Get relevant variables from data
  data.keys <- 1:nrow(data.dt)
  target <- "target"# Regression
  regressors <- setdiff(names(data.dt), target)

  #Define main sampler and sub-sampler
  train_size = round(0.8 * nrow(data.dt))
  main_sampler <- srsSampler(key = data.keys, ssize = train_size)
  main_sample <- getSample(main_sampler)
  sub_sampler <- srsSampler(key = main_sample, ssize = train_size*0.5)

  #Define modeler
  params <- list(
    "num.trees" = 50,
    "max.depth" = 20
  )

  modeler <- rangerRegressorModeler(params = params, target = target, regressors = regressors)

  #Call with expected errors
  expect_error(MCSubRBDomainEstimate(auxModeler = "modeler",
                                     data = data.dt,
                                     mainSampler = main_sampler,
                                     subSampler = sub_sampler,
                                     targetVar = target,
                                     domains = data.dt[, domain_class],
                                     ssNumber = 100))
  expect_error(MCSubRBDomainEstimate(auxModeler = modeler,
                                     data = data.dt,
                                     mainSampler = "main_sampler",
                                     subSampler = sub_sampler,
                                     targetVar = target,
                                     domains = data.dt[, domain_class],
                                     ssNumber = 100))
  expect_error(MCSubRBDomainEstimate(auxModeler = modeler,
                                     data = data.dt,
                                     mainSampler = main_sampler,
                                     subSampler = "sub_sampler",
                                     targetVar = target,
                                     domains = data.dt[, domain_class],
                                     ssNumber = 100))

  #Properly call function
  MCSubRBDomainEstimate(auxModeler = modeler,
                        data = data.dt,
                        mainSampler = main_sampler,
                        subSampler = sub_sampler,
                        targetVar = target,
                        domains = data.dt[, domain_class],
                        ssNumber = 100)
})

#test_that("biasVarianceEstimate behaves as expected", {
#  #Generate data
#  set.seed(273)
#  data.dt <- generateStratifiedDomainModelAssistedEstimateData(n_samples = 1000, generate_random_weights = FALSE)
#
#  #Get relevant variables from data
#  data.keys <- 1:nrow(data.dt)
#  target <- "target"# Regression
#  regressors <- setdiff(names(data.dt), target)
#
#  #Define main sampler and sub-sampler
#  train_size = round(0.8 * nrow(data.dt))
#  main_sampler <- srsSampler(key = data.keys, ssize = train_size)
#  main_sample <- getSample(main_sampler)
#  sub_sampler <- srsSampler(key = main_sample, ssize = train_size*0.5)
#  sub_sample <- getSample(sub_sampler)
#
#  #Define modeler
#  params <- list(
#    "num.trees" = 50,
#    "max.depth" = 20
#  )
#  modeler <- rangerRegressorModeler(params = params, target = target, regressors = regressors)
#
#  #Train modeler with sub-sample
#  modeler <- train(modeler, data = data.dt, subSet = sub_sample)
#
#  #Define conditionedSampler
#  cSampler <- conditionedSampler(object = main_sampler, sampleKeys = sub_sample)
#
#  #Get model predictions over s2
#  errorsKeys <- setdiff(sub_sampler$key, sub_sample)
#
#  preds <- getPredictions(object = modeler, data = data.dt, subSet = errorsKeys)
#
#  y_true <- data.dt[errorsKeys, target]
#
#  errors <- y_true - preds
#
#  #Call with expected errors
#  expect_error(biasVarianceEstimate(cSampler = "cSampler",
#                                    errorsKeys = errorsKeys,
#                                    errors = errors))
#
#  #Properly call function
#  biasVarianceEstimate(cSampler = cSampler,
#                       errorsKeys = errorsKeys,
#                       errors = errors)
#
#})

#test_that("predictionEstimate behaves as expected", {
#  set.seed(273)
#  data.dt <- generateStratifiedDomainModelAssistedEstimateData(n_samples = 1000, generate_random_weights = FALSE)
#
#  #Get relevant variables from data
#  data.keys <- 1:nrow(data.dt)
#  target <- "target"# Regression
#  regressors <- setdiff(names(data.dt), target)
#
#  #Define main sampler and sub-sampler
#  train_size = round(0.8 * nrow(data.dt))
#  main_sampler <- srsSampler(key = data.keys, ssize = train_size)
#  main_sample <- getSample(main_sampler)
#  sub_sampler <- srsSampler(key = main_sample, ssize = train_size*0.5)
#
#  #Define modeler
#  params <- list(
#    "num.trees" = 50,
#    "max.depth" = 20
#  )
#  modeler <- rangerRegressorModeler(params = params, target = target, regressors = regressors)
#
#  #Call with expected errors
#  expect_error(predictionEstimate(modeler = "modeler",
#                                  data = data.dt,
#                                  mainSampler = main_sampler,
#                                  subSampler = sub_sampler,
#                                  ssNumber = 50))
#  expect_error(predictionEstimate(modeler = modeler,
#                                  data = data.dt,
#                                  mainSampler = "main_sampler",
#                                  subSampler = sub_sampler,
#                                  ssNumber = 50))
#  expect_error(predictionEstimate(modeler = modeler,
#                                  data = data.dt,
#                                  mainSampler = main_sampler,
#                                  subSampler = "sub_sampler",
#                                  ssNumber = 50))
#
#  #Properly call function
#  predictionEstimate(modeler = modeler,
#                     data = data.dt,
#                     mainSampler = main_sampler,
#                     subSampler = sub_sampler,
#                     ssNumber = 50)
#})
#
#test_that("predictionDomainEstimate behaves as expected", {
#  set.seed(273)
#  data.dt <- generateStratifiedDomainModelAssistedEstimateData(n_samples = 1000, generate_random_weights = FALSE)
#
#  #Get relevant variables from data
#  data.keys <- 1:nrow(data.dt)
#  target <- "target"# Regression
#  regressors <- setdiff(names(data.dt), target)
#
#  #Define main sampler and sub-sampler
#  train_size = round(0.8 * nrow(data.dt))
#  main_sampler <- srsSampler(key = data.keys, ssize = train_size)
#  main_sample <- getSample(main_sampler)
#  sub_sampler <- srsSampler(key = main_sample, ssize = train_size*0.5)
#
#  #Define modeler
#  params <- list(
#    "num.trees" = 50,
#    "max.depth" = 20
#  )
#  modeler <- rangerRegressorModeler(params = params, target = target, regressors = regressors)
#
#  #Call with expected errors
#  expect_error(predictionDomainEstimate(modeler = "modeler",
#                                        data = data.dt,
#                                        mainSampler = main_sampler,
#                                        subSampler = sub_sampler,
#                                        domains = data.dt[, domain_class],
#                                        ssNumber = 50))
#  expect_error(predictionDomainEstimate(modeler = modeler,
#                                        data = data.dt,
#                                        mainSampler = "main_sampler",
#                                        subSampler = sub_sampler,
#                                        domains = data.dt[, domain_class],
#                                        ssNumber = 50))
#  expect_error(predictionDomainEstimate(modeler = modeler,
#                                        data = data.dt,
#                                        mainSampler = main_sampler,
#                                        subSampler = "sub_sampler",
#                                        domains = data.dt[, domain_class],
#                                        ssNumber = 50))
#
#  #Properly call function
#  predictionDomainEstimate(modeler = modeler,
#                           data = data.dt,
#                           mainSampler = main_sampler,
#                           subSampler = sub_sampler,
#                           domains = data.dt[, domain_class],
#                           ssNumber = 50)
#})

test_that("preTrainedEstimate behaves as expected",{
  set.seed(273)
  data.dt <- generateStratifiedDomainModelAssistedEstimateData(n_samples = 1000, generate_random_weights = FALSE)

  #Get relevant variables from data
  data.keys <- 1:nrow(data.dt)
  target <- "target"# Regression
  regressors <- setdiff(names(data.dt), target)

  #Define main sampler and sub-sampler
  train_size = round(0.8 * nrow(data.dt))
  smplr <- srsSampler(key = data.keys, ssize = train_size)
  train.keys <- getSample(smplr)
  test.keys <- setdiff(data.keys, train.keys)

  #Define modeler
  params <- list(
    "num.trees" = 50,
    "max.depth" = 20
  )
  modeler <- rangerRegressorModeler(params = params, target = target, regressors = regressors)

  #Call with expected errors
  expect_error(preTrainedEstimate(modeler, data = data.dt[test.keys]))
  expect_error(preTrainedEstimate("modeler", data = data.dt[test.keys]))

  modeler <- train(modeler, data = data.dt, subSet = train.keys)

  #Call function properly
  preTrainedEstimate(modeler, data = data.dt[test.keys])

})

test_that("preTrainedDomainEstimate behaves as expected",{
  set.seed(273)
  data.dt <- generateStratifiedDomainModelAssistedEstimateData(n_samples = 1000, generate_random_weights = FALSE)

  #Get relevant variables from data
  data.keys <- 1:nrow(data.dt)
  target <- "target"# Regression
  regressors <- setdiff(names(data.dt), target)

  #Define main sampler and sub-sampler
  train_size = round(0.8 * nrow(data.dt))
  smplr <- srsSampler(key = data.keys, ssize = train_size)
  train.keys <- getSample(smplr)
  test.keys <- setdiff(data.keys, train.keys)

  #Define modeler
  params <- list(
    "num.trees" = 50,
    "max.depth" = 20
  )
  modeler <- rangerRegressorModeler(params = params, target = target, regressors = regressors)

  modeler <- train(modeler, data = data.dt, subSet = train.keys)

  #Call function properly
  expect_no_error(preTrainedDomainEstimate(modeler, data = data.dt, domain_column = "domain_class"))

})
