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

# Regression costFunctions ####

test_that("r2CostFunction behaves as expected", {
  #Generate data
  data.dt <- generateRegressionData(n_samples = 1000, generate_random_weights = FALSE)
  data.keys <- 1:nrow(data.dt)
  #Instantiate cost function
  cf <- r2CostFunction(weightsColumn = "")

  target <- "regression_target"# Regression

  regressors <- setdiff(names(data.dt), target)

  #Define modeler
  params <- list(
    "num.trees" = 50,
    "max.depth" = 20
  )

  modeler <- rangerRegressorModeler(params = params, target = target, regressors = regressors)
  modeler <- train(modeler, data = data.dt)

  cost <- expect_no_error(getCost(object = cf,
                          data = data.dt,
                          modeler = modeler,
                          fSample = 1:nrow(data.dt),
                          cSample = 1:nrow(data.dt)))
  print(cost)

  cvcost <- expect_no_error(crossValidate(modeler = modeler,
                                          data = data.dt,
                                          CVSampler = cvSampler(key = data.keys, folds = 3),
                                          costFunction = cf))
  print(cvcost)
})

test_that("squareBiasCostFunction behaves as expected", {
  #Generate data
  data.dt <- generateRegressionData(n_samples = 1000, generate_random_weights = FALSE)
  data.keys <- 1:nrow(data.dt)
  #Instantiate cost function
  cf <- squareBiasCostFunction(weightsColumn = "")

  target <- "regression_target"# Regression

  regressors <- setdiff(names(data.dt), target)

  #Define modeler
  params <- list(
    "num.trees" = 50,
    "max.depth" = 20
  )

  modeler <- rangerRegressorModeler(params = params, target = target, regressors = regressors)
  modeler <- train(modeler, data = data.dt)

  cost <- expect_no_error(getCost(object = cf,
                                  data = data.dt,
                                  modeler = modeler,
                                  fSample = 1:nrow(data.dt),
                                  cSample = 1:nrow(data.dt)))
  print(cost)

  cvcost <- expect_no_error(crossValidate(modeler = modeler,
                                          data = data.dt,
                                          CVSampler = cvSampler(key = data.keys, folds = 3),
                                          costFunction = cf))
  print(cvcost)
})

test_that("MSECostFunction behaves as expected", {
  #Generate data
  data.dt <- generateRegressionData(n_samples = 1000, generate_random_weights = FALSE)
  data.keys <- 1:nrow(data.dt)
  #Instantiate cost function
  cf <- MSECostFunction(weightsColumn = "")

  target <- "regression_target"# Regression

  regressors <- setdiff(names(data.dt), target)

  #Define modeler
  params <- list(
    "num.trees" = 50,
    "max.depth" = 20
  )

  modeler <- rangerRegressorModeler(params = params, target = target, regressors = regressors)
  modeler <- train(modeler, data = data.dt)

  cost <- expect_no_error(getCost(object = cf,
                                  data = data.dt,
                                  modeler = modeler,
                                  fSample = 1:nrow(data.dt),
                                  cSample = 1:nrow(data.dt)))
  print(cost)

  cvcost <- expect_no_error(crossValidate(modeler = modeler,
                                          data = data.dt,
                                          CVSampler = cvSampler(key = data.keys, folds = 3),
                                          costFunction = cf))
  print(cvcost)
})

test_that("groupMSECostFunction behaves as expected", {
  #Generate data
  data.dt <- generateRegressionData(n_samples = 1000)

  data.keys <- 1:nrow(data.dt)
  train_size = round(0.8 * nrow(data.dt))

  #Split train/test #
  sampler <- srsSampler(data.keys, ssize = train_size)

  train.keys <- getSample(sampler)
  test.keys <- setdiff(data.keys, train.keys)

  train.dt <- data.dt[train.keys]
  test.dt <- data.dt[test.keys]

  #Define modeler
  target <- "regression_target"
  regressors <- setdiff(names(data.dt), target)
  params <- list(
    "num.trees" = 50,
    "max.depth" = 20
  )

  modeler <- rangerRegressorModeler(params = params, target = target, regressors = regressors)
  modeler <- train(modeler, data = train.dt)

  #Define cost function and get cost

  cf <- groupMSECostFunction(weightsColumn = "weights",
                             groupWeightsFunction = sum,
                             groupColumn = "cat_1",
                             groupFunction = mean)

  cost <- expect_no_error(getCost(object = cf,
                                  data = data.dt,
                                  modeler = modeler,
                                  cSample = test.keys))
  print(cost)

  cvcost <- expect_no_error(crossValidate(modeler = modeler,
                                          data = data.dt,
                                          CVSampler = cvSampler(key = data.keys, folds = 3),
                                          costFunction = cf))
  print(cvcost)
})

test_that("groupTotalErrorCostFunction behaves as expected", {
  data.dt <- generateStratifiedDomainModelAssistedEstimateData(n_samples = 1000,
                                                               generate_random_weights = TRUE)
  data.keys <- 1:nrow(data.dt)
  set.seed(123)

  data.keys <- 1:nrow(data.dt)
  train_size = round(0.8 * nrow(data.dt))

  #Split train/test
  sampler <- srsSampler(data.keys, ssize = train_size)

  train.keys <- getSample(sampler)
  test.keys <- setdiff(data.keys, train.keys)

  train.dt <- data.dt[train.keys]
  test.dt <- data.dt[test.keys]

  #Define modeler
  target <- "target"
  regressors <- setdiff(names(data.dt), target)
  params <- list(
    "num.trees" = 50,
    "max.depth" = 20
  )

  modeler <- rangerRegressorModeler(params = params, target = target, regressors = regressors)
  modeler <- train(modeler, data = train.dt)

  #Define cost function and get cost

  cf <- groupTotalErrorCostFunction(weightsColumn = "weights",
                                    groupVars = c("strata_class", "domain_class"))

  cost <- expect_no_error(getCost(object = cf,
                                  data = data.dt,
                                  modeler = modeler,
                                  cSample = test.keys))
  print(cost)

  cvcost <- expect_no_error(crossValidate(modeler = modeler,
                                          data = data.dt,
                                          CVSampler = cvSampler(key = data.keys, folds = 3),
                                          costFunction = cf))
  print(cvcost)
})

test_that("MCSubRBMSECostFunction behaves as expected", {
  data.dt <- generateStratifiedDomainModelAssistedEstimateData(n_samples = 1000)
  data.keys <- 1:nrow(data.dt)
  set.seed(123)

  data.keys <- 1:nrow(data.dt)
  train_size = round(0.8 * nrow(data.dt))

  #Split train
  sampler <- srsSampler(data.keys, ssize = train_size)

  SKeys <- getSample(sampler)

  train.dt <- data.dt[SKeys]

  #Define modeler
  target <- "target"
  regressors <- setdiff(names(data.dt), target)
  params <- list(
    "num.trees" = 50,
    "max.depth" = 20
  )

  modeler <- rangerRegressorModeler(params = params, target = target, regressors = regressors)
  modeler <- train(modeler, data = train.dt)

  #Define cost function and get cost
  cf <- MCSubRBMSECostFunction(sampler, SKeys, nonSampleData = data.dt[-SKeys,])

  cost <- expect_no_error(crossValidate(modeler,
                          data.dt[SKeys,],
                          srsSampler(1:length(SKeys),150),#150 subsamples
                          cf,
                          times = 100))
  print(cost)
})

test_that("holdoutTemporalCostFunction behaves as expected", {
  #Define a dataset with 36 periods (months)
  years <- c(2023, 2024, 2025)
  total_periods <- c(sapply(years, function(y){
    months <- 1:12
    months[1:9] <- paste0("0", months[1:9])
    return(paste0("MM", months, y))
  }, USE.NAMES = FALSE))

  #Generate data for each period
  data.dt <- rbindlist(lapply(total_periods, function(p){
    dt <- generateRegressionData(n_samples = 100,
                                 generate_random_weights = FALSE)
    dt[, period := p]
    return(dt)
  }))
  #head(data.dt)
  data.keys <- 1:nrow(data.dt)
  smplr <- cvTemporalSampler(key = 1:nrow(data.dt),
                             data = data.dt,
                             identifier = "period",
                             folds = 5,
                             n_valPeriods = 6,#Require 2 months for validation
                             orderPeriods = total_periods)

  #getSample(smplr)
  params <- list(
    "num.trees" = 50,
    "max.depth" = 20
  )
  target <- "regression_target"
  regressors <- setdiff(colnames(data.dt), target)

  modeler <- rangerRegressorModeler(params = params,
                                    target = target,
                                    regressors = regressors)

  modeler <- train(modeler, data = data.dt)

  #Define holdoutTemporalCostFunction
  cf <- holdoutTemporalCostFunction(costFunction = MSECostFunction(),
                                    cvTemporalSampler = smplr)

  cost <- expect_no_error(getCost(object = cf,
                          data = data.dt,
                          modeler = modeler,
                          cSample = 1:nrow(data.dt)))
  print(cost$regression_target$cost)

  cost <- expect_no_error(crossValidate(modeler = modeler,
                          data = data.dt,
                          CVSampler = cvSampler(key = data.keys, folds = 3),
                          costFunction = cf))
  print(cost)
})

# Classification cost functions ####

test_that("confusionMatrixCostFunction behaves as expected",{
  #Generate data
  data.dt <- generateMulticlassClassificationData(n_samples = 1000,
                                                  generate_random_weights = TRUE)
  data.keys <- 1:nrow(data.dt)

  #Train simple modeler
  target <- "classification_target"
  regressors <- setdiff(names(data.dt), c(target, "weights"))
  params <- list(
    "num.trees" = 50,
    "max.depth" = 20
  )

  modeler <- rangerClassifierModeler(params = params, target = target, regressors = regressors)
  modeler <- train(modeler, data = data.dt)

  n_types <-  list(NULL, "true", "pred", "all")
  for (i in 1:length(n_types)){
    #Instantiate cost function
    cf <- confusionMatrixCostFunction(weightsColumn = "weights",
                                      class_map = maxScoreClass(),
                                      normalize = n_types[[i]],
                                      class_names = levels(data.dt[["classification_target"]]))

    #Evaluate with getCost
    cost <- expect_no_error(getCost(object = cf, data = data.dt, modeler = modeler, cSample = 1:nrow(data.dt)))
    print(cost)

    cvcost <- expect_no_error(crossValidate(modeler = modeler,
                                            data = data.dt,
                                            CVSampler = cvSampler(key = data.keys, folds = 3),
                                            costFunction = cf))
    print(cvcost)
  }

})

test_that("accuracyCostFunction behaves as expected",{
  #Generate data
  data.dt <- generateMulticlassClassificationData(n_samples = 1000,
                                                  generate_random_weights = TRUE)
  data.keys <- 1:nrow(data.dt)


  #Train simple modeler
  target <- "classification_target"
  regressors <- setdiff(names(data.dt), c(target, "weights"))
  params <- list(
    "num.trees" = 50,
    "max.depth" = 20
  )

  modeler <- rangerClassifierModeler(params = params, target = target, regressors = regressors)
  modeler <- train(modeler, data = data.dt)

  avg_types <- c("micro", "macro", "weighted")
  for (i in 1:length(avg_types)){
    #Instantiate cost function
    cf <- accuracyCostFunction(weightsColumn = "weights",
                               class_map = maxScoreClass(),
                               average = avg_types[[i]])

    #Evaluate with getCost
    cost <- expect_no_error(getCost(object = cf, data = data.dt, modeler = modeler, cSample = 1:nrow(data.dt)))
    print(cost)

    cvcost <- expect_no_error(crossValidate(modeler = modeler,
                                            data = data.dt,
                                            CVSampler = cvSampler(key = data.keys, folds = 3),
                                            costFunction = cf))
    print(cvcost)
  }
})

test_that("precisionCostFunction behaves as expected",{
  #Generate data
  data.dt <- generateMulticlassClassificationData(n_samples = 1000,
                                                  generate_random_weights = TRUE)
  data.keys <- 1:nrow(data.dt)

  #Train simple modeler
  target <- "classification_target"
  regressors <- setdiff(names(data.dt), c(target, "weights"))
  params <- list(
    "num.trees" = 10,
    "max.depth" = 3
  )

  modeler <- rangerClassifierModeler(params = params, target = target, regressors = regressors)
  modeler <- train(modeler, data = data.dt)

  avg_types <- c("micro", "macro", "weighted")
  for (i in 1:length(avg_types)){
    #Instantiate cost function
    cf <- precisionCostFunction(weightsColumn = "weights",
                                class_map = maxScoreClass(),
                                average = avg_types[[i]])
    #Evaluate with getCost
    cost <- expect_no_error(getCost(object = cf, data = data.dt, modeler = modeler, cSample = 1:nrow(data.dt)))
    print(cost)

    cvcost <- expect_no_error(crossValidate(modeler = modeler,
                                            data = data.dt,
                                            CVSampler = cvSampler(key = data.keys, folds = 3),
                                            costFunction = cf))
    print(cvcost)
  }
})

test_that("recallCostFunction behaves as expected",{
  #Generate data
  data.dt <- generateMulticlassClassificationData(n_samples = 1000,
                                                  generate_random_weights = TRUE)
  data.keys <- 1:nrow(data.dt)

  #Train simple modeler
  target <- "classification_target"
  regressors <- setdiff(names(data.dt), c(target, "weights"))
  params <- list(
    "num.trees" = 10,
    "max.depth" = 3
  )

  modeler <- rangerClassifierModeler(params = params, target = target, regressors = regressors)
  modeler <- train(modeler, data = data.dt)

  avg_types <- c("micro", "macro", "weighted")
  for (i in 1:length(avg_types)){
    #Instantiate cost function
    cf <- recallCostFunction(weightsColumn = "weights",
                             class_map = maxScoreClass(),
                             average = avg_types[[i]])

    #Evaluate with getCost
    cost <- expect_no_error(getCost(object = cf, data = data.dt, modeler = modeler, cSample = 1:nrow(data.dt)))
    print(cost)

    cvcost <- expect_no_error(crossValidate(modeler = modeler,
                                            data = data.dt,
                                            CVSampler = cvSampler(key = data.keys, folds = 3),
                                            costFunction = cf))
    print(cvcost)
  }
})

test_that("f1ScoreCostFunction behaves as expected",{
  #Generate data
  data.dt <- generateMulticlassClassificationData(n_samples = 1000,
                                                  generate_random_weights = TRUE)
  data.keys <- 1:nrow(data.dt)

  #Train simple modeler
  target <- "classification_target"
  regressors <- setdiff(names(data.dt), c(target, "weights"))
  params <- list(
    "num.trees" = 10,
    "max.depth" = 3
  )

  modeler <- rangerClassifierModeler(params = params, target = target, regressors = regressors)
  modeler <- train(modeler, data = data.dt)

  avg_types <- c("micro", "macro", "weighted")
  for (i in 1:length(avg_types)){
    #Instantiate cost function
    cf <- f1ScoreCostFunction(weightsColumn = "weights",
                              class_map = maxScoreClass(),
                              average = avg_types[[i]])

    #Evaluate with getCost
    cost <- expect_no_error(getCost(object = cf, data = data.dt, modeler = modeler, cSample = 1:nrow(data.dt)))
    print(cost)

    cvcost <- expect_no_error(crossValidate(modeler = modeler,
                                            data = data.dt,
                                            CVSampler = cvSampler(key = data.keys, folds = 3),
                                            costFunction = cf))
    print(cvcost)
  }
})

test_that("ROCAUCCostFunction behaves as expected",{
  #Generate data
  data.dt <- generateMulticlassClassificationData(n_samples = 1000,
                                                  generate_random_weights = TRUE)
  data.keys <- 1:nrow(data.dt)

  #Train simple modeler
  target <- "classification_target"
  regressors <- setdiff(names(data.dt), c(target, "weights"))
  params <- list(
    "num.trees" = 10,
    "max.depth" = 3
  )

  modeler <- rangerClassifierModeler(params = params, target = target, regressors = regressors)
  modeler <- train(modeler, data = data.dt)

  avg_types <- c("micro", "macro", "weighted")
  for (i in 1:length(avg_types)){
    #Instantiate cost function
    cf <- ROCAUCCostFunction(weightsColumn = "weights",
                             average = avg_types[[i]])

    #Evaluate with getCost
    cost <- expect_no_error(getCost(object = cf, data = data.dt, modeler = modeler, cSample = 1:nrow(data.dt)))
    print(cost$classification_target$cost)

    cvcost <- expect_no_error(crossValidate(modeler = modeler,
                                            data = data.dt,
                                            CVSampler = cvSampler(key = data.keys, folds = 3),
                                            costFunction = cf))
    print(cvcost)
  }
})
