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

# Simple modelers ####

test_that("directRegressorModeler behaves as expected",{
  #Generate data and select only the regression target
  set.seed(273)
  data.dt <- generateRegressionData(n_samples = 100, generate_random_weights = FALSE)
  data.dt <- data.dt[, c("regression_target"), with = FALSE]

  #Define a simple protoTarget such as the median of a sample
  smlp <- sample(x = 1:nrow(data.dt), size = 50, replace = FALSE)
  med <- data.dt[smlp, median(regression_target)]
  data.dt[, proto_target := med]

  #Define modeler parameters
  proto_target <- "proto_target"
  scaleFactor <- 1
  target <- "regression_target"

  #Cost function
  cf <- MSECostFunction()

  #Modeler save path
  path <- "./"
  fileName <- "test_modeler.mlu"

  #Instantiate with expected errors
  expect_error(directRegressorModeler(protoTarget = 1,
                                      scaleFactor = 1,
                                      target = "regression_target"))
  expect_error(directRegressorModeler(protoTarget = "proto_target",
                                      scaleFactor = "1",
                                      target = "regression_target"))
  expect_error(directRegressorModeler(protoTarget = "proto_target",
                                      scaleFactor = 1,
                                      target = NULL))

  #Properly define and train modeler
  modeler <- expect_no_error(directRegressorModeler(protoTarget = "proto_target",
                                                    scaleFactor = 1,
                                                    target = "regression_target"))
  #directModeler requires no training
  cost <- evaluate(modeler,
                   data = data.dt,
                   cost_function = cf)

  #Check that modelSave and modelLoad works
  modelSave(object = modeler,
            path = path,
            fileName = fileName)

  l_mdlr <- modelLoad(path = path,
                      fileName = fileName)

  cost2 <- evaluate(object = modeler,
                    data = data.dt,
                    cost_function = cf)
  print(paste0("Metric for initial model: ", cost))
  print(paste0("Metric for loaded model: ", cost2))
  #Delete current modeler saved file
  file.remove(file.path(path, fileName))
})

test_that("HTRegressorModeler behaves as expected", {
  #Generate data
  set.seed(273)
  data.dt <- generateStratifiedDomainEstimateData(n_samples = 100, generate_random_weights = FALSE)
  head(data.dt)

  #Get one of the variables and define sampling weights proportional to its size
  data.dt <- data.dt[, c("income"), with = FALSE]

  #Define sample size
  n <- round(0.5 * nrow(data.dt))

  #Variables with pi >= 1 have its selection probability set to 1
  data.dt[, pi := n*income/sum(income)]
  data.dt[, pi := ifelse(pi >= 1, 1, pi)]

  #Define sampling weights
  data.dt[, weights := 1/pi]
  head(data.dt)

  #Get sample proportional to sampling probabilities
  data.keys <- 1:nrow(data.dt)
  train.keys <- sample(x = data.keys, size = n, replace = FALSE, prob = data.dt[, pi])
  test.keys <- setdiff(data.keys, train.keys)

  #Modeler parameters
  target <- "income"
  weights_column <- "weights"

  #Cost function
  cf <- MSECostFunction()

  #Modeler save path
  path <- "./"
  fileName <- "test_modeler.mlu"

  #Instantiate with expected errors
  expect_error(HTRegressorModeler(target = 1,
                                  weightsColumn = weights_column))
  expect_error(HTRegressorModeler(target = target,
                                  weightsColumn = NULL))

  #Properly define and train modeler
  modeler <- expect_no_error(HTRegressorModeler(target = target,
                                                weightsColumn = weights_column))

  #crossValidate
  expect_no_error(crossValidate(modeler = modeler,
                                data = data.dt,
                                CVSampler = cvSampler(key = data.keys, folds = 3),
                                costFunction = cf))

  modeler <- train(object = modeler, data = data.dt, subSet = train.keys)

  preds <- getPredictions(object = modeler,
                          data = data.dt,
                          subSet = train.keys)

  cost <- evaluate(object = modeler,
                   data = data.dt,
                   cost_function = cf,
                   subSet = test.keys)

  #Check that modelSave and modelLoad works
  modelSave(object = modeler,
            path = path,
            fileName = fileName)

  l_mdlr <- modelLoad(path = path,
                      fileName = fileName)

  cost2 <- evaluate(object = modeler,
                    data = data.dt,
                    cost_function = cf,
                    subSet = test.keys)
  print(paste0("Metric for initial model: ", cost))
  print(paste0("Metric for loaded model: ", cost2))
  #Delete current modeler saved file
  file.remove(file.path(path, fileName))
})

test_that("strataHTModeler behaves as expected", {
  #Generate data
  set.seed(273)
  data.dt <- generateStratifiedDomainEstimateData(n_samples = 100, generate_random_weights = FALSE)

  #Get one of the variables and define sampling weights proportional to its size
  data.dt <- data.dt[, c("income", "strata_class"), with = FALSE]

  #Define sample size
  n <- round(0.5 * nrow(data.dt))

  #Variables with pi >= 1 have its selection probability set to 1
  data.dt[, pi := n*income/sum(income)]
  data.dt[, pi := ifelse(pi >= 1, 1, pi)]

  #Define sampling weights
  data.dt[, weights := 1/pi]
  head(data.dt)

  #Get sample proportional to sampling probabilities
  data.keys <- 1:nrow(data.dt)
  train.keys <- sample(x = data.keys, size = n, replace = FALSE, prob = data.dt[, pi])
  test.keys <- setdiff(data.keys, train.keys)

  #Modeler parameters
  target <- "income"
  weights_column <- "weights"
  strata_column <- "strata_class"

  #Cost function
  cf <- MSECostFunction()

  #Modeler save path
  path <- "./"
  fileName <- "test_modeler.mlu"

  #Instantiate with expected errors
  expect_error(strataHTRegressorModeler(weightsColumn = 1,
                                        target = target,
                                        strata = strata_column))
  expect_error(strataHTRegressorModeler(weightsColumn = weights_column,
                                        target = NA,
                                        strata = strata_column))
  expect_error(strataHTRegressorModeler(weightsColumn = weights_column,
                                        target = target,
                                        strata = NULL))

  #Define and train modeler
  modeler <- expect_no_error(strataHTRegressorModeler(weightsColumn = weights_column,
                                                      target = target,
                                                      strata = strata_column))

  #crossValidate
  expect_no_error(crossValidate(modeler = modeler,
                                data = data.dt,
                                CVSampler = cvSampler(key = data.keys, folds = 3),
                                costFunction = cf))

  modeler <- train(object = modeler, data = data.dt, subSet = train.keys)

  preds <- getPredictions(object = modeler,
                          data = data.dt,
                          subSet = train.keys)

  cost <- evaluate(object = modeler,
                   data = data.dt,
                   cost_function = cf,
                   subSet = test.keys)

  #Check that modelSave and modelLoad works
  modelSave(object = modeler,
            path = path,
            fileName = fileName)

  l_mdlr <- modelLoad(path = path,
                      fileName = fileName)

  cost2 <- evaluate(object = modeler,
                    data = data.dt,
                    cost_function = cf,
                    subSet = test.keys)
  print(paste0("Metric for initial model: ", cost))
  print(paste0("Metric for loaded model: ", cost2))
  #Delete current modeler saved file
  file.remove(file.path(path, fileName))
})

test_that("strataMeanModeler behaves as expected", {
  set.seed(273)
  data.dt <- generateStratifiedDomainModelAssistedEstimateData(n_samples = 100,
                                                               generate_random_weights = TRUE)

  #Define sampler
  data.keys <- 1:nrow(data.dt)
  train_size = round(0.8 * nrow(data.dt))
  smplr <- srsSampler(key = data.keys, ssize = train_size)
  train.keys <- getSample(smplr)
  test.keys <- setdiff(data.keys, train.keys)

  #Modeler parameters
  target <- "target"# Regression
  weightsColumn <- "weights"
  regressors <- c("strata_class", "domain_class")

  #Cost function
  cf <- MSECostFunction()

  #Modeler save path
  path <- "./"
  fileName <- "test_modeler.mlu"

  #Instantiate with expected errors
  expect_error(strataMeanRegressorModeler(target = 1,
                                          regressors = regressors,
                                          weightsColumn = weightsColumn))
  expect_error(strataMeanRegressorModeler(target = target,
                                          regressors = NA,
                                          weightsColumn = weightsColumn))
  expect_error(strataMeanRegressorModeler(target = target,
                                          regressors = regressors,
                                          weightsColumn = NULL))
  #Error with non-categorical regressors when training
  wrong_modeler <- strataMeanRegressorModeler(target = target,
                                              regressors = c("cost_of_living", "strata_class", "domain_class"),
                                              weightsColumn = weightsColumn)
  expect_error(train(object = wrong_modeler,
                     data = data.dt,
                     subSet = train.keys))

  #Properly define and train modeler
  modeler <- strataMeanRegressorModeler(target = target,
                                        regressors = regressors,
                                        weightsColumn = weightsColumn)


  #crossValidate
  expect_no_error(crossValidate(modeler = modeler,
                                data = data.dt,
                                CVSampler = cvSampler(key = data.keys, folds = 3),
                                costFunction = cf))

  modeler <- train(object = modeler, data = data.dt, subSet = train.keys)

  preds <- getPredictions(object = modeler,
                          data = data.dt,
                          subSet = train.keys)

  cost <- evaluate(object = modeler,
                   data = data.dt,
                   cost_function = cf,
                   subSet = test.keys)

  #Check that modelSave and modelLoad works
  modelSave(object = modeler,
            path = path,
            fileName = fileName)

  l_mdlr <- modelLoad(path = path,
                      fileName = fileName)

  cost2 <- evaluate(object = modeler,
                    data = data.dt,
                    cost_function = cf,
                    subSet = test.keys)
  print(paste0("Metric for initial model: ", cost))
  print(paste0("Metric for loaded model: ", cost2))
  #Delete current modeler saved file
  file.remove(file.path(path, fileName))
})

test_that("h2o regressor modeler behaves as expected", {

  #Define variable to store error (if it happens)
  captured_error <- NULL

  #Start h2o cloud (throws warning)
  suppressWarnings(h2o::h2o.init(port = 54321, startH2O = TRUE))

  #Define tryCatch statement to avoid issues with H2O Cloud
  #As stopping mid-way may cause the H2O session to get stuck.
  tryCatch(
    expr = {
      #Dataset to use
      #Generate data
      data.dt <- generateRegressionData(n_samples = 1000, generate_random_weights = FALSE)

      #Define train/test partitions
      set.seed(273)
      data.keys <- 1:nrow(data.dt)
      train_size = round(0.8 * nrow(data.dt))
      smplr <- srsSampler(key = data.keys, ssize = train_size)
      train.keys <- getSample(smplr)
      test.keys <- setdiff(data.keys, train.keys)

      #Modeler parameters
      h2o_model <- "randomForest"
      params <- list()
      target <- "regression_target"
      regressors <- setdiff(colnames(data.dt), target)

      #Cost function
      cf <- MSECostFunction()

      #Modeler save path
      path <- "./"
      fileName <- "test_modeler.mlu"

      #Instantiate with expected errors
      expect_error(H2ORegressorModeler(model = "wrong_modeler",
                                        params = params,
                                        target = target,
                                        regressors = regressors))

      expect_error(H2ORegressorModeler(model = h2o_model,
                                        params = "params",
                                        target = target,
                                        regressors = regressors))

      expect_error(H2ORegressorModeler(model = h2o_model,
                                        params = params,
                                        target = 1,
                                        regressors = regressors))

      expect_error(H2ORegressorModeler(model = h2o_model,
                                        params = params,
                                        target = target,
                                        regressors = NA))

      #Define modeler correctly
      modeler <- expect_no_error(H2ORegressorModeler(model = h2o_model,
                                                     params = params,
                                                     target = target,
                                                     regressors = regressors))
      #crossValidate
      expect_no_error(crossValidate(modeler = modeler,
                                    data = data.dt,
                                    CVSampler = cvSampler(key = data.keys, folds = 3),
                                    costFunction = cf))

      #Train
      modeler <- train(object = modeler,
                       data = data.dt,
                       subSet = train.keys)

      #Call getPredictions and getClasses
      preds <- getPredictions(object = modeler,
                              data = data.dt,
                              subSet = train.keys)

      #Evaluate
      cost <- evaluate(object = modeler,
                       data = data.dt,
                       cost_function = cf,
                       subSet = test.keys)

      #Check that modelSave and modelLoad works
      modelSave(object = modeler,
                path = path,
                fileName = fileName)

      l_mdlr <- modelLoad(path = path,
                          fileName = fileName)

      cost2 <- evaluate(object = modeler,
                        data = data.dt,
                        cost_function = cf,
                        subSet = test.keys)
      print(paste0("Metric for initial model: ", cost))
      print(paste0("Metric for loaded model: ", cost2))

      #Delete current modeler saved file
      file.remove(file.path(path, fileName))
    },
    #warning = function(w) {
    #  message("Caught a warning: ", w$message)
    #},
    error = function(e) {
      #Save in outer scope
      captured_error <<- e
    },
    finally = {
      h2o::h2o.shutdown(prompt = FALSE)

      if (!is.null(captured_error)) {
        stop(captured_error)
      }
    }
  )#End tryCatch
})

test_that("LGB regressor modeler behaves as expected", {

  #Generate data
  data.dt <- generateRegressionData(n_samples = 1000,
                                    generate_random_weights = FALSE)

  #Define train/test partitions
  set.seed(273)
  data.keys <- 1:nrow(data.dt)
  train_size = round(0.8 * nrow(data.dt))
  smplr <- srsSampler(key = data.keys, ssize = train_size)
  train.keys <- getSample(smplr)
  test.keys <- setdiff(data.keys, train.keys)

  #Modeler parameters
  params <- list(
    "learning_rate" = 0.07,
    "nrounds" = 10,
    "objective" = "regression"
  )
  target <- "regression_target"
  regressors <- setdiff(colnames(data.dt), target)

  #Cost function
  cf <- MSECostFunction()

  #Modeler save path
  path <- "./"
  fileName <- "test_modeler.mlu"

  #Instantiate with expected errors
  expect_error(LGBRegressorModeler(params = "params",
                                    target = target,
                                    regressors = regressors))
  expect_error(LGBRegressorModeler(params = params,
                                    target = 1,
                                    regressors = regressors))
  expect_error(LGBRegressorModeler(params = params,
                                    target = target,
                                    regressors = NA))
  warning_params <- list(
    "learning_rate" = 0.07,
    "objective" = "regression"
  )
  expect_warning(LGBRegressorModeler(params = warning_params,
                                      target = target,
                                      regressors = regressors))
  error_params <- list(
    "learning_rate" = 0.07,
    "nrounds" = 10,
    "objective" = "wrong_objective"
  )
  expect_error(LGBRegressorModeler(params = error_params,
                                    target = target,
                                    regressors = regressors))

  #Properly define and train modeler
  modeler <- expect_no_error(LGBRegressorModeler(params = params,
                                                  target = target,
                                                  regressors = regressors))

  #crossValidate
  expect_no_error(crossValidate(modeler = modeler,
                                data = data.dt,
                                CVSampler = cvSampler(key = data.keys, folds = 3),
                                costFunction = cf))

  modeler <- train(object = modeler,
                   data = data.dt,
                   subSet = train.keys)

  #Get predictions and classes
  preds <- getPredictions(object = modeler,
                          data = data.dt,
                          subSet = train.keys)

  #Obtain cost with evaluate
  cost <- evaluate(object = modeler,
                   data = data.dt,
                   cost_function = cf,
                   subSet = test.keys)

  #Check that modelSave and modelLoad works
  modelSave(object = modeler,
            path = path,
            fileName = fileName)

  l_mdlr <- modelLoad(path = path,
                      fileName = fileName)

  cost2 <- evaluate(object = modeler,
                    data = data.dt,
                    cost_function = cf,
                    subSet = test.keys)
  print(paste0("Metric for initial model: ", cost))
  print(paste0("Metric for loaded model: ", cost2))
  #Delete current modeler saved file
  file.remove(file.path(path, fileName))
})

test_that("ranger regressor modeler behaves as expected", {

  #Generate data
  data.dt <- generateRegressionData(n_samples = 1000,
                                    generate_random_weights = FALSE)

  #Define train/test partitions
  set.seed(273)
  data.keys <- 1:nrow(data.dt)
  train_size = round(0.8 * nrow(data.dt))
  smplr <- srsSampler(key = data.keys, ssize = train_size)
  train.keys <- getSample(smplr)
  test.keys <- setdiff(data.keys, train.keys)

  #Modeler parameters
  target <- "regression_target"
  regressors <- setdiff(colnames(data.dt), target)
  params <- list(
    "num.trees" = 50,
    "max.depth" = 20
  )
  #Cost function
  cf <- MSECostFunction()

  #Modeler save path
  path <- "./"
  fileName <- "test_modeler.mlu"

  #Instantiate with expected errors
  expect_error(rangerRegressorModeler(params = "params",
                                       target = target,
                                       regressors = regressors))
  expect_error(rangerRegressorModeler(params = params,
                                       target = 1,
                                       regressors = regressors))
  expect_error(rangerRegressorModeler(params = params,
                                       target = target,
                                       regressors = NA))

  #Properly define and train modeler
  modeler <- expect_no_error(rangerRegressorModeler(params = params,
                                                     target = target,
                                                     regressors = regressors))
  #cross validate
  expect_no_error(crossValidate(modeler = modeler,
                                data = data.dt,
                                CVSampler = cvSampler(key = data.keys, folds = 3),
                                costFunction = cf))

  modeler <- train(object = modeler,
                   data = data.dt,
                   subSet = train.keys)

  #Get predictions and classes
  preds <- getPredictions(object = modeler,
                          data = data.dt,
                          subSet = train.keys)

  #Obtain cost with evaluate
  cost <- evaluate(object = modeler,
                   data = data.dt,
                   cost_function = cf,
                   subSet = test.keys)

  #Check that modelSave and modelLoad works
  modelSave(object = modeler,
            path = path,
            fileName = fileName)

  l_mdlr <- modelLoad(path = path,
                      fileName = fileName)

  cost2 <- evaluate(object = modeler,
                    data = data.dt,
                    cost_function = cf,
                    subSet = test.keys)
  print(paste0("Metric for initial model: ", cost))
  print(paste0("Metric for loaded model: ", cost2))
  #Delete current modeler saved file
  file.remove(file.path(path, fileName))
})

test_that("catboost regressor modeler behaves as expected", {

  #Generate data
  data.dt <- generateRegressionData(n_samples = 1000,
                                    generate_random_weights = FALSE)

  #Define train/test partitions
  set.seed(273)
  data.keys <- 1:nrow(data.dt)
  train_size = round(0.8 * nrow(data.dt))
  smplr <- srsSampler(key = data.keys, ssize = train_size)
  train.keys <- getSample(smplr)
  test.keys <- setdiff(data.keys, train.keys)

  #Modeler parameters
  params <- list(
    loss_function = 'RMSE',#Regression
    iterations = 50,
    metric_period=10)
  target <- "regression_target"
  regressors <- setdiff(colnames(data.dt), target)

  #Cost function
  cf <- MSECostFunction()

  #Modeler save path
  path <- "./"
  fileName <- "test_modeler.mlu"

  #Instantiate with expected errors
  expect_error(catboostRegressorModeler(params = "params",
                                         target = target,
                                         regressors = regressors))
  expect_error(catboostRegressorModeler(params = params,
                                         target = 1,
                                         regressors = regressors))
  expect_error(catboostRegressorModeler(params = params,
                                         target = target,
                                         regressors = NA))
  expect_error(catboostRegressorModeler(params = wrong_params,
                                         target = target,
                                         regressors = regressors,
                                         val_data = NA))

  #Properly define and train modeler
  modeler <- expect_no_error(catboostRegressorModeler(params = params,
                                                      target = target,
                                                      regressors = regressors))

  #cross validate
  expect_no_error(crossValidate(modeler = modeler,
                                data = data.dt,
                                CVSampler = cvSampler(key = data.keys, folds = 3),
                                costFunction = cf))

  modeler <- train(object = modeler,
                   data = data.dt,
                   subSet = train.keys)

  #Get predictions and classes
  preds <- getPredictions(object = modeler,
                          data = data.dt,
                          subSet = train.keys)

  #Obtain cost with evaluate
  cost <- evaluate(object = modeler,
                   data = data.dt,
                   cost_function = cf,
                   subSet = test.keys)

  #Check that modelSave and modelLoad works
  modelSave(object = modeler,
            path = path,
            fileName = fileName)

  l_mdlr <- modelLoad(path = path,
                      fileName = fileName)

  cost2 <- evaluate(object = modeler,
                    data = data.dt,
                    cost_function = cf,
                    subSet = test.keys)
  print(paste0("Metric for initial model: ", cost))
  print(paste0("Metric for loaded model: ", cost2))
  #Delete current modeler saved file
  file.remove(file.path(path, fileName))
})

test_that("mice regressor modeler behaves as expected", {

  #Generate data
  data.dt <- generateRegressionData(n_samples = 1000,
                                    generate_random_weights = FALSE)

  #Define train/test partitions
  set.seed(273)
  data.keys <- 1:nrow(data.dt)
  train_size = round(0.8 * nrow(data.dt))
  smplr <- srsSampler(key = data.keys, ssize = train_size)
  train.keys <- getSample(smplr)
  test.keys <- setdiff(data.keys, train.keys)

  #Modeler parameters
  target <- "regression_target"
  regressors <- setdiff(colnames(data.dt), target)
  params <- list(m = 1,
                 method = "rf",
                 print = FALSE)
  #Cost function
  cf <- MSECostFunction()

  #Modeler save path
  path <- "./"
  fileName <- "test_modeler.mlu"

  #Instantiate with expected errors
  expect_error(miceRegressorModeler(params = "params",
                                     target = target,
                                     regressors = regressors))
  expect_error(miceRegressorModeler(params = params,
                                     target = 1,
                                     regressors = regressors))
  expect_error(miceRegressorModeler(params = params,
                                     target = target,
                                     regressors = NA))

  #Properly define and train modeler
  modeler <- expect_no_error(miceRegressorModeler(params = params,
                                                   target = target,
                                                   regressors = regressors))
  #cross validate
  expect_no_error(crossValidate(modeler = modeler,
                                data = data.dt,
                                CVSampler = cvSampler(key = data.keys, folds = 3),
                                costFunction = cf))

  modeler <- train(object = modeler,
                   data = data.dt,
                   subSet = train.keys)

  #Get predictions and classes
  preds <- getPredictions(object = modeler,
                          data = data.dt,
                          subSet = train.keys)

  #Obtain cost with evaluate
  cost <- evaluate(object = modeler,
                   data = data.dt,
                   cost_function = cf,
                   subSet = test.keys)

  #Check that modelSave and modelLoad works
  modelSave(object = modeler,
            path = path,
            fileName = fileName)

  l_mdlr <- modelLoad(path = path,
                      fileName = fileName)

  cost2 <- evaluate(object = modeler,
                    data = data.dt,
                    cost_function = cf,
                    subSet = test.keys)
  print(paste0("Metric for initial model: ", cost))
  print(paste0("Metric for loaded model: ", cost2))
  #Delete current modeler saved file
  file.remove(file.path(path, fileName))
})

# Complex modelers ####

#Check this one first as there is an associated check in
#all other complex modelers related to it
test_that("memoryRegressorModeler behaves as expected", {
  #Generate data
  set.seed(273)
  data.dt <- generateStratifiedDomainModelAssistedEstimateData(n_samples = 1000, generate_random_weights = FALSE)
  data2.dt <- generateStratifiedDomainModelAssistedEstimateData(n_samples = 100, generate_random_weights = FALSE)
  data3.dt <- generateStratifiedDomainModelAssistedEstimateData(n_samples = 50, generate_random_weights = FALSE)

  #Get relevant variables from data
  data.keys <- 1:nrow(data.dt)
  target <- "target"# Regression
  regressors <- setdiff(names(data.dt), target)

  #Define main sampler
  train_size = round(0.8 * nrow(data.dt))
  smplr <- srsSampler(key = data.keys, ssize = train_size)
  train.keys <- getSample(smplr)
  test.keys <- setdiff(data.keys, train.keys)

  #Define base modeler
  params <- list(
    "num.trees" = 50,
    "max.depth" = 20
  )
  base_modeler <- rangerRegressorModeler(params = params, target = target, regressors = regressors)

  #Modeler parameters
  data <- list(data2.dt)
  maxData <- 3L

  #Cost function
  cf <- MSECostFunction()

  #Modeler save path
  path <- "./"
  fileName <- "test_modeler.mlu"

  #Instantiate with expected errors
  expect_error(memoryRegressorModeler(data = list(data2.dt,data2.dt,data2.dt,data2.dt),
                                      modeler = base_modeler,
                                      maxData = maxData))
  expect_error(memoryRegressorModeler(data = data,
                                      modeler = base_modeler,
                                      maxData = 0))
  m_modeler <- memoryRegressorModeler(data = data,
                                    modeler = base_modeler,
                                    maxData = maxData)
  expect_error(memoryRegressorModeler(data = data,
                                      modeler = m_modeler,
                                      maxData = maxData))
  #Error with classifierModeler
  wrong_base_modeler <- rangerClassifierModeler(params = params,
                                                target = target,
                                                regressors = regressors)
  expect_error(memoryRegressorModeler(data = data,
                                      modeler = wrong_base_modeler,
                                      maxData = maxData))

  #Properly define modeler
  modeler <- memoryRegressorModeler(data = data,
                                    modeler = base_modeler,
                                    maxData = maxData)

  #CrossValidate
  expect_no_error(crossValidate(modeler = modeler,
                                data = data.dt,
                                CVSampler = cvSampler(key = data.keys, folds = 3),
                                costFunction = cf))

  #Test addMemory
  expect_error(addMemory(object = NA,
                         data = data3.dt))
  expect_error(addMemory(object = modeler,
                         data = NULL))
  modeler <- addMemory(object = modeler,
                       data = data3.dt)

  #Test cleanMemory
  expect_error(cleanMemory(object = NA,
                           index = 2))
  modeler <- cleanMemory(object = modeler, index = 2)

  #Test updateMemory
  expect_error(updateMemory(object = modeler,
                            data = data3.dt,
                            index = -1))
  expect_error(updateMemory(object = NA,
                            data = data3.dt,
                            index = 2))
  expect_error(updateMemory(object = modeler,
                            data = NULL,
                            index = 2))
  modeler <- updateMemory(object = modeler,
                          data = data3.dt,
                          index = 2)

  #Train and getPredictions
  modeler <- train(modeler,
                   data = data.dt,
                   subSet = train.keys)

  #Get predictions and classes
  preds <- getPredictions(object = modeler,
                          data = data.dt,
                          subSet = train.keys)

  #Obtain cost with evaluate
  cost <- evaluate(object = modeler,
                   data = data.dt,
                   cost_function = cf,
                   subSet = test.keys)

  #Check that modelSave and modelLoad works
  modelSave(object = modeler,
            path = path,
            fileName = fileName)

  l_mdlr <- modelLoad(path = path,
                      fileName = fileName)

  cost2 <- evaluate(object = modeler,
                    data = data.dt,
                    cost_function = cf,
                    subSet = test.keys)
  print(paste0("Metric for initial model: ", cost))
  print(paste0("Metric for loaded model: ", cost2))
  #Delete current modeler saved file
  file.remove(file.path(path, fileName))
})

test_that("benchRegressorModeler behaves as expected", {
  #Generate data
  original_data.dt <- generateRegressionData(n_samples = 1000,
                                             generate_random_weights = FALSE)

  #Define new variables in the data that have a specific restriction
  #In our case: regression_target = A + B
  data.dt <- data.table::copy(original_data.dt)
  data.dt[, A := round(runif(dim(data.dt)[1], min = 0, max = 5))]
  data.dt[, B := regression_target - A]

  #Define train/test partitions
  set.seed(273)
  data.keys <- 1:nrow(data.dt)
  train_size = round(0.8 * nrow(data.dt))
  smplr <- srsSampler(key = data.keys, ssize = train_size)
  train.keys <- getSample(smplr)
  test.keys <- setdiff(data.keys, train.keys)

  #Define submodeler
  #Note that none of the modelers sees any other target variable besides than their own.
  params_b1 <- list(
    "num.trees" = 20,
    "max.depth" = 20
  )
  target_b1 <- "regression_target"
  regressors_b1 <- setdiff(colnames(original_data.dt), target_b1)

  params_b2 <- list(
    "num.trees" = 30,
    "max.depth" = 10
  )
  target_b2 <- "A"
  regressors_b2 <- setdiff(colnames(original_data.dt), c(target_b2, target_b1))

  params_b3 <- list(
    "num.trees" = 40,
    "max.depth" = 15
  )
  target_b3 <- "B"
  regressors_b3 <- setdiff(colnames(original_data.dt), c(target_b3, target_b1))

  modeler1 <- rangerRegressorModeler(params = params_b1,
                                     target = target_b1,
                                     regressors = regressors_b1)
  modeler2 <- rangerRegressorModeler(params = params_b2,
                                     target = target_b2,
                                     regressors = regressors_b2)
  modeler3 <- rangerRegressorModeler(params = params_b3,
                                     target = target_b3,
                                     regressors = regressors_b3)

  #Define benchModeler parameters
  submodels <-  list(modeler1,modeler2,modeler3)
  benchmarks <- "A+B-regression_target==0"

  #Cost function
  cf <- MSECostFunction()

  #Modeler save path
  path <- "./"
  fileName <- "test_modeler.mlu"

  #TESTING STARTS HERE
  #Instantiate with expected errors

  #Check that restrictions are linear
  expect_error(benchRegressorModeler(submodels = submodels, benchmarks = "A^2+B-regression_target==0"))

  #Check that there are no more benchvars than models
  expect_error(benchRegressorModeler(submodels = submodels, benchmarks = "A+B+C-regression_target==0"))

  expect_error(benchRegressorModeler(submodels = list(1,2,3),
                                     benchmarks = benchmarks))

  #memoryModeler as submodeler raises error
  m_modeler <- memoryRegressorModeler(data = list(data.dt),
                                      modeler = modeler1)
  expect_error(benchRegressorModeler(submodels = list(m_modeler,modeler2,modeler3),
                                     benchmarks = benchmarks))

  #One of the submodelers has the target var of another submodeler in its regressors
  wrong_modeler1 <- modeler1
  wrong_modeler1 <- setRegressors(wrong_modeler1,
                                  newRegressors = c(target_b2, getRegressors(wrong_modeler1)))
  expect_error(benchRegressorModeler(submodels = list(wrong_modeler1,modeler2,modeler3),
                                     benchmarks = benchmarks))

  #Properly instantiate modeler
  modeler <- benchRegressorModeler(submodels = submodels,
                                   benchmarks = benchmarks)

  #cross validate
  expect_no_error(crossValidate(modeler = modeler,
                                data = data.dt,
                                CVSampler = cvSampler(key = data.keys, folds = 3),
                                costFunction = cf))

  modeler <- train(object = modeler,
                   data = data.dt,
                   subSet = train.keys)

  #Get predictions and classes
  preds <- getPredictions(object = modeler,
                          data = data.dt,
                          subSet = train.keys)

  #Obtain cost with evaluate
  cost <- evaluate(object = modeler,
                   data = data.dt,
                   cost_function = cf,
                   subSet = test.keys)

  #Check that modelSave and modelLoad works
  modelSave(object = modeler,
            path = path,
            fileName = fileName)

  l_mdlr <- modelLoad(path = path,
                      fileName = fileName)

  cost2 <- evaluate(object = modeler,
                    data = data.dt,
                    cost_function = cf,
                    subSet = test.keys)
  print(paste0("Metric for initial model: ", cost))
  print(paste0("Metric for loaded model: ", cost2))
  #Delete current modeler saved file
  file.remove(file.path(path, fileName))
})

test_that("classifRegRegressorModeler behaves as expected", {
  #Generate data
  data.dt <- generateRegressionData(n_samples = 1000, generate_random_weights = FALSE)

  # Convert a reasonable number of registers to zero
  data.dt[regression_target < 0, regression_target := 0]
  table(data.dt[, regression_target == 0])
  #We will apply  a classifReg modeler to first predict whether the result is 0
  #or not, and then apply regression over the remaining data

  #Define train/test partitions
  data.keys <- 1:nrow(data.dt)
  train_size = round(0.8 * nrow(data.dt))
  smplr <- srsSampler(key = data.keys, ssize = train_size)
  train.keys <- getSample(smplr)
  test.keys <- setdiff(data.keys, train.keys)

  #Target variable name is the same for both modelers
  target <- "regression_target"
  #Regressors could be different, but it is not necessary in this case.
  regressors <- setdiff(names(data.dt), target)

  #Define a classifier modeler
  params_cls <- list(
    "num.trees" = 50,
    "max.depth" = 20
  )

  cls_modeler <- rangerClassifierModeler(params = params_cls,
                                         target = target,
                                         regressors = regressors)
  #Define a regressor modeler
  params_reg <- list(
    "learning_rate" = 0.07,
    "nrounds" = 100,
    "objective" = "regression"
  )
  reg_modeler <- LGBRegressorModeler(params = params_reg,
                                     target = target,
                                     regressors = regressors)

  #classifRegModeler parameters
  specialCats <- 0
  epsilon <- 0

  #Cost function
  cf <- MSECostFunction()

  #Modeler save path
  path <- "./"
  fileName <- "test_modeler.mlu"

  #TESTING STARTS HERE
  #Instantiate with expected errors
  #Different target variable
  wrong_cls_modeler <- cls_modeler
  wrong_cls_modeler <- setTarget(wrong_cls_modeler, newTarget = "wrong_target")
  expect_error(classifRegRegressorModeler(classifModeler = wrong_cls_modeler,
                                          regModeler = reg_modeler,
                                          specialCats = specialCats,
                                          epsilon = epsilon))

  expect_error(classifRegRegressorModeler(classifModeler = 1,
                                          regModeler = reg_modeler,
                                          specialCats = specialCats,
                                          epsilon = epsilon))

  expect_error(classifRegRegressorModeler(classifModeler = cls_modeler,
                                          regModeler = 1,
                                          specialCats = specialCats,
                                          epsilon = epsilon))

  expect_error(classifRegRegressorModeler(classifModeler = cls_modeler,
                                          regModeler = reg_modeler,
                                          specialCats = specialCats,
                                          epsilon = -1))

  expect_error(classifRegRegressorModeler(classifModeler = cls_modeler,
                             regModeler = reg_modeler,
                             specialCats = c(0,0.05),
                             epsilon = 0.1))

  #memoryModeler as regModeler raises error
  m_reg_modeler <- memoryRegressorModeler(data = list(data.dt),
                                          modeler = reg_modeler)
  expect_error(classifRegRegressorModeler(classifModeler = cls_modeler,
                                          regModeler = m_reg_modeler,
                                          specialCats = specialCats,
                                          epsilon = epsilon))
  #benchModeler as regModeler raises error
  b_reg_modeler <- benchRegressorModeler(submodels = list(reg_modeler, reg_modeler),
                                         benchmarks = "regression_target-regression_target==0")
  expect_error(classifRegRegressorModeler(classifModeler = cls_modeler,
                                          regModeler = b_reg_modeler,
                                          specialCats = specialCats,
                                          epsilon = epsilon))

  #Properly instantiate and train modeler
  modeler <- classifRegRegressorModeler(classifModeler = cls_modeler,
                                        regModeler = reg_modeler,
                                        specialCats = specialCats,
                                        epsilon = epsilon)

  #cross validate
  expect_no_error(crossValidate(modeler = modeler,
                                data = data.dt,
                                CVSampler = cvSampler(key = data.keys, folds = 3),
                                costFunction = cf))

  modeler <- train(object = modeler,
                   data = data.dt,
                   subSet = train.keys)

  #Get predictions and classes
  preds <- getPredictions(object = modeler,
                          data = data.dt,
                          subSet = train.keys)

  #Obtain cost with evaluate
  cost <- evaluate(object = modeler,
                   data = data.dt,
                   cost_function = cf,
                   subSet = test.keys)

  #Check that modelSave and modelLoad works
  modelSave(object = modeler,
            path = path,
            fileName = fileName)

  l_mdlr <- modelLoad(path = path,
                      fileName = fileName)

  cost2 <- evaluate(object = modeler,
                    data = data.dt,
                    cost_function = cf,
                    subSet = test.keys)
  print(paste0("Metric for initial model: ", cost))
  print(paste0("Metric for loaded model: ", cost2))
  #Delete current modeler saved file
  file.remove(file.path(path, fileName))
})

test_that("prePredRegressorModeler behaves as expected", {
  #Generate data
  data.dt <- generateRegressionData(n_samples = 1000, generate_random_weights = FALSE)
  #Create an additional column that depends on regression_target
  #This will be the actual target of this modeler
  data.dt[, main_regression_target := regression_target + float_1 - float_2]

  #Define train/test partitions
  set.seed(273)
  data.keys <- 1:nrow(data.dt)
  train_size = round(0.8 * nrow(data.dt))
  smplr <- srsSampler(key = data.keys, ssize = train_size)
  train.keys <- getSample(smplr)
  test.keys <- setdiff(data.keys, train.keys)

  #Define previous modeler (this will predict regression_target)
  pred_params <- list(
    "num.trees" = 50,
    "max.depth" = 20
  )
  pred_target <- "regression_target"
  pred_regressors <- setdiff(colnames(data.dt), c(pred_target, "main_regression_target"))
  predModeler <- rangerRegressorModeler(params = pred_params,
                                        target = pred_target,
                                        regressors = pred_regressors)

  #prePredModeler parameters
  #Define main modeler
  #(this will predict main_regression_target,
  #with regression_target as an additional regressor)
  main_params <- list(
    "num.trees" = 50,
    "max.depth" = 20
  )
  main_target <- "main_regression_target"
  main_regressors <- setdiff(colnames(data.dt), c(main_target))
  mainModeler <- rangerRegressorModeler(params = main_params,
                                        target = main_target,
                                        regressors = main_regressors)

  #Cost function
  cf <- MSECostFunction()

  #Modeler save path
  path <- "./"
  fileName <- "test_modeler.mlu"

  #Instantiate with expected errors
  #memoryModeler as regModeler raises error
  m_wrong_modeler <- memoryRegressorModeler(data = list(data.dt),
                                          modeler = predModeler)
  expect_error(prePredRegressorModeler(predModeler = m_wrong_modeler,
                                       mainModeler = mainModeler))
  expect_error(prePredRegressorModeler(predModeler = predModeler,
                                       mainModeler = m_wrong_modeler))
  #wrong regressors for mainModeler
  wrong_mainModeler <- rangerRegressorModeler(params = main_params,
                                              target = main_target,
                                              regressors = c("reg_1", "reg_2"))
  expect_error(prePredRegressorModeler(predModeler = predModeler,
                                       mainModeler = wrong_mainModeler))

  #Properly instantiate and train model
  modeler <- prePredRegressorModeler(predModeler = predModeler,
                                            mainModeler = mainModeler)

  #cross validate
  expect_no_error(crossValidate(modeler = modeler,
                                data = data.dt,
                                CVSampler = cvSampler(key = data.keys, folds = 3),
                                costFunction = cf))

  modeler <- train(object = modeler,
                   data = data.dt,
                   subSet = train.keys)

  #Get predictions and classes
  preds <- getPredictions(object = modeler,
                          data = data.dt,
                          subSet = train.keys)

  #Obtain cost with evaluate
  cost <- evaluate(object = modeler,
                   data = data.dt,
                   cost_function = cf,
                   subSet = test.keys)

  #Check that modelSave and modelLoad works
  modelSave(object = modeler,
            path = path,
            fileName = fileName)

  l_mdlr <- modelLoad(path = path,
                      fileName = fileName)

  cost2 <- evaluate(object = modeler,
                    data = data.dt,
                    cost_function = cf,
                    subSet = test.keys)
  print(paste0("Metric for initial model: ", cost))
  print(paste0("Metric for loaded model: ", cost2))
  #Delete current modeler saved file
  file.remove(file.path(path, fileName))
})

test_that("preproRegressorModeler behaves as expected", {
  #Generate data
  data.dt <- generateRegressionData(n_samples = 1000, generate_random_weights = FALSE)

  #Define train/test partitions
  data.keys <- 1:nrow(data.dt)
  train_size = round(0.8 * nrow(data.dt))
  smplr <- srsSampler(key = data.keys, ssize = train_size)
  train.keys <- getSample(smplr)
  test.keys <- setdiff(data.keys, train.keys)

  target <- "regression_target"
  regressors <- setdiff(colnames(data.dt), target)

  #Preprocessing parameters
  prepro_params <- list(modeler_type = NULL, pretrained_min_max = NULL)

  #Define preproObject
  prepro_object <- preproObject(preproFunction = minMaxScalerPreproFunction,
                                params = prepro_params)

  #Define a regressor modeler
  params <- list(
    "num.trees" = 50,
    "max.depth" = 20
  )
  simple_modeler <- rangerRegressorModeler(params = params,
                                           target = target,
                                           regressors = regressors)

  #Cost function
  cf <- MSECostFunction()

  #Modeler save path
  path <- "./"
  fileName <- "test_modeler.mlu"

  #Instantiate with expected errors
  expect_error(preproRegressorModeler(modeler = "simple_modeler",
                                      preproObject = prepro_object))
  suppressWarnings(expect_error(preproRegressorModeler(modeler = simple_modeler,
                                                       preproObject = "prepro_object")))

  #Properly define and train modeler
  modeler <- preproRegressorModeler(modeler = simple_modeler,
                                    preproObject = prepro_object)

  #cross validate
  expect_no_error(crossValidate(modeler = modeler,
                                data = data.dt,
                                CVSampler = cvSampler(key = data.keys, folds = 3),
                                costFunction = cf))

  modeler <- train(object = modeler,
                   data = data.dt,
                   subSet = train.keys)

  #Get predictions and classes
  preds <- getPredictions(object = modeler,
                          data = data.dt,
                          subSet = train.keys)

  #Obtain cost with evaluate
  cost <- evaluate(object = modeler,
                   data = data.dt,
                   cost_function = cf,
                   subSet = test.keys)

  #Check that modelSave and modelLoad works
  modelSave(object = modeler,
            path = path,
            fileName = fileName)

  l_mdlr <- modelLoad(path = path,
                      fileName = fileName)

  cost2 <- evaluate(object = modeler,
                    data = data.dt,
                    cost_function = cf,
                    subSet = test.keys)
  print(paste0("Metric for initial model: ", cost))
  print(paste0("Metric for loaded model: ", cost2))
  #Delete current modeler saved file
  file.remove(file.path(path, fileName))
})

#Complex modeler combinations ####

test_that("prepro and classifReg work when combined", {
  #Generate data
  data.dt <- generateRegressionData(n_samples = 1000, generate_random_weights = FALSE)

  # Convert a reasonable number of registers to zero
  data.dt[regression_target < 0, regression_target := 0]
  table(data.dt[, regression_target == 0])
  #We will apply  a classifReg modeler to first predict whether the result is 0
  #or not, and then apply regression over the remaining data

  #Define train/test partitions
  data.keys <- 1:nrow(data.dt)
  train_size = round(0.8 * nrow(data.dt))
  smplr <- srsSampler(key = data.keys, ssize = train_size)
  train.keys <- getSample(smplr)
  test.keys <- setdiff(data.keys, train.keys)

  #Target variable name is the same for both modelers
  target <- "regression_target"
  #Regressors could be different, but it is not necessary in this case.
  regressors <- setdiff(names(data.dt), target)

  #Define a classifier modeler
  params_cls <- list(
    "num.trees" = 50,
    "max.depth" = 20
  )

  cls_modeler <- rangerClassifierModeler(params = params_cls,
                                         target = target,
                                         regressors = regressors)
  #Define a regressor modeler
  params_reg <- list(
    "learning_rate" = 0.07,
    "nrounds" = 100,
    "objective" = "regression"
  )
  reg_modeler <- LGBRegressorModeler(params = params_reg,
                                     target = target,
                                     regressors = regressors)

  clreg_modeler <- classifRegRegressorModeler(classifModeler = cls_modeler,
                                              regModeler = reg_modeler,
                                              specialCats = 0)

  #Preprocessing parameters
  prepro_params <- list(modeler_type = "classifRegModeler", pretrained_min_max = NULL)

  #Define preproObject
  prepro_object <- preproObject(preproFunction = minMaxScalerPreproFunction,
                                params = prepro_params)

  #Cost function
  cf <- MSECostFunction()

  #Modeler save path
  path <- "./"
  fileName <- "test_modeler.mlu"

  modeler <- preproRegressorModeler(modeler = clreg_modeler,
                                    preproObject = prepro_object)
  #cross validate
  expect_no_error(crossValidate(modeler = modeler,
                                data = data.dt,
                                CVSampler = cvSampler(key = data.keys, folds = 3),
                                costFunction = cf))

  modeler <- train(object = modeler,
                   data = data.dt,
                   subSet = train.keys)

  #Get predictions and classes
  preds <- getPredictions(object = modeler,
                          data = data.dt,
                          subSet = train.keys)

  #Obtain cost with evaluate
  cost <- evaluate(object = modeler,
                   data = data.dt,
                   cost_function = cf,
                   subSet = test.keys)

  #Check that modelSave and modelLoad works
  modelSave(object = modeler,
            path = path,
            fileName = fileName)

  l_mdlr <- modelLoad(path = path,
                      fileName = fileName)

  cost2 <- evaluate(object = modeler,
                    data = data.dt,
                    cost_function = cf,
                    subSet = test.keys)
  print(paste0("Metric for initial model: ", cost))
  print(paste0("Metric for loaded model: ", cost2))
  #Delete current modeler saved file
  file.remove(file.path(path, fileName))
})

test_that("prepro and prePred work when combined", {
  #Generate data
  data.dt <- generateRegressionData(n_samples = 1000, generate_random_weights = FALSE)
  #Create an additional column that depends on regression_target
  #This will be the actual target of this modeler
  data.dt[, main_regression_target := regression_target + float_1 - float_2]

  #Define train/test partitions
  set.seed(273)
  data.keys <- 1:nrow(data.dt)
  train_size = round(0.8 * nrow(data.dt))
  smplr <- srsSampler(key = data.keys, ssize = train_size)
  train.keys <- getSample(smplr)
  test.keys <- setdiff(data.keys, train.keys)

  #Define previous modeler (this will predict regression_target)
  params <- list(
    "num.trees" = 50,
    "max.depth" = 20
  )
  target <- "regression_target"
  regressors <- setdiff(colnames(data.dt), c(target, "main_regression_target"))

  predModeler <- rangerRegressorModeler(params = params, target = target, regressors = regressors)

  #Define main modeler
  #(this will predict main_regression_target,
  #with regression_target as an additional regressor)
  params <- list(
    "num.trees" = 50,
    "max.depth" = 20
  )
  target <- "main_regression_target"
  regressors <- setdiff(colnames(data.dt), c(target))

  mainModeler <- rangerRegressorModeler(params = params, target = target, regressors = regressors)

  prePredModeler <- prePredRegressorModeler(predModeler = predModeler, mainModeler = mainModeler)

  #Preprocessing parameters
  prepro_params <- list(modeler_type = "prePredModeler", pretrained_min_max = NULL)

  #Define preproObject
  prepro_object <- preproObject(preproFunction = minMaxScalerPreproFunction,
                                params = prepro_params)

  #Cost function
  cf <- MSECostFunction()

  #Modeler save path
  path <- "./"
  fileName <- "test_modeler.mlu"

  modeler <- preproRegressorModeler(modeler = prePredModeler,
                                    preproObject = prepro_object)

  #cross validate
  expect_no_error(crossValidate(modeler = modeler,
                                data = data.dt,
                                CVSampler = cvSampler(key = data.keys, folds = 3),
                                costFunction = cf))

  modeler <- train(object = modeler,
                   data = data.dt,
                   subSet = train.keys)

  #Get predictions and classes
  preds <- getPredictions(object = modeler,
                          data = data.dt,
                          subSet = train.keys)

  #Obtain cost with evaluate
  cost <- evaluate(object = modeler,
                   data = data.dt,
                   cost_function = cf,
                   subSet = test.keys)

  #Check that modelSave and modelLoad works
  modelSave(object = modeler,
            path = path,
            fileName = fileName)

  l_mdlr <- modelLoad(path = path,
                      fileName = fileName)

  cost2 <- evaluate(object = modeler,
                    data = data.dt,
                    cost_function = cf,
                    subSet = test.keys)
  print(paste0("Metric for initial model: ", cost))
  print(paste0("Metric for loaded model: ", cost2))
  #Delete current modeler saved file
  file.remove(file.path(path, fileName))
})

test_that("prepro and bench work when combined", {
  #Generate data
  original_data.dt <- generateRegressionData(n_samples = 1000, generate_random_weights = FALSE)

  #Define train/test partitions
  data.keys <- 1:nrow(original_data.dt)
  train_size = round(0.8 * nrow(original_data.dt))
  smplr <- srsSampler(key = data.keys, ssize = train_size)
  train.keys <- getSample(smplr)
  test.keys <- setdiff(data.keys, train.keys)

  #Define new variables in the data that have a specific restriction
  #In our case: regression_target = A + B
  data.dt <- data.table::copy(original_data.dt)
  data.dt[, A := round(runif(dim(data.dt)[1], min = 0, max = 5))]
  data.dt[, B := regression_target - A]

  #Define submodeler parameters
  #Note that none of the modelers sees any other target variable besides than their own.
  params_b1 <- list(
    "num.trees" = 20,
    "max.depth" = 20
  )
  target_b1 <- "regression_target"
  regressors_b1 <- setdiff(colnames(original_data.dt), target_b1)

  params_b2 <- list(
    "num.trees" = 30,
    "max.depth" = 10
  )
  target_b2 <- "A"
  regressors_b2 <- setdiff(colnames(original_data.dt), target_b1)

  params_b3 <- list(
    "num.trees" = 40,
    "max.depth" = 15
  )
  target_b3 <- "B"
  regressors_b3 <- setdiff(colnames(original_data.dt), target_b1)

  modeler1 <- rangerRegressorModeler(params = params_b1, target = target_b1, regressors = regressors_b1)

  modeler2 <- rangerRegressorModeler(params = params_b2, target = target_b2, regressors = regressors_b2)

  modeler3 <- rangerRegressorModeler(params = params_b3, target = target_b3, regressors = regressors_b3)

  #Define bench regressor modeler
  b_modeler <- benchRegressorModeler(submodels = list(modeler1,modeler2,modeler3),
                                     benchmarks = "A+B-regression_target==0")

  #Preprocessing parameters
  prepro_params <- list(modeler_type = "benchModeler", pretrained_min_max = NULL)

  #Define preproObject
  prepro_object <- preproObject(preproFunction = minMaxScalerPreproFunction,
                                params = prepro_params)

  #Cost function
  cf <- MSECostFunction()

  #Modeler save path
  path <- "./"
  fileName <- "test_modeler.mlu"

  modeler <- preproRegressorModeler(modeler = b_modeler,
                                    preproObject = prepro_object)

  #cross validate
  expect_no_error(crossValidate(modeler = modeler,
                                data = data.dt,
                                CVSampler = cvSampler(key = data.keys, folds = 3),
                                costFunction = cf))

  modeler <- train(object = modeler,
                   data = data.dt,
                   subSet = train.keys)

  #Get predictions and classes
  preds <- getPredictions(object = modeler,
                          data = data.dt,
                          subSet = train.keys)

  #Obtain cost with evaluate
  cost <- evaluate(object = modeler,
                   data = data.dt,
                   cost_function = cf,
                   subSet = test.keys)

  #Check that modelSave and modelLoad works
  modelSave(object = modeler,
            path = path,
            fileName = fileName)

  l_mdlr <- modelLoad(path = path,
                      fileName = fileName)

  cost2 <- evaluate(object = modeler,
                    data = data.dt,
                    cost_function = cf,
                    subSet = test.keys)
  print(paste0("Metric for initial model: ", cost))
  print(paste0("Metric for loaded model: ", cost2))
  #Delete current modeler saved file
  file.remove(file.path(path, fileName))
})

test_that("memory and prepro work when combined", {
  #The preproModeler goes inside the memoryModeler
  #data2 and data3 do not need to be preprocessed, since they automatically get
  #processed when train.preproModeler gets called.
  set.seed(273)
  #Generate data
  data.dt <- generateRegressionData(n_samples = 1000, generate_random_weights = FALSE)
  data2.dt <- generateRegressionData(n_samples = 900, generate_random_weights = FALSE)
  data3.dt <- generateRegressionData(n_samples = 100, generate_random_weights = FALSE)

  #Define train/test partitions
  data.keys <- 1:nrow(data.dt)
  train_size = round(0.8 * nrow(data.dt))
  smplr <- srsSampler(key = data.keys, ssize = train_size)
  train.keys <- getSample(smplr)
  test.keys <- setdiff(data.keys, train.keys)

  target <- "regression_target"
  regressors <- setdiff(colnames(data.dt), target)

  #Preprocessing parameters
  prepro_params <- list(modeler_type = NULL, pretrained_min_max = NULL)

  #Define preproObject
  prepro_object <- preproObject(preproFunction = minMaxScalerPreproFunction, params = prepro_params)

  #Define a regressor modeler
  params <- list(
    "num.trees" = 50,
    "max.depth" = 20
  )

  simple_modeler <- rangerRegressorModeler(params = params,
                                           target = target,
                                           regressors = regressors)

  p_modeler <- preproRegressorModeler(modeler = simple_modeler,
                                      preproObject = prepro_object)


  #Cost function
  cf <- MSECostFunction()

  #Modeler save path
  path <- "./"
  fileName <- "test_modeler.mlu"

  modeler <- memoryRegressorModeler(list(data2.dt, data3.dt), modeler = p_modeler)
  #cross validate
  expect_no_error(crossValidate(modeler = modeler,
                                data = data.dt,
                                CVSampler = cvSampler(key = data.keys, folds = 3),
                                costFunction = cf))

  modeler <- train(object = modeler,
                   data = data.dt,
                   subSet = train.keys)

  #Get predictions and classes
  preds <- getPredictions(object = modeler,
                          data = data.dt,
                          subSet = train.keys)

  #Obtain cost with evaluate
  cost <- evaluate(object = modeler,
                   data = data.dt,
                   cost_function = cf,
                   subSet = test.keys)

  #Check that modelSave and modelLoad works
  modelSave(object = modeler,
            path = path,
            fileName = fileName)

  l_mdlr <- modelLoad(path = path,
                      fileName = fileName)

  cost2 <- evaluate(object = modeler,
                    data = data.dt,
                    cost_function = cf,
                    subSet = test.keys)
  print(paste0("Metric for initial model: ", cost))
  print(paste0("Metric for loaded model: ", cost2))
  #Delete current modeler saved file
  file.remove(file.path(path, fileName))
})

test_that("prePred with 2 classifReg work when combined", {
  #Generate data
  data.dt <- generateRegressionData(n_samples = 1000, generate_random_weights = FALSE)

  # Convert a reasonable number of registers to zero
  data.dt[regression_target < 0, regression_target := 0]

  #This will be the actual target of this modeler
  data.dt[, main_regression_target := regression_target + float_1 - float_2]
  # Convert a reasonable number of registers to zero
  data.dt[main_regression_target < 0, main_regression_target := 0]

  #Check that both regression targets hace a relatively large amount of zeros
  print(table(data.dt[, regression_target == 0]))
  print(table(data.dt[, main_regression_target == 0]))

  #Define train/test partitions
  data.keys <- 1:nrow(data.dt)
  train_size = round(0.8 * nrow(data.dt))
  smplr <- srsSampler(key = data.keys, ssize = train_size)
  train.keys <- getSample(smplr)
  test.keys <- setdiff(data.keys, train.keys)

  ### Define previous modeler (this will predict regression_target) ####

  #Target variable name is the same for both modelers
  pred_target <- "regression_target"
  #Regressors must exclude main target and current modeler target
  pred_regressors <- setdiff(names(data.dt), c(pred_target, "main_regression_target"))

  #Define a classifier modeler
  pred_params_cls <- list(
    "num.trees" = 50,
    "max.depth" = 20
  )

  cls_modeler1 <- rangerClassifierModeler(params = pred_params_cls,
                                          target = pred_target,
                                          regressors = pred_regressors)
  #Define a regressor modeler
  pred_params_reg <- list(
    "num.trees" = 50,
    "max.depth" = 20
  )
  reg_modeler1 <- rangerRegressorModeler(params = pred_params_reg,
                                         target = pred_target,
                                         regressors = pred_regressors)

  predModeler <- classifRegRegressorModeler(classifModeler = cls_modeler1,
                                            regModeler = reg_modeler1,
                                            specialCats = 0)

  ### Define main modeler (this will predict main_regression_target) ####

  #Target variable name is the same for both modelers
  main_target <- "main_regression_target"
  #Regressors must exclude main target and current modeler target
  main_regressors <- setdiff(names(data.dt), c(main_target))

  #Define a classifier modeler
  main_params_cls <- list(
    "num.trees" = 50,
    "max.depth" = 20
  )

  cls_modeler2 <- rangerClassifierModeler(params = main_params_cls,
                                          target = main_target,
                                          regressors = main_regressors)
  #Define a regressor modeler
  main_params_reg <- list(
    "num.trees" = 50,
    "max.depth" = 20
  )
  reg_modeler2 <- rangerRegressorModeler(params = main_params_reg,
                                         target = main_target,
                                         regressors = main_regressors)

  mainModeler <- classifRegRegressorModeler(classifModeler = cls_modeler2,
                                            regModeler = reg_modeler2,
                                            specialCats = 0)

  #Cost function
  cf <- MSECostFunction()

  #Modeler save path
  path <- "./"
  fileName <- "test_modeler.mlu"

  ### Join into single object ####

  modeler <- prePredRegressorModeler(predModeler = predModeler,
                                     mainModeler = mainModeler)

  #cross validate
  expect_no_error(crossValidate(modeler = modeler,
                                data = data.dt,
                                CVSampler = cvSampler(key = data.keys, folds = 3),
                                costFunction = cf))

  modeler <- train(object = modeler,
                   data = data.dt,
                   subSet = train.keys)

  #Get predictions and classes
  preds <- getPredictions(object = modeler,
                          data = data.dt,
                          subSet = train.keys)

  #Obtain cost with evaluate
  cost <- evaluate(object = modeler,
                   data = data.dt,
                   cost_function = cf,
                   subSet = test.keys)

  #Check that modelSave and modelLoad works
  modelSave(object = modeler,
            path = path,
            fileName = fileName)

  l_mdlr <- modelLoad(path = path,
                      fileName = fileName)

  cost2 <- evaluate(object = modeler,
                    data = data.dt,
                    cost_function = cf,
                    subSet = test.keys)
  print(paste0("Metric for initial model: ", cost))
  print(paste0("Metric for loaded model: ", cost2))
  #Delete current modeler saved file
  file.remove(file.path(path, fileName))
})
