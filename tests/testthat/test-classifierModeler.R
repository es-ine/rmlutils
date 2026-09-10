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

test_that("h2o classifier modeler behaves as expected", {

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
      data.dt <- generateBinaryClassificationData(n_samples = 1000, generate_random_weights = FALSE)

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
      target <- "classification_target"
      regressors <- setdiff(colnames(data.dt), target)

      #Cost function
      cf <- accuracyCostFunction()

      #Modeler save path
      path <- "./"
      fileName <- "test_modeler.mlu"

      #Instantiate with expected errors
      expect_error(H2OClassifierModeler(model = "wrong_modeler",
                                        params = params,
                                        target = target,
                                        regressors = regressors))

      expect_error(H2OClassifierModeler(model = h2o_model,
                                        params = "params",
                                        target = target,
                                        regressors = regressors))

      expect_error(H2OClassifierModeler(model = h2o_model,
                                        params = params,
                                        target = 1,
                                        regressors = regressors))

      expect_error(H2OClassifierModeler(model = h2o_model,
                                        params = params,
                                        target = target,
                                        regressors = NA))

      #Define modeler correctly
      modeler <- expect_no_error(H2OClassifierModeler(model = h2o_model,
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

      clss <- getClasses(object = modeler,
                         data = data.dt,
                         subSet = train.keys,
                         class_mapping = maxScoreClass())
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

test_that("LGB classifier modeler behaves as expected", {

  #Generate data
  data.dt <- generateBinaryClassificationData(n_samples = 1000,
                                              generate_random_weights = FALSE)

  #Define train/test partitions
  set.seed(273)
  data.keys <- 1:nrow(data.dt)
  train_size = round(0.8 * nrow(data.dt))
  smplr <- srsSampler(key = data.keys, ssize = train_size)
  train.keys <- getSample(smplr)
  test.keys <- setdiff(data.keys, train.keys)

  #Modeler parameters
  target <- "classification_target"
  regressors <- setdiff(colnames(data.dt), target)
  params <- list(
    "learning_rate" = 0.07,
    "nrounds" = 10,
    "objective" = "multiclass",
    "num_class" = length(unique(data.dt[, classification_target]))
  )
  #Cost function
  cf <- accuracyCostFunction()

  #Modeler save path
  path <- "./"
  fileName <- "test_modeler.mlu"

  #Instantiate with expected errors
  expect_error(LGBClassifierModeler(params = "params",
                                    target = target,
                                    regressors = regressors))
  expect_error(LGBClassifierModeler(params = params,
                                    target = 1,
                                    regressors = regressors))
  expect_error(LGBClassifierModeler(params = params,
                                               target = target,
                                               regressors = NA))
  warning_params <- list(
    "learning_rate" = 0.07,
    "objective" = "multiclass",
    "num_class" = length(unique(data.dt[, classification_target]))
  )
  expect_warning(LGBClassifierModeler(params = warning_params,
                                                 target = target,
                                                 regressors = regressors))
  error_params <- list(
    "learning_rate" = 0.07,
    "nrounds" = 10,
    "objective" = "wrong_objective",
    "num_class" = length(unique(data.dt[, classification_target]))
  )
  expect_error(LGBClassifierModeler(params = error_params,
                                               target = target,
                                               regressors = regressors))

  #Properly define and train modeler
  modeler <- expect_no_error(LGBClassifierModeler(params = params,
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

  clss <- getClasses(object = modeler,
                     data = data.dt,
                     subSet = train.keys,
                     class_mapping = maxScoreClass())

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

test_that("ranger classifier modeler behaves as expected", {

  #Generate data
  data.dt <- generateBinaryClassificationData(n_samples = 1000,
                                              generate_random_weights = FALSE)

  #Define train/test partitions
  set.seed(273)
  data.keys <- 1:nrow(data.dt)
  train_size = round(0.8 * nrow(data.dt))
  smplr <- srsSampler(key = data.keys, ssize = train_size)
  train.keys <- getSample(smplr)
  test.keys <- setdiff(data.keys, train.keys)

  #Modeler parameters
  target <- "classification_target"
  regressors <- setdiff(colnames(data.dt), target)
  params <- list(
    "num.trees" = 50,
    "max.depth" = 20
  )
  #Cost function
  cf <- accuracyCostFunction()

  #Modeler save path
  path <- "./"
  fileName <- "test_modeler.mlu"

  #Instantiate with expected errors
  expect_error(rangerClassifierModeler(params = "params",
                                    target = target,
                                    regressors = regressors))
  expect_error(rangerClassifierModeler(params = params,
                                    target = 1,
                                    regressors = regressors))
  expect_error(rangerClassifierModeler(params = params,
                                    target = target,
                                    regressors = NA))

  #Properly define and train modeler
  modeler <- expect_no_error(rangerClassifierModeler(params = params,
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

  clss <- getClasses(object = modeler,
                     data = data.dt,
                     subSet = train.keys,
                     class_mapping = maxScoreClass())

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

test_that("catboost classifier modeler behaves as expected", {

  #Generate data
  data.dt <- generateBinaryClassificationData(n_samples = 1000,
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
    #loss_function = 'MultiClass',
    #loss_function = 'MultiClassOneVsAll',
    loss_function = 'Logloss',#Binary
    #loss_function = 'CrossEntropy',#Binary
    iterations = 50,
    metric_period=10)
  target <- "classification_target"
  regressors <- setdiff(colnames(data.dt), target)
  #Cost function
  cf <- accuracyCostFunction()

  #Modeler save path
  path <- "./"
  fileName <- "test_modeler.mlu"

  #Instantiate with expected errors
  expect_error(catboostClassifierModeler(params = "params",
                                       target = target,
                                       regressors = regressors))
  expect_error(catboostClassifierModeler(params = params,
                                       target = 1,
                                       regressors = regressors))
  expect_error(catboostClassifierModeler(params = params,
                                       target = target,
                                       regressors = NA))
  wrong_params <- list(
    #loss_function = 'MultiClass',
    #loss_function = 'MultiClassOneVsAll',
    loss_function = 'wrong_loss_function',#Binary
    #loss_function = 'CrossEntropy',#Binary
    iterations = 50,
    metric_period=10)
  expect_error(catboostClassifierModeler(params = wrong_params,
                                         target = target,
                                         regressors = regressors))
  expect_error(catboostClassifierModeler(params = wrong_params,
                                         target = target,
                                         regressors = regressors,
                                         val_data = NA))
  expect_error(catboostClassifierModeler(params = params,
                                         target = target,
                                         regressors = regressors,
                                         positive_class = 1))

  #Properly define and train modeler
  modeler <- expect_no_error(catboostClassifierModeler(params = params,
                                                       target = target,
                                                       regressors = regressors,
                                                       positive_class = "positive"))

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

  clss <- getClasses(object = modeler,
                     data = data.dt,
                     subSet = train.keys,
                     class_mapping = maxScoreClass())

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

test_that("mice classifier modeler behaves as expected", {

  #Generate data
  data.dt <- generateBinaryClassificationData(n_samples = 1000,
                                              generate_random_weights = FALSE)

  #Define train/test partitions
  set.seed(273)
  data.keys <- 1:nrow(data.dt)
  train_size = round(0.8 * nrow(data.dt))
  smplr <- srsSampler(key = data.keys, ssize = train_size)
  train.keys <- getSample(smplr)
  test.keys <- setdiff(data.keys, train.keys)

  #Modeler parameters
  target <- "classification_target"
  regressors <- setdiff(colnames(data.dt), target)
  params <- list(m = 1,
                 method = "rf",
                 print = FALSE)
  #Cost function
  cf <- accuracyCostFunction()

  #Modeler save path
  path <- "./"
  fileName <- "test_modeler.mlu"

  #Instantiate with expected errors
  expect_error(miceClassifierModeler(params = "params",
                                     target = target,
                                     regressors = regressors))
  expect_error(miceClassifierModeler(params = params,
                                     target = 1,
                                     regressors = regressors))
  expect_error(miceClassifierModeler(params = params,
                                     target = target,
                                     regressors = NA))

  #Properly define and train modeler
  modeler <- expect_no_error(miceClassifierModeler(params = params,
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

  clss <- getClasses(object = modeler,
                     data = data.dt,
                     subSet = train.keys,
                     class_mapping = maxScoreClass())

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

#TODO: preproClassifierModeler

# Complex Modelers ####

test_that("prepro classifier modeler behaves as expected", {
  #Generate data
  data.dt <- generateMulticlassClassificationData(n_samples = 1000, generate_random_weights = FALSE)

  #Define train/test partitions
  set.seed(273)
  data.keys <- 1:nrow(data.dt)
  train_size = round(0.8 * nrow(data.dt))
  smplr <- srsSampler(key = data.keys, ssize = train_size)
  train.keys <- getSample(smplr)
  test.keys <- setdiff(data.keys, train.keys)

  #Internal model parameters
  target <- "classification_target"
  regressors <- setdiff(colnames(data.dt), target)
  params <- list(
    "num.trees" = 50,
    "max.depth" = 20
  )

  #Cost function
  cf <- accuracyCostFunction()

  #Modeler save path
  path <- "./"
  fileName <- "test_modeler.mlu"

  #Internal modeler
  modeler <- rangerClassifierModeler(params = params,
                                     target = target,
                                     regressors = regressors)

  #Preprocessing parameters
  prepro_params <- list(modeler_type = NULL, pretrained_min_max = NULL)

  #Expected errors with preproObject
  expect_error(preproObject(preproFunction = 1,
                            params = prepro_params))
  expect_error(preproObject(preproFunction = minMaxScalerPreproFunction,
                            params = NULL))
  expect_error(preproObject(preproFunction = sample,
                            params = prepro_params))

  #Instantiate preproObject properly
  prepro_object <- expect_no_error(preproObject(preproFunction = minMaxScalerPreproFunction,
                                                params = prepro_params))

  #Expected errors with preproClassifierModeler
  expect_error(preproClassifierModeler(modeler = 1,
                                       preproObject = prepro_object))
  expect_error(preproClassifierModeler(modeler = modeler,
                                       preproObject = 1))

  p_modeler <- expect_no_error(preproClassifierModeler(modeler = modeler,
                                                       preproObject = prepro_object))

  expect_no_error(crossValidate(modeler = p_modeler,
                                data = data.dt,
                                CVSampler = cvSampler(key = train.keys, folds = 3),
                                costFunction = cf))

  p_modeler <- train(object = p_modeler, data = data.dt, subSet = train.keys)

  #cf <- confusionMatrixCostFunction()
  cf <- accuracyCostFunction()
  #Obtain cost with evaluate
  cost <- evaluate(object = p_modeler,
                   data = data.dt,
                   cost_function = cf,
                   subSet = train.keys)
  #Check that modelSave and modelLoad works
  modelSave(object = p_modeler,
            path = path,
            fileName = fileName)

  l_mdlr <- modelLoad(path = path,
                      fileName = fileName)

  cost2 <- evaluate(object = p_modeler,
                    data = data.dt,
                    cost_function = cf,
                    subSet = train.keys)
  print(paste0("Metric for initial model: ", cost))
  print(paste0("Metric for loaded model: ", cost2))
  #Delete current modeler saved file
  file.remove(file.path(path, fileName))
})
