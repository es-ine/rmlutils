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

#' @title Saves a modeler object.
#' @description Saves a modeler object to disk.
#'
#' @param object \code{\link{modeler}} object to store
#' @param path Path to store the model
#' @param fileName Name under which save.
#'
#' @seealso \code{\link{regressorModeler}}
#' @seealso \code{\link{classifierModeler}}
#' @seealso \code{\link{modelLoad}}
#' @seealso \code{\link{modelClean}}
#'
#' @examples
#'
#' #Modeler to save
#' modeler <- directRegressorModeler(protoTarget = "proto_target",
#'                                   scaleFactor = 1,
#'                                   target = "regression_target")
#'
#' path <-  "../../stored_models"
#' filename <- "myModel"
#' #modelSave(modeler, path, filename)
#'
#' @export
modelSave <- function(object, path, fileName) {
  UseMethod("modelSave")
}

#' @rdname modelSave
#' @export
#'
modelSave.directModeler <- function(object, path, fileName) {
  #Validate and get full path for .mlu file
  full_path <- RMLutils:::.validate_modeler_save_load_arguments(path,
                                                     fileName,
                                                     action = "save")
  #Save main object to disk
  main_object_path <- paste0(path, "/", "directRegressorModeler", ".RData")
  save(object, file = main_object_path)

  files_to_zip <- c(main_object_path)

  #Add files to .mlu file
  zip::zipr(
    zipfile = full_path,
    files = files_to_zip
  )

  #Remove uncompressed files
  invisible(file.remove(files_to_zip))
}

#modelSave.directModeler <- function(object, path, fileName) {
#  full_path <- RMLutils:::.validate_modeler_save_load_arguments(path, fileName, action = "save")
#  if(!object$isTrained){
#    warning("Saving untrained modeler. If this was unexpected, please review if your modeler has actually been trained.")
#  }
#  save(object, file = full_path, compress = "gzip")
#}

#' @rdname modelSave
#' @export
modelSave.HTRegressorModeler <- function(object, path, fileName) {
  #Validate and get full path for .mlu file
  full_path <- RMLutils:::.validate_modeler_save_load_arguments(path,
                                                     fileName,
                                                     action = "save")
  if(!object$isTrained){
    warning("Saving untrained modeler. Please review your inputs if this was unexpected.")
  }
  #Save main object to disk
  main_object_path <- paste0(path, "/", "HTRegressorModeler", ".RData")
  save(object, file = main_object_path)

  files_to_zip <- c(main_object_path)

  #Add files to .mlu file
  zip::zipr(
    zipfile = full_path,
    files = files_to_zip
  )

  #Remove uncompressed files
  invisible(file.remove(files_to_zip))
}

#' @rdname modelSave
#' @export
modelSave.strataHTModeler <- function(object, path, fileName) {
  #Validate and get full path for .mlu file
  full_path <- RMLutils:::.validate_modeler_save_load_arguments(path,
                                                     fileName,
                                                     action = "save")
  if(!object$isTrained){
    warning("Saving untrained modeler. Please review your inputs if this was unexpected.")
  }
  #Save main object to disk
  main_object_path <- paste0(path, "/", "strataHTRegressorModeler", ".RData")
  save(object, file = main_object_path)

  files_to_zip <- c(main_object_path)

  #Add files to .mlu file
  zip::zipr(
    zipfile = full_path,
    files = files_to_zip
  )

  #Remove uncompressed files
  invisible(file.remove(files_to_zip))
}

#' @rdname modelSave
#' @export
modelSave.strataMeanModeler <- function(object, path, fileName) {
  #Validate and get full path for .mlu file
  full_path <- RMLutils:::.validate_modeler_save_load_arguments(path,
                                                     fileName,
                                                     action = "save")
  if(!object$isTrained){
    warning("Saving untrained modeler. Please review your inputs if this was unexpected.")
  }
  #Save main object to disk
  main_object_path <- paste0(path, "/", "strataMeanRegressorModeler", ".RData")
  save(object, file = main_object_path)

  files_to_zip <- c(main_object_path)

  #Add files to .mlu file
  zip::zipr(
    zipfile = full_path,
    files = files_to_zip
  )

  #Remove uncompressed files
  invisible(file.remove(files_to_zip))
}

#' @rdname modelSave
#' @export
modelSave.benchModeler <- function(object, path, fileName) {
  #Validate and get full path for .mlu file
  full_path <- RMLutils:::.validate_modeler_save_load_arguments(path,
                                                     fileName,
                                                     action = "save")
  #Take everything besides submodels
  plain_contents <- names(object)[!names(object) %in% c("submodels")]
  plainObject <- object[plain_contents]
  class(plainObject) <- class(object)

  #Save main object to disk
  main_object_path <- paste0(path, "/", "benchModeler", ".RData")
  save(plainObject, file = main_object_path)

  files_to_zip <- c(main_object_path)

  submodeler_files <- paste0("submodel_", 1:length(object$submodels), ".mlu")

  for (i in 1:length(object$submodels)){
    #Save submodeler
    RMLutils::modelSave(object = object$submodels[[i]],
                        path = path,
                        fileName = submodeler_files[[i]])
    #Add files to files_to_zip
    files_to_zip <- c(files_to_zip, file.path(path, submodeler_files[[i]]))
  }

  #Add files to .mlu file
  zip::zipr(
    zipfile = full_path,
    files = files_to_zip
  )

  #Remove uncompressed files
  invisible(file.remove(files_to_zip))
}

#' @rdname modelSave
#' @export
modelSave.H2OModeler <- function(object, path, fileName) {
  #Validate and get full path for .mlu file
  full_path <- RMLutils:::.validate_modeler_save_load_arguments(path,
                                                     fileName,
                                                     action = "save")
  #Take everything besides trainedModel
  plain_contents <- names(object)[names(object) != "trainedModel"]
  plainObject <- object[plain_contents]
  class(plainObject) <- class(object)

  #Save main object to disk
  main_object_path <- paste0(path, "/", "LGBModeler", ".RData")
  save(plainObject, file = main_object_path)

  files_to_zip <- c(main_object_path)

  #Save trained model and add to files-to-zip
  if (object$isTrained){
    trained_model_path <- file.path(path, "H2OModeler")
    h2o::h2o.download_model(model = object$trainedModel,
                            path = path,
                            filename = "H2OModeler")
    files_to_zip <- c(files_to_zip, trained_model_path)
  }else{
    warning("Saving untrained modeler. Please review your inputs if this was unexpected.")
  }

  #Add files to .mlu file
  zip::zipr(
    zipfile = full_path,
    files = files_to_zip
  )

  #Remove uncompressed files
  invisible(file.remove(files_to_zip))
}
#' @rdname modelSave
#' @export
modelSave.preproModeler <- function(object, path, fileName) {
  #Validate and get full path for .mlu file
  full_path <- RMLutils:::.validate_modeler_save_load_arguments(path,
                                                     fileName,
                                                     action = "save")
  #Take everything besides modeler and data
  plain_contents <- names(object)[!names(object) %in% c("modeler")]
  plainObject <- object[plain_contents]
  class(plainObject) <- class(object)

  #Save main object to disk
  main_object_path <- paste0(path, "/", "preproModeler", ".RData")
  save(plainObject, file = main_object_path)

  files_to_zip <- c(main_object_path)

  #Save internal modeler
  fileNamePrepro <- "preproModeler_preproModel.mlu"
  RMLutils::modelSave(object = object$modeler, path = path, fileName = fileNamePrepro)

  #Add files to files_to_zip
  files_to_zip <- c(files_to_zip, file.path(path, fileNamePrepro))

  #Add files to .mlu file
  zip::zipr(
    zipfile = full_path,
    files = files_to_zip
  )

  #Remove uncompressed files
  invisible(file.remove(files_to_zip))
}

#' @rdname modelSave
#' @export
modelSave.classifRegRegressorModeler <- function(object, path, fileName) {
  #Validate and get full path for .mlu file
  full_path <- RMLutils:::.validate_modeler_save_load_arguments(path,
                                                     fileName,
                                                     action = "save")
  #Take everything besides classifModeler and regModeler
  plain_contents <- names(object)[!names(object) %in% c("classifModeler", "regModeler")]
  plainObject <- object[plain_contents]

  class(plainObject) <- class(object)

  #Save main object to disk
  main_object_path <- paste0(path, "/", "classifRegModeler", ".RData")
  save(plainObject, file = main_object_path)

  files_to_zip <- c(main_object_path)

  #Define temporary name for classif and reg modelers
  fileName_classif <- paste0("classifRegModeler", "_classif")
  fileName_reg <- paste0("classifRegModeler", "_reg")

  #.mlu extension is added automatically
  RMLutils::modelSave(object = object$classifModeler, path = path, fileName = fileName_classif)
  RMLutils::modelSave(object = object$regModeler, path = path, fileName = fileName_reg)

  #Define classif and reg modeler paths and add them to files_to_zip
  classif_path <- file.path(path, paste0(fileName_classif,".mlu"))
  reg_path <- file.path(path, paste0(fileName_reg, ".mlu"))
  files_to_zip <- c(files_to_zip, classif_path, reg_path)

  #Add files to .mlu file
  zip::zipr(
    zipfile = full_path,
    files = files_to_zip
  )

  #Remove uncompressed files
  invisible(file.remove(files_to_zip))
}

#' @rdname modelSave
#' @export
modelSave.LGBModeler <- function(object, path, fileName) {
  #Validate and get full path for .mlu file
  full_path <- RMLutils:::.validate_modeler_save_load_arguments(path,
                                                     fileName,
                                                     action = "save")
  #Take everything besides trainedModel
  plain_contents <- names(object)[names(object) != "trainedModel"]
  plainObject <- object[plain_contents]
  class(plainObject) <- class(object)

  #Save main object to disk
  main_object_path <- paste0(path, "/", "LGBModeler", ".RData")
  save(plainObject, file = main_object_path)

  files_to_zip <- c(main_object_path)

  #Save trained model and add to files-to-zip
  if (object$isTrained){
    trained_model_path <- file.path(path, "LGBModeler")
    lightgbm::lgb.save(object$trainedModel, trained_model_path)
    files_to_zip <- c(files_to_zip, trained_model_path)
  }else{
    warning("Saving untrained modeler. Please review your inputs if this was unexpected.")
  }

  #Add files to .mlu file
  zip::zipr(
    zipfile = full_path,
    files = files_to_zip
  )

  #Remove uncompressed files
  invisible(file.remove(files_to_zip))
}

#' @rdname modelSave
#' @export
modelSave.rangerModeler <- function(object, path, fileName) {

  #Validate and get full path for .mlu file
  full_path <- RMLutils:::.validate_modeler_save_load_arguments(path,
                                                     fileName,
                                                     action = "save")
  #Take everything besides trainedModel
  plain_contents <- names(object)[names(object) != "trainedModel"]
  plainObject <- object[plain_contents]
  class(plainObject) <- class(object)

  #Save main object to disk
  main_object_path <- paste0(path, "/", "rangerModeler", ".RData")
  save(plainObject, file = main_object_path)

  files_to_zip <- c(main_object_path)

  #Save trained model and add to files-to-zip
  if (object$isTrained){
    trained_model_path <- file.path(path, "rangerModeler")
    saveRDS(object$trainedModel, trained_model_path)
    files_to_zip <- c(files_to_zip, trained_model_path)
  }else{
    warning("Saving untrained modeler. Please review your inputs if this was unexpected.")
  }

  #Add files to .mlu file
  zip::zipr(
    zipfile = full_path,
    files = files_to_zip
  )

  #Remove uncompressed files
  invisible(file.remove(files_to_zip))
}

#' @rdname modelSave
#' @export
modelSave.catboostModeler <- function(object, path, fileName) {
  #Validate and get full path for .mlu file
  full_path <- RMLutils:::.validate_modeler_save_load_arguments(path,
                                                     fileName,
                                                     action = "save")
  #Take everything besides trainedModel
  plain_contents <- names(object)[names(object) != "trainedModel"]
  plainObject <- object[plain_contents]
  class(plainObject) <- class(object)

  #Save main object to disk
  main_object_path <- paste0(path, "/", "catboostModeler", ".RData")
  save(plainObject, file = main_object_path)

  files_to_zip <- c(main_object_path)

  #Save trained model and add to files-to-zip
  if (object$isTrained){
    trained_model_path <- file.path(path, "catboostModeler")
    catboost::catboost.save_model(object$trainedModel, model_path = trained_model_path)
    files_to_zip <- c(files_to_zip, trained_model_path)
  }else{
    warning("Saving untrained modeler. Please review your inputs if this was unexpected.")
  }

  #Add files to .mlu file
  zip::zipr(
    zipfile = full_path,
    files = files_to_zip
  )

  #Remove uncompressed files
  invisible(file.remove(files_to_zip))
}

#' @rdname modelSave
#' @export
modelSave.miceModeler <- function(object, path, fileName) {
  #Validate and get full path for .mlu file
  full_path <- RMLutils:::.validate_modeler_save_load_arguments(path,
                                                     fileName,
                                                     action = "save")
  #Take everything besides trainedModel
  plain_contents <- names(object)[names(object) != "trainedModel"]
  plainObject <- object[plain_contents]
  class(plainObject) <- class(object)

  #Save main object to disk
  main_object_path <- paste0(path, "/", "miceModeler", ".RData")
  save(plainObject, file = main_object_path)

  files_to_zip <- c(main_object_path)

  #Save trained model and add to files-to-zip
  if (object$isTrained){
    trained_model_path <- file.path(path, "miceModeler")
    saveRDS(object$trainedModel, trained_model_path)
    files_to_zip <- c(files_to_zip, trained_model_path)
  }else{
    warning("Saving untrained modeler. Please review your inputs if this was unexpected.")
  }

  #Add files to .mlu file
  zip::zipr(
    zipfile = full_path,
    files = files_to_zip
  )

  #Remove uncompressed files
  invisible(file.remove(files_to_zip))
}

#' @rdname modelSave
#' @export
modelSave.prePredModeler <- function(object, path, fileName) {
  #Validate and get full path for .mlu file
  full_path <- RMLutils:::.validate_modeler_save_load_arguments(path,
                                                     fileName,
                                                     action = "save")
  #Take everything besides modeler and data
  plain_contents <- names(object)[!names(object) %in% c("predModeler", "mainModeler")]
  plainObject <- object[plain_contents]
  class(plainObject) <- class(object)

  #Save main object to disk
  main_object_path <- paste0(path, "/", "prePredModeler", ".RData")
  save(plainObject, file = main_object_path)

  files_to_zip <- c(main_object_path)

  fileName_preds <- "prePredModeler_preds.mlu"
  fileName_main <- "prePredModeler_main.mlu"

  RMLutils::modelSave(object = object$predModeler,
                      path = path,
                      fileName = fileName_preds)

  RMLutils::modelSave(object = object$mainModeler,
                      path = path,
                      fileName = fileName_main)

  #Add files to files_to_zip
  files_to_zip <- c(files_to_zip,
                    file.path(path, fileName_preds),
                    file.path(path, fileName_main))

  #Add files to .mlu file
  zip::zipr(
    zipfile = full_path,
    files = files_to_zip
  )

  #Remove uncompressed files
  invisible(file.remove(files_to_zip))
}

#' @rdname modelSave
#' @export
modelSave.memoryModeler <- function(object, path, fileName) {
  #Validate and get full path for .mlu file
  full_path <- RMLutils:::.validate_modeler_save_load_arguments(path,
                                                     fileName,
                                                     action = "save")
  #Take everything besides modeler and data
  plain_contents <- names(object)[!names(object) %in% c("modeler", "data")]
  plainObject <- object[plain_contents]
  class(plainObject) <- class(object)

  #Save main object to disk
  main_object_path <- paste0(path, "/", "memoryModeler", ".RData")
  save(plainObject, file = main_object_path)

  files_to_zip <- c(main_object_path)

  #Save data and internal modeler
  fileName_modeler <- "memoryModeler_modeler.mlu"
  fileName_data <- "memoryModeler_data.rds"

  RMLutils::modelSave(object = object$modeler,
                      path = path,
                      fileName = fileName_modeler)
  data <- object$data
  saveRDS(data, file = paste0(path, "/", fileName_data))
  rm(data)

  #Add files to files_to_zip
  files_to_zip <- c(files_to_zip,
                    file.path(path, fileName_modeler),
                    file.path(path, fileName_data))

  #Add files to .mlu file
  zip::zipr(
    zipfile = full_path,
    files = files_to_zip
  )

  #Remove uncompressed files
  invisible(file.remove(files_to_zip))
}
