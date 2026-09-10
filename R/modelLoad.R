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

#' @title Loads a modeler object (new version).
#'
#' @description Loads a modeler object from disk.
#'
#' @param path path to the stored modeler object
#' @param fileName Name of the file where the modeler is stored.
#' If the fileName does not end with .mlu, this method will attempt to load
#' the modeler using an older version of modelLoad. It is recommended,
#' however, to load and save again older modelers, as such method will be
#' removed in the future. When saving older modelers, they will automatically
#' be converted to their new version.
#'
#' @seealso \code{\link{regressorModeler}}
#' @seealso \code{\link{classifierModeler}}
#' @seealso \code{\link{modelSave}}
#' @seealso \code{\link{modelClean}}
#'
#' @examples
#' path <- "../../models/stored_models"
#' filename <- "my_model.mlu"
#'
#' #mdl <- modelLoad(path, filename)
#'
#' @export
modelLoad <- function(path, fileName, object = NULL) {
  UseMethod("modelLoad")
}

#' @rdname modelLoad
#' @export
modelLoad.character <- function(path, fileName, object = NULL) {
  if(endsWith(fileName, ".mlu")){
    #New loading method
    full_path <- RMLutils:::.validate_modeler_save_load_arguments(path,
                                                       fileName,
                                                       action = "load")
    if(!file.exists(full_path)){
      stop("Provided file was not found. Please review your inputs.")
    }
    #Unzip in temporary folder
    #The temporary folder must only contain one single RData at a time
    temp_dir <- file.path(tempdir(), "rmlutils_temp")
    dir.create(temp_dir, showWarnings = FALSE)
    zip::unzip(full_path, exdir = temp_dir)
    #List files and load main .RData file
    unzipped_files <- list.files(temp_dir)
    main_file <- unzipped_files[endsWith(unzipped_files, ".RData")]
    if(length(main_file) != 1){
      stop(paste0("Only a single .RData file must be present when loading a modeler, but ",
                  length(main_file),
                  " were found.\n",
                  "Current RData files:\n ",
                  paste0(main_file, collapse = " \n ")))
    }
    object <- get(load(file.path(temp_dir, main_file)))
    #Call modelLoad method with object
    #modelLoad immediately deletes the current .RData,
    #so that submodelers may be easily loaded with the same method
    loadedObject <- RMLutils::modelLoad(object,
                                        temp_dir,
                                        main_file)
    #Clean temp file
    #unlink(temp_dir, recursive = TRUE)
  }else{
    #Attempt old loading method
    loadedObject <- RMLutils::modelLoadOld(path,
                                           fileName,
                                           object = NULL)
  }


  return(loadedObject)
}

#' @rdname modelLoad
#' @export
modelLoad.directModeler <- function(object, path, fileName) {
  #Since main object has already been loaded, remove it
  unlink(file.path(path, fileName), recursive = TRUE)
  return(object)
}

#' @rdname modelLoad
#' @export
modelLoad.HTModeler <- function(object, path, fileName) {
  #Since main object has already been loaded, remove it
  unlink(file.path(path, fileName), recursive = TRUE)
  return(object)
}

#' @rdname modelLoad
#' @export
modelLoad.strataHTModeler <- function(object, path, fileName) {
  #Since main object has already been loaded, remove it
  unlink(file.path(path, fileName), recursive = TRUE)
  return(object)
}

#' @rdname modelLoad
#' @export
modelLoad.strataMeanModeler <- function(object, path, fileName) {
  #Since main object has already been loaded, remove it
  unlink(file.path(path, fileName), recursive = TRUE)
  return(object)
}

#' @rdname modelLoad
#' @export
modelLoad.LGBModeler <- function(object, path, fileName) {
  #Since main object has already been loaded, remove it
  unlink(file.path(path, fileName), recursive = TRUE)

  #Check if trained model exists
  if(file.exists(file.path(path, "LGBModeler"))){
    object$trainedModel <- lightgbm::lgb.load(file.path(path, "LGBModeler"))
  }else{
    object$trainedModel <- NULL
  }
  #Clean temp files
  files_to_rm <- c(file.path(path, fileName),
                   file.path(path, "LGBModeler"))
  unlink(files_to_rm, recursive = TRUE)
  return(object)
}

#' @rdname modelLoad
#' @export
modelLoad.rangerModeler <- function(object, path, fileName) {
  #Since main object has already been loaded, remove it
  unlink(file.path(path, fileName), recursive = TRUE)

  #Check if trained model exists
  if(file.exists(file.path(path, "rangerModeler"))){
    object$trainedModel <- readRDS(file.path(path, "rangerModeler"))
  }else{
    object$trainedModel <- NULL
  }
  #Clean temp files
  files_to_rm <- c(file.path(path, fileName),
                   file.path(path, "rangerModeler"))
  unlink(files_to_rm, recursive = TRUE)
  return(object)
}

#' @rdname modelLoad
#' @export
modelLoad.catboostModeler <- function(object, path, fileName) {
  #Since main object has already been loaded, remove it
  unlink(file.path(path, fileName), recursive = TRUE)

  #Check if trained model exists
  if(file.exists(file.path(path, "catboostModeler"))){
    object$trainedModel <- catboost::catboost.load_model(file.path(path, "catboostModeler"))
  }else{
    object$trainedModel <- NULL
  }
  #Clean temp files
  files_to_rm <- c(file.path(path, fileName),
                   file.path(path, "catboostModeler"))
  unlink(files_to_rm, recursive = TRUE)

  return(object)
}

#' @rdname modelLoad
#' @export
modelLoad.miceModeler <- function(object, path, fileName) {
  #Since main object has already been loaded, remove it
  unlink(file.path(path, fileName), recursive = TRUE)

  #Check if trained model exists
  if(file.exists(file.path(path, "miceModeler"))){
    object$trainedModel <- readRDS(file.path(path, "miceModeler"))
  }else{
    object$trainedModel <- NULL
  }
  #Clean temp files
  files_to_rm <- c(file.path(path, fileName),
                   file.path(path, "miceModeler"))
  unlink(files_to_rm, recursive = TRUE)

  return(object)
}

#' @rdname modelLoad
#' @export
modelLoad.H2OModeler <- function(object, path, fileName) {
  #Since main object has already been loaded, remove it
  unlink(file.path(path, fileName), recursive = TRUE)

  #Check if trained model exists
  if(file.exists(file.path(path, "H2OModeler"))){
    object$trainedModel <- h2o::h2o.upload_model(file.path(path, "H2OModeler"))
  }else{
    object$trainedModel <- NULL
  }
  #Clean temp files
  files_to_rm <- c(file.path(path, fileName),
                   file.path(path, "H2OModeler"))
  unlink(files_to_rm, recursive = TRUE)

  return(object)
}

#' @rdname modelLoad
#' @export
modelLoad.classifRegModeler <- function(object, path, fileName) {
  #Since main object has already been loaded, remove it
  unlink(file.path(path, fileName), recursive = TRUE)

  fileName_classif <- paste0("classifRegModeler", "_classif.mlu")
  fileName_reg <- paste0("classifRegModeler", "_reg.mlu")

  if(file.exists(paste0(path, "/", fileName_classif)) & file.exists(paste0(path, "/", fileName_reg))){
    #Load classif modeler
    object$classifModeler <- RMLutils::modelLoad(path, fileName_classif)
    #Remove classifRegModeler_classif.mlu
    unlink(file.path(path, fileName_classif), recursive = TRUE)

    #Load reg modeler
    object$regModeler <- RMLutils::modelLoad(path, fileName_reg)
    #Remove classifRegModeler_reg.mlu
    unlink(file.path(path, fileName_reg), recursive = TRUE)
  }

  #Clean temp file
  #unlink(temp_dir, recursive = TRUE)
  return(object)
}

#' @rdname modelLoad
#' @export
modelLoad.memoryModeler <- function(object, path, fileName) {
  #Since main object has already been loaded, remove it
  unlink(file.path(path, fileName), recursive = TRUE)

  fileName_modeler <- "memoryModeler_modeler.mlu"
  fileName_data <- "memoryModeler_data.rds"
  modeler_path <- file.path(path, fileName_modeler)
  data_path <- file.path(path, fileName_data)

  # Load the modeler first
  if (file.exists(modeler_path)){
    object$modeler <- RMLutils::modelLoad(path, fileName_modeler)
    #Remove memoryModeler_modeler.mlu from temp folder
    unlink(modeler_path, recursive = TRUE)
  }
  # Load the data sets
  if (file.exists(data_path)){
    object$data <- readRDS(file = data_path)
    #Remove data from temp folder
    unlink(data_path, recursive = TRUE)
  }

  return(object)
}

#' @rdname modelLoad
#' @export
modelLoad.prePredModeler <- function(object, path, fileName) {
  #Since main object has already been loaded, remove it
  unlink(file.path(path, fileName), recursive = TRUE)

  fileName_preds <- "prePredModeler_preds.mlu"
  fileName_main <- "prePredModeler_main.mlu"
  preds_modeler_path <- file.path(path, fileName_preds)
  main_modeler_path <- file.path(path, fileName_main)

  # Check whether the models exist before loading
  if(file.exists(preds_modeler_path) & file.exists(main_modeler_path)){
    object$predModeler <- RMLutils::modelLoad(path, fileName_preds)
    #Remove prePredModeler_preds.mlu from temp folder
    unlink(preds_modeler_path, recursive = TRUE)

    object$mainModeler <- RMLutils::modelLoad(path, fileName_main)
    #Remove prePredModeler_main.mlu from temp folder
    unlink(main_modeler_path, recursive = TRUE)
  }

  return(object)
}

#' @rdname modelLoad
#' @export
modelLoad.benchModeler <- function(object, path, fileName) {
  #Since main object has already been loaded, remove it
  unlink(file.path(path, fileName), recursive = TRUE)

  #Scan submodeler files
  submodel_files <- grep("submodel",list.files(path), value = TRUE)
  #Get number at the end to define a sorted sequence
  submodel_codes <- as.numeric(regmatches(submodel_files, gregexpr("[0-9]+", submodel_files)))
  #Get sorting index
  sorting_index <- sort(submodel_codes, index.return = TRUE)$ix
  #Sort file names
  sorted_submodel_files <- submodel_files[sorting_index]

  #Load submodelers one by one in the same order they originally had
  saved_submodels <- list()
  for(i in 1:(length(sorted_submodel_files))){#This automatically loads .RData and trainedModel
    saved_submodels[[i]] <- RMLutils::modelLoad(path = path,
                                                fileName = sorted_submodel_files[[i]])
    #Remove current submodeler from temp folder
    unlink(file.path(path, sorted_submodel_files[[i]]),recursive = TRUE)
  }
  object$submodels <- saved_submodels

  return(object)
}

#' @rdname modelLoad
#' @export
modelLoad.preproModeler <- function(object, path, fileName) {
  #Since main object has already been loaded, remove it
  unlink(file.path(path, fileName), recursive = TRUE)

  fileNamePrepro <- "preproModeler_preproModel.mlu"
  modeler_path <- file.path(path, fileNamePrepro)

  # Load the modeler
  if (file.exists(modeler_path)){
    object$modeler <- RMLutils::modelLoad(path, fileNamePrepro)
    #Remove preproModeler_preproModel.mlu from temp folder
    unlink(modeler_path, recursive = TRUE)
  }

  return(object)
}

