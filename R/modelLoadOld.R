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

#' @title Loads a modeler object (old version, DO NOT USE).
#'
#' @description Loads a modeler object from disk using an outdated method.
#' The user should avoid calling this function directly and use
#' modelLoad instead, as it will automatically handle older modelers without
#' having to call this one manually.
#'
#' @param path path to the stored modeler object
#' @param fileName Name of the file where the modeler is stored.
#'
#' @examples
#' path <- "../../models/stored_models"
#' filename <- "my_model"
#' #mdl <- modelLoadOld(path, filename)
#'
#' @export
modelLoadOld <- function(path, fileName, object = NULL) {
  UseMethod("modelLoadOld")
}

#' @rdname modelLoadOld
#' @export
modelLoadOld.character <- function(path, fileName, object = NULL) {
  warning("Deprecation warning: Specified fileName does not have a .mlu extension.\n Attempting to load the modeler using an older method.\n Please, make sure to save the loaded model again to save it as its newer version.")
  object <- get(load(paste0(path, "/", fileName, ".RData")))
  loadedObject <- RMLutils::modelLoadOld(object, path, fileName)

  return(loadedObject)
}


#' @rdname modelLoadOld
#' @export
modelLoadOld.directModeler <- function(object, path, fileName) {
  #Fix modeler
  object$isTrained <- TRUE#Always true for directModeler
  object$regressors <- NULL#Remove regressors, as they are unnecessary
  return(object)
}

#' @rdname modelLoadOld
#' @export
modelLoadOld.HTModeler <- function(object, path, fileName) {
  if (is.null(object$trainedModel)){
    object$isTrained <- FALSE
  }else{
    object$isTrained <- TRUE
  }
  object$regressors <- NULL#Remove regressors, as they are unnecessary
  return(object)
}

#' @rdname modelLoadOld
#' @export
modelLoadOld.strataHTModeler <- function(object, path, fileName) {
  if (is.null(object$trainedModel)){
    object$isTrained <- FALSE
  }else{
    object$isTrained <- TRUE
  }
  object$regressors <- NULL#Remove regressors, as they are unnecessary
  return(object)
}

#' @rdname modelLoadOld
#' @export
modelLoadOld.strataMeanModeler <- function(object, path, fileName) {
  if (is.null(object$trainedModel)){
    object$isTrained <- FALSE
  }else{
    object$isTrained <- TRUE
  }
  return(object)
}

#' @rdname modelLoadOld
#' @export
modelLoadOld.benchModeler <- function(object, path, fileName) {
  are_submodels_trained <- c()
  submodels_path <- paste0(path,"/",fileName,"_submodels")
  submodels <- list.files(submodels_path)
  saved_submodels <- list()
  for(i in 1:(length(submodels)/2)){#This automatically loads .RData and trainedModel
    saved_submodels[[i]] <- RMLutils::modelLoadOld(path = submodels_path, fileName = paste0("submodel_",as.character(i)))
    are_submodels_trained <- c(are_submodels_trained, saved_submodels[[i]]$isTrained)
  }
  object$submodels <- saved_submodels
  #Fix object
  if (all(are_submodels_trained)){
    object$isTrained <- TRUE
  }else{
    object$isTrained <- FALSE
  }
  return(object)
}

#' @rdname modelLoadOld
#' @export
modelLoadOld.LGBModeler <- function(object, path, fileName) {

  if(file.exists(paste0(path, "/", fileName))){
    object$trainedModel <- lightgbm::lgb.load(paste0(path, "/", fileName))
  }
  #Fix object
  object$weightsColumn <- object$params$weight_column
  if (is.null(object$trainedModel)){
    object$isTrained <- FALSE
  }else{
    object$isTrained <- TRUE
  }
  if(class(object)[3] == "classifierModeler"){
    if(object$params$objective == "binary" & object$isTrained){
      warning(paste0("Old model is a binary classifier, assuming positive class to be the last saved class in object$class_names:\n",
                     object$class_names[2],
                     "\n If this is not correct, the user should manually assign the positive class to avoid problems."))
      object$positive_class <- object$class_names[2]
    }else{
      object$positive_class <- NULL
    }
  }

  return(object)
}

#' @rdname modelLoadOld
#' @export
modelLoadOld.H2OModeler <- function(object, path, fileName) {

  if(file.exists(paste0(path, "/", fileName))){
    object$trainedModel <- h2o::h2o.upload_model(paste0(path, "/", fileName))
  }
  #Fix object
  object$weightsColumn <- object$params$weights_column
  if (is.null(object$trainedModel)){
    object$isTrained <- FALSE
  }else{
    object$isTrained <- TRUE
  }

  return(object)
}

#' @rdname modelLoadOld
#' @export
modelLoadOld.rangerModeler <- function(object, path, fileName) {

  if(file.exists(paste0(path, "/", fileName))){
    object$trainedModel <- readRDS(paste0(path, "/", fileName))
  }
  #Fix object
  object$weightsColumn <- object$params$case.weights
  if (is.null(object$trainedModel)){
    object$isTrained <- FALSE
  }else{
    object$isTrained <- TRUE
  }
  return(object)
}

#' @rdname modelLoadOld
#' @export
modelLoadOld.miceModeler <- function(object, path, fileName) {

  if(file.exists(paste0(path, "/", fileName))){
    object$trainedModel <- readRDS(paste0(path, "/", fileName))
  }
  #Fix object
  object$weightsColumn <- NULL
  if (is.null(object$trainedModel)){
    object$isTrained <- FALSE
  }else{
    object$isTrained <- TRUE
  }

  return(object)
}

#' @rdname modelLoadOld
#' @export
modelLoadOld.catboostModeler <- function(object, path, fileName) {

  if(file.exists(paste0(path, "/", fileName))){
    object$trainedModel <- catboost::catboost.load_model(paste0(path, "/", fileName))
  }
  #Fix object
  object$weightsColumn <- object$params$weight_column
  if (is.null(object$trainedModel)){
    object$isTrained <- FALSE
  }else{
    object$isTrained <- TRUE
  }

  if(class(object)[3] == "classifierModeler"){
    if(object$params$loss_function %in% c("Logloss", "CrossEntropy") & object$isTrained){
      warning(paste0("Old model is a binary classifier, assuming positive class to be the last saved class in object$class_names.\n",
                     object$class_names[2],
                     "\n If this is not correct, the user should manually assign the positive class to avoid problems."))
      object$positive_class <- object$class_names[2]
    }else{
      object$positive_class <- NULL
    }
  }

  return(object)
}

#' @rdname modelLoadOld
#' @export
modelLoadOld.preproModeler <- function(object, path, fileName) {
  #First, load internal modeler object
  fileNamePrepro <- paste0(fileName, "_preproModel")
  #Check if it exists!!
  if(file.exists(paste0(path, "/",fileNamePrepro))){
    #This automatically loads trained modeler as well (if it exists)
    object$modeler    <- RMLutils::modelLoadOld(path = path, fileName = fileNamePrepro)#, object)
    object$target     <- object$modeler$target
    object$regressors <- object$modeler$regressors
  }
  #Fix object
  if (object$modeler$isTrained){
    object$isTrained <- TRUE
  }else{
    object$isTrained <- FALSE
  }

  return(object)
}

#' @rdname modelLoadOld
#' @export
modelLoadOld.classifRegModeler <- function(object, path, fileName) {
  fileName_classif <- paste0(fileName, "_classif")
  fileName_reg <- paste0(fileName, "_reg")
  if(file.exists(paste0(path, "/", fileName_classif, ".RData")) & file.exists(paste0(path, "/", fileName_reg, ".RData"))){
    object$classifModeler <- RMLutils::modelLoadOld(path, fileName_classif)
    object$regModeler <- RMLutils::modelLoadOld(path, fileName_reg)
  }
  #Fix object
  if (object$classifModeler$isTrained & object$regModeler$isTrained){
    object$isTrained <- TRUE
  }else{
    object$isTrained <- FALSE
  }

  return(object)
}

#' @rdname modelLoadOld
#' @export
modelLoadOld.prePredModeler <- function(object, path, fileName) {
  fileName_preds <- paste0(fileName, "_preds")
  fileName_main <- paste0(fileName, "_main")
  # Check whether the models exist before loading
  if(file.exists(paste0(path, "/", fileName_preds, ".RData")) & file.exists(paste0(path, "/", fileName_main, ".RData"))){
    object$predModeler <- RMLutils::modelLoadOld(path, fileName_preds)
    object$mainModeler <- RMLutils::modelLoadOld(path, fileName_main)
  }
  #Fix object
  if (object$predModeler$isTrained & object$mainModeler$isTrained){
    object$isTrained <- TRUE
  }else{
    object$isTrained <- FALSE
  }

  return(object)
}

#' @rdname modelLoadOld
#' @export
modelLoadOld.memoryModeler <- function(object, path, fileName) {
  fileName_modeler <- paste0(fileName, "_modeler")
  fileName_data <- paste0(fileName, "_data")
  # Load the object first
  if (file.exists(paste0(path, "/", fileName_modeler, ".RData"))){
    object$modeler <- RMLutils::modelLoadOld(path, fileName_modeler)
  }
  # Load the data sets
  if (file.exists(paste0(path, "/", fileName_data, ".RData"))){
      object$data <- get(load(file = paste0(path, "/", fileName_data, ".RData")))
  }
  #Fix object
  if (object$modeler$isTrained){
    object$isTrained <- TRUE
  }else{
    object$isTrained <- FALSE
  }

  return(object)
}
