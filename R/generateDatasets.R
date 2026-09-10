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

#' @title Generate Multiclass Classification Data
#'
#' @description A custom function to generate a simple dataset for multiclass classification tasks.
#'
#' @param n_samples Amount of samples (rows) to return.
#' @param generate_random_weights Boolean indicating whether to add a randomised weights column to the dataset.
#'
#' @return data.table with of dim (n_samples, 12) containing five categorical variables, five numeric variables, a randomized weights column (if generate_random_weights was set to true) and a categorical target variable named classification_target.
#'
#' @importFrom data.table data.table
#'
#' @examples
#' data <- generateMulticlassClassificationData(n_samples = 100, generate_random_weights = TRUE)
#' print(data)
#'
#' @seealso \code{\link{generateBinaryClassificationData}} for binary case.
#' @seealso \code{\link{generateRegressionData}} for regression case.
#'
#' @export
generateMulticlassClassificationData <- function(n_samples = 100, generate_random_weights = TRUE){
  #Generate a simple dataset for multiclass classification
  n_float_features <- 5
  n_cat_features <- 5
  n_classes <- 3
  seed <- 273
  set.seed(seed)

  #Define colnames
  cat_names <- paste0("cat_", 1:n_cat_features)
  float_names <- paste0("float_", 1:n_float_features)

  #Generate categorical data
  cat_data <- paste0("Class_",sample.int(n = n_classes, size = n_samples*n_cat_features, replace = TRUE))
  cat_data.mat <- matrix(data = cat_data, ncol = n_cat_features, dimnames = list(NULL,cat_names))
  cat_data.dt <- data.table(cat_data.mat, stringsAsFactors = TRUE)
  #cat_data.dt[,(cat_names):=lapply(.SD, as.factor),.SDcols=cat_names]

  #Generate numeric data
  float_data.mat <- matrix(rnorm(n_samples*n_float_features), ncol = n_float_features, dimnames = list(NULL,float_names))
  float_data.dt <- data.table(float_data.mat)

  #Join data
  data.dt <- cbind(float_data.dt, cat_data.dt)

  if(generate_random_weights){
    data.dt[, weights := sample(x = 2:10, size = n_samples, replace = TRUE)]
  }

  #Define target variable
  data.dt[, classification_target := ""]
  data.dt[float_1 >= -0.3 & cat_1 %in% c("Class_1","Class_2"), classification_target := "Target_1"]
  data.dt[float_2 <= 0.8 & cat_2 %in% c("Class_2","Class_3"), classification_target := "Target_2"]
  data.dt[classification_target == "", classification_target := "Target_3"]

  data.dt[, classification_target := factor(classification_target)]

  return(data.dt)
}

#' @title Generate Binary Classification Data
#'
#' @description A custom function to generate a simple dataset for Binary classification tasks.
#'
#' @param n_samples Amount of samples (rows) to return.
#' @param generate_random_weights Boolean indicating whether to add a randomised weights column to the dataset.
#'
#' @return data.table with of dim (n_samples, 12) containing five categorical variables, five numeric variables, a randomized weights column (if generate_random_weights was set to true) and a categorical target variable named classification_target.
#'
#' @examples
#' data <- generateBinaryClassificationData(n_samples = 100, generate_random_weights = TRUE)
#' print(data)
#'
#' @importFrom data.table data.table
#'
#' @seealso \code{\link{generateMulticlassClassificationData}} for multiclass case.
#' @seealso \code{\link{generateRegressionData}} for regression case.
#'
#' @export
generateBinaryClassificationData <- function(n_samples = 100, generate_random_weights = TRUE){
  #Generate a simple dataset for multiclass classification
  n_float_features <- 5
  n_cat_features <- 5
  n_classes <- 3
  seed <- 273
  set.seed(seed)

  #Define colnames
  cat_names <- paste0("cat_", 1:n_cat_features)
  float_names <- paste0("float_", 1:n_float_features)

  #Generate categorical data
  cat_data <- paste0("Class_",sample.int(n = n_classes, size = n_samples*n_cat_features, replace = TRUE))
  cat_data.mat <- matrix(data = cat_data, ncol = n_cat_features, dimnames = list(NULL,cat_names))
  cat_data.dt <- data.table(cat_data.mat, stringsAsFactors = TRUE)
  #cat_data.dt[,(cat_names):=lapply(.SD, as.factor),.SDcols=cat_names]

  #Generate numeric data
  float_data.mat <- matrix(rnorm(n_samples*n_float_features), ncol = n_float_features, dimnames = list(NULL,float_names))
  float_data.dt <- data.table(float_data.mat)

  #Join data
  data.dt <- cbind(float_data.dt, cat_data.dt)

  if(generate_random_weights){
    data.dt[, weights := sample(x = 2:10, size = n_samples, replace = TRUE)]
  }

  #Define target variable
  data.dt[, classification_target := ""]
  data.dt[float_2 <= 0.8 & cat_2 %in% c("Class_2","Class_3"), classification_target := "positive"]
  data.dt[classification_target == "", classification_target := "negative"]

  data.dt[, classification_target := factor(classification_target)]

  return(data.dt)
}


#' @title Generate Regression Data
#'
#' @description A custom function to generate a simple dataset for regression tasks.
#'
#' @param n_samples Amount of samples (rows) to return.
#' @param generate_random_weights Boolean indicating whether to add a randomised weights column to the dataset.
#'
#' @return data.table with of dim (n_samples, 12) containing five categorical variables, five numeric variables, a randomized weights column (if generate_random_weights was set to true) and a categorical target variable named regression_target.
#'
#' @examples
#' data <- generateRegressionData(n_samples = 100, generate_random_weights = TRUE)
#' print(data)
#'
#' @importFrom data.table data.table
#'
#' @seealso \code{\link{generateMulticlassClassificationData}} for classification (multiclass) case.
#' @seealso \code{\link{generateBinaryClassificationData}} for classification (binary) case.
#' @seealso \code{\link{generateEstimateData}} for estimation case.
#' @seealso \code{\link{generateModelAssistedRegressionData}} for model-assisted regression (and/or estimation) case.
#'
#' @export
generateRegressionData <- function(n_samples = 100, generate_random_weights = TRUE){
  n_float_features <- 5
  n_cat_features <- 5
  n_classes <- 3
  seed <- 273
  set.seed(seed)

  #Define colnames
  cat_names <- paste0("cat_", 1:n_cat_features)
  float_names <- paste0("float_", 1:n_float_features)

  #Generate categorical data
  cat_data <- paste0("Class_",sample.int(n = n_classes, size = n_samples*n_cat_features, replace = TRUE))
  cat_data.mat <- matrix(data = cat_data, ncol = n_cat_features, dimnames = list(NULL,cat_names))
  cat_data.dt <- data.table(cat_data.mat, stringsAsFactors = TRUE)
  #cat_data.dt[,(cat_names):=lapply(.SD, as.factor),.SDcols=cat_names]

  #Generate numeric data
  float_data.mat <- matrix(rnorm(n_samples*n_float_features), ncol = n_float_features, dimnames = list(NULL,float_names))
  float_data.dt <- data.table(float_data.mat)

  #Join data
  data.dt <- cbind(float_data.dt, cat_data.dt)

  if(generate_random_weights){
    data.dt[, weights := sample(x = 2:10, size = n_samples, replace = TRUE)]
  }

  #Define target variable
  data.dt[, regression_target := 2*float_1 - 1.5*float_2 + as.numeric(cat_3)]
  return(data.dt)
}


#' @title Generate Estimate Data
#'
#' @description A custom function to generate a simple dataset for estimation tasks.
#' The output contains three columns named Target_1, Target_2 and Target_3, where:
#'
#' \itemize{
#'   \item Target_1 follows a Normal(10, 2) distribution
#'   \item Target_2 follows a Normal(-20, 10) distribution
#'   \item Target_3 follows a Normal(0, 5) distribution
#' }
#'
#' @param n_samples Amount of samples (rows) to return.
#' @param generate_random_weights Boolean indicating whether to add a randomised weights column to the dataset.
#'
#' @return data.table with of dim (n_samples, 3) containing three numeric variables, explained above.
#'
#' @examples
#' data <- generateEstimateData(n_samples = 100)
#' print(data)
#'
#' @importFrom data.table data.table
#'
#' @seealso \code{\link{generateRegressionData}} for simple regression case.
#' @seealso \code{\link{generateStratifiedEstimateData}} for stratified estimation case.
#' @seealso \code{\link{generateStratifiedDomainEstimateData}} for stratified estimation with domains case.
#' @seealso \code{\link{generateModelAssistedRegressionData}} for model-assisted regression (and/or estimation) case.
#' @seealso \code{\link{generateStratifiedDomainModelAssistedEstimateData}} for model-assisted regression (and/or estimation) in a domain-stratified case.
#'
#'
#' @export
generateEstimateData <- function(n_samples = 100, generate_random_weights = FALSE){
  n_targets <- 3
  seed <- 273
  set.seed(seed)

  #Define colnames
  col_names <- paste0("Target_", 1:n_targets)

  #Generate data
  Y_1 <- rnorm(n=n_samples, mean = 10, sd = 2)
  Y_2 <- rnorm(n=n_samples, mean = -20, sd = 10)
  Y_3 <- rnorm(n=n_samples, mean = 0, sd = 5)
  float_data.mat <- matrix(c(Y_1, Y_2, Y_3), ncol = n_targets, dimnames = list(NULL,col_names))
  data.dt <- data.table(float_data.mat)

  if(generate_random_weights){
    data.dt[, weights := sample(x = 2:10, size = n_samples, replace = TRUE)]
  }

  return(data.dt)
}


#' @title Generate Stratified Estimate Data
#'
#' @description A custom function to generate a simple dataset for estimation tasks.
#' The output contains four columns named Target_1, Target_2, Target_3 and strata_class, where:
#'
#' \strong{If strata_class == Class_1:}
#' \itemize{
#'   \item Target_1 follows a Normal(10, 2) distribution
#'   \item Target_2 follows a Normal(-20, 10) distribution
#'   \item Target_3 follows a Normal(0, 5) distribution
#' }
#'
#' \strong{If strata_class == Class_2:}
#' \itemize{
#'   \item Target_1 follows a Normal(1, 2) distribution
#'   \item Target_2 follows a Normal(-20, 1) distribution
#'   \item Target_3 follows a Normal(30, 20) distribution
#' }
#'
#' \strong{If strata_class == Class_3:}
#' \itemize{
#'   \item Target_1 follows a Normal(20, 2) distribution
#'   \item Target_2 follows a Normal(-20, 20) distribution
#'   \item Target_3 follows a Normal(10, 10) distribution
#' }
#' \strong{Strata characteristics:}
#' \itemize{
#'  \item Target_1 has a different mean and identical standard deviation in each strata.
#'  \item Target_2 has an identical mean but different standard deviation in each strata.
#'  \item Target_3 has a different mean and standard deviation in each strata.
#' }
#' \strong{The distribution of strata is as follows:}
#' \itemize{
#'   \item Class_1 contains around 50\% of the total samples
#'   \item Class_2 contains around 30\% of the total samples
#'   \item Class_3 contains around 20\% of the total samples
#' }
#'
#' @param n_samples Amount of samples (rows) to return.
#' @param generate_random_weights Boolean indicating whether to add a randomised weights column to the dataset.
#'
#' @return data.table with dimensions (n_samples, 4) containing three numeric variables
#'         (Target_1, Target_2, Target_3) and one factor variable (strata_class).
#'
#' @examples
#' data <- generateStratifiedEstimateData(n_samples = 100)
#' print(data)
#'
#' @importFrom data.table data.table rbindlist
#'
#' @seealso \code{\link{generateRegressionData}} for simple regression case.
#' @seealso \code{\link{generateEstimateData}} for estimation case.
#' @seealso \code{\link{generateStratifiedDomainEstimateData}} for stratified estimation with domains case.
#' @seealso \code{\link{generateModelAssistedRegressionData}} for model-assisted regression (and/or estimation) case.
#' @seealso \code{\link{generateStratifiedDomainModelAssistedEstimateData}} for model-assisted regression (and/or estimation) in a domain-stratified case.
#'
#' @export
generateStratifiedEstimateData <- function(n_samples = 100, generate_random_weights = FALSE){
  n_targets <- 3
  seed <- 273
  set.seed(seed)

  #Define colnames
  target_names <- paste0("Target_", 1:n_targets)
  col_names <- c(target_names, "strata_class")

  #Truncate strata sizes
  s_2_size <- as.integer(n_samples*0.3)
  s_3_size <- as.integer(n_samples*0.2)
  s_1_size <- n_samples - s_2_size - s_3_size#Add remaining elements to strata 0

  #Define strata vectors
  sizes.lst <- list(
    s_1_size,
    s_2_size,
    s_3_size
  )

  strata.lst <- list(
    strata_class_1 <- rep(x = "Class_1", times = s_1_size),
    strata_class_2 <- rep(x = "Class_2", times = s_2_size),
    strata_class_3 <- rep(x = "Class_3", times = s_3_size)
  )

  mu.lst <- list(
    c(10,-20,0),
    c(1,-20,30),
    c(20,-20,10)
  )

  sigma.lst <- list(
    c(2,10,5),
    c(2,1,20),
    c(2,20,10)
  )

  data.lst <- lapply(1:length(strata.lst), function(s){
    Y_1 <- rnorm(n = sizes.lst[[s]], mean = mu.lst[[s]][[1]], sd = sigma.lst[[s]][[1]])
    Y_2 <- rnorm(n = sizes.lst[[s]], mean = mu.lst[[s]][[2]], sd = sigma.lst[[s]][[2]])
    Y_3 <- rnorm(n = sizes.lst[[s]], mean = mu.lst[[s]][[3]], sd = sigma.lst[[s]][[3]])
    data.dt <- data.table(matrix(data = c(Y_1, Y_2, Y_3), ncol = n_targets, dimnames = list(NULL,target_names)))
    data.dt[, strata_class := strata.lst[[s]]]
  })
  data.dt <- rbindlist(data.lst)

  if(generate_random_weights){
    data.dt[, weights := sample(x = 2:10, size = n_samples, replace = TRUE)]
  }

  return(data.dt)
}

#' @title Generate Stratified Domain Estimate Data
#'
#' @description  A custom function to generate a simple dataset for estimation tasks.
#'  The output contains four columns named income, cost_of_living, strata_class and domain_class where:
#'
#' \itemize{
#'   \item income follows a lognormal(8, 0.5) distribution, multiplied by strata_class_factor
#'   \item cost_of_living follows a normal(1000, 200) distribution, multiplied by domain_class_factor
#'   \item strata_class : Categorical variable with two levels; (Urban, Rural)
#'   \item domain_class : Categorical variable with three levels; (Province_1, Province_2, Province_3)
#' }
#'
#' Where its class factors are:
#' \itemize{
#'   \item strata_class_factor = {'Rural': 1, 'Urban': 4}
#'   \item domain_class_factor = {'Province_1': 2.4, 'Province_2': 1.35, 'Province_3': 0.6}
#' }
#' Province_1, Province_2 and Province_3 contain 50%, 30% and 20% of the population respectively. With the following Urban/Rural ratio:
#' \itemize{
#'  \item Province_1: 80% Urban, 20% Rural.
#'  \item Province_2: 30% Urban, 70% Rural.
#'  \item Province_3: 0% Urban, 100% Rural.
#' }
#'
#' @param n_samples Amount of samples (rows) to return.
#' @param generate_random_weights Boolean indicating whether to add a randomised weights column to the dataset.
#'
#' @return data.table with dimensions (n_samples, 4) containing two numeric variables and two categorical variables, explained above.
#'
#' @examples
#' data <- generateStratifiedDomainEstimateData(n_samples = 100)
#' print(data)
#'
#' @importFrom data.table data.table rbindlist setcolorder
#'
#' @seealso \code{\link{generateRegressionData}} for simple regression case.
#' @seealso \code{\link{generateEstimateData}} for estimation case.
#' @seealso \code{\link{generateStratifiedEstimateData}} for stratified estimation case.
#' @seealso \code{\link{generateModelAssistedRegressionData}} for model-assisted regression (and/or estimation) case.
#' @seealso \code{\link{generateStratifiedDomainModelAssistedEstimateData}} for model-assisted regression (and/or estimation) in a domain-stratified case.
#'
#' @export
generateStratifiedDomainEstimateData <- function(n_samples = 100, generate_random_weights = FALSE){
  col_names <- c("income", "cost_of_living", "strata_class", "domain_class")
  n_domains <- 3
  seed <- 273
  set.seed(seed)

  #Define strata and domain class factors
  strata_class_factor <- c("Urban" = 4, "Rural" = 1)
  domain_class_factor <- c("Province_1" = 2.4, "Province_2" = 1.35, "Province_3" = 0.6)

  #Truncate domain sizes
  d_2_size <- as.integer(n_samples*0.3)
  d_3_size <- as.integer(n_samples*0.2)
  d_1_size <- n_samples - d_2_size - d_3_size#Add remaining elements to domain 1

  domain_sizes.lst <- list(
    d_1_size,
    d_2_size,
    d_3_size
  )

  #Define domain_class variable
  domain.lst <- list(
    domain_class_1 <- rep(x = "Province_1", times = d_1_size),
    domain_class_2 <- rep(x = "Province_2", times = d_2_size),
    domain_class_3 <- rep(x = "Province_3", times = d_3_size)
  )
  #Define strata_class distribution
  strata_size_dist.lst <- list(
    c("Urban" =  d_1_size - round(0.2*d_1_size), "Rural" = round(0.2*d_1_size)),
    c("Urban" =  d_2_size - round(0.7*d_2_size), "Rural" = round(0.7*d_2_size)),
    c("Urban" =  0, "Rural" = d_3_size)
  )
  #Generate data.table for each domain and then join them
  data.lst <- lapply(1:n_domains, function(domain){
    #Build strata_class variable
    strata_class <- c(rep("Urban", times = strata_size_dist.lst[[domain]][["Urban"]]), rep("Rural", times = strata_size_dist.lst[[domain]][["Rural"]]))
    mat <- matrix(data = c(strata_class, domain.lst[[domain]]), ncol = 2, dimnames = list(NULL, c("strata_class", "domain_class")))
    dt <- data.table(mat)
    dt[, income := rlnorm(n = domain_sizes.lst[[domain]], meanlog = 8, sdlog = 0.5) * strata_class_factor[strata_class]]
    dt[, cost_of_living := rnorm(n = domain_sizes.lst[[domain]], mean = 1000, sd = 200) * domain_class_factor[domain_class]]
    return(dt)
  })
  data.dt <- rbindlist(data.lst)
  setcolorder(data.dt, neworder = col_names)
  if(generate_random_weights){
    data.dt[, weights := sample(x = 2:10, size = n_samples, replace = TRUE)]
  }
  return(data.dt)
}

#' @title Generate Model Assisted Regression Data
#'
#' @description  A custom function to generate a simple dataset for testing model-assisted estimation in the MCSubRBEstimator.
#'  It generates a dataset with two numeric target variables linearly dependent on the remaining columns.
#'  Some random noise was added to complicate the regression task.
#'
#' @param n_samples Amount of samples (rows) to return.
#' @param generate_random_weights Boolean indicating whether to add a randomised weights column to the dataset.
#'
#' @return data.table with dimensions (n_samples, 10) or (n_samples, 11) if generate_random_weights was set to true.
#'
#' @examples
#' data <- generateModelAssistedRegressionData(n_samples = 100, generate_random_weights = TRUE)
#' print(data)
#'
#' @importFrom data.table data.table rbindlist setcolorder
#'
#' @seealso \code{\link{generateRegressionData}} for simple regression case.
#' @seealso \code{\link{generateEstimateData}} for estimation case.
#' @seealso \code{\link{generateStratifiedEstimateData}} for stratified estimation case.
#' @seealso \code{\link{generateStratifiedDomainEstimateData}} for stratified estimation with domains case.
#' @seealso \code{\link{generateStratifiedDomainModelAssistedEstimateData}} for model-assisted regression (and/or estimation) in a domain-stratified case.
#'
#' @export
generateModelAssistedRegressionData <- function(n_samples = 100, generate_random_weights = FALSE){
  n_float_features <- 4
  n_cat_features <- 4
  n_classes <- 3
  seed <- 273
  set.seed(seed)

  #Define colnames
  cat_names <- paste0("cat_", 1:n_cat_features)
  float_names <- paste0("float_", 1:n_float_features)

  #Generate categorical data
  cat_data <- paste0("Class_",sample.int(n = n_classes, size = n_samples*n_cat_features, replace = TRUE))
  cat_data.mat <- matrix(data = cat_data, ncol = n_cat_features, dimnames = list(NULL,cat_names))
  cat_data.dt <- data.table(cat_data.mat, stringsAsFactors = TRUE)
  #cat_data.dt[,(cat_names):=lapply(.SD, as.factor),.SDcols=cat_names]

  #Generate numeric data
  float_data.mat <- matrix(rnorm(n_samples*n_float_features), ncol = n_float_features, dimnames = list(NULL,float_names))
  float_data.dt <- data.table(float_data.mat)

  #Join data
  data.dt <- cbind(float_data.dt, cat_data.dt)

  #Define target variables
  data.dt[, regression_target_1 := 100 * float_1 + -15 * float_2 + 20 * as.integer(cat_1) -4 * as.integer(cat_2)]
  data.dt[, regression_target_2 := 10 * float_3 + -15 * float_4 + 2 * as.integer(cat_3) -4 * as.integer(cat_4)]
  #Add noise to variables
  noise_1 <- rnorm(n = n_samples, mean = 0, sd = sd(data.dt[, regression_target_1]))
  noise_2 <- rnorm(n = n_samples, mean = 0, sd = sd(data.dt[, regression_target_2]))
  data.dt[, regression_target_1 := regression_target_1 + noise_1]
  data.dt[, regression_target_2 := regression_target_2 + noise_2]
  setcolorder(data.dt, neworder = c(colnames(data.dt)[c(10,11)], colnames(data.dt)[-c(10,11)]))

  if(generate_random_weights){
    data.dt[, weights := sample(x = 2:10, size = n_samples, replace = TRUE)]
  }

  return(data.dt)
}

#' @title Generate Stratified Domain Model Assisted Estimate Data
#'
#' @description  A custom function to generate a simple dataset for testing model-assisted estimation in a domain-based setting.
#' It makes use of the generateStratifiedDomainEstimateData function, adding a noisy target variable to the dataset.
#'
#' @param n_samples Amount of samples (rows) to return.
#'
#' @return data.table with dimensions (n_samples, 5), explained above and in the generateStratifiedDomainEstimateData documentation.
#'
#' @examples
#' data <- generateStratifiedDomainModelAssistedEstimateData(n_samples = 100)
#' print(data)
#'
#' @importFrom data.table data.table rbindlist setcolorder
#'
#' @seealso \code{\link{generateRegressionData}} for simple regression case.
#' @seealso \code{\link{generateEstimateData}} for estimation case.
#' @seealso \code{\link{generateStratifiedEstimateData}} for stratified estimation case.
#' @seealso \code{\link{generateStratifiedDomainEstimateData}} for stratified estimation with domains case.
#' @seealso \code{\link{generateModelAssistedRegressionData}} for model-assisted regression (and/or estimation) case.
#'
#' @export
generateStratifiedDomainModelAssistedEstimateData <- function(n_samples = 100,
                                                              generate_random_weights = FALSE){
  col_names <- c("target", "income", "cost_of_living", "strata_class", "domain_class")
  data.dt <- generateStratifiedDomainEstimateData(n_samples = n_samples, generate_random_weights = FALSE)
  seed <- 273
  set.seed(seed)
  data.dt[, target := income - cost_of_living]
  #Generate noise based on strata and domain
  data.dt[, sigma := sd(target)/2, by =c("strata_class", "domain_class")]
  data.dt[, target := target + rnorm(.N, mean = 0, sd = sigma)]
  data.dt[, sigma := NULL]
  setcolorder(data.dt, neworder = col_names)
  if(generate_random_weights){
    data.dt[, weights := sample(x = 2:10, size = n_samples, replace = TRUE)]
  }
  return(data.dt)
}
