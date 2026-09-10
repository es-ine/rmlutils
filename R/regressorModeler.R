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

#' @name regressorModeler
#' @title Class constructors for regression modeler classes.
#'
#' @description Creates a modeler object as specified.
#'
#' The modeler classes contain different types of machine learning models to
#' provide a common interface for all of them.
#'
#' @section Current implementations:
#'
#' \strong{Simple modelers:}
#' \itemize{
#' \item \code{\link{directRegressorModeler}}
#' \item \code{\link{HTRegressorModeler}}
#' \item \code{\link{strataHTRegressorModeler}}
#' \item \code{\link{strataMeanRegressorModeler}}
#' \item \code{\link{H2ORegressorModeler}}
#' \item \code{\link{LGBRegressorModeler}}
#' \item \code{\link{rangerRegressorModeler}}
#' \item \code{\link{catboostRegressorModeler}}
#' \item \code{\link{miceRegressorModeler}}
#' }
#'
#' \strong{Complex modelers:}
#' \itemize{
#' \item \code{\link{classifRegRegressorModeler}}
#' \item \code{\link{benchRegressorModeler}}
#' \item \code{\link{preproRegressorModeler}}
#' \item \code{\link{prePredRegressorModeler}}
#' \item \code{\link{memoryRegressorModeler}}
#' }
#'
#' @return A regressorModeler object.
NULL


#' @rdname directRegressorModeler
#' @title Class constructor for directRegressorModeler.
#'
#' @description A direct imputation modeler that allows to scale its
#' predictions by a specified scale factor. By definition, it does not need to
#' be trained, as the protoTarget and scaleFactor are enough for predicting.
#'
#' @param protoTarget First estimation of the target
#' (this values are assumed to be close enough to the target).
#' @param scaleFactor Factor to scale the protoTarget by. Set to 1 by default.
#' @param target Name of the target variable.
#'
#' @return A directRegressorModeler.
#'
#' @seealso \code{\link{regressorModeler}}
#'
#' @examples
#' #Generate data and select only the regression target
#' set.seed(273)
#' data.dt <- generateRegressionData(n_samples = 100, generate_random_weights = FALSE)
#' data.dt <- data.dt[, c("regression_target"), with = FALSE]
#'
#' #Define a simple protoTarget such as the median of a sample
#' smlp <- sample(x = 1:nrow(data.dt), size = 50, replace = FALSE)
#' med <- data.dt[smlp, median(regression_target)]
#' data.dt[, proto_target := med]
#' head(data.dt)
#'
#' modeler <- directRegressorModeler(protoTarget = "proto_target",
#'                                   scaleFactor = 1,
#'                                   target = "regression_target")
#'
#' cf <- MSECostFunction()
#' evaluate(modeler, data = data.dt, cost_function = cf)
#'
#' @export
directRegressorModeler <- function(protoTarget, scaleFactor, target) {
  UseMethod("directRegressorModeler")
}
#' @rdname directRegressorModeler
#' @export
directRegressorModeler.character <- function(protoTarget,
                                             scaleFactor = 1,
                                             target) {
  object <- list(protoTarget = protoTarget,
                 scaleFactor = scaleFactor,
                 target = target,
                 isTrained = TRUE)#Does not need training by default
  class(object) <- c("directRegressorModeler",
                     "directModeler",
                     "regressorModeler",
                     "modeler")
  object <- RMLutils:::.validate_direct_modeler_args(object)
  return(object)
}

#' @rdname HTRegressorModeler
#'
#' @title Class constructor for HTRegressorModeler
#'
#' @description A regressor modeler that assumes that the target variable is
#' somewhat proportional to its sampling weight.
#'
#' A mean HT factor is calculated with the train data:
#'
#' \deqn{f_{HT} = \sum_{i = 1}^{n}\frac{\left(\frac{1}{\pi_{i}} - 1\right)}{\left(n - \sum_{j = 1}^{n} \pi_{j}\right)} \cdot y_{i}}
#'
#' Which is then used for predicting, dividing it by the sampling weight of the
#' units to predict, assuming that weights are defined as
#' \eqn{w_{i} = \frac{1}{\pi_{i}}}, where \eqn{\pi_{i}} is the
#' sampling probability for unit i.
#'
#' @param weightsColumn Name of the weights column.
#' @param target Name of the target variable.
#'
#' @return An HTRegressorModeler object.
#'
#' @seealso \code{\link{regressorModeler}}
#' @seealso \code{\link{strataHTRegressorModeler}}
#'
#' @examples
#' #Generate data
#' set.seed(273)
#' data.dt <- generateStratifiedDomainEstimateData(n_samples = 100, generate_random_weights = FALSE)
#' head(data.dt)
#'
#' #Get one of the variables and define sampling weights proportional to its size
#' data.dt <- data.dt[, c("income"), with = FALSE]
#'
#' #Define sample size
#' n <- round(0.5 * nrow(data.dt))
#'
#' #Variables with pi >= 1 have its selection probability set to 1
#' data.dt[, pi := n*income/sum(income)]
#' data.dt[, pi := ifelse(pi >= 1, 1, pi)]
#'
#' #Define sampling weights
#' data.dt[, weights := 1/pi]
#' head(data.dt)
#'
#' #Get sample proportional to sampling probabilities
#' data.keys <- 1:nrow(data.dt)
#' train.keys <- sample(x = data.keys, size = n, replace = FALSE, prob = data.dt[, pi])
#' test.keys <- setdiff(data.keys, train.keys)
#'
#' target <- "income"
#' weights_column <- "weights"
#'
#' #Define and train modeler
#' modeler <- HTRegressorModeler(target = target, weightsColumn = weights_column)
#'
#' modeler <- train(object = modeler, data = data.dt, subSet = train.keys)
#'
#' #Evaluate modeler
#' cf <- r2CostFunction(weightsColumn = "weights")
#'
#' evaluate(object = modeler, data = data.dt, cost_function = cf, subSet = train.keys)
#' evaluate(object = modeler, data = data.dt, cost_function = cf, subSet = test.keys)
#'
#'
#' @export
HTRegressorModeler <- function(weightsColumn, target) {
  UseMethod("HTRegressorModeler")
}
#' @rdname HTRegressorModeler
#' @export
HTRegressorModeler.character <- function(weightsColumn, target) {
  object <- list(weightsColumn = weightsColumn,
                 target = target,
                 isTrained = FALSE)
  class(object) <- c("HTRegressorModeler",
                     "HTModeler",
                     "regressorModeler",
                     "modeler")
  object <- RMLutils:::.validate_HT_modeler_args(object)
  return(object)
}

#' @rdname strataHTRegressorModeler
#'
#' @title Class constructor for strataHTRegressorModeler
#'
#' @description An alternate version of \code{\link{HTRegressorModeler}} for
#' stratified data.
#'
#' @param weightsColumn Name of the weights column.
#' @param target Name of the target variable.
#' @param strata Name of the strata information column.
#'
#' @return An strataHTRegressorModeler object.
#'
#' @seealso \code{\link{regressorModeler}}
#' @seealso \code{\link{HTRegressorModeler}}
#'
#' @examples
#' #Generate data
#' set.seed(273)
#' data.dt <- generateStratifiedDomainEstimateData(n_samples = 100, generate_random_weights = FALSE)
#'
#' #Get one of the variables and define sampling weights proportional to its size
#' data.dt <- data.dt[, c("income", "strata_class"), with = FALSE]
#'
#' #Define sample size
#' n <- round(0.5 * nrow(data.dt))
#'
#' #Variables with pi >= 1 have its selection probability set to 1
#' data.dt[, pi := n*income/sum(income)]
#' data.dt[, pi := ifelse(pi >= 1, 1, pi)]
#'
#' #Define sampling weights
#' data.dt[, weights := 1/pi]
#' head(data.dt)
#'
#' #Get sample proportional to sampling probabilities
#' data.keys <- 1:nrow(data.dt)
#' train.keys <- sample(x = data.keys, size = n, replace = FALSE, prob = data.dt[, pi])
#' test.keys <- setdiff(data.keys, train.keys)
#'
#' target <- "income"
#' weights_column <- "weights"
#' strata_column <- "strata_class"
#'
#' #Define and train modeler
#' modeler <- strataHTRegressorModeler(weightsColumn = weights_column,
#'                                     target = target,
#'                                     strata = strata_column)
#'
#' modeler <- train(object = modeler, data = data.dt, subSet = train.keys)
#'
#' #Evaluate modeler
#' cf <- r2CostFunction(weightsColumn = "weights")
#'
#' evaluate(object = modeler, data = data.dt, cost_function = cf, subSet = train.keys)
#' evaluate(object = modeler, data = data.dt, cost_function = cf, subSet = test.keys)
#'
#' @export
strataHTRegressorModeler <- function(weightsColumn, target, strata) {
  UseMethod("strataHTRegressorModeler")
}
#' @rdname strataHTRegressorModeler
#' @export
strataHTRegressorModeler.character <- function(weightsColumn, target, strata) {
  object <- list(weightsColumn = weightsColumn,
                 target = target,
                 strata = strata,
                 isTrained = FALSE)
  class(object) <- c("strataHTRegressorModeler",
                     "strataHTModeler",
                     "regressorModeler",
                     "modeler")
  object <- RMLutils:::.validate_strataHT_modeler_args(object)
  return(object)
}

#' @rdname strataMeanRegressorModeler
#'
#' @title Class constructor for strataMeanRegressorModeler.
#'
#' @description A regressor modeler that predicts a target variable by
#' calculating its average over several regressors used as stratifying variables.
#'
#' @param target Name of the target variable.
#' @param regressors Name or list of names of the variables to use as regressors.
#' Note that these variables must be categorical or of character type to
#' properly define strata.
#' @param weightsColumn Name of the weights column.
#'
#' @return A strataMeanRegressorModeler object.
#'
#' @seealso \code{\link{regressorModeler}}
#'
#' @examples
#' set.seed(273)
#' data.dt <- generateStratifiedDomainModelAssistedEstimateData(n_samples = 100,
#'                                                              generate_random_weights = TRUE)
#' #Define sampler
#' data.keys <- 1:nrow(data.dt)
#' train_size = round(0.8 * nrow(data.dt))
#' smplr <- srsSampler(key = data.keys, ssize = train_size)
#' train.keys <- getSample(smplr)
#' test.keys <- setdiff(data.keys, train.keys)
#'
#' #Define modeler parameters
#' target <- "target"# Regression
#' weightsColumn <- "weights"
#' regressors <- c("strata_class", "domain_class")#Only categorical variables
#'
#' #Define and train modeler
#' modeler <- strataMeanRegressorModeler(target = target,
#'                                       regressors = regressors,
#'                                       weightsColumn = "weights")
#'
#' modeler <- train(object = modeler, data = data.dt, subSet = train.keys)
#'
#' #Evaluate modeler
#' cf <- MSECostFunction(weightsColumn = "weights")
#'
#' evaluate(object = modeler, data = data.dt,
#'          cost_function = cf, subSet = train.keys)
#' evaluate(object = modeler, data = data.dt,
#'          cost_function = cf, subSet = test.keys)
#'
#'
#' @export
strataMeanRegressorModeler <- function(target, regressors, weightsColumn) {
  UseMethod("strataMeanRegressorModeler")
}
#' @rdname strataMeanRegressorModeler
#' @export
strataMeanRegressorModeler.character <- function(target,
                                                 regressors,
                                                 weightsColumn = "") {
  object <- list(weightsColumn = weightsColumn,
                 target = target,
                 regressors = regressors,
                 isTrained = FALSE)
  class(object) <- c("strataMeanRegressorModeler",
                     "strataMeanModeler",
                     "regressorModeler",
                     "modeler")
  object <- RMLutils:::.validate_strataMean_modeler_args(object)
  return(object)
}

#' @rdname classifRegRegressorModeler
#'
#' @title Class constructor for classifRegRegressorModeler
#'
#' @description A two-step complex modeler. Useful when there are large amounts
#' of repeated numbers in a numeric target. It works by first performing a
#' classification task for the repeated numbers.
#' Then, a regressorModeler is used to predict the remaining samples
#' (as long as it had a minimum amount of rows to train).
#'
#' @param classifModeler A classifierModeler to perform a preliminary
#' classification task.
#' @param regModeler A regressorModeler to perform a regression task after the
#' preliminary classification.
#' @param specialCats Special numbers in the target variable, that is, the ones
#' that should be classified in the first step.
#' @param minRows Minimum amount of rows to train the regressorModeler. If this
#' number is not reached after training the classifierModeler, only an average
#' will be outputted.
#' @param epsilon Tolerance factor. If any target variable value is closer to a
#' specialCats than this amount, then it gets replaced by the specialCats value.
#' @param class_mapping Class mapping algorithm to be used by classifModeler.
#' Set to maxScoreClass by default.
#'
#' @return A classifRegRegressorModeler object.
#'
#' @seealso \code{\link{regressorModeler}}
#' @seealso \code{\link{classifierModeler}}
#'
#' @examples
#' #Generate data
#' set.seed(273)
#' data.dt <- generateRegressionData(n_samples = 1000,
#'                                   generate_random_weights = FALSE)
#'
#' # Convert a reasonable number of registers to zero
#' data.dt[regression_target < 0, regression_target := 0]
#' table(data.dt[, regression_target == 0])
#' #We will apply a classifReg modeler to first predict whether the result is 0
#' #or not, and then automatically apply regression over the remaining data.
#' #This complete process is internally processed by the classifReg modeler.
#'
#' #Define train/test partitions
#' data.keys <- 1:nrow(data.dt)
#' train_size = round(0.8 * nrow(data.dt))
#' smplr <- srsSampler(key = data.keys, ssize = train_size)
#' train.keys <- getSample(smplr)
#' test.keys <- setdiff(data.keys, train.keys)
#'
#' #Target variable name is the same for both modelers
#' target <- "regression_target"
#' #Regressors could be different, but it is not necessary in this case.
#' regressors <- setdiff(names(data.dt), target)
#'
#' #Define a classifier modeler
#' params_cls <- list(
#'   "num.trees" = 50,
#'   "max.depth" = 20
#' )
#'
#' cls_modeler <- rangerClassifierModeler(params = params_cls,
#'                                        target = target,
#'                                        regressors = regressors)
#' #Define a regressor modeler
#' params_reg <- list(
#'   "learning_rate" = 0.07,
#'   "nrounds" = 100,
#'   "objective" = "regression"
#' )
#' reg_modeler <- LGBRegressorModeler(params = params_reg,
#'                                    target = target,
#'                                    regressors = regressors)
#'
#' #Define classifReg modeler
#' modeler <- classifRegRegressorModeler(classifModeler = cls_modeler,
#'                                       regModeler = reg_modeler,
#'                                       specialCats = 0)
#'
#' modeler <- train(object = modeler, data = data.dt, subSet = train.keys)
#' #Note that classifRegModeler introduces NAs internally while predicting but does
#' #not alter the proper output
#'
#' #Define cost function
#' cf <- r2CostFunction()
#'
#' #Evaluate using getCost
#' getCost(object = cf,
#'         data = data.dt,
#'         modeler = modeler,
#'         fSample = data.keys,
#'         cSample = test.keys)
#'
#' #Evaluate using evaluate function (only complete modeler)
#' evaluate(modeler,
#'          data = data.dt,
#'          cost_function = cf,
#'          subSet = test.keys)
#'
#' #Evaluate internal modelers separately
#' cf2 <- list("classifModeler" = confusionMatrixCostFunction(),
#'             "regModeler" = r2CostFunction())
#' evaluate(object = modeler,
#'          data = data.dt,
#'          cost_function = cf2,
#'          subSet = train.keys)
#'
#'
#' @export
classifRegRegressorModeler <- function(classifModeler,
                                       regModeler,
                                       specialCats,
                                       minRows = 100,
                                       epsilon = 0,
                                       class_mapping = maxScoreClass()) {
  UseMethod("classifRegRegressorModeler")
}

#' @rdname classifRegRegressorModeler
#'
#' @export
classifRegRegressorModeler.modeler <- function(classifModeler,
                                               regModeler,
                                               specialCats,
                                               minRows = 100,
                                               epsilon = 0,
                                               class_mapping = maxScoreClass()) {

  # target is included for convenience
  object <- list(classifModeler = classifModeler,
                 regModeler = regModeler,
                 target = getTarget(classifModeler),
                 specialCats = specialCats,
                 minRows = minRows,# Minimum number of rows to fit regression
                 epsilon = epsilon,
                 class_mapping = class_mapping,
                 isTrained = FALSE)
  class(object) <- c("classifRegRegressorModeler",
                     "classifRegModeler",
                     "regressorModeler",
                     "modeler")

  object <- RMLutils:::.validate_classifRegRegressorModeler(object)

  return(object)
}

#' @rdname benchRegressorModeler
#'
#' @title Class constructor for benchRegressorModeler
#'
#' @description A multivariate modeler where the target variables must accomplish a certain restriction. It requires multiple modelers to be instantiated.
#'
#' @param submodels List of modelers to be used when predicting.
#' @param benchmarks String or character vector of linear restrictions to be
#'   taken into account by \code{benchRegressorModeler}. Each restriction must
#'   be a linear equality or inequality of the form
#'   \code{LHS == RHS}, \code{LHS >= RHS}, or \code{LHS <= RHS}, where \code{LHS}
#'   is a sum or difference of terms. A term can be an R identifier
#'   (\code{X}), a signed identifier (\code{-X}), or a numeric coefficient
#'   multiplied by an identifier (\code{2 * X}, \code{-2.5 * X}). The
#'   right-hand side must be numeric. For example:
#'   \code{"2 * X - Y + 0.5 * Z >= -10"}.
#' @param optWeights Parameter indicating the type of optional weights: "uniform" or "MSE". "uniform" is set as default.
#'
#' @return A benchRegressorModeler object.
#'
#' @seealso \code{\link{regressorModeler}}
#'
#' @examples
#' #Generate data
#' original_data.dt <- generateRegressionData(n_samples = 1000, generate_random_weights = FALSE)
#'
#' #Define new variables in the data that have a specific restriction
#' #In our case: regression_target = A + B
#' data.dt <- data.table::copy(original_data.dt)
#' data.dt[, A := round(runif(dim(data.dt)[1], min = 0, max = 5))]
#' data.dt[, B := regression_target - A]
#'
#' #Define train/test partitions
#' set.seed(273)
#' data.keys <- 1:nrow(data.dt)
#' train_size = round(0.8 * nrow(data.dt))
#' smplr <- srsSampler(key = data.keys, ssize = train_size)
#' train.keys <- getSample(smplr)
#' test.keys <- setdiff(data.keys, train.keys)
#'
#' #Define submodeler parameters
#' #Note that none of the modelers sees any other target variable besides than their own.
#' params_b1 <- list(
#'   "num.trees" = 20,
#'   "max.depth" = 20
#' )
#' target_b1 <- "regression_target"
#' regressors_b1 <- setdiff(colnames(original_data.dt), target_b1)
#'
#' params_b2 <- list(
#'   "num.trees" = 30,
#'   "max.depth" = 10
#' )
#' target_b2 <- "A"
#' regressors_b2 <- setdiff(colnames(original_data.dt), c(target_b2, target_b1))
#'
#' params_b3 <- list(
#'   "num.trees" = 40,
#'   "max.depth" = 15
#' )
#' target_b3 <- "B"
#' regressors_b3 <- setdiff(colnames(original_data.dt), c(target_b3, target_b1))
#'
#' modeler1 <- rangerRegressorModeler(params = params_b1, target = target_b1, regressors = regressors_b1)
#'
#' modeler2 <- rangerRegressorModeler(params = params_b2, target = target_b2, regressors = regressors_b2)
#'
#' modeler3 <- rangerRegressorModeler(params = params_b3, target = target_b3, regressors = regressors_b3)
#'
#' #Define bench regressor modeler
#' b_modeler <- benchRegressorModeler(submodels = list(modeler1,modeler2,modeler3),
#'                                    benchmarks = "A+B-regression_target==0")
#'
#' #Train and evaluate modeler
#' b_modeler <- train(b_modeler, data = data.dt, subSet = train.keys)
#' predictions <- getPredictions(b_modeler, data = data.dt, subSet = test.keys)
#' cf <- MSECostFunction()
#' evaluate(b_modeler, data = data.dt, cost_function = cf, subSet = train.keys)
#' evaluate(b_modeler, data = data.dt, cost_function = cf, subSet = test.keys)
#'
#' @export
benchRegressorModeler <- function(submodels,
                                  benchmarks,
                                  models,
                                  optWeights="uniform") {
  #Check requirements
  if (!requireNamespace("gsubfn", quietly = TRUE)) {
    stop("Package 'gsubfn' required but not installed")
  }else if(!requireNamespace("osqp", quietly = TRUE)){
    stop("Package 'osqp' required but not installed")
  }else if(!requireNamespace("Matrix", quietly = TRUE)){
    stop("Package 'Matrix' required but not installed")
  }
  UseMethod("benchRegressorModeler")
}

#' @rdname benchRegressorModeler
#' @export
benchRegressorModeler.list <- function(submodels,
                                       benchmarks,
                                       optWeights="uniform") {
  #Required preliminary validation
  #Validate submodels
  m_check <- sapply(submodels, function(m){
    return("modeler" %in% class(m))
  }, USE.NAMES = FALSE)
  if(!all(m_check)){
    stop("Some of the provided submodels are not modelers. Please, review your inputs")
  }

  # Checks restrictions have the appropriate form
  if (!all(sapply(benchmarks, .validateRestrictions))) {
    stop("Some benchmarks are invalid.")
  }

  # Extract variables from the restrictions and models for those variables
  bvars <- sort(unique(unlist(lapply(
    benchmarks,
    function(x) all.vars(parse(text = x))
  ))))

  # Define targets
  targets <- sapply(submodels, getTarget)

  # Checks if all variables in benchmarks are in the modeler
  if(!all(bvars %in% targets)) {
    stop("There are variables in the constraint that are not in the data. Please, review your inputs.")
  }

  # Define restrictions: l <= A <= u
  # Create matrix H with the coefficients of bvars in the benchmark equations
  A <- matrix(0,nrow=length(benchmarks),ncol=length(targets), dimnames = list(benchmarks, targets))
  for (i in seq_along(benchmarks)) {
    eq <- benchmarks[i]
    lhs <- sub("(<=|>=|==).*", "", eq)
    expr <- parse(text = lhs)[[1]]
    ## Derivative wrt each variable is the coefficient
    for (v in bvars) {
      A[i, v] <- eval(D(expr, v))
    }
  }
  A <- Matrix::Matrix(A, sparse = TRUE)

  lures <- matrix(
    NA_real_,
    nrow = length(benchmarks),
    ncol = 2,
    dimnames = list(benchmarks, c("l", "u"))
  )

  for (i in seq_along(benchmarks)) {
    expr <- parse(text = benchmarks[i])[[1]]
    op  <- as.character(expr[[1]])
    rhs <- eval(expr[[3]])
    switch(
      op,
      "==" = lures[i, ] <- c(rhs, rhs),
      ">=" = lures[i, ] <- c(rhs, Inf),
      "<=" = lures[i, ] <- c(-Inf, rhs),
      stop("Unsupported operator: ", op)
    )
  }

  l <- lures[,"l"]
  u <- lures[,"u"]

  object <- list(submodels = submodels,
                 benchmarks = benchmarks,
                 optWeights = optWeights,
                 target=targets,
                 benchvars = bvars,
                 isTrained = FALSE,
                 A=A,
                 l=l,
                 u=u)
  class(object) <- c("benchRegressorModeler", "benchModeler","regressorModeler", "modeler")
  object <- RMLutils:::.validate_benchRegressorModeler(object)
  return(object)
}

#' @rdname H2ORegressorModeler
#'
#' @title Class constructor for H2ORegressorModeler
#'
#' @description H2OModeler implementation for regression tasks.
#' This modeler offers access to the H2O library, but requires the user to run an H2O cluster for it to work.
#'
#' @param model Name of H2O model to be used. The following models are available:
#' \itemize{
#'   \item deeplearning
#'   \item gam (General additive model)
#'   \item gbm (Gradient boosting model)
#'   \item glm (Generalized linear model)
#'   \item randomForest
#'   \item rulefit
#'   \item xgboost (Only works on linux machines)
#' }
#' @param params List of parameters to be used for training an H2O model.
#' @param target String indicating the name of the target variable in the data.
#' @param regressors List or vector of strings indicating the regressor column names in the data.
#' @param DF Alternate H2O modeler definition.
#'
#' @return An H2ORegressorModeler object.
#'
#' @seealso \code{\link{regressorModeler}}
#' @seealso \code{\link{H2OClassifierModeler}}
#'
#' @examples
#' #NOTE: Running this tutorial may take a few seconds, since the
#' #H2O cloud needs to start
#' #Generate data
#' data.dt <- generateRegressionData(n_samples = 1000, generate_random_weights = FALSE)
#'
#' #Define train/test partitions
#' set.seed(273)
#' data.keys <- 1:nrow(data.dt)
#' train_size = round(0.8 * nrow(data.dt))
#' smplr <- srsSampler(key = data.keys, ssize = train_size)
#' train.keys <- getSample(smplr)
#' test.keys <- setdiff(data.keys, train.keys)
#'
#' target <- "regression_target"
#' regressors <- setdiff(colnames(data.dt), target)
#'
#' #Start H2O cloud (change port if that one is not available)
#' h2o::h2o.init(port = 54321, startH2O = TRUE)
#'
#' #Define modeler
#' h2o_model <- "randomForest"
#' params <- list()
#' modeler <- H2ORegressorModeler(model = h2o_model, params = params, target = target, regressors = regressors)
#'
#' #Train modeler (note that data can either be a data.table or an H2OFrame)
#' modeler <- train(modeler, data = data.dt, subSet = train.keys)
#'
#' predictions <- getPredictions(modeler, data = data.dt, subSet = train.keys)
#'
#' cf <- r2CostFunction()
#'
#' evaluate(object = modeler, data = data.dt, cost_function = cf, subSet = test.keys)
#'
#' #Close H2O cloud
#' h2o::h2o.shutdown(prompt = FALSE)
#'
#' @export
H2ORegressorModeler <- function(model, params, target, regressors, DF) {
  if (!requireNamespace("h2o", quietly = TRUE)){
    stop("Package 'h2o' required but not installed")
  }
  library(h2o)
  UseMethod("H2ORegressorModeler")
}

#' @rdname H2ORegressorModeler
#' @export
H2ORegressorModeler.character <- function(model, params, target, regressors) {
  object <- list(model = model,
                 params = params,
                 target = target,
                 regressors = regressors,
                 weightsColumn = params$weights_column,
                 isTrained = FALSE)
  class(object) <- c("H2ORegressorModeler",
                     "H2OModeler",
                     "regressorModeler",
                     "modeler")
  object <- RMLutils:::.validate_H2OModeler(object)
  return(object)
}

#' @rdname H2ORegressorModeler
#' @export
H2ORegressorModeler.data.frame <- function(DF, regressors) {
  object <- lapply(split(DF, 1:nrow(DF)), function(x) {
    x <- as.list(x)
    x <- x[!sapply(x, is.na)]
    x <- lapply(x, function(p) {
      p <- as.character(p)
      if (is.na(as.numeric(p)) && !startsWith(p, "[")) {
        return(p) } else if (!startsWith(p, "[")) {
          return(as.numeric(p))} else {
            p <- paste0("c(", substr(p, 2, nchar(p) - 1), ")")
            return(eval(parse(text = p)))}})
    model <- x$model
    target <- x$target
    params <- x[setdiff(names(x), c("model", "target"))]
    return(H2ORegressorModeler(model, params, target, regressors))
  })
  return(unname(object))
}

#' @rdname preproRegressorModeler
#' @title Class constructor for preproRegressorModeler
#'
#' @description preproModeler implementation for regression tasks.
#'
#' A preproModeler applies a preprocessing function to the data before sending it to another modeler.
#' This pre-processing step will be applied right before training or predicting with the model.
#'
#' @param modeler A regressorModeler object.
#' @param preproObject A \link{preproObject} structure, containing a preprocesing function to apply before training (or predicting) with the modeler.
#'
#' @seealso \code{\link{regressorModeler}}
#' @seealso \code{\link{preproClassifierModeler}}
#' @seealso \code{\link{preproObject}}
#' @seealso \code{\link{minMaxScalerPreproFunction}}
#'
#' @return A preproRegressorModeler object.
#'
#' @examples
#' #Generate data
#' set.seed(273)
#' data.dt <- generateRegressionData(n_samples = 1000, generate_random_weights = FALSE)
#'
#' #Define train/test partitions
#' data.keys <- 1:nrow(data.dt)
#' train_size = round(0.8 * nrow(data.dt))
#' smplr <- srsSampler(key = data.keys, ssize = train_size)
#' train.keys <- getSample(smplr)
#' test.keys <- setdiff(data.keys, train.keys)
#'
#' target <- "regression_target"
#' regressors <- setdiff(colnames(data.dt), target)
#'
#' #Preprocessing parameters
#' prepro_params <- list(pretrained_min_max = NULL)
#'
#' #Define preproObject
#' prepro_object <- preproObject(preproFunction = minMaxScalerPreproFunction,
#'                               params = prepro_params)
#'
#' #Define a regressor modeler
#' params <- list(
#'   "num.trees" = 50,
#'   "max.depth" = 20
#' )
#'
#' modeler <- rangerRegressorModeler(params = params,
#'                                   target = target,
#'                                   regressors = regressors)
#'
#' p_modeler <- preproRegressorModeler(modeler = modeler,
#'                                     preproObject = prepro_object)
#'
#' p_modeler <- train(object = p_modeler, data = data.dt, subSet = train.keys)
#'
#' cf <- r2CostFunction()
#'
#' evaluate(object = p_modeler,
#'          data = data.dt,
#'          cost_function = cf,
#'          subSet = test.keys)
#'
#' @export
preproRegressorModeler <- function(modeler, preproObject){
  UseMethod("preproRegressorModeler")
}

#' @rdname preproRegressorModeler
#' @export
preproRegressorModeler.modeler <- function(modeler, preproObject){

  #Validate modeler inputs
  RMLutils:::.validate_complex_modeler(object = modeler)
  #Add important metadata to preproObject
  preproObject <- assignExternalAttributes(modeler = modeler,
                                           object = preproObject)

  object <- list(modeler = modeler,
                 target = getTarget(modeler),
                 regressors = getRegressors(modeler),
                 preproObject = preproObject,
                 isTrained = FALSE)

  class(object) <- c("preproRegressorModeler",
                     "preproModeler",
                     "regressorModeler",
                     "modeler")
  object <- RMLutils:::.validate_preproModeler(object)
  return(object)
}

#' @rdname LGBRegressorModeler
#' @title Class constructor for LGBRegressorModeler
#'
#' @description LightGBM implementation in rmlutils for regression tasks.
#'
#' @param params List of parameters to be used for training a LightGBM model.
#' @param target String indicating the name of the target variable in the data.
#' @param regressors List or vector of strings indicating the regressor column names in the data.
#'
#' @return An LGBRegressorModeler object.
#'
#' @seealso \code{\link{regressorModeler}}
#' @seealso \code{\link{LGBClassifierModeler}}
#'
#' @examples
#' #Generate data
#' data.dt <- generateRegressionData(n_samples = 1000, generate_random_weights = FALSE)
#'
#' #Define train/test partitions
#' set.seed(273)
#' data.keys <- 1:nrow(data.dt)
#' train_size = round(0.8 * nrow(data.dt))
#' smplr <- srsSampler(key = data.keys, ssize = train_size)
#' train.keys <- getSample(smplr)
#' test.keys <- setdiff(data.keys, train.keys)
#'
#' #Define modeler
#' params <- list(
#'   "learning_rate" = 0.07,
#'   "nrounds" = 10,
#'   "objective" = "regression"
#' )
#' target <- "regression_target"
#' regressors <- setdiff(colnames(data.dt), target)
#'
#' modeler <- LGBRegressorModeler(params = params, target = target, regressors = regressors)
#'
#' #Train, predict and evaluate
#' modeler <- train(modeler, data = data.dt, subSet = train.keys)
#'
#' preds <- getPredictions(modeler, data = data.dt, subSet = train.keys)
#'
#' cf <- r2CostFunction()
#'
#' evaluate(object = modeler, data = data.dt, cost_function = cf, subSet = train.keys)
#'
#' @export
LGBRegressorModeler <- function(params, target, regressors) {
  if (!requireNamespace("lightgbm", quietly = TRUE)){
    stop("Package 'lightgbm' required but not installed")
  }
  UseMethod("LGBRegressorModeler")
}

#' @rdname LGBRegressorModeler
#' @export
LGBRegressorModeler.list <- function(params, target, regressors) {

  object <- list(model= "lightgbm",
                 params = params,
                 target = target,
                 regressors = regressors,
                 weightsColumn = params$weight_column,
                 isTrained = FALSE)
  class(object) <- c("LGBRegressorModeler",
                     "LGBModeler",
                     "regressorModeler",
                     "modeler")

  object <- RMLutils:::.validate_LGBModeler(object)

  return(object)
}

#' @rdname rangerRegressorModeler
#'
#' @title Class constructor for rangerRegressorModeler.
#'
#' @description ranger implementation in rmlutils for regression tasks.
#'
#' Wright, M. N. & Ziegler, A. (2017). ranger: A fast implementation of random
#' forests for high dimensional data in C++ and R. J Stat Softw
#' 77:1-17. \url{https://doi.org/10.18637/jss.v077.i01}.
#'
#' @param params List of parameters to be used for training a ranger model.
#' @param target String indicating the name of the target variable in the data.
#' @param regressors List or vector of strings indicating the regressor column names in the data.
#'
#' @return A rangerRegressorModeler object.
#'
#' @seealso \code{\link{regressorModeler}}
#' @seealso \code{\link{rangerClassifierModeler}}
#'
#' @examples
#' #Generate data
#' data.dt <- generateRegressionData(n_samples = 1000, generate_random_weights = FALSE)
#'
#' #Define train/test partitions
#' set.seed(273)
#' data.keys <- 1:nrow(data.dt)
#' train_size = round(0.8 * nrow(data.dt))
#' smplr <- srsSampler(key = data.keys, ssize = train_size)
#' train.keys <- getSample(smplr)
#' test.keys <- setdiff(data.keys, train.keys)
#'
#' #Define modeler
#' params <- list(
#'   "num.trees" = 50,
#'   "max.depth" = 20
#' )
#' target <- "regression_target"
#' regressors <- setdiff(colnames(data.dt), target)
#'
#' modeler <- rangerRegressorModeler(params = params, target = target, regressors = regressors)
#'
#' #Train, predict and evaluate
#' modeler <- train(object = modeler, data = data.dt, subSet = train.keys)
#'
#' preds <- getPredictions(modeler, data = data.dt, subSet = train.keys)
#'
#' cf <- r2CostFunction()
#'
#' evaluate(object = modeler, data = data.dt, cost_function = cf, subSet = train.keys)
#'
#' @export
rangerRegressorModeler <- function(params, target, regressors) {
  if (!requireNamespace("ranger", quietly = TRUE)){
    stop("Package 'ranger' required but not installed")
  }
  UseMethod("rangerRegressorModeler")
}

#' @rdname rangerRegressorModeler
#' @export
rangerRegressorModeler.list <- function(params, target, regressors) {

  object <- list(model= "ranger",
                 params = params,
                 target = target,
                 regressors = regressors,
                 weightsColumn = params$case.weights,
                 isTrained = FALSE)
  class(object) <- c("rangerRegressorModeler",
                     "rangerModeler",
                     "regressorModeler",
                     "modeler")
  object <- RMLutils:::.validate_rangerModeler(object)
  return(object)
}

#' @rdname catboostRegressorModeler
#' @title Class constructor for catboostRegressorModeler
#'
#' @description Catboost implementation in rmlutils for regression tasks.
#'
#' @param params List of parameters to be used for training a catboost model.
#' @param target String indicating the name of the target variable in the data.
#' @param regressors List or vector of strings indicating the regressor column names in the data.
#' @param val_data Validation dataset to be used when training.
#' @param ntree_start Additional parameter for catboost's predict method.
#' @param ntree_end Additional parameter for catboost's predict method.
#' @param thread_count Additional parameter for catboost's predict method.
#'
#' @return A catboostRegressorModeler object.
#'
#' @seealso \code{\link{regressorModeler}}
#' @seealso \code{\link{catboostClassifierModeler}}
#'
#' @examples
#' #Generate data
#' data.dt <- generateRegressionData(n_samples = 1000, generate_random_weights = FALSE)
#'
#' #Define train/test partitions
#' set.seed(273)
#' data.keys <- 1:nrow(data.dt)
#' train_size = round(0.8 * nrow(data.dt))
#' smplr <- srsSampler(key = data.keys, ssize = train_size)
#' train.keys <- getSample(smplr)
#' test.keys <- setdiff(data.keys, train.keys)
#'
#' #Define modeler
#' params <- list(loss_function = 'RMSE',
#'                iterations = 100,
#'                metric_period=10)
#' target <- "regression_target"
#' regressors <- setdiff(colnames(data.dt), target)
#'
#' modeler <- catboostRegressorModeler(params = params, target = target, regressors = regressors)
#'
#' #Train, predict and evaluate
#' modeler <- train(object = modeler, data = data.dt, subSet = train.keys)
#'
#' preds <- getPredictions(modeler, data = data.dt, subSet = train.keys)
#'
#' cf <- r2CostFunction()
#'
#' evaluate(object = modeler, data = data.dt, cost_function = cf, subSet = train.keys)
#'
#' @export
catboostRegressorModeler <- function(params,
                                     target,
                                     regressors,
                                     val_data,
                                     ntree_start = 0,
                                     ntree_end = 0,
                                     thread_count = -1) {
  if (!requireNamespace("catboost", quietly = TRUE)){
    stop("Package 'catboost' required but not installed")
  }
  UseMethod("catboostRegressorModeler")
}

#' @rdname catboostRegressorModeler
#' @export
catboostRegressorModeler.list <- function(params,
                                          target,
                                          regressors,
                                          val_data = NULL,
                                          ntree_start = 0,
                                          ntree_end = 0,
                                          thread_count = -1) {

  object <- list(model= "catboost",
                 params = params,
                 target = target,
                 regressors = regressors,
                 weightsColumn = params$weight_column,
                 isTrained = FALSE,
                 prediction_type = "RawFormulaVal",
                 val_data = val_data,
                 ntree_start = ntree_start,
                 ntree_end = ntree_end,
                 thread_count = thread_count)
  class(object) <- c("catboostRegressorModeler",
                     "catboostModeler",
                     "regressorModeler",
                     "modeler")
  object <- RMLutils:::.validate_catboostModeler(object)
  return(object)
}


#' @rdname miceRegressorModeler
#' @title Class constructor for miceRegressorModeler
#'
#' @description mice implementation in rmlutils for regression tasks.
#' Note that due to the way that mice works, its results are stochastic,
#' meaning that predicting over the same dataset multiple times will
#' usually provide different results. The user should use it with caution.
#'
#' @param params List of parameters to be used for training a mice model.
#' @param target String indicating the name of the target variable in the data.
#' @param regressors List or vector of strings indicating the regressor column names in the data.
#'
#' @return A miceRegressorModeler object.
#'
#' @seealso \code{\link{regressorModeler}}
#' @seealso \code{\link{miceClassifierModeler}}
#'
#' @examples
#' #Generate data
#' data.dt <- generateRegressionData(n_samples = 1000, generate_random_weights = FALSE)
#'
#' #Define train/test partitions
#' set.seed(273)
#' data.keys <- 1:nrow(data.dt)
#' train_size = round(0.8 * nrow(data.dt))
#' smplr <- srsSampler(key = data.keys, ssize = train_size)
#' train.keys <- getSample(smplr)
#' test.keys <- setdiff(data.keys, train.keys)
#'
#' #Define modeler
#' params <- list(m = 1,
#'                method = "rf",
#'                print = FALSE)
#' target <- "regression_target"
#' regressors <- setdiff(colnames(data.dt), target)
#'
#' modeler <- miceRegressorModeler(params = params, target = target, regressors = regressors)
#'
#' #Train, predict and evaluate
#' modeler <- train(object = modeler, data = data.dt, subSet = train.keys)
#'
#' preds <- getPredictions(modeler, data = data.dt, subSet = train.keys)
#'
#' cf <- r2CostFunction()
#'
#' evaluate(object = modeler, data = data.dt, cost_function = cf, subSet = train.keys)
#'
#'
#' @export
miceRegressorModeler <- function(params, target, regressors) {
  if (!requireNamespace("mice", quietly = TRUE)){
    stop("Package 'mice' required but not installed")
  }
  UseMethod("miceRegressorModeler")
}

#' @rdname miceRegressorModeler
#' @export
miceRegressorModeler.list <- function(params, target, regressors = NULL) {

  object <- list(model= "mice",
                 params = params,
                 target = target,
                 regressors = regressors,
                 weightsColumn = NULL,
                 isTrained = FALSE)
  class(object) <- c("miceRegressorModeler",
                     "miceModeler",
                     "regressorModeler",
                     "modeler")
  object <- RMLutils:::.validate_miceModeler(object)
  return(object)
}


#' @rdname prePredRegressorModeler
#'
#' @title Class constructor for prePredRegressorModeler
#'
#' @description A two-step complex modeler. It works by first getting predictions for a set of variables, each having its own modeler object specified.
#' Thereupon, such predictions are implemented as regressors for a second modeler.
#'
#' @param predModeler A multivariate or univariate modeler to perform preliminary predictions for a set of regressor variables.
#' @param mainModeler The main modeler. The aforementioned predictions will be used as regressors, in addition to the ones already specified inside the main modeler.
#'
#' @return A prePredRegressorModeler object.
#'
#' @seealso \code{\link{regressorModeler}}
#'
#' @examples
#' #Generate data
#' data.dt <- generateRegressionData(n_samples = 1000, generate_random_weights = FALSE)
#' #Create an additional column that depends on regression_target
#' #This will be the actual target of this modeler
#' data.dt[, main_regression_target := regression_target + float_1 - float_2]
#'
#' #Define train/test partitions
#' set.seed(273)
#' data.keys <- 1:nrow(data.dt)
#' train_size = round(0.8 * nrow(data.dt))
#' smplr <- srsSampler(key = data.keys, ssize = train_size)
#' train.keys <- getSample(smplr)
#' test.keys <- setdiff(data.keys, train.keys)
#'
#' #Define previous modeler (this will predict regression_target)
#' params <- list(
#'   "num.trees" = 50,
#'   "max.depth" = 20
#' )
#' target <- "regression_target"
#' regressors <- setdiff(colnames(data.dt), c(target, "main_regression_target"))
#'
#' predModeler <- rangerRegressorModeler(params = params,
#'                                       target = target,
#'                                       regressors = regressors)
#'
#' #Define main modeler
#' #(this will predict main_regression_target,
#' #with regression_target as an additional regressor)
#' params <- list(
#'   "num.trees" = 50,
#'   "max.depth" = 20
#' )
#' target <- "main_regression_target"
#' regressors <- setdiff(colnames(data.dt), c(target))
#'
#' mainModeler <- rangerRegressorModeler(params = params,
#'                                       target = target,
#'                                       regressors = regressors)
#'
#' prePredModeler <- prePredRegressorModeler(predModeler = predModeler, mainModeler = mainModeler)
#'
#' #Train, predict and evaluate
#' prePredModeler <- train(prePredModeler, data = data.dt, subSet = train.keys)
#'
#' preds <- getPredictions(prePredModeler, data = data.dt, subSet = train.keys)
#'
#' #Evaluate global output (mainModeler)
#' cf <- r2CostFunction()
#' evaluate(object = prePredModeler,
#'          data = data.dt,
#'          cost_function = cf,
#'          subSet = train.keys)
#'
#' #Evaluate both internal modelers (predModeler and mainModeler)
#' cf2 <- list("predModeler" = r2CostFunction(),
#'             "mainModeler" = r2CostFunction())
#' evaluate(object = prePredModeler,
#'          data = data.dt,
#'          cost_function = cf2,
#'          subSet = train.keys)
#'
#'
#' @export
prePredRegressorModeler <- function(predModeler, mainModeler) {
  UseMethod("prePredRegressorModeler")
}

#' @rdname prePredRegressorModeler
#' @export
prePredRegressorModeler.modeler <- function(predModeler, mainModeler) {

  #Validate modeler inputs
  RMLutils:::.validate_complex_modeler(object = predModeler)
  RMLutils:::.validate_complex_modeler(object = mainModeler)
  object <- list(predModeler = predModeler,
                 mainModeler = mainModeler,
                 isTrained = FALSE)
  class(object) <- c("prePredRegressorModeler",
                     "prePredModeler",
                     "regressorModeler",
                     "modeler")

  object <- RMLutils:::.validate_prePredRegressorModeler(object)

  return(object)
}


#' @rdname memoryRegressorModeler
#'
#' @title Class constructor for memoryRegressorModeler
#'
#' @description This modeler saves together a list of data sets from different time instances (for example, years)
#' and a defined modeler. When training the modeler, all these data sets will be added to the data inputed
#' in the training.
#' If a single data set, instead of a list, is inputed, it will be considered as coming from a single time period.
#'
#' It has additional methods to manage its internal data storage, namely:
#' \code{\link{addMemory}}, \code{\link{cleanMemory}} and \code{\link{updateMemory}}.
#'
#' @param data list of data sets or a single data set
#' @param modeler a modeler object
#' @param maxData Optional value, maximum number of data sets in the memoryModeler
#'
#' @return A memoryRegressorModeler object.
#'
#' @seealso \code{\link{addMemory}}
#' @seealso \code{\link{cleanMemory}}
#' @seealso \code{\link{updateMemory}}
#'
#' @examples
#' #Generate data
#' set.seed(273)
#' data.dt <- generateStratifiedDomainModelAssistedEstimateData(n_samples = 1000, generate_random_weights = FALSE)
#' data2.dt <- generateStratifiedDomainModelAssistedEstimateData(n_samples = 100, generate_random_weights = FALSE)
#' data3.dt <- generateStratifiedDomainModelAssistedEstimateData(n_samples = 50, generate_random_weights = FALSE)
#'
#' #Define train/test partitions
#' data.keys <- 1:nrow(data.dt)
#' train_size = round(0.8 * nrow(data.dt))
#' smplr <- srsSampler(key = data.keys, ssize = train_size)
#' train.keys <- getSample(smplr)
#' test.keys <- setdiff(data.keys, train.keys)
#'
#' #Define base modeler
#' target <- "target"# Regression
#' regressors <- setdiff(names(data.dt), target)
#' params <- list(
#'   "num.trees" = 50,
#'   "max.depth" = 20
#' )
#' base_modeler <- rangerRegressorModeler(params = params,
#'                                        target = target,
#'                                        regressors = regressors)
#'
#' #Instantiate memoryModeler
#' #memoryModeler parameters
#' data <- list(data2.dt)
#' maxData <- 3L
#' modeler <- memoryRegressorModeler(data = data,
#'                                   modeler = base_modeler,
#'                                   maxData = maxData)
#'
#' #Train, getPredictions and evaluate
#' modeler <- train(modeler,
#'                  data = data.dt,
#'                  subSet = train.keys)
#'
#' preds <- getPredictions(object = modeler,
#'                         data = data.dt,
#'                         subSet = test.keys)
#'
#' #Define a cost function and evaluate
#' cf <- r2CostFunction()
#' #Evaluate over train set
#' evaluate(object = modeler,
#'          data = data.dt,
#'          cost_function = cf,
#'          subSet = train.keys)
#' #Evaluate over test set
#' evaluate(object = modeler,
#'          data = data.dt,
#'          cost_function = cf,
#'          subSet = test.keys)
#'
#' @export
memoryRegressorModeler <- function(data, modeler, maxData = 5L) {
  UseMethod("memoryRegressorModeler")
}

#' @rdname memoryRegressorModeler
#' @export
memoryRegressorModeler.list <- function(data, modeler, maxData = 5L) {

  object <- list(modeler = modeler,
                 data = data,
                 maxData = maxData,
                 target = getTarget(modeler),
                 isTrained = FALSE)
  class(object) <- c("memoryRegressorModeler",
                     "memoryModeler",
                     "regressorModeler",
                     "modeler")
  #Validate modeler
  object <- RMLutils:::.validate_memoryRegressorModeler(object)

  return(object)
}

#' @rdname memoryRegressorModeler
#' @export
memoryRegressorModeler.data.frame <- function(data, modeler, maxData = 5L) {
  warning("It is considered that the whole dataset comes from a single time instance")
  #Validate modeler inputs
  RMLutils:::.validate_complex_modeler(object = modeler)
  object <- list(modeler = modeler,
                 data = list(data),
                 maxData = maxData,
                 isTrained = FALSE)
  class(object) <- c("memoryRegressorModeler",
                     "memoryModeler",
                     "regressorModeler",
                     "modeler")

  object <- RMLutils:::.validate_memoryRegressorModeler(object)

  return(object)
}
