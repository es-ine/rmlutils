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

#' @name estimator
#' @title A collection of estimator functions.
#'
#' @description Each estimator outputs the estimate and variance for a given target variable.
#'
#' @section Current implementations:
#'
#' \itemize{
#' \item \code{\link{HTEstimate}}
#' \item \code{\link{HTDomainEstimate}}
#' \item \code{\link{REstimate}}
#' \item \code{\link{RDomainEstimate}}
#' \item \code{\link{MCSubRBEstimate}}
#' \item \code{\link{MCSubRBDomainEstimate}}
#' \item \code{\link{biasVarianceEstimate}}
#' \item \code{\link{predictionEstimate}}
#' \item \code{\link{predictionDomainEstimate}}
#' \item \code{\link{preTrainedEstimate}}
#' }
#'
#'
NULL

#' @title Estimate Horvitz-Thompson estimator.
#'
#' @description Computes the Horvitz-Thompson estimator according to the sampling scheme
#' and sample. Returns the estimation for all the variables of the sampled data.
#'
#'
#' @param object sampler object to sample from
#' @param sampleKeys vector of indices of the sample
#' @param sampleData Data sampled from the full population
#'
#' @return A list with the Horvitz-Thompson estimation of the total and its
#' variance estimation for each variable of the dataframe.
#'
#' @seealso \code{\link{estimator}}
#' @seealso \code{\link{HTDomainEstimate}}
#' @seealso \code{\link{REstimate}}
#'
#' @import data.table
#'
#' @examples
#' set.seed(273)
#' #Generate simple data for estimation purposes
#' data.dt <- generateEstimateData(n_samples = 1000,
#'                                 generate_random_weights = FALSE)
#' head(data.dt)
#'
#' key <- 1:nrow(data.dt)
#' srsmplr <- srsSampler(key = key, ssize = 800)
#' sample_1 <- getSample(srsmplr)
#'
#' #HT estimate for simple random sampling
#' estim1 <- HTEstimate(object = srsmplr,
#'                      sampleKeys = sample_1,
#'                      sampleData = data.dt[sample_1])
#' print(estim1)
#'
#' #Theoretical values are known (see help(generateEstimateData) for more details)
#' c("Target_1" = 10 * 1000,
#'   "Target_2" = -20 * 1000,
#'   "Target_3" = 0)
#'
#' #Standard error of estimate
#' sqrt(estim1$variance)
#'
#' #Generate simple stratified data for estimation purposes
#' data2.dt <- generateStratifiedEstimateData(n_samples = 1000,
#'                                            generate_random_weights = FALSE)
#' head(data2.dt)
#'
#' key = 1:nrow(data2.dt)
#' sssmplr <- ssSampler(key = key,
#'                      strata = data2.dt[, strata_class],
#'                      ssize = c("Class_1" = 400, "Class_2" = 240, "Class_3" = 160))
#' sample_2 <- getSample(sssmplr)
#' #Define target columns
#' target_cols <- c("Target_1", "Target_2", "Target_3")
#' #HT estimate for stratified random sampling
#' estim2 <- HTEstimate(object = sssmplr,
#'                      sampleKeys = sample_2,
#'                      sampleData = data2.dt[sample_2, ..target_cols])
#' print(estim2)
#'
#' #Theoretical values are known (see help(generateStratifiedEstimateData) for more details)
#' c("Target_1" = 500 * 10 + 300 * 1 + 200 * 20,
#'   "Target_2" = 500 * (-20) + 300 * (-20) + 200 * (-20),
#'   "Target_3" = 500 * 0 + 300 * 30 + 200 * 10)
#'
#' #Standard error of estimate
#' sqrt(estim2$variance)
#'
#' @export
HTEstimate <- function(object, sampleKeys, sampleData) {
  RMLutils:::.validate_HTEstimate(object, sampleKeys, sampleData)
  UseMethod("HTEstimate")
}
#' @rdname HTEstimate
#' @import data.table
#' @export
HTEstimate.srsSampler <- function(object, sampleKeys, sampleData) {
  if (is.null(dim(sampleData)))
    dim(sampleData) <- c(length(sampleData), 1)
  output <- list()
  strata <- table(object$strata)
  auxDT <- data.table(sampleData)
  output <- list()
  output$total <- as.matrix(auxDT[, lapply(.SD,
                                           function(x) length(object$key) * mean(x))])
  output$variance <- as.matrix(auxDT[, lapply(.SD,
                                              function(x) length(object$key)**2 *
                                                (1- length(sampleKeys) / length(object$key)) *
                                                var(x) / length(sampleKeys))])
  return(output)
}

#' @rdname HTEstimate
#' @import data.table
#' @export
HTEstimate.ssSampler <- function(object, sampleKeys, sampleData) {
  if (is.null(dim(sampleData)))
    dim(sampleData) <- c(length(sampleData), 1)
  output <- list()
  strata <- table(object$strata)
  auxDT <- data.table(okeys = c(sampleKeys), sampleData)
  auxDT[, ostrata := object$strata[match(okeys, object$key)]]
  auxDT[, ostrataN := as.numeric(strata[ostrata])]
  auxDT[, okeys := NULL]
  totalDT <- auxDT[, lapply(.SD, function(x) mean(x) * ostrataN),
                   by = c("ostrata", "ostrataN")][,-"ostrataN"]
  output$total <- as.matrix(totalDT[, -"ostrata"])
  rownames(output$total) <- totalDT$ostrata
  varianceDT <- auxDT[, lapply(.SD, function(x) ostrataN**2 *
                                 (1 - .N / ostrataN) * var(x) / .N),
                      by = c("ostrata", "ostrataN")][,-"ostrataN"]
  output$variance <- as.matrix(varianceDT[, -"ostrata"])
  rownames(output$variance) <- varianceDT$ostrata
  finalOutput <- list(total = apply(output$total, 2, sum),
                      variance = apply(output$variance, 2, sum),
                      strata = output)
  return(finalOutput)
}

#' @title Horvitz-Thompson estimator for domains.
#'
#' @description Computes the Horvitz-Thompson estimator for domains according to the
#' sampling scheme and sample.
#'
#'
#' @return A list with the Horvitz-Thompson estimation of the total and its
#' variance estimation.
#'
#' @param object A \code{\link{sampler}} object.
#' @param sampleKeys List of indices of the sample.
#' @param sampleData Data sampled from the full population.
#' @param domains List, matrix or data.table of domains in the sampled data.
#'
#' @seealso \code{\link{estimator}}
#' @seealso \code{\link{HTEstimate}}
#' @seealso \code{\link{RDomainEstimate}}
#'
#' @import data.table
#'
#' @examples
#' set.seed(273)
#' #Define data to use
#' data.dt <- generateStratifiedDomainModelAssistedEstimateData(n_samples = 1000,
#'                                                              generate_random_weights = FALSE)
#' head(data.dt)
#'
#' #Define simple random sampler
#' key <- 1:nrow(data.dt)
#' srsmplr <- srsSampler(key = key, ssize = 800)
#' sample_1 <- getSample(srsmplr)
#' sample_data.dt <- data.dt[sample_1]
#' domains <- sample_data.dt[, domain_class]
#' target_cols <- c("income", "cost_of_living")
#'
#' #HT estimate for simple random sampling
#' HTDomainEstimate(object = srsmplr,
#'                  sampleKeys = sample_1,
#'                  sampleData = sample_data.dt[, ..target_cols],
#'                  domains = domains)
#' print(paste0("Real total value of income: ", std_ma_data.dt[, sum(income)]))
#' print(paste0("Real total value of  cost_of_living: ", std_ma_data.dt[, sum(cost_of_living)]))
#'
#' #Use same data with ssSampler
#' sssmplr <- ssSampler(key = key,
#'                      strata = data.dt[, strata_class],
#'                      ssize = c("Rural" = 408, "Urban" = 392))
#' sample_2 <- getSample(srsmplr)
#' sample_data_2.dt <- data.dt[sample_2]
#' domains2 <- sample_data_2.dt[, domain_class]
#' target_cols <- c("income", "cost_of_living")
#'
#' #HT domain estimate for stratified random sampling
#' HTDomainEstimate(object = sssmplr,
#'                  sampleKeys = sample_2,
#'                  sampleData = sample_data_2.dt[, ..target_cols],
#'                  domains = domains2)
#' print(paste0("Real total value of income: ", std_ma_data.dt[, sum(income)]))
#' print(paste0("Real total value of  cost_of_living: ", std_ma_data.dt[, sum(cost_of_living)]))
#'
#' @export
HTDomainEstimate <- function(object, sampleKeys, sampleData, domains) {
  RMLutils:::.validate_HTDomainEstimate(object, sampleKeys, sampleData, domains)
  UseMethod("HTDomainEstimate")
}
#' @rdname HTDomainEstimate
#' @export
HTDomainEstimate.sampler <- function(object, sampleKeys, sampleData, domains) {
  domainsNames <- unique(domains)
  sampleData <- as.matrix(sampleData)
  if (is.null(dim(sampleData)))
    dim(sampleData) <- c(length(sampleData), 1)
  completeSampleData <- matrix(0, nrow = nrow(sampleData),
                               ncol = length(domainsNames) * ncol(sampleData))
  colnames(completeSampleData) <- rep(domainsNames, ncol(sampleData))
  colnames(completeSampleData) <- paste0(rep(colnames(sampleData),
                                             each = length(domainsNames)),
                                         "_", colnames(completeSampleData))
  domainsMatrix <-
    matrix(0, nrow = nrow(sampleData), ncol = length(domainsNames))
  for (i in 1:length(domainsNames))
    domainsMatrix[, i] <- domains == domainsNames[i]
  for (i in 1:ncol(sampleData)) {
    completeSampleData[, ((i - 1) * length(domainsNames) + 1):
                         (i * length(domainsNames))] <-
      domainsMatrix * sampleData[, i]
  }
  return(HTEstimate(object, sampleKeys, completeSampleData))
}

#' @title Raulin's formula for variance estimation.
#'
#' @description Computes the Horvitz-Thompson estimator using Raulin's
#' formula for variance (assuming stratified random sampling without replacement).
#'
#' It is an approximation of variance that only requires the first-order selection
#' probabilities of a sample for obtaining it.
#'
#' \deqn{
#'  \hat{V}(\hat{Y}) = \sum_{h=1}^{H} \frac{n_h}{n_h-1} \sum_{i \in h} w_i(w_i-1) (y_i - \bar{y}_h)^2
#' }
#'
#' where:
#' \itemize{
#'   \item \eqn{H}: Total amount of strata.
#'   \item \eqn{n_h}: sample size in each strata \eqn{h}
#'   \item \eqn{y_{i} = y_i}: value of the target variable.
#'   \item \eqn{\bar{y}_{h} = \frac{1}{n_h} \sum_{i \in h} y_{i}}: unweighted average of the target variable within the stratum \eqn{h}
#'   \item \eqn{w_i}: sampling weight for unit \eqn{i}
#' }
#'
#' All numeric (and non-parameter) columns in the data are assumed to be target variables to compute the estimate.
#'
#' @param data Dataset of information and variables of the sampled Data.
#' @param strata_colummn String indicating the column name that identifies strata.
#' @param weights_column String indicating the column name that contains sampling weights.
#'
#' @return A list with the Horvitz-Thompson estimation of the total and its
#' variance estimation.
#'
#' @seealso \code{\link{estimator}}
#' @seealso \code{\link{RDomainEstimate}}
#'
#' @import data.table
#'
#' @examples
#' #Generate stratified data
#' data.dt <- generateStratifiedEstimateData(n_samples = 1000,
#'                                           generate_random_weights = FALSE)
#' #Define stratified random sampler proportional to sample size
#' set.seed(273)
#' key = 1:nrow(data.dt)
#' sssmplr <- ssSampler(key = key,
#'                      strata = data.dt[, strata_class],
#'                      ssize = c("Class_1" = 400, "Class_2" = 240, "Class_3" = 160))
#' sample_2 <- getSample(sssmplr)
#'
#' #Define weights column
#' #Since sampling probabilities are pi = n_h/N_h
#' #Weights are 1/pi
#' data.dt[, N_h := .N , by = c("strata_class")]
#' #Vector of n_h
#' strata_to_sample_size <- c("Class_1" = 400, "Class_2" = 240, "Class_3" = 160)
#' data.dt[, weights := N_h/strata_to_sample_size[strata_class]]
#' data.dt[, N_h := NULL]
#' strata_colummn <- "strata_class"
#' weights_column <- "weights"
#' estim <- REstimate(data = data.dt[sample_2],
#'                    strata_colummn = strata_colummn,
#'                    weights_column = weights_column)
#' print(estim)
#'
#' #Since correct valies are known (see help(generateStratifiedEstimateData) for more details)
#' c("Target_1" = 500 * 10 + 300 * 1 + 200 * 20,
#'   "Target_2" = 500 * (-20) + 300 * (-20) + 200 * (-20),
#'   "Target_3" = 500 * 0 + 300 * 30 + 200 * 10)
#'
#' @export
REstimate <- function(data, strata_colummn, weights_column) {
  RMLutils:::.validate_REstimate(data, strata_colummn, weights_column)
  UseMethod("REstimate")
}

#' @rdname REstimate
#' @import data.table
#' @export
REstimate.default <- function(data = NULL, strata_colummn = NULL, weights_column = NULL) {

  data.dt <- data.table(data)

  #Split auxiliary column names from other column names
  #(we will assume numeric columns different from strata and weights are targets)
  auxiliary_cols <- c(strata_colummn, weights_column)
  target_cols <- setdiff(colnames(data.dt), auxiliary_cols)
  num_cols <- sapply(target_cols, function(t){
    return(data.dt[, is.numeric(get(t))])
  }, USE.NAMES = FALSE)
  target_cols <- target_cols[num_cols]

  #Define column names to store stratum average value for each target
  stratum_avg_target_colnames <-paste0(target_cols, "_stratum_avg")

  #Get strata size and average (for each target variable)
  data.dt[, stratum_size := .N, by = get(strata_colummn)]
  data.dt[,  (stratum_avg_target_colnames) := lapply(.SD, mean), by = get(strata_colummn), .SDcols = target_cols]

  #REstimate does not take domains into account
  #Apply calculations to each target
  output.lst <- lapply(1:length(target_cols), function(i){
    out.lst <- list("Estimate" = data.dt[, sum(get(weights_column) * get(target_cols[i]))],
                    "Variance" = data.dt[stratum_size > 1,
                                         sum((stratum_size / (stratum_size - 1)) * get(weights_column) * (get(weights_column) - 1) *
                                               (get(target_cols[i]) - get(stratum_avg_target_colnames[i]))**2)])
  })
  names(output.lst) <- target_cols
  return(output.lst)
}

#' @title Raulin's formula for variance estimation by domain.
#'
#' @description Computes the Horvitz-Thompson estimator using Raulin's
#' formula for variance when data has multiple subpopulations (domains) of
#' interest (assuming stratified random sampling without replacement):
#'
#' \deqn{
#'  \hat{V}(\hat{Y}_d) = \sum_{h=1}^{H} \frac{n_h}{n_h-1} \sum_{i \in h} w_i(w_i-1) (y_{i,d} - \bar{y}_{h,d})^2
#' }
#'
#' where:
#' \itemize{
#'   \item \eqn{H}: Total amount of strata.
#'   \item \eqn{n_h}: sample size in each strata \eqn{h}
#'   \item \eqn{y_{i,d} = y_i \cdot I(i \in d)}: value of the target variable adjusted by the domain belonging indicator.
#'   \item \eqn{\bar{y}_{h,d} = \frac{1}{n_h} \sum_{i \in h} y_{i,d}}: unweighted average of the domain within the stratum \eqn{h}
#'   \item \eqn{w_i}: sampling weight for unit \eqn{i}
#' }
#'
#' All numeric (and non-parameter) columns in the data are assumed to be target variables to compute the estimate.
#'
#' @param data Dataset of information and variables of the sampled Data.
#' @param strata_colummn String indicating the column name that identifies strata.
#' @param domain_colummn String indicating the column name that identifies domains (subpopulations of interest).
#' @param weights_column String indicating the column name that contains sampling weights.
#'
#' @return A list of lists, with the Horvitz-Thompson estimation of the total and its
#' variance estimation for each domain in the data.
#'
#' @seealso \code{\link{estimator}}
#' @seealso \code{\link{REstimate}}
#'
#' @import data.table
#'
#' @examples
#' data.dt <- generateStratifiedDomainModelAssistedEstimateData(n_samples = 1000,
#'                                                              generate_random_weights = FALSE)
#' head(data.dt)
#' data.dt[, target := NULL]
#' #Define stratified random sampler proportional to sample size
#' set.seed(273)
#' key = 1:nrow(data.dt)
#' sssmplr <- ssSampler(key = key,
#'                      strata = data.dt[, strata_class],
#'                      ssize = c("Rural" = 408, "Urban" = 392))
#' sample_2 <- getSample(sssmplr)
#' #Define weights column
#' #Since sampling probabilities are pi = n_h/N_h
#' #Weights are 1/pi
#' data.dt[, N_h := .N , by = c("strata_class")]
#' #Vector of n_h
#' strata_to_sample_size <- c("Rural" = 408, "Urban" = 392)
#' data.dt[, weights := N_h/strata_to_sample_size[strata_class]]
#' data.dt[, N_h := NULL]
#'
#' strata_colummn <- "strata_class"
#' domain_column <- "domain_column"
#' weights_column <- "weights"
#'
#' estim <- RDomainEstimate(data = data.dt[sample_2,],
#'                          strata_colummn = strata_colummn,
#'                          domain_column = domain_column,
#'                          weights_column = weights_column)
#' print(estim)
#' print(paste0("Real total value of income: ", data.dt[, sum(income)]))
#' print(paste0("Real total value of cost_of_living: ", data.dt[, sum(cost_of_living)]))
#'
#' @export
RDomainEstimate <- function(data, strata_colummn, domain_column, weights_column) {
  RMLutils:::.validate_RDomainEstimate(data, strata_colummn, domain_column, weights_column)
  UseMethod("RDomainEstimate")
}

#' @rdname RDomainEstimate
#' @import data.table
#' @export
RDomainEstimate.default <- function(data, strata_colummn, domain_column, weights_column) {

  data.dt <- data.table(data)
  #Get unique domains from data
  domains <- sort(unique(data[, get(domain_column)]))
  #Obtain output calling the REstimate function for each domain separately
  output.lst <- lapply(domains, function(d){
    return(REstimate(data = data.dt[get(domain_column) == d],
                     strata_colummn = strata_colummn,
                     weights_column = weights_column))
  })
  names(output.lst) <- domains

  return(output.lst)
}

#' @title Monte Carlo Subsampling Rao-Blackwellized estimator.
#'
#' @description Computes the Monte Carlo Subsampling Rao-Blackwellized estimator from
#' Sanguiao-Zhang (2020) (\url{https://arxiv.org/abs/2003.11423}).
#'
#' Specifically, it makes use of equation (11) for model-assisted estimation.
#' Since exact Rao-Blackwellization is computationally expensive, a Monte Carlo
#' Subsampling method is used to obtain a reasonable estimation, which is
#' determined by the ssNumber parameter.
#'
#' It proceeds as follows:
#'
#' \enumerate{
#'  \item A total of ssNumber samples are generated by the subSampler
#'  \item For each subSample (s1):
#'  \itemize{
#'    \item A complementary sample to the subSample is defined (U/s1) (considering the mainSampler key as the full population).
#'    \item The auxiliary modeler is trained on the subSample data (s1).
#'    \item The auxiliary modeler predicts over the complementary data (U/s1).
#'    \item The modeler predictions (U/s1) and the true values in the subSample (s1) data are concatenated.
#'    \item A complementary sampler (cSampler) is defined, with its key set to be the complementary set of the current subSample for the total population (U/s1).
#'    \item A conditioned sample (cSample) is defined, as the set difference of the subSampler keys and the current subSample (s2).
#'    \item Errors are calculated comparing the modeler prediction and the real values (note that its shape matches the complementary sampler keys)(U/s1).
#'    \item The horvitz-thompson estimate for the sum of errors is calculated, assuming the conditioned_sampler as its sampling design and the conditioned sample (s2) as the subset seen by the HT modeler.
#'    \item Equation (11) is applied, adding up modeler predictions for the complementary subSample (U/s1), real values for the current subSample (s1), and estimate of total prediction errors for the conditioned sample (s2).
#'  }
#'  \item Finally, Monte Carlo errors are calculated for the total and variance estimates.
#' }
#'
#' @param auxModeler A regressorModeler object to use for model-assisted
#'  estimation on each subSample.
#' @param data A dataset containing the target variable of interest along with
#'  other covariates used for model-assisted estimation.
#' @param mainSampler Sampler used to define the keys of the subSampler
#'  Its keys should match the sample_data indexes.
#' @param subSampler Sampler used to get ssNumber samples from the target
#'  variable. Note that its key should be defined from a mainSampler sample.
#' @param targetVar Name of the target variable in the dataset.
#' @param ssNumber Amount of times to sub-sample.
#'
#' @return A list of lists containing the estimate, its variance and the associated Monte Carlo errors.
#'
#' @seealso \code{\link{estimator}}
#' @seealso \code{\link{MCSubRBDomainEstimate}}
#' @seealso \code{\link{biasVarianceEstimate}}
#'
#' @import data.table
#'
#' @examples
#' #Generate data
#' set.seed(273)
#' data.dt <- generateStratifiedDomainModelAssistedEstimateData(n_samples = 1000, generate_random_weights = FALSE)
#' head(data.dt)
#'
#' #Get relevant variables from data
#' data.keys <- 1:nrow(data.dt)
#' target <- "target"
#' regressors <- setdiff(names(data.dt), target)
#'
#' #Define main sampler and sub-sampler
#' train_size = round(0.8 * nrow(data.dt))
#' main_sampler <- srsSampler(key = data.keys, ssize = train_size)
#' main_sample <- getSample(main_sampler)
#' sub_sampler <- srsSampler(key = main_sample, ssize = train_size*0.5)
#'
#' #Define modeler
#' params <- list(
#'   "num.trees" = 50,
#'   "max.depth" = 20
#' )
#'
#' modeler <- rangerRegressorModeler(params = params, target = target, regressors = regressors)
#'
#' estim <- MCSubRBEstimate(auxModeler = modeler,
#'                          data = data.dt,
#'                          mainSampler = main_sampler,
#'                          subSampler = sub_sampler,
#'                          targetVar = target,
#'                          ssNumber = 100)
#'
#' print(paste0("Real value of target: ", data.dt[, sum(target)]))
#' print(paste0("Value of target estimate:", estim$total$value))
#' print(paste0("Monte Carlo error of estimate: ", estim$total$MC_error))
#' print(paste0("Standard error of estimate: ", sqrt(estim$variance$value)))
#' print(paste0("Standard error for the estimate of the Monte Carlo error: ", sqrt(estim$variance$MC_error)))
#'
#' @export
MCSubRBEstimate <- function(auxModeler, data, mainSampler, subSampler,
                            targetVar, ssNumber = 50) {
  RMLutils:::.validate_MCSubRBEstimate(auxModeler, data, mainSampler,
                                       subSampler, targetVar, ssNumber)
  UseMethod("MCSubRBEstimate")
}
#' @rdname MCSubRBEstimate
#' @export
MCSubRBEstimate.regressorModeler <- function(auxModeler, data, mainSampler, subSampler,
                                             targetVar, ssNumber = 50) {
  vtotal <- double(ssNumber)
  vvariance <- double(ssNumber)
  samples <- getSample(subSampler, times = ssNumber)
  for (i in 1:ssNumber) {
    subSample <- sort(samples[, i])
    auxModeler <- train(auxModeler, data, subSample)
    predictions <- getPredictions(auxModeler, data)
    predictions[subSample] <- data[subSample,][[targetVar]]
    cSampler <- conditionedSampler(mainSampler, subSample)
    cSample <- sort(setdiff(subSampler$key, subSample))
    CVerror <- data[cSample,][[targetVar]] - predictions[cSample]
    HTError <- HTEstimate(cSampler, cSample, CVerror)
    vtotal[i] <- sum(predictions) + HTError$total
    vvariance[i] <- HTError$variance
  }
  output <- list()
  output$total$value <- mean(vtotal)
  output$total$MC_error <- sqrt(var(vtotal) / ssNumber)
  vvariance <- vvariance - (vtotal - mean(vtotal))**2
  output$variance$value <- mean(vvariance)
  output$variance$MC_error <- sqrt(var(vvariance) / ssNumber)
  return(output)
}

#' @title Monte Carlo Subsampling Rao-Blackwellized estimator by domains
#'
#' @description Computes the Monte Carlo Subsampling Rao-Blackwellized estimator from
#' Sanguiao-Zhang (2020) (https://arxiv.org/abs/2003.11423) by domains. For a more
#' in-depth explanation, check \code{\link{MCSubRBEstimate}} documentation.
#'
#' @param auxModeler A regressorModeler object to use for model-assisted
#'  estimation on each subsample.
#' @param data A dataset containing the target variable of interest along with
#'  other covariates used for model-assisted estimation.
#' @param mainSampler Sampler used to define the keys of the sub_sampler.
#'  Its keys should match the sample_data indexes.
#' @param subSampler Sampler used to get ss_number samples from the target
#'  variable. Note that its key should be defined from a main_sampler sample.
#' @param targetVar Name of the target variable in the dataset.
#' @param domains A list, vector, array or dataframe column indicating the
#'  domains of interest. Its shape should match the rows of data.
#' @param ssNumber Amount of times to subsample.
#'
#' @return A list of lists containing the estimates and their variances for each domain, along their respective monte carlo errors.
#'
#' @seealso \code{\link{estimator}}
#' @seealso \code{\link{MCSubRBEstimate}}
#' @seealso \code{\link{biasVarianceEstimate}}
#'
#' @import data.table
#'
#' @examples
#' #Generate data
#' set.seed(273)
#' data.dt <- generateStratifiedDomainModelAssistedEstimateData(n_samples = 1000, generate_random_weights = FALSE)
#' head(data.dt)
#'
#' #Get relevant variables from data
#' data.keys <- 1:nrow(data.dt)
#' target <- "target"
#' regressors <- setdiff(names(data.dt), target)
#'
#' #Define main sampler and sub-sampler
#' train_size = round(0.8 * nrow(data.dt))
#' main_sampler <- srsSampler(key = data.keys, ssize = train_size)
#' main_sample <- getSample(main_sampler)
#' sub_sampler <- srsSampler(key = main_sample, ssize = train_size*0.5)
#'
#' #Define modeler
#' params <- list(
#'   "num.trees" = 50,
#'   "max.depth" = 20
#' )
#'
#' modeler <- rangerRegressorModeler(params = params, target = target, regressors = regressors)
#'
#' estim <- MCSubRBDomainEstimate(auxModeler = modeler,
#'                          data = data.dt,
#'                          mainSampler = main_sampler,
#'                          subSampler = sub_sampler,
#'                          targetVar = target,
#'                          domains = data.dt[, domain_class],
#'                          ssNumber = 100)
#'
#' print(paste0("Real value of target: ", data.dt[, sum(target), by = c("domain_class")][, V1]))
#' print(paste0("Value of target estimate:", estim$total$value))
#' print(paste0("Monte Carlo error of estimate: ", estim$total$MC_error))
#' print(paste0("Standard error of estimate: ", sqrt(estim$variance$value)))
#' print(paste0("Standard error for the estimate of the Monte Carlo error: ", sqrt(estim$variance$MC_error)))
#'
#' @export
MCSubRBDomainEstimate <- function(auxModeler, data, mainSampler, subSampler,
                                  targetVar, domains, ssNumber = 50) {
  RMLutils:::.validate_MCSubRBDomainEstimate(auxModeler, data, mainSampler,
                                             subSampler, targetVar, domains,
                                             ssNumber)
  UseMethod("MCSubRBDomainEstimate")
}
#'
#' @rdname MCSubRBDomainEstimate
#' @export
MCSubRBDomainEstimate.regressorModeler <- function(auxModeler, data, mainSampler, subSampler,
                                          targetVar, domains, ssNumber = 50) {
  uDomains <- unique(domains)
  vtotal <- matrix(0, nrow = ssNumber, ncol = length(uDomains))
  vvariance <- matrix(0, nrow = ssNumber, ncol = length(uDomains))
  samples <- getSample(subSampler, times = ssNumber)
  for (i in 1:ssNumber) {
    subSample <- sort(samples[, i])
    auxModeler <- train(auxModeler, data, subSample)
    predictions <- getPredictions(auxModeler, data)
    predictions[subSample] <- data[subSample,][[targetVar]]
    cSampler <- conditionedSampler(mainSampler, subSample)
    cSample <- sort(setdiff(subSampler$key, subSample))
    CVerror <- data[cSample,][[targetVar]] - predictions[cSample]
    HTError <- HTDomainEstimate(cSampler, cSample, CVerror, domains[cSample])
    for (j in 1:length(uDomains)) {
      vtotal[i, j] <- sum(predictions[domains == uDomains[j]])
      if (paste0("_", uDomains[j]) %in% colnames(HTError$total))
        vtotal[i, j] <- vtotal[i, j] + HTError$total[,paste0("_", uDomains[j])]
      if (paste0("_", uDomains[j]) %in% colnames(HTError$variance))
        vvariance[i, j] <- HTError$variance[,paste0("_", uDomains[j])]
    }
  }
  output <- list()
  output$total$value <- apply(vtotal, 2, mean)
  names(output$total$value) <- uDomains
  output$total$MC_error <- sqrt(apply(vtotal, 2, var) / ssNumber)
  names(output$total$MC_error) <- uDomains
  vvariance <- vvariance - apply(vtotal, 2, function(x) x - mean(x))**2
  output$variance$value <- apply(vvariance, 2, mean)
  names(output$variance$value) <- uDomains
  output$variance$MC_error <- sqrt(apply(vvariance, 2, var) / ssNumber)

  return(output)
}

#' @title Pre-trained estimator
#'
#' @description Computes the prediction estimation from a pre-trained model,
#' assuming that the model is capable of outputting a reasonably unbiased estimate
#' of the target variable.
#'
#' @param object Modeler object to make the estimation with.
#' @param data Dataset to make the estimation.
#'
#' @return A vector with the estimates.
#'
#' @seealso \code{\link{estimator}}
#' @seealso \code{\link{preTrainedDomainEstimate}}
#' @seealso \code{\link{getPredictions}}
#'
#' @import data.table
#'
#' @examples
#' set.seed(273)
#' data.dt <- generateStratifiedDomainModelAssistedEstimateData(n_samples = 1000, generate_random_weights = FALSE)
#'
#' #Get relevant variables from data
#' data.keys <- 1:nrow(data.dt)
#' target <- "target"# Regression
#' regressors <- setdiff(names(data.dt), target)
#'
#' #Define sampler
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
#' modeler <- rangerRegressorModeler(params = params, target = target, regressors = regressors)
#'
#' modeler <- train(modeler, data = data.dt, subSet = train.keys)
#'
#' estim <- preTrainedEstimate(modeler, data = data.dt)
#' print(estim)
#' print(data.dt[, sum(target)])
#'
#' @export
preTrainedEstimate <- function(object, data) {
  RMLutils:::.validate_preTrainedEstimate(object, data)
  UseMethod("preTrainedEstimate")
}

#' @rdname preTrainedEstimate
#' @export
preTrainedEstimate.regressorModeler <- function(object, data){
  preds <- getPredictions(object, data)
  targets <- colnames(preds)
  output <- lapply(targets, function(t){
    return(sum(preds[, t]))
  })
  names(output) <- targets
  return(output)
}

#' @title Pre-trained estimator by domains
#'
#' @description Generalization of preTrained estimator to estimate by domains.
#'
#' @param object Modeler object to make the estimation with.
#' @param data Dataset to make the estimation.
#' @param domain_column String indicating the domain column in the data.
#'
#' @return A vector with the estimates.
#'
#' @seealso \code{\link{estimator}}
#' @seealso \code{\link{preTrainedEstimate}}
#' @seealso \code{\link{getPredictions}}
#'
#' @import data.table
#'
#' @examples
#' set.seed(273)
#' data.dt <- generateStratifiedDomainModelAssistedEstimateData(n_samples = 1000, generate_random_weights = FALSE)
#'
#' #Get relevant variables from data
#' data.keys <- 1:nrow(data.dt)
#' target <- "target"# Regression
#' regressors <- setdiff(names(data.dt), target)
#'
#' #Define sampler
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
#' modeler <- rangerRegressorModeler(params = params, target = target, regressors = regressors)
#'
#' modeler <- train(modeler, data = data.dt, subSet = train.keys)
#'
#' estim <- preTrainedDomainEstimate(modeler, data = data.dt, domain_column = "domain_class")
#' print(estim)
#' print(data.dt[, .(sum(target)), by = c("domain_class")])
#'
#' @export
preTrainedDomainEstimate <- function(object, data, domain_column) {
  UseMethod("preTrainedDomainEstimate")
}

#' @rdname preTrainedDomainEstimate
#' @export
preTrainedDomainEstimate.regressorModeler <- function(object, data, domain_column){
  preds <- getPredictions(object, data)
  targets <- colnames(preds)
  unique_domains <- data[, unique(get(domain_column))]
  output <- lapply(targets, function(t){
    current_preds.dt <- data.table::data.table(preds = preds[, t], domain = data[[c(domain_column)]])
    current_preds.dt <- current_preds.dt[, .(total = sum(preds)), by = c("domain")]
    m <- t(as.matrix(current_preds.dt[, total]))
    colnames(m) <- current_preds.dt[, domain]
    return(m)
  })
  names(output) <- targets
  return(output)
}

#' @title Prediction estimator.
#'
#' @description Computes the prediction estimation from a sample using a similar
#' method to MCSubRBEstimate, but without Rao-Blackwellization. It will provide
#' a prediction for the global estimator.
#'
#' @param modeler A trained \code{\link{modeler}} object to estimate with
#' @param data Dataset.
#' @param mainSampler Sampler object
#' @param subSampler Sampler object
#' @param ssNumber Sample size.
#'
#' @return A list with a matrix containing the estimated values of the predictor along its MSE.
#'
#' @seealso \code{\link{estimator}}
#' @seealso \code{\link{predictionDomainEstimate}}
#'
#' @import data.table
#'
#' @examples
#' set.seed(273)
#' data.dt <- generateStratifiedDomainModelAssistedEstimateData(n_samples = 1000, generate_random_weights = FALSE)
#'
#' #Get relevant variables from data
#' data.keys <- 1:nrow(data.dt)
#' target <- "target"# Regression
#' regressors <- setdiff(names(data.dt), target)
#'
#' #Define main sampler and sub-sampler
#' train_size = round(0.8 * nrow(data.dt))
#' main_sampler <- srsSampler(key = data.keys, ssize = train_size)
#' main_sample <- getSample(main_sampler)
#' sub_sampler <- srsSampler(key = main_sample, ssize = train_size*0.5)
#'
#' #Define modeler
#' params <- list(
#'   "num.trees" = 50,
#'   "max.depth" = 20
#' )
#' modeler <- rangerRegressorModeler(params = params, target = target, regressors = regressors)
#'
#' predictionEstimate(modeler = modeler,
#'                    data = data.dt,
#'                    mainSampler = main_sampler,
#'                    subSampler = sub_sampler,
#'                    ssNumber = 50)
#' print(paste0("Real value of target:", std_ma_data.dt[, sum(target)]))
#'
#' @noRd
predictionEstimate <- function(modeler, data, mainSampler,
                               subSampler, ssNumber = 50) {
  RMLutils:::.validate_predictionEstimate(modeler, data, mainSampler,
                                          subSampler, ssNumber)
  UseMethod("predictionEstimate")
}
#' @rdname predictionEstimate
#' @noRd
predictionEstimate.regressorModeler <- function(modeler, data, mainSampler,
                                       subSampler, ssNumber = 50) {
  targets <- getTarget(modeler)
  output <- list()
  output$EST <- matrix(0, ncol = ssNumber, nrow = length(targets))
  output$MSE <- output$EST
  samples <- getSample(subSampler, times = ssNumber)
  for(i in 1:ssNumber) {
    subSample <- sort(samples[, i])
    modeler <- train(modeler, data, subSample)
    notSubSample <- sort(setdiff(mainSampler$key, subSample))
    output$EST[, i] <- apply(rbind(
      matrix(as.matrix(data[subSample, ..targets])[, 1:length(targets)], ncol = length(targets)),
      getPredictions(modeler, data, notSubSample)), 2, sum)
    cSample <- sort(setdiff(subSampler$key, subSample))
    cSampler <- conditionedSampler(mainSampler, subSample)
    cError <- as.matrix(data[cSample, ..targets])[,1:length(targets)] -
      getPredictions(modeler, data, cSample)
    output$MSE[, i] <- apply(cError, 2, function(x) {
      cEstimate <- HTEstimate(cSampler, cSample, x)
      return(sum(cEstimate$total)**2 - sum(cEstimate$variance))
    })
  }
  modelClean(modeler)
  output$MSE <- apply(output$MSE, 1, mean)
  return(output)
}

#' @title Prediction estimator for domains.
#'
#' @description Computes the MSE of the prediction estimator for domains.
#'
#' @param modeler Model object
#' @param data Dataset
#' @param mainSampler Sampler object
#' @param subSampler Sampler object
#' @param domains List of domains
#' @param ssize size of sample
#'
#'
#' @return A matrix with the MSE for each domain
#'
#' @seealso \code{\link{estimator}}
#' @seealso \code{\link{predictionEstimate}}
#'
#' @import data.table
#'
#' @examples
#' set.seed(273)
#' data.dt <- generateStratifiedDomainModelAssistedEstimateData(n_samples = 1000, generate_random_weights = FALSE)
#'
#' #Get relevant variables from data
#' data.keys <- 1:nrow(data.dt)
#' target <- "target"# Regression
#' regressors <- setdiff(names(data.dt), target)
#'
#' #Define main sampler and sub-sampler
#' train_size = round(0.8 * nrow(data.dt))
#' main_sampler <- srsSampler(key = data.keys, ssize = train_size)
#' main_sample <- getSample(main_sampler)
#' sub_sampler <- srsSampler(key = main_sample, ssize = train_size*0.5)
#'
#' #Define modeler
#' params <- list(
#'   "num.trees" = 50,
#'   "max.depth" = 20
#' )
#' modeler <- rangerRegressorModeler(params = params, target = target, regressors = regressors)
#'
#' predictionDomainEstimate(modeler = modeler,
#'                          data = data.dt,
#'                          mainSampler = main_sampler,
#'                          subSampler = sub_sampler,
#'                          domains = data.dt[, domain_class],
#'                          ssNumber = 50)
#'
#' @noRd
predictionDomainEstimate <- function(modeler, data, mainSampler,
                                     subSampler, domains,
                                     ssNumber = 50) {
  RMLutils:::.validate_predictionDomainEstimate(modeler, data, mainSampler,
                                                subSampler, domains, ssNumber)
  UseMethod("predictionDomainEstimate")
}
#' @rdname predictionDomainEstimate
#' @noRd
predictionDomainEstimate.regressorModeler <- function(modeler, data, mainSampler,
                                             subSampler, domains,
                                             ssNumber = 50) {
  targets <- getTarget(modeler)
  output <- array(0, dim = c(length(unique(domains)),
                             length(targets), ssNumber), dimnames = list(unique(domains), NULL))
  samples <- getSample(subSampler, times = ssNumber)
  for(i in 1:ssNumber) {
    subSample <- sort(samples[, i])
    modeler <- train(modeler, data, subSample)
    cSample <- sort(setdiff(subSampler$key, subSample))
    cSampler <- conditionedSampler(mainSampler, subSample)
    cError <- matrix(as.matrix(data[cSample, ..targets])[,1:length(targets)], ncol = length(targets)) -
      getPredictions(modeler, data, cSample)
    for(j in unique(domains)) {
      output[j, , i] <- apply(cError * (as.matrix(domains[cSample] == j) %*%
                                          t(as.matrix(rep(1, length(targets))))),
                              2, function(x) {
                                cEstimate <- HTEstimate(cSampler, cSample, x)
                                return(sum(cEstimate$total)**2 - sum(cEstimate$variance))
                              })
    }
  }
  modelClean(modeler)
  return(apply(output, c(1,2), mean))
}


