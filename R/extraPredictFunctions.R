# add the functions for the exisitng models here
#======= add custom function here...


#' Prediction for existing GLM
#'
#' @details
#' This applies the existing models and calcualtes the risk for a population
#'
#' @param plpModel The model being applied
#' @param plpData  The new data
#' @param population The new population
#'
#' @return
#' The population with an extra column 'value' corresponding to the patients risk
#'
#' @export
predict.nonPlpGlm <- function(plpModel, plpData, population){

  coeff <- plpModel$model$coefficients
  finalMapping <- plpModel$model$finalMapping
  type <- attr(plpModel, 'predictionType')
  offset <- plpModel$model$offset
  baselineHazard <- plpModel$model$baselineHazard

  finalMapping <- eval(str2lang(paste0(finalMapping, collapse = ' ')))

  plpData$covariateData$coefficients <- coeff
  on.exit(plpData$covariateData$coefficients <- NULL, add = TRUE)

  if(sum(c('power','offset')%in%colnames(coeff))==2){
    prediction <- plpData$covariateData$covariates %>%
      dplyr::inner_join(plpData$covariateData$coefficients, by= 'covariateId') %>%
      dplyr::mutate(values = (.data$covariateValue-.data$offset)^.data$power*.data$points) %>%
      dplyr::group_by(.data$rowId) %>%
      dplyr::summarise(value = sum(.data$values, na.rm = TRUE)) %>%
      dplyr::select(.data$rowId, .data$value) %>%
      dplyr::collect()
  } else{
    prediction <- plpData$covariateData$covariates %>%
      dplyr::inner_join(plpData$covariateData$coefficients, by= 'covariateId') %>%
      dplyr::mutate(values = .data$covariateValue*.data$points) %>%
      dplyr::group_by(.data$rowId) %>%
      dplyr::summarise(value = sum(.data$values, na.rm = TRUE)) %>%
      dplyr::select(.data$rowId, .data$value) %>%
      dplyr::collect()
  }

  prediction <- merge(population, prediction, by ="rowId", all.x = TRUE)
  prediction$value[is.na(prediction$value)] <- 0

  # add any final mapping here (e.g., add intercept and mapping)
  prediction$value <- finalMapping(prediction$value)

  metaData <- list(predictionType = type,
                   cohortId = attr(population,'metaData')$cohortId,
                   outcomeId = attr(population,'metaData')$outcomeId,
                   timepoint = attr(population,'metaData')$riskWindowEnd)
  attr(prediction, "metaData") <- metaData

  attr(prediction, "baselineHazard") <- baselineHazard
  attr(prediction, "offset") <-  offset
  attr(prediction, "timepoint") <- attr(population,'metaData')$riskWindowEnd

  return(prediction)
}

#' Prediction for python models saved as json objects
#'
#' @details
#' This applies the existing models and calculates the risk for a population
#'
#' @param plpModel The model being applied
#' @param plpData  The new data
#' @param population The new population
#'
#' @return
#' The population with an extra column 'value' corresponding to the patients risk
#'
#' @export
predict.pythonJson <- function(plpModel, plpData, population){

  data <- PatientLevelPrediction::toSparseM(plpData = plpData,
                                            population = population,
                                            map = plpModel$covariateMap)

  ParallelLogger::logInfo(paste0('Full data dimensions: ', nrow(data$data) ,',',ncol(data$data) ))

  included <- plpModel$varImp$covariateId[plpModel$varImp$included>0] # does this include map?
  included <- data$map$newCovariateId[data$map$oldCovariateId%in%included]

  #reticulate::conda_install(envname = 'r-reticulate', packages = 'sklearn-json')
  skljson <- tryCatch({reticulate::import('sklearn_json')},
                      error = function(e){ParallelLogger::logInfo("Need to run: reticulate::conda_install(envname = 'r-reticulate', packages = 'sklearn-json')")})
  modelTrained <- skljson$from_json(plpModel$model) # if adaBoost/Keras use different load

  dataMat <- data$data[population$rowId,included, drop = F]
  ParallelLogger::logInfo(paste0('Model data dimensions: ', nrow(dataMat) ,',',ncol(dataMat) ))

  if(is.null(dim(dataMat))){
    ParallelLogger::logInfo('Converting dimensions')
    dataMat <- matrix(as.vector(data$data[population$rowId]), ncol = 1)
  }

  pred <- modelTrained$predict_proba(dataMat)

  prediction <- cbind(population, pred[,2])
  colnames(prediction)[ncol(prediction)] <- 'value'

  metaData <- list(predictionType = 'pythonJson',
                   cohortId = attr(population,'metaData')$cohortId,
                   outcomeId = attr(population,'metaData')$outcomeId,
                   timepoint = attr(population,'metaData')$riskWindowEnd)
  attr(prediction, "metaData") <- metaData

  return(prediction)

}
