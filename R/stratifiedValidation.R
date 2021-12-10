#' stratifiedValidation
#'
#' @description
#' Evaluates PLP model in subpopulations
#'
#' @details
#' Append the subpopulation results into the main results
#'
#' @param prediction           the prediction object from the results object
#' @param analysisId           The analysis identifier
#' @return
#' An object of class \code{runPlp} that is recalibrated on the new data
#'
#' @export
stratifiedValidation <- function(prediction, analysisId) {

  # if recalibration has been done, as is done in this specific package
  # CovCoagBaseValidation, locally replace values with it
  if("recalibrationInTheLargeValue" %in% colnames(prediction))
  {
    ParallelLogger::logInfo('Using mean recalibration prediction values')
    prediction$value <- prediction$recalibrationInTheLargeValue
  }

  # split off younger population and older population
  pred_0_64_eval <- prediction %>%
    dplyr::filter(ageYear >=0, ageYear <= 64) %>%
    PatientLevelPrediction::evaluatePlp() %>%
    formatEvaluation(analysisId = analysisId, eval = "age0064")

  pred_65_150_eval <- prediction %>%
    dplyr::filter(ageYear >=65, ageYear <= 150) %>%
    PatientLevelPrediction::evaluatePlp() %>%
    formatEvaluation(analysisId = analysisId, eval = "age65150")

  pred_male_eval <- prediction %>%
    dplyr::filter(gender == 8507) %>%
    PatientLevelPrediction::evaluatePlp() %>%
    formatEvaluation(analysisId = analysisId, eval = "sexMale")

  pred_female_eval <- prediction %>%
    dplyr::filter(gender == 8532) %>%
    PatientLevelPrediction::evaluatePlp() %>%
    formatEvaluation(analysisId = analysisId, eval = "sexFemale")


  return(list(pred_0_64_eval = pred_0_64_eval,
              pred_65_150_eval = pred_65_150_eval,
              pred_male_eval = pred_male_eval,
              pred_female_eval = pred_female_eval))
}


#' addEvaluation
#'
#' @description
#' Adds the subpopulation results to the main results
#'
#' @details
#' Append the subpopulation results into the main results
#'
#' @param performanceEvaluation           The main result performanceEvaluation
#' @param subpopEvaluation                   The subpopulation result
#' @return
#' An object of class \code{runPlp} that is recalibrated on the new data
#'
#' @export
addEvaluation <- function(performanceEvaluation, subpopEvaluation){

  if(!is.null(subpopEvaluation$demographicSummary)){
    ParallelLogger::logInfo('Appending subpopulation demographicSummary')
    performanceEvaluation$demographicSummary <- rbind(performanceEvaluation$demographicSummary,
                                                      subpopEvaluation$demographicSummary)
  }

  if(!is.null(subpopEvaluation$calibrationSummary )){
    ParallelLogger::logInfo('Appending subpopulation calibrationSummary ')
    performanceEvaluation$calibrationSummary  <- rbind(performanceEvaluation$calibrationSummary ,
                                                       subpopEvaluation$calibrationSummary )
  }

  if(!is.null(subpopEvaluation$thresholdSummary )){
    ParallelLogger::logInfo('Appending subpopulation thresholdSummary ')
    performanceEvaluation$thresholdSummary  <- rbind(performanceEvaluation$thresholdSummary ,
                                                     subpopEvaluation$thresholdSummary )
  }

  if(!is.null(subpopEvaluation$evaluationStatistics )){
    ParallelLogger::logInfo('Appending subpopulation evaluationStatistics ')

    performanceEvaluation$evaluationStatistics <- as.data.frame(performanceEvaluation$evaluationStatistics)
    performanceEvaluation$evaluationStatistics$Metric <- as.character(performanceEvaluation$evaluationStatistics$Metric)
    performanceEvaluation$evaluationStatistics$Value <- as.character(performanceEvaluation$evaluationStatistics$Value)
    performanceEvaluation$evaluationStatistics <- rbind(performanceEvaluation$evaluationStatistics ,
                                                        subpopEvaluation$evaluationStatistics )
  }

  return(performanceEvaluation)
}



formatEvaluation <- function(evaluation, analysisId, eval){
  if(!is.null(evaluation$demographicSummary)){
    demoNames <- colnames(evaluation$demographicSummary)
    evaluation$demographicSummary$analysisId  <- analysisId
    evaluation$demographicSummary$Eval <- eval
    evaluation$demographicSummary <- evaluation$demographicSummary[,c("analysisId","Eval", demoNames )]
  }

  if(!is.null(evaluation$calibrationSummary)){
    calNames <- colnames(evaluation$calibrationSummary)
    evaluation$calibrationSummary$analysisId  <- analysisId
    evaluation$calibrationSummary$Eval <- eval
    evaluation$calibrationSummary <- evaluation$calibrationSummary[,c("analysisId","Eval", calNames )]
  }

  if(!is.null(evaluation$thresholdSummary)){
    thresNames <- colnames(evaluation$thresholdSummary)
    evaluation$thresholdSummary$analysisId  <- analysisId
    evaluation$thresholdSummary$Eval <- eval
    evaluation$thresholdSummary <- evaluation$thresholdSummary[,c("analysisId","Eval", thresNames )]
  }

  evaluation$evaluationStatistics$analysisId <- NULL
  evaluation$evaluationStatistics <- data.frame(analysisId = analysisId,
                                                Eval = eval,
                                                Metric = names(unlist(evaluation$evaluationStatistics)),
                                                Value = unlist(evaluation$evaluationStatistics))
  return(evaluation)
}
