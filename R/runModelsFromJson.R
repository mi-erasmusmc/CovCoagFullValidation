# edit to make popSetting and ids an option as input to overwrite.
runModelsFromJson <- function(outputFolder,
                                 connectionDetails,
                                 cohortDatabaseSchema,
                                 outcomeDatabaseSchema,
                                 cdmDatabaseSchema,
                              cdmVersion,
                                 oracleTempSchema,
                                 cdmDatabaseName,
                                 cohortTable,
                                 outcomeTable,
                                 sampleSize,
                                 keepPrediction,
                                 recalibrate,
                              stratifiedEval,
                                 verbosity){

  settingsLocation <- system.file("settings/plpAnalysisList.json",
                                  package = "CovCoagFullValidation")

  jsonSettings <- Hydra::loadSpecifications(settingsLocation)
  jsonSettings <- RJSONIO::fromJSON(jsonSettings)
  analysisSettings <- getAnalyses(jsonSettings)

  # analysisId, modelName, modelType, cohortId, cohortName, outcomeId, outcomeName ...

  for(i in 1:nrow(analysisSettings)){

    analysisId <- analysisSettings$analysisId[i]

    if(file.exists(file.path(outputFolder,cdmDatabaseName,analysisId, 'validationResult.rds'))){
      ParallelLogger::logInfo(paste0('Analysis at ',
                                     file.path(outputFolder,cdmDatabaseName,analysisId),
                                     ' exists - skipping'))
    }else {

      populationSettings <- getPopulationSettings(jsonSettings,
                                                  analysisSettings$modelName[i])
      populationSettings$outcomeId <- analysisSettings$outcomeId[i]

      covariateSettings <- getCovariateSettings(jsonSettings,
                                                analysisSettings$modelName[i],
                                                cohortDatabaseSchema,
                                                cohortTable)

      plpDataSettings <- list(connectionDetails = connectionDetails,
                              cdmDatabaseSchema = cdmDatabaseSchema,
                              cohortDatabaseSchema = cohortDatabaseSchema,
                              cohortTable = cohortTable,
                              cohortId = analysisSettings$cohortId[i],
                              outcomeDatabaseSchema = cohortDatabaseSchema,
                              outcomeTable = cohortTable,
                              outcomeIds = analysisSettings$outcomeId[i],
                              oracleTempSchema = oracleTempSchema,
                              firstExposureOnly = populationSettings$firstExposureOnly,
                              sampleSize = sampleSize,
                              cdmVersion = cdmVersion,
                              covariateSettings = covariateSettings)

      # get data
      if(!file.exists(file.path(outputFolder,cdmDatabaseName,analysisId, 'plpData'))){
        plpData <- tryCatch({do.call(PatientLevelPrediction::getPlpData, plpDataSettings)},
                            error = function(e){ParallelLogger::logError(e); return(NULL)})

        if(!is.null(plpData)){
          if(!dir.exists(file.path(outputFolder,cdmDatabaseName,analysisId))){
            dir.create(file.path(outputFolder,cdmDatabaseName,analysisId))
          }
          PatientLevelPrediction::savePlpData(plpData, file.path(outputFolder,cdmDatabaseName,analysisId, 'plpData'))
        }

      } else{
        ParallelLogger::logInfo(paste0('Data exists at ',
                                       file.path(outputFolder,cdmDatabaseName,analysisId),
                                       ' - loading'))
        plpData <- PatientLevelPrediction::loadPlpData(file.path(outputFolder,cdmDatabaseName,analysisId, 'plpData'))
      }

      # get population
      if(!file.exists(file.path(outputFolder,cdmDatabaseName,analysisId, 'population.rds'))){

        populationSettings$plpData <- plpData
        population <- tryCatch({do.call(PatientLevelPrediction::createStudyPopulation, populationSettings)},
                               error = function(e){ParallelLogger::logError(e); return(NULL)})
        if(!is.null(population)){
          saveRDS(population, file.path(outputFolder,cdmDatabaseName,analysisId, 'population.rds'))
        }
      } else{
        ParallelLogger::logInfo(paste0('Population exists at ',
                                       file.path(outputFolder,cdmDatabaseName,analysisId),
                                       ' - loading'))
        population <- readRDS(file.path(outputFolder,cdmDatabaseName,analysisId, 'population.rds'))
      }


      # get model
      plpModel <- tryCatch({ loadModelFromJson(jsonSettings,
                                               analysisSettings$modelName[i])
      },error = function(e){ParallelLogger::logError(e); return(NULL)})


      # apply model
      evaluation <- tryCatch({PatientLevelPrediction::applyModel(population = population,
                                                                 plpData = plpData,
                                                                 plpModel = plpModel)},
                             error = function(e){ParallelLogger::logError(e); return(NULL)})

      if(!is.null(evaluation)){

        # add recalibration if desired
        if(!is.null(recalibrate)){
          ParallelLogger::logInfo('Recalibrating')
          for(k in 1:length(recalibrate)){
            if(recalibrate[k] %in% c('recalibrationintheLarge', 'weakRecalibration')){
              ParallelLogger::logInfo(paste0('Using method ', recalibrate[k]))
              recal <- PatientLevelPrediction::recalibratePlp(evaluation$prediction,
                                                              analysisId = analysisId,
                                                              method = recalibrate[k])

              evaluation$prediction <- recal$prediction
              evaluation$performanceEvaluation <- PatientLevelPrediction::addRecalibration(evaluation$performanceEvaluation,
                                                                                           recalibration = recal$performanceEvaluation)
            }

          }
        }

        if(!is.null(stratifiedEval)) {
          ParallelLogger::logInfo('Evaluating in subpopulations')
          stratEvaluation <- stratifiedValidation(prediction = evaluation$prediction,
                                                  analysisId = analysisId)
          for (i in 1:length(stratEvaluation)) {
            evaluation$performanceEvaluation <- addEvaluation(evaluation$performanceEvaluation,
                                                              subpopEvaluation = stratEvaluation[[i]])
          }
        }

        # format results
        result <- formatResults(result = evaluation,
                                populationSettings = populationSettings,
                                covariateSettings = covariateSettings,
                                cdmDatabaseName = cdmDatabaseName,
                                modelType = analysisSettings$modelType[i],
                                modelName = analysisSettings$omodelName[i])

        if(!is.null(result)){

          if(!dir.exists(file.path(outputFolder,cdmDatabaseName,analysisId))){
            dir.create(file.path(outputFolder,cdmDatabaseName,analysisId), recursive = T)
          }

          ParallelLogger::logInfo("Saving results")
          saveRDS(result, file.path(outputFolder,cdmDatabaseName,analysisId, 'validationResult.rds'))
          ParallelLogger::logInfo(paste0("Results saved to:",file.path(outputFolder,cdmDatabaseName,analysisId)))
        }

      }
    }
  } # analysis

  return(invisible(NULL))
}

formatResults <- function(result,
                          populationSettings,
                          covariateSettings,
                          cdmDatabaseName ,
                          modelType,
                          modelName){
  result$inputSetting$database <- cdmDatabaseName
  result$model$modelType <- modelType
  result$model$name <- modelName
  result$inputSetting$modelSettings <- result$model
  result$inputSetting$dataExtrractionSettings <- list(covariateSettings = covariateSettings)
  result$inputSetting$populationSettings <- populationSettings

  return(result)
}


getAnalyses <- function(jsonSettings){

  result <- data.frame(modelName = unlist(lapply(jsonSettings$models, function(x) x$name)),
                       modelType = unlist(lapply(jsonSettings$models, function(x) x$attr_type)),
                       cohortId = unlist(lapply(jsonSettings$models, function(x) x$cohortId)),
                       cohortName = getCohortName(jsonSettings,unlist(lapply(jsonSettings$models, function(x) x$cohortId))),
                       outcomeId = unlist(lapply(jsonSettings$models, function(x) x$outcomeId)),
                       outcomeName = getCohortName(jsonSettings,unlist(lapply(jsonSettings$models, function(x) x$outcomeId)))
  )

  result$analysisId <- paste(result$modelName, result$cohortId, result$outcomeId, sep='_')

  return(result)
}

getCohortName <- function(jsonSettings, cohortIds){

  cohortIdentifiers <- do.call('rbind', lapply(jsonSettings$cohortDefinition, function(x){c(x$name, x$id)}))
  colnames(cohortIdentifiers) <- c('name','id')

  names <- c()
  for(id in cohortIds){
    ind <- which(cohortIdentifiers[,'id'] ==id)

    if(length(ind)==0){
      ParallelLogger::logInfo('cohort id missing from cohortDefinitions')
      names <- c(names,'Missing')
    } else{
      names <- c(names,cohortIdentifiers[min(ind),'name'])
    }
  }

  return(names)
}

getPopulationSettings <- function(jsonSettings,
                                  modelName){

  ind <- which(modelName == unlist(lapply(jsonSettings$models, function(x) x$name)))
  populationSettings <- jsonSettings$models[[ind]]$populationSettings

  return(populationSettings)

}

