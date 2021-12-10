loadModelFromJson <- function(jsonSettings,
                  modelName,
                  modelLoc = NULL){

  ind <- which(modelName == unlist(lapply(jsonSettings$models, function(x) x$name)))
  modelSettings <- jsonSettings$models[[ind]]

  if(modelSettings$attr_type %in% c("pythonJson",
                                    'xgboost',
                                    'plp')){

    plpModel <- fromModelJson(modelSettings, modelLoc = modelLoc)
  } else{
    ParallelLogger::logInfo('type currently not supported as JSON')
  }

  return(plpModel)

}

fromModelJson <- function(modelSettings,modelLoc){

  if(is.null(modelLoc)){
    modelLocation <- system.file(paste0("models/",modelSettings$name,"/model.json"),
                                 package = "CovCoagFullValidation")

    cvMapLocation <- tryCatch({system.file(paste0("models/",modelSettings$name,"/covariateMap.csv"),
                                           package = "CovCoagFullValidation")},
                              error = function(e){return(NULL)})
    varImpLocation <- tryCatch({system.file(paste0("models/",modelSettings$name,"/varImp.csv"),
                                            package = "CovCoagFullValidation")},
                               error = function(e){return(NULL)})
  } else{
    modelLocation <- file.path(modelLoc, modelSettings$name, "model.json")
    cvMapLocation <- file.path(modelLoc, modelSettings$name, "covariateMap.csv")
    varImpLocation <- file.path(modelLoc, modelSettings$name, "varImp.csv")
  }

  plpModel <- editPreprocessing(modelSettings$settings)
  plpModel$covariateMap <- tryCatch({utils::read.csv(cvMapLocation)},
                                    error = function(e){return(NULL)})
  plpModel$varImp <- tryCatch({utils::read.csv(varImpLocation)},
                              error = function(e){return(NULL)})


  if(modelSettings$attr_type %in% c('plp','nonPlpGlm')){
    # load the model from the json
    plpModel$model <- Hydra::loadSpecifications(modelLocation)
    plpModel$model <- RJSONIO::fromJSON(plpModel$model)
  } else if(modelSettings$attr_type %in% c('xgboost')){
    plpModel$model <- xgboost::xgb.load(modelLocation)
  } else{
    plpModel$model <- modelLocation
  }

  class(plpModel) <- 'plpModel'
  attr(plpModel, 'type') <- modelSettings$attr_type
  attr(plpModel, 'predictionType') <- modelSettings$attr_predictionType

  plpModel$predict <- PatientLevelPrediction:::createTransform(plpModel)

  return(plpModel)
}


editPreprocessing <- function(settings){
  if(!is.null(settings$metaData$preprocessSettings$deletedInfrequentCovariateIds)){
    settings$metaData$preprocessSettings$deletedInfrequentCovariateIds <- unlist(settings$metaData$preprocessSettings$deletedInfrequentCovariateIds)
  }
  if(!is.null(settings$metaData$preprocessSettings$deletedRedundantCovariateIds)){
    settings$metaData$preprocessSettings$deletedRedundantCovariateIds <- unlist(settings$metaData$preprocessSettings$deletedRedundantCovariateIds)
  }
  if(!is.null(settings$metaData$preprocessSettings$normFactors)){
    settings$metaData$preprocessSettings$normFactors <- data.frame(maxValue = unlist(settings$metaData$preprocessSettings$normFactors$maxValue),
                                                                   covariateId = unlist(settings$metaData$preprocessSettings$normFactors$covariateId)
    )
  }

  return(settings)
}
