getCovariateSettings <- function(jsonSettings,
                                 modelName,
                                 cohortDatabaseSchema,
                                 cohortTable){

  ind <- which(modelName == unlist(lapply(jsonSettings$models, function(x) x$name)))
  covariateSettings <- jsonSettings$models[[ind]]$covariateSettings

  covariateSettings <- updateSettings(covariateSettings,
                                      cohortDatabaseSchema,
                                      cohortTable)

  return(covariateSettings)

}

updateSettings <- function(covariateSettings,
                           cohortDatabaseSchema,
                           cohortTable){

  #covariateSettings has: "fnct" and "settings"

  if(is.null(covariateSettings$fnct)){
    # a list of settings
    for(j in 1:length(covariateSettings)){
      covariateSettings[[j]]$settings <- addExtras(covariateSettings[[j]]$fnct,
                                                   covariateSettings[[j]]$settings,
                                                   cohortDatabaseSchema,
                                                   cohortTable)
      covariateSettings[[j]]$settings <- evalFunction(covariateSettings[[j]]$settings)

      if(covariateSettings[[j]]$fnct == 'createCovariateSettings'){
        covariateSettings[[j]] <- covariateSettings[[j]]$settings
        attr(covariateSettings[[j]], 'fun') <- covariateSettings[[j]]$attr_fun
        covariateSettings[[j]]$attr_fun <- NULL
        attr(covariateSettings[[j]], 'class') <- covariateSettings[[j]]$attr_class
        covariateSettings[[j]]$attr_class <- NULL

      }else{
        covariateSettings[[j]] <- do.call(get(covariateSettings[[j]]$fnct),
                                          covariateSettings[[j]]$settings)
      }
    }
  }else{
    # a single setting
    covariateSettings$settings <- addExtras(covariateSettings$fnct,
                                            covariateSettings$settings,
                                            cohortDatabaseSchema,
                                            cohortTable)
    covariateSettings$settings <- evalFunction(covariateSettings$settings)

    if(covariateSettings$fnct == 'createCovariateSettings'){
      covariateSettings <- covariateSettings$settings
      attr(covariateSettings, 'fun') <- covariateSettings$attr_fun
      covariateSettings$attr_fun <- NULL
      attr(covariateSettings, 'class') <- covariateSettings$attr_class
      covariateSettings$attr_class <- NULL

    }else{
      covariateSettings <- do.call(get(covariateSettings$fnct),
                                   covariateSettings$settings)
    }
  }

  return(covariateSettings)
}

addExtras <- function(fname, list, cohortDatabaseSchema, cohortTable){
  if(fname%in%c('createCohortCovariateSettings', 'measurementCohortCovariateSettings')){
    list$cohortDatabaseSchema <- cohortDatabaseSchema
    list$cohortTable <- cohortTable
  }
  return(list)
}

evalFunction <- function(list){
  ind <- names(list)%in%c('scaleMap')

  if(sum(ind)>0){
    for( i in which(ind)){
      list[[i]] <- eval(str2lang(paste0(list[[i]], collapse = ' ')))
    }
  }
  return(list)
}





