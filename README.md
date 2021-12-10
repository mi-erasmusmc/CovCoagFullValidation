CovCoagFullValidation
======================

Introduction
============
This package contains code to externally validate models for the prediction quesiton <add question> developed on the database <add database>.

Features
========
  - Applies models developed using the OHDSI PatientLevelPrediction package
  - Evaluates the performance of the models on new data
  - Packages up the results (after removing sensitive date) to share with study owner

Technology
==========
  CovCoagFullValidation is an R package.

System Requirements
===================
  * Requires: OMOP CDM database and connection details
  * Requires: Java runtime enviroment (for the database connection)
  * Requires: R (version 3.3.0 or higher).
  * Sometimes required: Python 

Dependencies
============
  * PatientLevelPrediction
  
Guide
============
A general guide for running a valdiation study package is available here: [Skeleton Validation Study guide](https://github.com/OHDSI/CovCoagFullValidation/tree/master/inst/doc/UsingSkeletonValidationPackage.pdf)
  
  
A1. Installing the package from GitHub
===============
```r
# first set up the environment using the lockfile:
# If you don't have renv as an R library you need to install it:
install.packages("renv")

# renv will create an environemnt with all the R libraries and versions that
# were used by the original study developer (this is handy if the study needs to be run 
# in the future when new versions are available and may have different code that 
# causes a study to break)

# You need to specify a project folder for the renv (the study specific environment will be 
# save here) and you need to set you R working direcory to this location before running renv
projectFolder <- "C:/CovCoagFullValidation"
if(!dir.exists(projectFolder)){
dir.create(projectFolder,   recursive = T)
}
setwd(projectFolder)
                                                                                              
# Download the lock file:
download.file("https://raw.githubusercontent.com/ohdsi-studies/CovCoagFullValidation/master/renv.lock", "renv.lock")

# Build the local library into projectFolder (takes a while):
renv::init()

# (When not in RStudio, you'll need to restart R now)

# To install the package from github:
install.packages("devtools")
devtools::install_github("ohdsi-studies/CovCoagFullValidation")
```

A2. Building the package inside RStudio
===============
  1. Open the validation package project file (file ending in .Rproj) 
  2. Build the package in RStudio by selecting the 'Build' option in the top right (the tabs contain  'Environment', 'History', 'Connections', 'Build', 'Git') and then clicking on the 'Install and Restart'

B. Getting Started
===============
  1. Make sure to have either: installed (A1) or built (A2) the package 
  2. In R, run the code in 'extras/codeToRun.R' (see [Skeleton Validation Study guide](https://github.com/OHDSI/CovCoagFullValidation/tree/master/inst/doc/UsingSkeletonValidationPackage.pdf) for guideance)


C. Example Code
===============
```r
library(CovCoagFullValidation)

# add details of your database setting:
databaseName <- 'add a shareable name for the database you are currently validating on'

# add the cdm database schema with the data
cdmDatabaseSchema <- 'your cdm database schema for the validation'

# add the work database schema this requires read/write privileges 
cohortDatabaseSchema <- 'your work database schema'

# if using oracle please set the location of your temp schema
oracleTempSchema <- NULL

# the name of the table that will be created in cohortDatabaseSchema to hold the cohorts
cohortTable <- 'CovCoagFullValidationCohortTable'

# the location to save the prediction models results to:
# NOTE: if you set the outputFolder to the 'Validation' directory in the 
#       prediction study outputFolder then the external validation will be
#       saved in a format that can be used by the shiny app 
outputFolder <- '../Validation'

# add connection details:
dbms <- "pdw"
user <- NULL
pw <- NULL
server <- Sys.getenv('server')
port <- Sys.getenv('port')
connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = dbms,
                                                                server = server,
                                                                user = user,
                                                                password = pw,
                                                                port = port)

# Now run the study:
CovCoagFullValidation::execute(connectionDetails = connectionDetails,
                 databaseName = databaseName,
                 cdmDatabaseSchema = cdmDatabaseSchema,
                 cohortDatabaseSchema = cohortDatabaseSchema,
                 oracleTempSchema = oracleTempSchema,
                 cohortTable = cohortTable,
                 outputFolder = outputFolder,
                 createCohorts = T,
                 runValidation = T,
                 packageResults = F,
                 minCellCount = 5,
                 sampleSize = NULL)
                 
# If the validation study runs to completion and returns results, package it up ready to share with the study owner (but remove counts less than 10) by running:
CovCoagFullValidation::execute(connectionDetails = connectionDetails,
                 databaseName = databaseName,
                 cdmDatabaseSchema = cdmDatabaseSchema,
                 cohortDatabaseSchema = cohortDatabaseSchema,
                 oracleTempSchema = oracleTempSchema,
                 cohortTable = cohortTable,
                 outputFolder = outputFolder,
                 createCohorts = F,
                 runValidation = F,
                 packageResults = T,
                 minCellCount = 10,
                 sampleSize = NULL)
                 
                 
# If your target cohort is large use the sampleSize setting to sample from the cohort:
CovCoagFullValidation::execute(connectionDetails = connectionDetails,
                 databaseName = databaseName,
                 cdmDatabaseSchema = cdmDatabaseSchema,
                 cohortDatabaseSchema = cohortDatabaseSchema,
                 oracleTempSchema = oracleTempSchema,
                 cohortTable = cohortTable,
                 outputFolder = outputFolder,
                 createCohorts = T,
                 runValidation = T,
                 packageResults = F,
                 minCellCount = 10,
                 sampleSize = 1000000)
                 
```

License
=======
  CovCoagFullValidation is licensed under Apache License 2.0

Development
===========
  CovCoagFullValidation is being developed in R Studio.
