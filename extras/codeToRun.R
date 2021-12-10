library(CovCoagFullValidation)

# the location to save the models validation results to:
outputFolder <- './CovCoagFullValidationResults'

# add the database connection details
dbms = 'your database management system'
server = 'your server'
user = 'your username'
password = 'top secret'
port = 'your port'
connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = dbms,
                                                                server = server,
                                                                user = user,
                                                                password = pw,
                                                                port = port)

# add cdm database details:
cdmDatabaseSchema <- 'your cdm database schema'

# add a schema you have read/write access to
# this is where the cohorts will be created (or are already created)
cohortDatabaseSchema <- 'your cohort database schema'

# if using oracle specify the temp schema
oracleTempSchema <- NULL

# Add a sharebale name for the database containing the OMOP CDM data
databaseName <- 'your database name'

# table name where the cohorts will be generated
cohortTable <- 'CovCoagFullValidationCohort'

#===== execution choices =====

# how much details do you want for in progress report?
verbosity <- "INFO"

# create the cohorts using the sql in the package?
createCohorts = T

# apply the models in the package to your data?
runValidation = T
# if you only want to apply models to a sample of
# patients put the number as the sampleSize
sampleSize = NULL
# do you want to recalibrate results?
# NULL means none (see ?CovCoagFullValidation::execute for options)
recalibrate <- c("recalibrationInTheLarge", "weakRecalibration")

# validates models in age and sex strata
stratifiedEval = T

# extract the results to share as a zip file?
packageResults = T
# when extracting results - what is the min cell count?
minCellCount = 5

#=============================

# Now run the study
CovCoagFullValidation::execute(connectionDetails = connectionDetails,
                               databaseName = databaseName,
                               cdmVersion = 5,
                               cdmDatabaseSchema = cdmDatabaseSchema,
                               cohortDatabaseSchema = cohortDatabaseSchema,
                               oracleTempSchema = oracleTempSchema,
                               cohortTable = cohortTable,
                               outputFolder = outputFolder,
                               createCohorts = createCohorts,
                               recalibrate = recalibrate,
                               runValidation = runValidation,
                               packageResults = packageResults,
                               minCellCount = minCellCount,
                               sampleSize = sampleSize,
                               stratifiedEval = stratifiedEval,
                               verbosity = verbosity)
