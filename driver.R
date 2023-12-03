
# -------------------------------------------------------------------------
#' This file is used to run the project from start to finish
#' It can be run using the shortcut `ctrl` + `shift` + `S`
#' 
# -------------------------------------------------------------------------

# setup -------------------------------------------------------------------


source("setup.R")


# processing --------------------------------------------------------------

# load data and clean data

source("processing/01_all-outpatient-data.R")


# analysis ----------------------------------------------------------------

# run models and create outputs

source("analysis/01_desc-stats-and-charts.R")
source("analysis/02_single-day-models.R")
source("analysis/05_sensitivity_analysis.R")
source("analysis/08_correlation-analysis.R")
source("analysis/09_composite-models.R")
source("analysis/04_single-day-plots.R")
source("analysis/11_composite-model-plots.R")
source("analysis/12_model-explainer-plots.R")



