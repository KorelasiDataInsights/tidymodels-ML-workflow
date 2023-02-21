
if (FALSE) {
  # Install the renv package
  install.packages('renv')
  
  # Initialize the renv
  renv::init(bare = TRUE)
}


# Activate the environment
renv::activate()


##################################
##### Use renv to install packages
##################################

# General
renv::install("tidyverse")
renv::install("reshape2")
renv::install("DT")
renv::install("markdown")
renv::install("daroczig/logger")
renv::install("here")
renv::install("roxygen2")
renv::install("testthat") # may not need this

# Excel spreadsheet
renv::install("openxlsx")

# Plotting
renv::install("gridExtra")

# Modeling and automated EDA
renv::install("tidymodels")
renv::install("xgboost")
renv::install("koalaverse/vip")
renv::install("ranger")
renv::install("lightgbm")
renv::install("bonsai")
renv::install("DataExplorer")
renv::install("DALEXtra")
renv::install("caret")

# Parallel processing
renv::install("parallelly")
renv::install("doParallel")

# Other
# renv::install("vetiver")
#renv::install("")
#renv::install("")
#renv::install("")

renv::status()

renv::snapshot()

renv::status()
