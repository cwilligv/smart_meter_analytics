################################################################################
# To reproduce the report, open this file in RStudio, and click the 'Source'
# button (this will run all of the code below). Once it has finished running,
# the datasets needed in the report can be found in the 'output' folder.
################################################################################

start.time <- Sys.time()

# Check needed packages are installed
using<-function(...) {
  libs<-unlist(list(...))
  req<-unlist(lapply(libs,require,character.only=TRUE))
  need<-libs[req==FALSE]
  if(length(need)>0){ 
    install.packages(need)
    lapply(need,require,character.only=TRUE)
  }
}

using("dplyr","ggplot2","lubridate","ClusterR", "clusterCrit", "cluster", "bookdown")

# Create output directories if they don't exist
if(!dir.exists("output/data")) {
  dir.create("output/data", recursive = TRUE)
}


# Import and clean the raw data
source("scripts/01_readings.R", local=new.env())

# Create summary dataframes for EDA
source("scripts/02_daily_mean_summaries.R", local=new.env())

# Feature engineering
source("scripts/03_feature_engineering.R", local=new.env())

# Data modeling
source("scripts/04_data_modeling.R", local=new.env())

# # Generate Report
# rmarkdown::render(input = "Major_project Christian Willig 1159820.Rmd")
# 
# # Open report
# browseURL(paste0(getwd(), "/", "Major_project-Christian-Willig-1159820.pdf"))

end.time <- Sys.time()
time.taken <- round(end.time - start.time,2)
time.taken
