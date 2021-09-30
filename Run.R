# Run-----
# load file
#source("code/Load_file_SR.R",local = knitr::knit_global()) # load file from Siem Reap

source("code/Load_file_BB.R",local = knitr::knit_global()) # load file from Battambang

# render report
rmarkdown::render(
  input = "code/AMC.Rmd",
  output_file = paste0("../Output/AMC Report ",Sys.Date(),".html")) 

# clear objects
rm(list = ls())

