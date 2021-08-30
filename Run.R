# load file----
source("code/Load_file.R",local = knitr::knit_global())

# render report----
rmarkdown::render(
  input = "code/AMC.Rmd",
  output_file = paste0("../Output/AMC Report ",Sys.Date(),".html")) 

# clear objects----
rm(list = ls())
