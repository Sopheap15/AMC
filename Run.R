
source("code/load_file.R",local = knitr::knit_global())

rmarkdown::render(
  input = "AMC.Rmd",
  output_file = paste("Output/AMC Report",Sys.Date(),".html")) 

