# Run (shortcut: CTR + A + Enter or Command + A + Enter)-----
source("code/dic.R",local = knitr::knit_global()) # dictionary
source("code/read_file.R",local = knitr::knit_global()) # function read file

# load file
if (str_detect(hospital,"Siem") == TRUE) {
	source("code/Load_file_SR.R",local = knitr::knit_global())
}

if (str_detect(hospital,"Bat") == TRUE) {
	source("code/Load_file_BB.R",local = knitr::knit_global())
}

if (str_detect(hospital,"Kam") == TRUE) {
	source("code/Load_file_KC.R",local = knitr::knit_global())
}

if (str_detect(hospital,"Tak") == TRUE) {
	source("code/Load_file_TK.R",local = knitr::knit_global())
}


# render report
rmarkdown::render(
  input = "code/AMC.Rmd",
  output_file = paste0("../Output/AMC Report ",Sys.Date(),".html")) 

# clear objects
rm(list = ls())

