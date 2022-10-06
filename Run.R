# Run (shortcut: CTR + A + Enter or Command + A + Enter) -----
source("code/function.R", local = knitr::knit_global()) # function read file
source("code/dic.R", local = knitr::knit_global()) # dictionary

# render report
rmarkdown::render(input = "code/AMC.Rmd",
									output_format = "html_document",
									output_dir = "output",
									output_file = paste0(h_abbr, " AMC Report ", Sys.Date())
									)

# clear objects
rm(list = ls())

