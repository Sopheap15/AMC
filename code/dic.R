# load library ----
library(tidyverse)
library(ggtext)
library(rio)
library(XLConnect)
library(janitor)
library(kableExtra)
library(AMR)

# Import dictionary ----
## sheet dict
dic <- import("dic/dictionary.xlsx", sheet = "Dict") %>%
	mutate(container = trimws(str_to_lower(container)),
				 strength = trimws(str_to_lower(strength))) %>%
	distinct(abbr, container, strength, gram, .keep_all = T)

## sheet hospital
hospital <- import("dic/dictionary.xlsx", sheet = "hospital") %>%
	clean_names()

start_date <- excel_numeric_to_date(as.numeric(as.character(hospital[1,"parameter"])),
																		date_system = "modern") # start date
end_date <- excel_numeric_to_date(as.numeric(as.character(hospital[2,"parameter"])),
																	date_system = "modern") # end date
hospital <- hospital[3,"parameter"] # hospital name

#load file
if (str_detect(hospital,"(?i)Siem")) {  
	h_abbr <- "SR"
	path <- str_glue("data/SR/SR {format(end_date, '%Y')}")
	source("code/Load_file_SR.R", local = knitr::knit_global())
} else if (str_detect(hospital,"(?i)Bat")) {
	h_abbr <- "BB"
	path <- str_glue("data/BB/BB {format(end_date, '%Y')}")
	source("code/Load_file_BB.R",local = knitr::knit_global())
} else if (str_detect(hospital,"(?i)Kam")) {
	h_abbr <- "KC"
	path <- str_glue("data/KC/KC {format(end_date, '%Y')}")
	source("code/Load_file_KC.R",local = knitr::knit_global())
} else if (str_detect(hospital,"(?i)Tak")) {
	h_abbr <- "TK"
	path <- str_glue("data/TK/TK {format(end_date, '%Y')}")
	source("code/Load_file_TK.R",local = knitr::knit_global())
} else {"Check hospital name !!!"}






