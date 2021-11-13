# load library----
library(tidyverse)
library(ggtext)
library(rio)
library(XLConnect)
library(janitor)
library(kableExtra)
library(AMR)

# Import dictionary----
## sheet dict
dic <- import("dic/dictionary.xlsx", sheet = "Dict") %>%
	mutate(container = trimws(str_to_lower(container)),
				 strength = trimws(str_to_lower(strength)))

## sheet hospital
hospital <- import("dic/dictionary.xlsx", sheet = "hospital") %>%
	clean_names()
start_date <- excel_numeric_to_date(as.numeric(as.character(hospital[1,"parameter"])),date_system = "modern") # start date
end_date <- excel_numeric_to_date(as.numeric(as.character(hospital[2,"parameter"])),date_system = "modern") # end date
hospital <- hospital[3,"parameter"] # hospital name

# path----
path <- case_when(str_detect(string = hospital, pattern = "[Ss]ie(.+)ap") == T ~ str_glue("data/SR/SR {format(end_date, '%Y')}"),
									str_detect(string = hospital, pattern = "[Bb]at(.+)ang") == T ~ str_glue("data/BB/BB {format(end_date, '%Y')}"),
									str_detect(string = hospital, pattern = "[Kk]am(.+)ham") == T ~ str_glue("data/KC/KC {format(end_date, '%Y')}"),
									str_detect(string = hospital, pattern = "[Tt]a(.+)o") == T ~ str_glue("data/TK/TK {format(end_date, '%Y')}"))



