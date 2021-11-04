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
