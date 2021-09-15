# load library----
library(tidyverse)
library(ggtext)
library(rio)
library(janitor)
library(kableExtra)
library(AMR)

# create function to import and clean data----
read_file <- function(data, sheet="Sheet", month){
	import(data, skip = 7, sheet = sheet) %>% 
		clean_names() %>% 
		remove_empty() %>% 
		filter(row_number() <= n() - 2) %>% 
		select(commodity_name, line_total, form, strength) %>% 
		mutate(month = rep(month, length(commodity_name))) 
}


# Import dictionary----
dic <- import("dic/dictionary.xlsx", sheet = "Dict")
patient_day <- import("dic/dictionary.xlsx", sheet = "Patient_day") %>%
	remove_empty("cols") %>% 
	filter(row_number() <= n() - 1) %>% 
	clean_names()

start_date <- excel_numeric_to_date(as.numeric(as.character(patient_day[1,"parameter"])),date_system = "modern") # start date
end_date <- excel_numeric_to_date(as.numeric(as.character(patient_day[2,"parameter"])),date_system = "modern") # end date

hospital <- patient_day[3,"parameter"] # hospital name

total_patient <- sum(patient_day$patient_day, na.rm = T) # Total patient for a whole year


# Import data

#import data-----
# Jan
Jan <- read_file(data = "data/Jan-Sep 2019, monthlyconsumption extracted from Hosdid.xls", 
								 sheet = "Jan",
								 month = "Jan")
# Feb
Feb <- read_file(data = "data/Jan-Sep 2019, monthlyconsumption extracted from Hosdid.xls", 
								 sheet = "Feb",
								 month = "Feb")
# Mar
Mar <- read_file(data = "data/Jan-Sep 2019, monthlyconsumption extracted from Hosdid.xls", 
								 sheet = "March",
								 month = "Mar")
# Apr
Apr <- read_file(data = "data/Jan-Sep 2019, monthlyconsumption extracted from Hosdid.xls", 
								 sheet = "April",
								 month = "Apr")
# May
May <- read_file(data = "data/Jan-Sep 2019, monthlyconsumption extracted from Hosdid.xls", 
								 sheet = "May",
								 month = "May")
# June
Jun <- read_file(data = "data/Jan-Sep 2019, monthlyconsumption extracted from Hosdid.xls", 
								 sheet = "June",
								 month = "Jun")
# July
Jul <- read_file(data = "data/Jan-Sep 2019, monthlyconsumption extracted from Hosdid.xls", 
								 sheet = "July",
								 month = "Jul")
# Aug
Aug <- read_file(data = "data/Jan-Sep 2019, monthlyconsumption extracted from Hosdid.xls", 
								 sheet = "Auguast",
								 month = "Aug")
# Sep
Sep <- read_file(data = "data/Jan-Sep 2019, monthlyconsumption extracted from Hosdid.xls", 
								 sheet = "September",
								 month = "Sep")
# Oct
Oct <- read_file(data = "data/octoberrptmonthlyconsumption.xls", 
								 sheet = "Sheet1",
								 month = "Oct")
# Nov
Nov <- read_file(data = "data/Nov.xlsx", 
								 sheet = "Sheet1",
								 month = "Nov")
# Dec
Dec <- read_file(data = "data/decemberrptmonthlyconsumption.xls", 
								 sheet = "Sheet1",
								 month = "Dec")


# Combine data monthly----
data <- bind_rows(Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec) %>% 
	#distinct(commodity_name, form, strength, month, .keep_all = TRUE) %>%
	filter(commodity_name %in% dic$commodity_name) 


# Joining data with dictionary and patient----
data <- left_join(data, dic) %>% 
	left_join(patient_day) %>% 
	mutate(Antibiotic = ab_name(Abbr),
				 Antibiotic = paste0(Antibiotic, " (", route,")"),
				 month = factor(month, levels = month.abb),
				 gram_ddd = as.numeric(line_total) * as.numeric(gram) / as.numeric(ddd)) %>%
	group_by(Antibiotic) %>% 
	mutate(ddd_1000_pyear = sum(gram_ddd) * 1000 / total_patient)

# Calculate quarterly----
quarter <- data %>%
	mutate(Q = paste0("Q",lubridate::quarter(match(month, month.abb)))) %>%
	group_by(Q) %>% 
	mutate(Qpatient_day = sum(unique(patient_day))) %>% 
	group_by(Antibiotic, Q, monitoring) %>% 
	summarise(Qgram_ddd_1000 = round(sum(gram_ddd) * 1000 / Qpatient_day, 1)) %>%
	distinct(Antibiotic, Q, Qgram_ddd_1000) %>% 
	group_by(Q, monitoring) %>% 
	summarise(total = sum(Qgram_ddd_1000)) %>% 
	group_by(Q) %>% 
	mutate(prop = round_half_up(total*100/sum(total)))



	




