# Import patient day ----
patient_day <- import(str_glue("{path}/Patient day.xlsx"), sheet = "Patient_day") %>% 
	clean_names()
t_patient_day <- sum(patient_day$patient_day, na.rm = T)

# Import data -----
data <- list.files(path, pattern = '.xls$', full.names = T) %>% 
	purrr::map_dfr(~read_file(., sheet = "Sheet1", st_r = 8), bind_rows) %>% 
	inner_join(dic,
						 by = c("abbr" = "abbr", 
						 			 "container" = "container",
						 			 "strength" = "strength"))


# Joining data with dictionary and patient ----
data <- data %>% 
	left_join(patient_day) %>% 
	cal_ddd()


# Calculate quarterly ----
quarter <- amc_q(data)
	




