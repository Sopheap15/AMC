# Import patient day ----
patient_day <- import(str_glue("{path}/Patient day.xlsx"), sheet = "Patient_day") %>% 
	clean_names()
t_patient_day <- sum(patient_day$patient_day, na.rm = T)

# Combine read file ----
data_all_destination <- list.files(str_glue("{path}/All destination"), 
																	 pattern = '.xls$', 
																	 full.names = T) %>%
	purrr::map_dfr(~read_file(., sheet = "Sheet1", st_r = 8), bind_rows) %>% 
	rename(line_total_all_des = line_total) %>% 
	inner_join(dic,
						 by = c("abbr" = "abbr", 
						 			 "container" = "container",
						 			 "strength" = "strength"))

data_OPD <- list.files(str_glue("{path}/OPD"), 
											 pattern = '.xls$',
											 full.names = T) %>%
	purrr::map_dfr(~read_file(., sheet = "Sheet1", st_r = 8), bind_rows) %>% 
	rename(line_total_OPD = line_total) %>% 
	inner_join(dic,
						 by = c("abbr" = "abbr", 
						 			 "container" = "container",
						 			 "strength" = "strength"))

if (dir.exists(str_glue("{path}/Covid"))) {
	
	data_covid <- list.files(str_glue("{path}/Covid"), 
													 pattern = '.xls$', 
													 full.names = T) %>%
		purrr::map_dfr(~read_file(., sheet = "Sheet1", st_r = 8), bind_rows) %>% 
		rename(line_total_covid = line_total) %>% 
		inner_join(dic,
							 by = c("abbr" = "abbr", 
							 			 "container" = "container",
							 			 "strength" = "strength"))
	
	# join 3 data including Covid
	data <- purrr::reduce(list(data_all_destination, data_OPD, data_covid), left_join) %>% 
		mutate_at(vars( starts_with("line_total")), ~replace(., is.na(.), 0)) %>% 
		mutate(line_total = line_total_all_des - line_total_OPD - line_total_covid,
					 month = str_to_title(month))
} else{
	# join 2 data
	data <- purrr::reduce(list(data_all_destination, data_OPD), left_join) %>% 
		mutate_at(vars( starts_with("line_total")), ~replace(., is.na(.), 0)) %>% 
		mutate(line_total = line_total_all_des - line_total_OPD,
					 month = str_to_title(month))
	
}

# Joining data with dictionary and patient ----
	data <- data %>% 
	left_join(patient_day) %>% 
	cal_ddd()

# Calculate quarterly ----
quarter <- amc_q(data)
















