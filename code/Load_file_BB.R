# Import dictionary ----
patient_day <- import(str_glue("{path}/Patient day.xlsx"), sheet = "Patient_day") %>% 
	clean_names()

t_patient_day <- sum(patient_day$patient_day, na.rm = T)

#import data -----
data <- list.files(str_glue("{path}/All destination"), 
														pattern = '.xls$', full.names = T) %>% 
	purrr::map_dfr(~read_file(., sheet = "Sheet1", st_r = 6), bind_rows) %>% 
	filter(str_detect(code,"(?i)nc") != T) %>% 
	select(-code, line_total_all_des = outgoing) %>% 
	inner_join(dic,
						by = c("abbr" = "abbr", 
									 "container" = "container",
									 "strength" = "strength"
									 )) %>%
	distinct(abbr, container, month, strength, .keep_all = T)

# covid ward
if (dir.exists(str_glue("{path}/Covid"))) {
	data_covid <- list.files(str_glue("{path}/Covid"), 
						 pattern = '.xls$', full.names = T) %>% 
		purrr::map_dfr(~read_file(., sheet = "Sheet1", st_r = 6), bind_rows) %>%
		filter(str_detect(code,"(?i)nc") != T) %>% 
		select(-code, line_total_covid = outgoing) %>% 
		inner_join(dic,
							 by = c("abbr" = "abbr", 
							 			 "container" = "container",
							 			 "strength" = "strength"
							 )) %>%
		distinct(abbr, container, month, strength, .keep_all = T)
	
	# join 2 data including Covid
	data <- purrr::reduce(list(data, data_covid), left_join) %>% 
		mutate_at(vars( starts_with("line_total")), 
							~replace(., is.na(.), 0)) %>% 
		mutate(line_total = line_total_all_des - line_total_covid,
					 month = str_to_title(month))
}else{
	data <- data %>% 
		rename(line_total = line_total_all_des)
}


# Joining data with dictionary and patient ----
data <- data %>% 
	left_join(patient_day) %>% 
	cal_ddd()

# Calculate quarterly ----
quarter <- amc_q(data)