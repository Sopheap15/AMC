# Import patient day----
patient_day <- import(str_glue("{path}/Patient day.xlsx"), sheet = "Patient_day") %>% 
	clean_names()
t_patient_day <- sum(patient_day$patient_day, na.rm = T)

# Combine read file----
data_all_destination <- list.files(str_glue("{path}/All destination"), pattern = '.xls$', full.names = T) %>%
	purrr::map(~read_file(.)) %>% reduce(., bind_rows) %>%
	rename(line_total_all_des = line_total) %>% 
	ab_cname() %>%
	mutate(commodity_name = str_to_lower(commodity_name),
				 container = trimws(str_to_lower(container)),
				 strength = trimws(str_to_lower(strength)),
				 month = trimws(str_to_title(month))) %>%  
	inner_join(dic,
						 by = c("abbr" = "abbr", 
						 			 "container" = "container",
						 			 "strength" = "strength"))

data_OPD <- list.files(str_glue("{path}/OPD"), pattern = '.xls$', full.names = T) %>%
	purrr::map(~read_file(.)) %>% reduce(., bind_rows) %>% 
	ab_cname() %>%
	rename(line_total_OPD = line_total) %>% 
	mutate(commodity_name = str_to_lower(commodity_name),
				 container = trimws(str_to_lower(container)),
				 strength = trimws(str_to_lower(strength)),
				 month = trimws(str_to_title(month))) %>%  
	inner_join(dic,
						 by = c("abbr" = "abbr", 
						 			 "container" = "container",
						 			 "strength" = "strength"))

data <- left_join(data_all_destination, data_OPD) %>% 
	mutate_at(vars( starts_with("line_total")), 
						~replace(., is.na(.), 0)) %>% 
	mutate(line_total = line_total_all_des - line_total_OPD, 
				 month = str_to_title(month)) %>%
	left_join(patient_day) %>% 
	mutate(antibiotic = ab_name(abbr),
				 antibiotic = paste0(antibiotic, " (", route,")"),
				 month = factor(month, levels = month.abb),
				 gram_ddd = as.numeric(line_total) * as.numeric(gram) / as.numeric(ddd)) %>%
	group_by(antibiotic) %>% 
	mutate(ddd_1000_pyear = sum(gram_ddd) * 1000 / t_patient_day)


# Calculate quarterly----
quarter <- data %>%
	mutate(Q = paste0("Q",lubridate::quarter(match(month, month.abb)))) %>%
	group_by(Q) %>% 
	mutate(Qpatient_day = sum(unique(patient_day), na.rm = T)) %>% 
	group_by(antibiotic, Q, monitoring) %>% 
	summarise(Qgram_ddd_1000 = round(sum(gram_ddd) * 1000 / Qpatient_day, 1)) %>%
	distinct(antibiotic, Q, Qgram_ddd_1000) %>% 
	group_by(Q, monitoring) %>% 
	summarise(total = sum(Qgram_ddd_1000)) %>% 
	group_by(Q) %>% 
	mutate(prop = round_half_up(total*100/sum(total)))


















