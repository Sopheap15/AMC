# Import dictionary----
patient_day <- import("data/BB/Patient day.xlsx", sheet = "Patient_day") %>% 
	clean_names()

#import data-----
data <- list.files("data/BB", pattern = '.xls$', full.names = T) %>%
	purrr::map(~read_file_BB(.)) %>% reduce(., bind_rows) %>%
	ab_cname() %>% 
	mutate(commodity_name = str_to_lower(commodity_name),
				 container = trimws(str_to_lower(container)),
				 strength = trimws(str_to_lower(strength)),
				 month = trimws(str_to_title(month))) %>% 
	inner_join(dic,
						by = c("abbr" = "abbr", 
									 "container" = "container",
									 "strength" = "strength"))


# Joining data with dictionary and patient----
data <- left_join(data, patient_day) %>% 
	mutate(antibiotic = ab_name(abbr),
				 antibiotic = paste0(antibiotic, " (", route,")"),
				 month = factor(month, levels = month.abb),
				 gram_ddd = as.numeric(line_total) * as.numeric(gram) / as.numeric(ddd)) %>%
	group_by(antibiotic) %>% 
	mutate(ddd_1000_pyear = sum(gram_ddd) * 1000 / sum(patient_day, na.rm = T))

# Calculate quarterly----
quarter <- data %>%
	mutate(Q = paste0("Q",lubridate::quarter(match(month, month.abb)))) %>%
	group_by(Q) %>% 
	mutate(Qpatient_day = sum(unique(patient_day))) %>% 
	group_by(antibiotic, Q, monitoring) %>% 
	summarise(Qgram_ddd_1000 = round(sum(gram_ddd) * 1000 / Qpatient_day, 1)) %>%
	distinct(antibiotic, Q, Qgram_ddd_1000) %>% 
	group_by(Q, monitoring) %>% 
	summarise(total = sum(Qgram_ddd_1000)) %>% 
	group_by(Q) %>% 
	mutate(prop = round_half_up(total*100/sum(total)))
