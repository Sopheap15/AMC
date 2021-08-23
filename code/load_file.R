library(tidyverse)
library(rio)
library(janitor)
library(AMR)

dic <- import("dic/dictionary.xlsx", sheet = "Dict")

patient_day <- import("dic/dictionary.xlsx", sheet = "Patient_day") %>% 
	filter(row_number() <= n() - 1)


# import data function
read_file <- function(data, sheet, m){
	import(data, skip = 7, sheet = sheet) %>% 
		clean_names() %>% 
		remove_empty() %>% 
		filter(row_number() <= n() - 2) %>% 
		select(commodity_name, line_total, form, strength) %>% 
		mutate(month = rep(m, length(commodity_name))) 
}

# Import data
# Jan----
Jan <- read_file(data = "data/Jan-Sep 2019, monthlyconsumption extracted from Hosdid.xls", 
					sheet = "Jan",
					m = "Jan")
# Feb----
Feb <- read_file(data = "data/Jan-Sep 2019, monthlyconsumption extracted from Hosdid.xls", 
								 sheet = "Feb",
								 m = "Feb")
# Mar----
Mar <- read_file(data = "data/Jan-Sep 2019, monthlyconsumption extracted from Hosdid.xls", 
								 sheet = "March",
								 m = "Mar")
# Apr----
Apr <- read_file(data = "data/Jan-Sep 2019, monthlyconsumption extracted from Hosdid.xls", 
								 sheet = "April",
								 m = "Apr")
# May----
May <- read_file(data = "data/Jan-Sep 2019, monthlyconsumption extracted from Hosdid.xls", 
								 sheet = "May",
								 m = "May")
# June-----
Jun <- read_file(data = "data/Jan-Sep 2019, monthlyconsumption extracted from Hosdid.xls", 
								 sheet = "June",
								 m = "Jun")
# July----
Jul <- read_file(data = "data/Jan-Sep 2019, monthlyconsumption extracted from Hosdid.xls", 
					sheet = "July",
					m = "Jul")
# Aug----
Aug <- read_file(data = "data/Jan-Sep 2019, monthlyconsumption extracted from Hosdid.xls", 
					sheet = "Auguast",
					m = "Aug")
# Sep----
Sep <- read_file(data = "data/Jan-Sep 2019, monthlyconsumption extracted from Hosdid.xls", 
								 sheet = "September",
								 m = "Sep")
# Oct----
Oct <- read_file(data = "data/octoberrptmonthlyconsumption.xls", 
								 sheet = "Sheet1",
								 m = "Oct")
# Nov----
Nov <- read_file(data = "data/Nov.xlsx", 
								 sheet = "Sheet1",
								 m = "Nov")
# Dec----

Dec <- read_file(data = "data/decemberrptmonthlyconsumption.xls", 
								 sheet = "Sheet1",
								 m = "Dec")

# Binding row-----
data <- bind_rows(Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec) %>% 
	# distinct(commodity_name, form, strength, month, .keep_all = TRUE) %>%
	filter(commodity_name %in% dic$commodity_name) 

data <- left_join(data, dic) 

data %>% 
	mutate(gram = as.numeric(gram) * line_total, 
				 ddd = round(as.numeric(ddd) * gram)) %>% 
	select(month, Abbr, ddd, monitoring) %>% 
	mutate(month = factor(month, levels = month.abb)) %>%
	group_by(month, Abbr, monitoring) %>% 
	summarise(ddd = sum(ddd, na.rm = T)) %>% 
	pivot_wider(names_from = month, values_from = ddd) %>% view()
	
	
	
	select(commodity_name, Abbr, -form, monitoring, strength, month, ddd) %>% 
	pivot_wider(names_from = month, values_from = ddd) %>% view()

a %>% 
	select(commodity_name, Abbr, form, monitoring, strength, month, ddd) %>% 
	pivot_wider(names_from = month, values_from = ddd) %>% view()
	arrange(commodity_name) %>%
		
data %>% 
	pivot_wider(names_from = month, values_from = line_total) %>%
	arrange(commodity_name) %>% view()
	adorn_totals("col", name = "Total_unit") %>%
	mutate(Total_gram = as.numeric(gram) * Total_unit,
				 Gram_ddd = as.numeric(ddd) * Total_gram,
				 ddd_1000 = Total_gram/sum(patient_day$Patient_day)) %>% 
	adorn_totals("row") %>% 
	export("SR data 2019.xlsx",overwrite = T)





data %>% 
	pivot_longer(cols = c(Jan:Dec), names_to = "month", values_to = "quantity") %>%
	filter(!is.na(monitoring)) %>% 
	group_by(monitoring, month) %>% 
	summarise(n = sum(quantity, na.rm = T)) %>% 
	arrange(factor(month,levels = month.abb),monitoring) %>% 
	ggplot(aes(month, n)) + geom_line(aes(group = monitoring, color = monitoring))




list.files("data1", pattern = ".xls", full.names = T) %>% 
	purrr::set_names(stringr::str_remove(basename(.),'.xls$')) %>%
	purrr::iwalk(function(x, i) assign(i, readxl::read_excel(x, skip = 7), .GlobalEnv)) 

ls()


data %>% 
	filter(!is.na(monitoring)) %>% 
	group_by(monitoring, month) %>% 
	summarise(n = sum(line_total)) %>% 
	group_by(month) %>% 
	mutate(pro = round_half_up(n/sum(n)*100)) %>% 
	ggplot(aes(month, pro, fill = monitoring)) +
	geom_bar(stat = "identity") +
	geom_text(aes(label = pro), position = position_stack(vjust = 0.5)) +
	labs(x = "Month", y = "Proportion (%)", title = "WHO AWaRe classification") +
	guides(fill = guide_legend("WHO AWaRe")) +
	theme_bw() +
	theme(plot.title = element_text(color = "black", face = "bold",hjust = 0.5))




data %>% 
mutate(Antibiotic = paste0(Antibiotic," (", route,")")) %>% view()

	group_by(Antibiotic, route) %>% 
	summarise(ddd_1000 = round(sum(ddd_1000),2)) %>% view() 
	
	
	
	ggplot(aes(reorder(Antibiotic, ddd_1000), ddd_1000, fill = Antibiotic)) + 
	geom_bar(stat = "identity") +
	geom_text(aes(label = ddd_1000), hjust = -0.2, size = 3) +
	coord_flip() +
	scale_y_continuous(expand = expansion(mult = c(0, .2))) +
	labs(x = "Antimicrobial",
			 y = "DDDs/1000 patient days", 
			 title = "Defined Daily Doses") +
	theme_classic() +
	theme(legend.position = "non",
				plot.title = element_text(hjust = 0.5, colour = "black", face = "bold" ),
				axis.text = element_text(colour = "black", size = 9))

	
	
	
	data %>% 
		filter(!is.na(monitoring)) %>% 
		group_by(monitoring) %>% 
		summarise(sum(ddd_1000)) %>% view()
	
	
	










