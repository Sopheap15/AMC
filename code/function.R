# function to capture antibiotic
ab_cname <- function(data){
data %>% 
	mutate(abbr = case_when(
		#Penicillin
		str_detect(str_to_lower(commodity_name), "^pen(\\w+)?lin[e]?(.+)?g") ~ "PEN",
		str_detect(str_to_lower(commodity_name), "^(phen|meth)(.+)?pen(\\w+)?lin[e]?") ~ "PHN",
		str_detect(str_to_lower(commodity_name), "^ben(.+)?pen(\\w+)?lin[e]?") ~ "BNB",
		str_detect(str_to_lower(commodity_name), "^ben(.+)?phe(.+)?pen(\\w+)?lin[e]?") ~ "BNP",
		str_detect(str_to_lower(commodity_name), "^amo(\\w+)?lin[e]?(?!(.+)?cla*)") ~ "AMX", 
		str_detect(str_to_lower(commodity_name), "^amo(.+)?cla*") ~ "AMC",
		str_detect(str_to_lower(commodity_name), "^amp(\\w+)?lin[e]?") ~ "AMP",
		# Cephalosporine
		str_detect(str_to_lower(commodity_name), "^cefa(\\w+)?lin[e]?") ~ "CZO",
		str_detect(str_to_lower(commodity_name), "^cefa(\\w+)?lor[e]?") ~ "CEC",
		str_detect(str_to_lower(commodity_name), "^cefi(\\w+)?[czx]im[e]?") ~ "CFM",
		str_detect(str_to_lower(commodity_name), "^cefo(\\w+)?[czx]im[e]?(?!(.+)?cla*)") ~ "CTX",
		str_detect(str_to_lower(commodity_name), "^cefo(.+)?cla*") ~ "CTC",
		str_detect(str_to_lower(commodity_name), "^cefu(\\w+)?[czx]im[e]?") ~ "CXM",
		str_detect(str_to_lower(commodity_name), "^ceft(\\w+)?[czx]on[e]?") ~ "CRO",
		str_detect(str_to_lower(commodity_name), "^ceft(\\w+)?dim[e]?(?!(.+)?cla*)") ~ "CAZ",
		str_detect(str_to_lower(commodity_name), "^ceft(.+)?cla*") ~ "CCV",
		str_detect(str_to_lower(commodity_name), "^cefe(\\w+)?[czx]im[e]?") ~ "FEP",
		# Carbapenem
		str_detect(str_to_lower(commodity_name), "mer(\\w+)?nem[e]?") ~ "MEM",
		str_detect(str_to_lower(commodity_name), "^imi(\\w+)?nem[e]?") ~ "IMP",
		# Aminoglycoside
		str_detect(str_to_lower(commodity_name), "^gen(\\w+)?[czx]in[e]?") ~ "GEN",
		str_detect(str_to_lower(commodity_name), "^ami(\\w+)?[czx]in[e]?") ~ "AMK",
		# Quinolone
		str_detect(str_to_lower(commodity_name), "^nal(\\w+)?a[czx]id[e]?") ~ "NA",
		str_detect(str_to_lower(commodity_name), "^cipr(\\w+)?[czx]in[e]?") ~ "CIP",
		str_detect(str_to_lower(commodity_name), "^ofl(\\w+)?[czx]in[e]?") ~ "OFX",
		str_detect(str_to_lower(commodity_name), "^lev(\\w+)?flo(\\w+)?[czx]in[e]?") ~ "LVX",
		str_detect(str_to_lower(commodity_name), "^mox(\\w+)?[czx]in[e]?") ~ "MFX",
		
		#Other
		str_detect(str_to_lower(commodity_name), "^ery(\\w+)?[czx]in[e]?") ~ "ERY",
		str_detect(str_to_lower(commodity_name), "^azi(\\w+)?[czx]in[e]?") ~ "AZM",
		str_detect(str_to_lower(commodity_name), "^clox(\\w+)?lin[e]?") ~ "CLO",
		str_detect(str_to_lower(commodity_name), "^clar(\\w+)?[czx]in[e]?") ~ "CLR",
		str_detect(str_to_lower(commodity_name), "^c[hl]+(\\w+)?col[e]?") ~ "CHL",
		str_detect(str_to_lower(commodity_name), "^cotr(\\w+)?[czx]ol[e]?") ~ "SXT",
		str_detect(str_to_lower(commodity_name), "(sulf(.+)?trim(\\w+)|trim(.+)?sulfa(\\w+))[czx]ol[e]?") ~ "SXT",
		str_detect(str_to_lower(commodity_name), "^vanc(\\w+)?[czx]in[e]?") ~ "VAN",
		str_detect(str_to_lower(commodity_name), "^dox[iy](\\w+)?lin[e]?") ~ "DOX",
		str_detect(str_to_lower(commodity_name), "^tetr(\\w+)?lin[e]?") ~ "TCY",
		str_detect(str_to_lower(commodity_name), "^metr(\\w+)?[czx]ol[e]?") ~ "MTR",
		str_detect(str_to_lower(commodity_name), "^nitr(\\w+)?oin[e]?") ~ "NIT",
		# anti-fungi 
		str_detect(str_to_lower(commodity_name), "^fluc(\\w+)?[czx]ol[e]?") ~ "FLU",
		str_detect(str_to_lower(commodity_name), "^amph(\\w+)?[czx]in[e]?(.+)?[bB]") ~ "AMB",
		str_detect(str_to_lower(commodity_name), "^n[iy]st(\\w+)?in[e]?") ~ "NYS",	
		TRUE ~ "F"),
		container = str_extract(str_to_lower(commodity_name), "\\d+(\\s)?ml")) %>% 
		#container = str_remove(container, "ml") 
	filter(abbr != "F", 
				 str_detect(commodity_name,"eye|di[sck]") != TRUE) 
}

# Read file function
read_file <- function(file, sheet, st_r){
	file %>% 
		readWorksheetFromFile(sheet = {{sheet}}, startRow = {{st_r}}) %>% 
		clean_names() %>% 
		remove_empty(c("rows", "cols")) %>% 
		filter(row_number() <= n()) %>% 
		select(commodity_name, any_of(c("code","line_total", "outgoing")), form, strength) %>% 
		mutate(month = rep(str_extract(str_to_lower(file),
																	 "jan|feb|mar|apr|may|jun|jul|aug|sep|oct|nov|dec"), 
											 length(commodity_name))) %>% 
		ab_cname() %>% 
		mutate(commodity_name = str_to_lower(commodity_name),
					 container = trimws(str_to_lower(container)),
					 strength = trimws(str_to_lower(strength)),
					 month = trimws(str_to_title(month)))
	
}  

# Joining data with dictionary to calculate ddd ----
cal_ddd <- function(data) {
	data %>% 
		mutate(antibiotic = ab_name(abbr),
					 antibiotic = paste0(antibiotic, " (", route,")"),
					 month = factor(month, levels = month.abb),
					 gram_ddd = as.numeric(line_total) * as.numeric(gram) / as.numeric(ddd)) %>%
		group_by(antibiotic) %>% 
		mutate(ddd_1000_pyear = sum(gram_ddd) * 1000 / t_patient_day)
}

# AMC Quarterly calculation
amc_q <- function(data){
	data %>%
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
}





