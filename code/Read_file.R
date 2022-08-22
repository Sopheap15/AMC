# Read file function----

## Battambang
read_file_BB <- function(x){
	readWorksheetFromFile(x, sheet = "Sheet1", startRow = 6) %>% 
		clean_names() %>% 
		remove_empty(c("rows", "cols")) %>% 
		filter(row_number() <= n() - 3, str_detect(tolower(code),"nc") != T) %>% 
		select(commodity_name, line_total = outgoing, form, strength) %>% 
		mutate(month = rep(str_extract(str_to_lower(x),"jan|feb|mar|apr|may|jun|jul|aug|sep|oct|nov|dec"), length(commodity_name))) 
}

## Kampong Cham
read_file_KC <- function(x){
	readWorksheetFromFile(x, sheet = "Sheet1", startRow = 2) %>% 
		clean_names() %>% 
		remove_empty(c("rows", "cols")) %>% 
		filter(row_number() <= n() - 2) %>% 
		select(commodity_name, line_total, form, strength) %>% 
		mutate(month = rep(str_extract(str_to_lower(x),"jan|feb|mar|apr|may|jun|jul|aug|sep|oct|nov|dec"), length(commodity_name))) 
}

## Siem Reap and Takeo
read_file <- function(x){
	readWorksheetFromFile(x, sheet = "Sheet1", startRow = 8) %>% 
		clean_names() %>% 
		remove_empty(c("rows", "cols")) %>% 
		filter(row_number() <= n() - 2) %>% 
		select(commodity_name, line_total, form, strength) %>% 
		mutate(month = rep(str_extract(str_to_lower(x),"jan|feb|mar|apr|may|jun|jul|aug|sep|oct|nov|dec"), length(commodity_name))) 
}


# function to capture antibiotic----
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
		#,container = str_remove(container, "ml") 
	filter(abbr != "F", 
				 str_detect(commodity_name,"eye|di[sck]") != TRUE) 
}





