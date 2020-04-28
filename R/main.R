library(taxize)
library(tidyverse)

amph <- read.csv("data/caatinga_amphibia.csv", h = T)

src <- gnr_datasources()

result <- as.character(amph$Species) %>% 
	gnr_resolve(data_source_ids = c(3))

sp_y <- result %>% 
	select(user_supplied_name, matched_name) %>% 
	mutate(new_names = str_extract(matched_name, "\\w*\\ \\w*"),
		   year = str_extract(matched_name, "\\d\\d\\d\\d"),
		   new_names = str_replace(new_names, "Boulenger", "galeata"),
		   year = ifelse(new_names == "Nyctimantis galeata", 2012, year),
		   year = as.Date(year, format = "%Y")) %>% 
	select(new_names, year) %>% 
	group_by(year) %>% 
	tally() %>% 
	arrange(year) %>% 
	mutate(sum = cumsum(n))

ggplot(sp_y, aes(x = year, group = 1)) +
	geom_line(aes(y = sum), size = 1.2, colour = "#435CD2") +
	geom_point(aes(y = sum), size = 2, colour = "#435CD2") +
	geom_col(aes(y = n), size = 2, colour = "#7648D6") +
	scale_x_date(breaks = "10 years", date_breaks = "25 years", date_labels = "%Y") +
	labs(x = "Ano de Descrição", y = "Espécies de Anfíbios Descritas") +
	ggtitle("Espécies de Anfíbios Endêmicos da Caatinga") +
	theme_minimal() +
	theme(axis.title = element_text(size = 16),
		  axis.text = element_text(size = 12))

