#
# Logistics
#
WD <- "~/Documents/TRES_IPM"
library(tidyverse)

setwd(WD)
data <- read_csv("Data/nest_records.csv") %>%
	 separate("Nest Key", into=c("drop", "Nest_ID"), sep="-") %>%
	 select(Nest_ID,
	 		Year = Year,
	 		Clutch_Size = "Eggs Laid")

occupiedYears <- data %>%
			  group_by(Nest_ID) %>%
			  tally() 

nestOrder <- occupiedYears %>%
		  arrange(-n) %>%
		  pull(Nest_ID)

data <- mutate(data, Nest_ID = factor(Nest_ID, levels=nestOrder))

plot <- ggplot(data) +
     geom_tile(aes(x=Year, y=Nest_ID, fill=!is.na(Clutch_Size))) +
     scale_fill_manual(values=c("black", "white")) +
     guides(fill="none") +
     theme_bw() +
     theme(axis.text.y = element_blank(),
     	   axis.ticks.y = element_blank(),
     	   panel.grid = element_blank(),
     	   panel.border = element_blank())




ggsave(plot, file="Output/Plot_nest_occupancy.png", 
	   width=8, height=8) 