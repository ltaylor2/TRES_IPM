#
# Logistics
#
WD <- "~/Documents/TRES_IPM"
library(tidyverse)

setwd(WD)
raw <- read_csv("Data/nest_records.csv") %>%
    separate("Nest Key", into=c("drop", "Nest_ID"), sep="-") %>%
    select(Nest_ID,
	  	 Year = Year,
	 	 Clutch_Size = "Eggs Laid")

occupiedYears <- raw %>%
			  group_by(Nest_ID) %>%
			  tally() 

nestOrder <- occupiedYears %>%
		  arrange(-n) %>%
		  pull(Nest_ID)

clean <- raw %>%
	 mutate(Grid_Letter = map_chr(Nest_ID, ~ substr(., 1, 1)),
	 	   Grid_Number = map_dbl(Nest_ID, ~ as.numeric(substr(., 2, 3)))) %>%
	 mutate(Nest_ID = factor(Nest_ID, level=nestOrder))


plot_occupancy <- ggplot(clean) +
     geom_tile(aes(x=Year, y=Nest_ID, fill=!is.na(Clutch_Size))) +
     scale_fill_manual(values=c("black", "white")) +
     guides(fill="none") +
     theme_bw() +
     theme(axis.text.y = element_blank(),
     	 axis.ticks.y = element_blank(),
     	 panel.grid = element_blank(),
     	 panel.border = element_blank())

ggsave(plot_occupancy, file="Output/Plot_nest_occupancy.png", 
	   width=6, height=8) 

spatial_tallies <- clean %>%
			 group_by(Grid_Number, Grid_Letter, Year) %>%
			 tally()

plot_spatial <- ggplot(spatial_tallies) +
		   geom_tile(aes(x=Grid_Letter, y=Grid_Number, fill=n)) +
		   scale_fill_gradient(low="gray", high="black") +
		   scale_y_reverse() +
		   facet_wrap(facets = vars(Year)) +
		   guides(fill="none") +
		   theme(axis.text.x = element_blank(),
		   	    axis.title.x = element_blank(),
		   	    axis.ticks.x = element_blank(),
		   	    panel.background = element_blank(),
		   	    axis.text.y = element_blank(),
		   	    axis.title.y = element_blank(),
		   	    axis.ticks.y = element_blank(),
		   	    strip.background = element_rect(colour="black", size=2, fill="white"))

ggsave(plot_spatial, file="Output/Plot_spatial_occupancy.png",
	  width=4, height=11)