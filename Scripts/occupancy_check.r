#
# Logistics
#
WD <- "~/Documents/TRES_IPM"
library(tidyverse)
library(cowplot)

setwd(WD)
raw <- read_csv("Data/nests.csv") %>%
    separate("Nest_Key", into=c("drop", "Nest_ID"), sep="-") %>%
    select(Nest_ID,
	  	   Year,
	  	   Eggs_Laid)

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
     geom_tile(aes(x=Year, y=Nest_ID, fill=!is.na(Eggs_Laid))) +
     scale_fill_manual(values=c("black", "white")) +
     guides(fill="none") +
     theme_bw() +
     theme(axis.text.y = element_blank(),
     	 axis.ticks.y = element_blank(),
     	 panel.grid = element_blank(),
     	 panel.border = element_blank())

ggsave(plot_occupancy, file="Output/Nestboxes/Plot_2_nest_occupancy.png", 
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

ggsave(plot_spatial, file="Output/Nestboxes/Plot_1_spatial_occupancy.png",
	  width=4, height=11)


mainOut <- readRDS("Output/ipm_out_endYear2011.rds")


boxOut <- mainOut %>%
	   filter(param=="phi.BOX")

lamOut <- mainOut %>%
	   filter(param=="lambda")

ts.boxOut <- ggplot(boxOut) +
		  geom_line(aes(x=year, y=mean)) +
		  geom_hline(aes(yintercept=mainOut[mainOut$param=="m.phi.BOX", "mean"]), linetype="dashed",
		    	     colour="black") +
		  geom_hline(aes(yintercept=mainOut[mainOut$param=="m.phi.BOX", "low"]), linetype="dashed",
		    	     colour="gray") +
		  geom_hline(aes(yintercept=mainOut[mainOut$param=="m.phi.BOX", "high"]), linetype="dashed",
		    	     colour="gray") +
		  geom_point(aes(x=year, y=mean)) +
		  geom_segment(aes(x=year, xend=year, y=low, yend=high)) +
	      scale_x_continuous(limits=c(1987, 2010),
		 	    	         breaks=seq(1987, 2010, by=2)) +
	      xlab("Year") +
	      ylab("App. box survival") +
	      theme(panel.background=element_rect(fill="white", colour="black"),
		        panel.grid=element_blank())

ts.lamOut <- ggplot(lamOut) +
		  geom_line(aes(x=year, y=mean)) +
		  geom_hline(aes(yintercept=mainOut[mainOut$param=="m.lambda", "mean"]), linetype="dashed",
		    	     colour="black") +
		  geom_hline(aes(yintercept=mainOut[mainOut$param=="m.lambda", "low"]), linetype="dashed",
		    	     colour="gray") +
		  geom_hline(aes(yintercept=mainOut[mainOut$param=="m.lambda", "high"]), linetype="dashed",
		    	     colour="gray") +
		  geom_hline(aes(yintercept=1), colour="lightgray") +
		  geom_point(aes(x=year, y=mean)) +
		  geom_segment(aes(x=year, xend=year, y=low, yend=high)) +
          scale_x_continuous(limits=c(1987, 2010),
		  	    	         breaks=seq(1987, 2010, by=2)) +
	      ylab("Pop. growth") +
	      theme(panel.background=element_rect(fill="white", colour="black"),
		        panel.grid=element_blank(),
		        axis.text.x=element_blank(),
		        axis.title.x=element_blank())
plot_timeseries <- plot_grid(plotlist=list(ts.lamOut, ts.boxOut),
		  				     nrow=2, ncol=1, align="hv")

ggsave(plot_timeseries, file="Output/Nestboxes/Plot_3_timeseries.png",
	   width=8, height=4)

boxLamCors <- mainOut %>%
		   filter(param=="lambda" | param=="phi.BOX") %>%
		   select(param, year, mean, low, high) %>%
		   pivot_wider(id_cols=year, names_from=param,
		   			   values_from=c(mean, low, high))

model <- lm(boxLamCors$mean_lambda ~ boxLamCors$mean_phi.BOX)
modelSummary <- summary(model)

plot_boxLamCor <- ggplot(boxLamCors) +
			   geom_point(aes(x=mean_phi.BOX, y=mean_lambda)) +
			   geom_segment(aes(x=mean_phi.BOX, xend=mean_phi.BOX,
			      				y=low_lambda, yend=high_lambda)) +
			   geom_segment(aes(x=low_phi.BOX, xend=high_phi.BOX, 
			   				y=mean_lambda, yend=mean_lambda)) +
			   geom_smooth(aes(x=mean_phi.BOX, y=mean_lambda), method="lm",
			   			   linetype="dashed", se=FALSE, colour="red") +
			   geom_text(aes(x=0.2, y=2,
			   			 label=paste0("Unadjusted Linear Regression\n",
			   			 			  "Mean pop. growth rate ~\n",
			   			   		      "\tMean app. box survival\n",
			   			  			  "R^2=", round(modelSummary$r.squared, 2),"\n",
			   			  			  "P=", round(modelSummary$coefficients[2,4], 2))),
			   		     hjust=0) +
			   xlab("App. box survival") +
			   ylab("Pop. growth rate") +
			   theme(panel.background=element_rect(fill="white", colour="black"))

ggsave(plot_boxLamCor, file="Output/Nestboxes/Plot_4_Lambda_PhiBOX_Cor.png",
	   width=8, height=5)

# See if summary contributions change
readContSummary <- function(endYear) {
	summary <- readRDS(paste0("Output/cont_summary_endYear", endYear, ".rds")) %>%
			mutate(End_Year = endYear)
	return(summary)
}

parameterNames <- c("phi.A.F" = "AdSurvF",
				    "phi.A.M" = "AdSurvM",
				    "phi.J.F" = "JvSurvF",
				    "phi.J.M" = "JvSurvM",
				    "cs" = "Clutch",
				    "hs" = "Hatch",
				    "fs" = "Fledge",
				    "omega.F" = "ImmF",
				    "omega.M" = "ImmM",
				    "prop.F" = "PropF",
				    "prop.M" = "PropM")

contSummaries <- map_df(1997:2011, readContSummary) %>%
			  group_by(End_Year) %>%
			  summarize_all(mean) %>%
			  pivot_longer(-End_Year, 
			  			   names_to="Parameter",
			  			   values_to="Contribution") %>%
			  arrange(End_Year, -Contribution) %>%
			  group_by(End_Year) %>%
			  mutate(Rank = rank(-Contribution)) %>%
			  mutate(Name = map_chr(Parameter, ~ parameterNames[.])) %>%
			  filter(Rank <= 5)

plot_contSummaries <- ggplot(contSummaries) +
				   geom_line(aes(x=End_Year, y=Rank, group=Name)) +
				   geom_label(aes(x=End_Year, y=Rank, label=Name),
				   			  size=2) +
				   scale_y_reverse() +
				   scale_x_continuous(limits=c(1997, 2011),
				       	              breaks=seq(1997, 2011, by=2)) +
				   xlab("Dataset end year") +
				   ylab("Rank overall LTRE cont.") +
    			   theme(panel.background=element_rect(fill="white", colour="black"),
    			         panel.grid=element_blank())
ggsave(plot_contSummaries, file="Output/Nestboxes/Plot_5_LTRE_Contributions.png",
	   width=9, height=3)


# Roughly coding box occupancy % from 
# Cox et al. 2020 Oecologia Fig. 1
roughCoxPops <- c(72
,84
,80
,90
,93
,125,
110, 
70
,100,
97
,83
,81
,95
,78
,60
,50
,58
,52
,59
,48
,49
,46
,53
,44
,30)

roughCoxData <- tibble(year=1987:2011,
					   Occupancy=roughCoxPops) %>%
			 mutate(nextOccupancy=lead(Occupancy)) %>%
			 mutate(roughCoxLambda = nextOccupancy / Occupancy)

coxComparison <- lamOut %>%
			  left_join(roughCoxData, by="year") %>%
			  select(year, ourLambda=mean, roughCoxLambda) %>%
			  mutate(diffLambda = ourLambda - roughCoxLambda)

insetPoints <- ggplot(coxComparison) +
			geom_abline(aes(intercept=0, slope=1), colour="gray") +
			geom_point(aes(x=ourLambda, y=roughCoxLambda, colour=year)) +
			scale_x_continuous(limits=c(0.5,1.5),breaks=1) +
			scale_y_continuous(limits=c(0.5,1.5),breaks=1) +
			xlab(expression("Kent"~lambda)) +
			ylab(expression("(Rough) Ontario"~lambda)) +
			guides(colour=guide_legend(title="Year")) +
			theme(panel.background=element_rect(fill="white",colour="black"),
				  axis.title.x=element_text(size=6),
				  axis.title.y=element_text(size=6),
				  legend.text=element_text(size=6),
				  legend.title=element_text(size=6),
				  legend.key=element_rect(fill="white", colour="white"),
				  legend.spacing.y=unit(0, "in"),
				  legend.key.size=unit(0, "in"),
				  legend.margin=margin(0,0,0,0),
				  legend.position=c(0.87, 0.2))

plot_coxComparisons <- ggplot(coxComparison) +
	                geom_hline(aes(yintercept=0), colour="gray") +
	                geom_line(aes(x=year, y=diffLambda)) +
	                geom_point(aes(x=year, y=diffLambda)) +
                    scale_x_continuous(limits=c(1987, 2010),
                        	           breaks=seq(1987, 2010, by=2)) +
                    xlab("Year") +
                    ylab("Difference in pop. growth rate\n(Kent - Ontario)") +
                    theme(panel.background=element_rect(fill="white", colour="black"),
	                      panel.grid=element_blank()) +
                    annotation_custom(ggplotGrob(insetPoints),
                    				  xmin=2003.5, xmax=2010.5,
                    				  ymin=-0.95, ymax=-0.33)

ggsave(plot_coxComparisons, file="Output/Nestboxes/Plot_6_CoxComparisons.png",
	   width=8, height=4)                