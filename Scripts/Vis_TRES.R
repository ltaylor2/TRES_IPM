# ------------------
# Analysis and visualization script for TRES IPM
# Analyses: 
#     goodness-of-fit for CJS and Productivity parameters
#
# Figures (saved PNG and PDF to Output/)
#     Figure 1 -- Study Site and Inset (to be combined separately)
#     Figure 3 -- Total population size and no. immigrants, by year
#     Figure 4 -- Vital rate estimates by year
#     Figure 5 -- Summary LTRE interval contributions
#     Figure 6 -- Adult CJS goodness-of-fit, by year
#     Figure 7 -- All LTRE interval contributions
#
# OUTPUT
#     gof -- posterior predictive check results
#
# LT 7/29/18
# ------------------

# goodness-of-fit (Bayesian p-values) -------------------------------------
bayes_gof <- function(org, new, name) {
  fit <- data_frame(new=new, org=org) %>%
          mutate(diff=new-org)
  
  bP <- dplyr::count(fit, diff>0)$n[2] / sum(dplyr::count(fit, diff>0)$n)
  if (is.na(bP)) { bP <- 0 }
  return(data.frame(name=name, bP=bP))
}

orgs <- list("fit.numEggs",
             "fit.numHatchlings",
             "fit.numFledglings", 
             "fit.J.F",
             "fit.J.M",
             "fit.A.F", 
             "fit.A.M") 

news <- list("fit.numEggs.new",
             "fit.numHatchlings.new",
             "fit.numFledglings.new", 
             "fit.J.F.new",
             "fit.J.M.new",
             "fit.A.F.new", 
             "fit.A.M.new") 

names <- list("cs", "hs", "fs",
              "phi.J.F", "phi.J.M",
              "phi.A.F", "phi.A.M")

gof <- ldply(1:length(orgs),
  function(x) {
    org <- ipm_sim[[orgs[[x]]]]
    new <- ipm_sim[[news[[x]]]]

    name <- names[[x]]

    df <- bayes_gof(org, new, name)
    df$bP <- round(df$bP,2)
    return(df)
})

# For yearly G.O.F
# commented out here-- extra nodes take signfiicantly longer to run JAGS model
y.gof.A.F <- ldply(1:24,
               function(t) {
                 org <- ipm_sim[["y.fit.A.F"]][,t]
                 new <- ipm_sim[["y.fit.A.F.new"]][,t]
                 
                 name <- "y.fit.A.F"
                 
                 df <- bayes_gof(org, new, name) %>%
                        mutate(year=t)

                 df$bP <- round(df$bP,2)
                 
                 return(df)
})

y.gof.A.M <- ldply(1:24,
                   function(t) {
                     org <- ipm_sim[["y.fit.A.M"]][,t]
                     new <- ipm_sim[["y.fit.A.M.new"]][,t]
                     
                     name <- "y.fit.A.M"
                     
                     df <- bayes_gof(org, new, name) %>%
                              mutate(year=t)
                     df$bP <- round(df$bP,2)
                     return(df)
})


# Figure 1---------------------------------------------------------------------
# library(OpenStreetMap)
# library(ggsn)
# library(rgdal)
# library(ggmap)
# library(scales)
# library(grid)
# library(sp)

# nf_poly = suppressWarnings(readOGR(dsn = 'Data/KI_north_field.kml'))

# nf_poly@polygons[[1]]@ID = 'N'
# rownames(nf_poly@data) = 'N'

# sf_poly = suppressWarnings(readOGR(dsn = "Data/KI_south_field.kml"))

# sf_poly@polygons[[1]]@ID = 'S'
# rownames(sf_poly@data) = 'S'

 
# OSM requires API key for download
# API_KEY = ????
# three_islands =
#   openmap(upperLeft = c(44.5999, -66.7730),
#           lowerRight = c(44.5652, -66.7510),
#           zoom = 15,
#           type = API_KEY)

# regionmap <-
#   openmap(upperLeft = c(47.5, -69),
#           lowerRight = c(43, -63.5),
#           type = API_KEY)

# KentMap <-
#   autoplot(three_islands) +
#   scalebar(dist = 0.5, 
#            x.min=three_islands$bbox$p1[1],
#            x.max=three_islands$bbox$p2[1],
#            y.min=three_islands$bbox$p1[2],
#            y.max=three_islands$bbox$p2[2],
#            anchor=c(x=three_islands$bbox$p1[1]+1300,
#                     y=three_islands$bbox$p1[2]-200),
#                     st.size=4) +
#   geom_polygon(data = spTransform(sf_poly, 
#                                   three_islands$tiles[[1]]$projection) %>%
#                       fortify(sf_poly),
#                aes(x = long + 30, y = lat - 12, group = id),
#                fill = 'dark orange') +
#   geom_polygon(data = spTransform(nf_poly, 
#                                   three_islands$tiles[[1]]$projection) %>%
#                       fortify(nf_poly),
#                aes(x = long + 25, y = lat - 30, group = id),
#                fill = 'dark orange') +
#   labs(x = NULL, y = NULL) +
#   theme_void()

# RegionMap <-
#   autoplot(regionmap) +
#   theme_void() +
#   theme(panel.border=element_rect(colour="black", 
#                                   fill="transparent", 
#                                   size=2.5))

# # now just make the thing in powerpoint! Insets in R = weak, still
# ggsave("Output/Figure_1/KentMap.png", plot=KentMap, 
#                 width=3.5, dpi=600, units="in")
# ggsave("Output/Figure_1/RegionMap.png", plot=RegionMap, 
#                 width=1.5, height=1.5, dpi=600, units="in")
# 
# 
# Figure 3 --------------------------------------------------------------------
no.imm <- out %>% 
            subset(grepl("N.imm", param)) %>% 
            group_by(year) %>% 
            summarise(mean=sum(mean), low=sum(low), high=sum(high)) %>%
            mutate(param="imm", year=year+0.2)

no <- out %>%
            subset(param == "Ntot") %>%
            select(year, mean, low, high) %>%
            mutate(param="tot") %>%
            bind_rows(no.imm)

fig3 <- ggplot(no) +
          geom_errorbar(aes(x=year, ymin=low, ymax=high, 
                            colour=param, linetype=param),
                        width=0.3) +
          geom_point(aes(x=year, y=mean), size=1) +
          geom_line(aes(x=year, y=mean, linetype=param)) +
          scale_linetype_manual(breaks=c("tot", "imm"),
                                  labels=c("Total", "Immigrants"),
                                  values=c("tot"="solid", "imm"="dashed")) +
          scale_colour_manual(values=c("tot"="black", "imm"="darkgray")) +
          guides(linetype=guide_legend(title=""), color=FALSE) +
          xlab("Year") +
          ylab("Population size (95% BCI)") +
          ggtitle("Figure 3") + 
          scale_x_continuous(limits=c(1986.8, 2010.5),
                             breaks=seq(1987, 2010, by=1),
                             labels=c(1987, "", 1989, "", 1991,
                                      "", 1993, "", 1995, "",
                                      1997, "", 1999, "", 2001, 
                                      "", 2003, "", 2005, "", 
                                      2007, "", 2009, "")) +
          theme(title=element_text(size=12), 
                axis.title.y=element_text(size=10,
                                          margin=margin(r=4), 
                                          vjust=0.5),
                axis.title.x=element_text(size=10, 
                                          margin=margin(t=4), 
                                          hjust=0.5),
                axis.text=element_text(size=6.5),
                panel.grid=element_blank(),
                legend.position=c(0.55, 0.75),
                legend.text=element_text(size=10),
                panel.border=element_blank(),
                plot.margin = unit(c(0, 0, 0.05, 0.05), "in"))

ggsave("Output/Figure_3.png", plot=fig3, 
       width=3.5, height=3.5, dpi=600, units="in")

ggsave("Output/PDFs/Figure_3.pdf", plot=fig3, device=cairo_pdf, 
        width=3.5, height=3.5, dpi=600, units="in")


# Figure 4 ----------------------------------------------------------------
shiftMales <- function(param, year) {
  y = year
  if (grepl(".M", param)) {
    y = year + 0.3
  }
  return(y)
}
yearPlots <- list()

yearPlots[[1]] <- ggdraw() + draw_label("Figure 4", fontface='bold')

# Clutch size
yearPlots[[2]] <- ggplot() +
                    geom_point(data=subset(out, param=="cs"), 
                               aes(x=year, y=mean), 
                               size=1.5) +
                    geom_line(data=subset(out, param=="cs"), 
                              aes(x=year, y=mean)) +
                    geom_hline(aes(yintercept=subset(out, param=="m.cs")$mean),
                               linetype="dashed") +
                    geom_errorbar(data=subset(out, param=="cs"), 
                                  aes(x=year, 
                                      ymin=low, ymax=high, 
                                      width=0.4)) +
                    annotate(geom="text", 
                             label="paste(\"(\")*bold(A)*paste(\")\")", 
                             parse=TRUE, 
                             x=-Inf, y=Inf, 
                             hjust=-0.5, vjust=1.1, 
                             size=3.5) +
                    scale_x_continuous(limits=c(1986.5, 2010.5),
                                       breaks=seq(1987, 2010, by=1),
                                       labels=c(1987, "", 1989, "", 1991,
                                                "", 1993, "", 1995, "",
                                                1997, "", 1999, "", 2001, 
                                                "", 2003, "", 2005, "", 
                                                2007, "", 2009, "")) +
                    xlab("") +
                    ylab("Clutch size") +
                    theme(title=element_text(size=12), 
                          axis.title.y=element_text(size=10, 
                                                    margin=margin(r=8), 
                                                    vjust=0.5),
                          axis.text=element_text(size=9),
                          axis.ticks.x=element_blank(), 
                          axis.text.x=element_blank(), 
                          axis.line.x=element_blank(),
                          panel.grid=element_blank(), 
                          panel.border=element_blank(),
                          plot.margin = unit(c(0.05, 0, 0.01, 0.05), "in"))

# Hatching success
yearPlots[[3]] <- ggplot() +
                    geom_point(data=subset(out, param=="hs"), 
                               aes(x=year, y=mean), size=1.5) +
                    geom_line(data=subset(out, param=="hs"), 
                              aes(x=year, y=mean)) +
                    geom_hline(aes(yintercept=subset(out, param=="m.hs")$mean),         linetype="dashed") +
                    geom_errorbar(data=subset(out, param=="hs"), 
                                  aes(x=year, 
                                      ymin=low, ymax=high, 
                                      width=0.4)) +
                    annotate(geom="text", 
                             label="paste(\"(\")*bold(B)*paste(\")\")", 
                             parse=TRUE, 
                             x=-Inf, y=Inf, 
                             hjust=-0.5, 
                             vjust=1.1, size=3.5) +
                    scale_x_continuous(limits=c(1986.5, 2010.5),
                                       breaks=seq(1987, 2010, by=1),
                                       labels=c(1987, "", 1989, "", 1991,
                                                "", 1993, "", 1995, "",
                                                1997, "", 1999, "", 2001, 
                                                "", 2003, "", 2005, "", 
                                                2007, "", 2009, "")) +
                    scale_y_continuous(breaks=c(0.6, 0.7, 0.8, 0.9),
                                       labels=c("0.60", "0.70", 
                                                "0.80", "0.90")) +
                    xlab("") +
                    ylab("Hatching success") +
                    theme(title=element_text(size=12), 
                          axis.title.y=element_text(size=10, 
                                                    margin=margin(r=8),
                                                    vjust=0.5),
                          axis.text=element_text(size=9),
                          axis.ticks.x=element_blank(), 
                          axis.text.x=element_blank(), 
                          axis.line.x=element_blank(),
                          panel.grid=element_blank(), 
                          panel.border=element_blank(),
                          plot.margin = unit(c(0, 0, 0.01, 0.05), "in"))

# Fledging success
yearPlots[[4]] <- ggplot() +
                    geom_point(data=subset(out, param=="fs"), 
                               aes(x=year, y=mean), size=1.5) +
                    geom_line(data=subset(out, param=="fs"), 
                              aes(x=year, y=mean)) +
                    geom_hline(aes(yintercept=subset(out, param=="m.fs")$mean),
                               linetype="dashed") +
                    geom_errorbar(data=subset(out, param=="fs"), 
                                  aes(x=year, 
                                      ymin=low, ymax=high, 
                                      width=0.4)) +
                    annotate(geom="text", 
                             label="paste(\"(\")*bold(C)*paste(\")\")", 
                             parse=TRUE, 
                             x=-Inf, y=Inf, 
                             hjust=-0.5, 
                             vjust=1.1, size=3.5) +
                    scale_x_continuous(limits=c(1986.5, 2010.5),
                                       breaks=seq(1987, 2010, by=1),
                                       labels=c(1987, "", 1989, "", 1991,
                                                "", 1993, "", 1995, "",
                                                1997, "", 1999, "", 2001, 
                                                "", 2003, "", 2005, "", 
                                                2007, "", 2009, "")) +
                    xlab("") +
                    ylab("Fledging success") +
                    theme(title=element_text(size=12), 
                          axis.title.y=element_text(size=10, 
                                                    margin=margin(r=8), 
                                                    vjust=0.5),
                          axis.text=element_text(size=9),
                          axis.ticks.x=element_blank(), 
                          axis.text.x=element_blank(), 
                          axis.line.x=element_blank(),
                          panel.grid=element_blank(), 
                          panel.border=element_blank(),
                          plot.margin = unit(c(0, 0, 0.01, 0.05), "in"))

# Juvenile survival (both sexes)
phijs <- out %>%
          subset(param %in% c("phi.J.F", "phi.J.M")) %>%
          select(param, year, mean, low, high) %>%
          mutate(year = pmap_dbl(list(param, year), shiftMales))

m.phijs <- out %>%
            subset(param %in% c("m.phi.J.F", "m.phi.J.M")) %>%
            select(param, mean)

yearPlots[[5]] <- ggplot() +
                    geom_hline(data=m.phijs,
                               aes(yintercept=mean, color=param),
                               linetype="dashed") +
                    geom_point(data=phijs,
                               aes(x=year, y=mean, color=param),
                               size=1.5) +
                    geom_line(data=phijs,
                              aes(x=year, y=mean, color=param)) + 
                    geom_errorbar(data=phijs,
                                  aes(x=year, color=param,
                                      ymin=low, ymax=high),
                                  width=0.4) +
                    scale_color_manual(labels=c("phi.J.F"="Female",
                                                 "phi.J.M"="Male"),
                                        values=c("phi.J.F"="black",
                                                 "phi.J.M"="darkgray",
                                                 "m.phi.J.F"="black",
                                                 "m.phi.J.M"="darkgray"),
                                        breaks=c("phi.J.F", "phi.J.M")) +
                    scale_y_continuous(breaks=c(0.01, 0.03, 
                                                0.05, 0.07, 0.09)) +
                    guides(color=guide_legend(title="", ncol=2)) +
                    annotate(geom="text", 
                             label="paste(\"(\")*bold(D)*paste(\")\")", 
                             parse=TRUE, 
                             x=-Inf, y=Inf, 
                             hjust=-0.5, vjust=1.1, 
                             size=3.5) +
                    scale_x_continuous(limits=c(1986.5, 2010.5),
                                       breaks=seq(1987, 2010, by=1),
                                       labels=c(1987, "", 1989, "", 1991,
                                                "", 1993, "", 1995, "",
                                                1997, "", 1999, "", 2001, 
                                                "", 2003, "", 2005, "", 
                                                2007, "", 2009, "")) +
                    xlab("") +
                    ylab("Juvenile survival") +
                    theme(title=element_text(size=12),
                          legend.position=c(0.68, 1.45),
                          legend.text=element_text(size=10),
                          axis.title.y=element_text(size=10, 
                                                    margin=margin(r=8), 
                                                    vjust=0.5),
                          axis.text=element_text(size=9),
                          axis.ticks.x=element_blank(), 
                          axis.text.x=element_blank(), 
                          axis.line.x=element_blank(),
                          panel.grid=element_blank(), 
                          panel.border=element_blank(),
                          plot.margin = unit(c(0, 0, 0.01, 0.05), "in"))

# Adult survival (both sexes)
phias <- out %>%
          subset(param %in% c("phi.A.F", "phi.A.M")) %>%
          select(param, year, mean, low, high) %>%
          mutate(year=pmap_dbl(list(param, year), shiftMales))

m.phias <- out %>%
            subset(param %in% c("m.phi.A.F", "m.phi.A.M")) %>%
            select(param, mean)

yearPlots[[6]] <- ggplot() +
                    geom_hline(data=m.phias,
                               aes(yintercept=mean, color=param),
                               linetype="dashed") +
                   geom_point(data=phias,
                               aes(x=year, y=mean, color=param),
                               size=1.5) +
                    geom_line(data=phias,
                              aes(x=year, y=mean, color=param)) + 
                    geom_errorbar(data=phias,
                                  aes(x=year, color=param,
                                      ymin=low, ymax=high),
                                  width=0.4) +
                    scale_color_manual(values=c("phi.A.F"="black",
                                                "phi.A.M"="darkgray",
                                                "m.phi.A.F"="black",
                                                "m.phi.A.M"="darkgray")) +
                    guides(color=FALSE) +
                    annotate(geom="text", 
                             label="paste(\"(\")*bold(E)*paste(\")\")", 
                             parse=TRUE, 
                             x=-Inf, y=Inf, 
                             hjust=-0.5, vjust=1.1, 
                             size=3.5) +
                    scale_x_continuous(limits=c(1986.5, 2010.5),
                                       breaks=seq(1987, 2010, by=1),
                                       labels=c(1987, "", 1989, "", 1991,
                                                "", 1993, "", 1995, "",
                                                1997, "", 1999, "", 2001, 
                                                "", 2003, "", 2005, "", 
                                                2007, "", 2009, "")) +
                    xlab("") +
                    ylab("Adult survival") +
                    theme(title=element_text(size=12), 
                          axis.title.y=element_text(size=10, 
                                                    margin=margin(r=8), 
                                                    vjust=0.5),
                          axis.text=element_text(size=9),
                          axis.ticks.x=element_blank(), 
                          axis.text.x=element_blank(), 
                          axis.line.x=element_blank(),
                          panel.grid=element_blank(), 
                          panel.border=element_blank(),
                          plot.margin = unit(c(0, 0, 0.01, 0.05), "in"))

# Immigration (both sexes)
imms <- out %>%
          subset(param %in% c("omega.F", "omega.M")) %>%
          select(param, year, mean, low, high) %>%
          mutate(year=pmap_dbl(list(param, year), shiftMales))

# derive arithmetic means
mOmega.F <- data_frame(param="m.omega.F", 
                       mean=mean(rowMeans(ipm_sim[["omega.F"]])))
mOmega.M <- data_frame(param="m.omega.M", 
                       mean=mean(rowMeans(ipm_sim[["omega.M"]])))

m.imms <- bind_rows(mOmega.F, mOmega.M) 

yearPlots[[7]] <- ggplot() +
                    geom_hline(data=m.imms,
                               aes(yintercept=mean, color=param),
                               linetype="dashed") +
                   geom_point(data=imms,
                               aes(x=year, y=mean, color=param),
                               size=1.5) +
                    geom_line(data=imms,
                              aes(x=year, y=mean, color=param)) + 
                    geom_errorbar(data=imms,
                                  aes(x=year, color=param,
                                      ymin=low, ymax=high),
                                  width=0.4) +
                    scale_color_manual(values=c("omega.F"="black",
                                                "omega.M"="darkgray",
                                                "m.omega.F"="black",
                                                "m.omega.M"="darkgray")) +
                    guides(color=FALSE) +
                    annotate(geom="text", 
                             label="paste(\"(\")*bold(F)*paste(\")\")", 
                             parse=TRUE, 
                             x=-Inf, y=Inf, 
                             hjust=-0.5, vjust=1.1, 
                             size=3.5) +
                    scale_x_continuous(limits=c(1986.5, 2010.5),
                                       breaks=seq(1987, 2010, by=1),
                                       labels=c(1987, "", 1989, "", 1991,
                                                "", 1993, "", 1995, "",
                                                1997, "", 1999, "", 2001, 
                                                "", 2003, "", 2005, "", 
                                                2007, "", 2009, "")) +
                    xlab("") +
                    ylab("Immigration") +
                    theme(title=element_text(size=12), 
                          axis.title.y=element_text(size=10, 
                                                    margin=margin(r=8), 
                                                    vjust=0.5),
                          axis.text=element_text(size=9),
                          axis.ticks.x=element_blank(), 
                          axis.text.x=element_blank(), 
                          axis.line.x=element_blank(),
                          panel.grid=element_blank(), 
                          panel.border=element_blank(),
                          plot.margin = unit(c(0, 0, 0.01, 0.05), "in"))

# Lambda
yearPlots[[8]] <- ggplot() +
                    geom_line(data=subset(out, param=="lambda"), 
                              aes(x=year, y=mean)) +
                    geom_hline(aes(yintercept=subset(out, 
                                                     param=="m.lambda")$mean),
                               linetype="dashed") +
                    geom_errorbar(data=subset(out, param=="lambda"), 
                                  aes(x=year, ymin=low, ymax=high, 
                                      width=0.4)) +
                    geom_point(data=subset(out, param=="lambda"), 
                               aes(x=year, y=mean), 
                               size=1.5) +
                    annotate(geom="text", 
                             label="paste(\"(\")*bold(G)*paste(\")\")", 
                             parse=TRUE, 
                             x=-Inf, y=Inf, 
                             hjust=-0.5, vjust=1.1, 
                             size=3.5) +
                    scale_x_continuous(limits=c(1986.5, 2010.5),
                                       breaks=seq(1987, 2010, by=1),
                                       labels=c(1987, "", 1989, "", 1991,
                                                "", 1993, "", 1995, "",
                                                1997, "", 1999, "", 2001, 
                                                "", 2003, "", 2005, "", 
                                                2007, "", 2009, "")) +
                    xlab("Year") +
                    ylab(expression(lambda)) +
                    theme(title=element_text(size=12), 
                          axis.title.y=element_text(size=10, 
                                                    margin=margin(r=8), 
                                                    vjust=0.5,
                                                    angle=0),
                          axis.title.x=element_text(size=10, 
                                                    margin=margin(t=8), 
                                                    hjust=0.5),
                          axis.text=element_text(size=9),
                          panel.grid=element_blank(), 
                          panel.border=element_blank(),
                          plot.margin = unit(c(0, 0, 0.05, 0.05), "in"))

fig4 <- plot_grid(plotlist=yearPlots, align="hv", nrow=8, ncol=1)

ggsave("Output/Figure_4.png", fig4, 
       width=7, height=9, dpi=600, units="in")

ggsave("Output/PDFs/Figure_4.pdf", fig4, device=cairo_pdf, 
       width=7, height=9, dpi=600, units="in")

# Figure 5 ------------------------------------------------------------------

contPlots <- list()

contPlots[[1]] <- ggdraw() + draw_label("Figure 5", fontface='bold')

# Productivity and Juvenile Survival summed
prodandjuv_sumCont <- yearly_cont_summary %>%
                        subset(key %in% c("cs", "hs", "fs", 
                                          "phi.J.F", "phi.J.M")) %>%
                        group_by(year) %>%
                        summarise(sumCont=sum(mean))

contPlots[[2]] <- ggplot(prodandjuv_sumCont) +
                    geom_hline(aes(yintercept=0), 
                               colour="lightgray", 
                               alpha=0.5) +
                    geom_col(aes(x=year, y=sumCont), 
                             fill="#0caf58",         # green
                             colour="#0caf58") +     # green
                    geom_col(data=diff_lams, 
                             aes(x=year, y=diff), 
                             fill=alpha("white", 0), 
                             colour="darkgray") +
                    annotate(geom="text", 
                             label="paste(\"(\")*bold(A)*paste(\")\")~paste(\"Productivity and\")", 
                             parse=TRUE, 
                             x=1986.5, y=0.60, 
                             hjust=0, 
                             size=3.5) +
                    annotate(geom="text", 
                             label="paste(\"juvenile survival\")", 
                             parse=TRUE, 
                             x=1987.47, y=0.425, 
                             hjust=0, 
                             size=3.5) +
                    scale_x_continuous(limits=c(1986.5, 2010.5),
                                       breaks=seq(1987, 2010, by=1),
                                       labels=c(1987, "", 1989, "", 1991,
                                                "", 1993, "", 1995, "",
                                                1997, "", 1999, "", 2001, 
                                                "", 2003, "", 2005, "", 
                                                2007, "", 2009, "")) +
                    guides(fill=FALSE) +
                    xlab("") +
                    ylab("") +
                    theme(title=element_text(size=12), 
                          axis.title.y=element_text(size=10, 
                                                    margin=margin(r=8), 
                                                    vjust=0.5),
                          axis.text=element_text(size=9),
                          axis.ticks.x=element_blank(), 
                          axis.text.x=element_blank(), 
                          axis.line.x=element_blank(),
                          panel.grid=element_blank(), 
                          panel.border=element_blank(),
                          plot.margin = unit(c(0.05, 0, 0.01, 0.05), "in"))

# Adult survival (both sexes)
contPlots[[3]] <- ggplot(subset(yearly_cont_summary, 
                                key %in% c("phi.A.F", "phi.A.M"))) +
                    geom_hline(aes(yintercept=0), 
                               colour="lightgray", 
                               alpha=0.5) +
                    geom_col(aes(x=year, y=mean, fill=key, colour=key)) +
                    geom_col(data=diff_lams, 
                             aes(x=year, y=diff), 
                             fill=alpha("white", 0), 
                             colour="darkgray") +
                    annotate(geom="text", 
                             label="paste(\"(\")*bold(B)*paste(\")\")~paste(\"Adult survival\")", 
                             parse=TRUE, 
                             x=1986.5, y=0.60, 
                             hjust=0, 
                             size=3.5) +
                    scale_x_continuous(limits=c(1986.5, 2010.5),
                                       breaks=seq(1987, 2010, by=1),
                                       labels=c(1987, "", 1989, "", 1991,
                                                "", 1993, "", 1995, "",
                                                1997, "", 1999, "", 2001, 
                                                "", 2003, "", 2005, "", 
                                                2007, "", 2009, "")) +
                    scale_fill_manual(values=c("phi.A.M"="#636bf9", # blue
                                               "phi.A.F"="#ff727e"), # red
                                      guide=FALSE) +
                    scale_colour_manual(values=c("phi.A.M"="#636bf9", # blue
                                                 "phi.A.F"="#ff727e"), # red
                                      guide=FALSE) +

                    xlab("") +
                    ylab(bquote(.("Contribution to") ~ Delta * lambda[t])) +
                    theme(title=element_text(size=12), 
                          axis.title.y=element_text(size=10, 
                                                    margin=margin(r=8), 
                                                    vjust=0.5, hjust=0),
                          axis.text=element_text(size=9),
                          axis.ticks.x=element_blank(), 
                          axis.text.x=element_blank(), 
                          axis.line.x=element_blank(),
                          panel.grid=element_blank(), 
                          panel.border=element_blank(),
                          plot.margin = unit(c(0, 0, 0.01, 0.05), "in"))

# Immigration (both sexes)
contPlots[[4]] <- ggplot(subset(yearly_cont_summary, 
                         key %in% c("omega.F", "omega.M"))) +
                    geom_hline(aes(yintercept=0), 
                               colour="lightgray", 
                               alpha=0.5) +
                    geom_col(aes(x=year, y=mean, fill=key, colour=key)) +
                    geom_col(data=diff_lams, aes(x=year, y=diff), 
                             fill=alpha("white", 0), 
                             colour="darkgray") +
                    annotate(geom="text", 
                             label="paste(\"(\")*bold(C)*paste(\")\")~paste(\"Immigration\")", 
                             parse=TRUE, 
                             x=1986.5, y=0.60, 
                             hjust=0, size=3.5) +
                    scale_x_continuous(limits=c(1986.5, 2010.5),
                                       breaks=seq(1987, 2010, by=1),
                                       labels=c(1987, "", 1989, "", 1991,
                                                "", 1993, "", 1995, "",
                                                1997, "", 1999, "", 2001, 
                                                "", 2003, "", 2005, "", 
                                                2007, "", 2009, "")) +
                    scale_fill_manual(breaks=c("omega.F", "omega.M"),
                                      values=c("omega.M"="#636bf9", # blue
                                               "omega.F"="#ff727e"), # red
                                      guide=FALSE) +
                    scale_colour_manual(breaks=c("omega.F", "omega.M"),
                                      values=c("omega.M"="#636bf9",  # blue
                                               "omega.F"="#ff727e"), # red
                                      guide=FALSE) +
                    xlab("Year") +
                    ylab("") +
                    theme(title=element_text(size=12), 
                          axis.title.y=element_text(size=10, 
                                                    margin=margin(r=8), 
                                                    vjust=0.5),
                          axis.title.x=element_text(size=10, 
                                                    margin=margin(t=8), 
                                                    hjust=0.5),
                          axis.text=element_text(size=9),
                          panel.grid=element_blank(), 
                          panel.border=element_blank(),
                          plot.margin = unit(c(0, 0, 0.05, 0.05), "in"))

fig5 <- plot_grid(plotlist=contPlots, align="hv", nrow=4, ncol=1)

ggsave("Output/Figure_5.png", fig5, 
       width=7, height=6, dpi=600, units="in")
ggsave("Output/PDFs/Figure_5.pdf", fig5, device=cairo_pdf, 
       width=7, height=6, dpi=600, units="in")

# Figure 6 (Appendix A)--------------------------------------------------------
# For yearly G.O.F


fig6 <- ggplot() +
          geom_hline(aes(yintercept=0.5), 
                     colour="lightgray", 
                     linetype="dashed") +
          geom_line(data=y.gof.A.M, aes(x=year+1986.2, y=bP, color=name)) +
          geom_point(data=y.gof.A.M, aes(x=year+1986.2, y=bP, color=name), 
                     size=1) +
          geom_line(data=y.gof.A.F, aes(x=year+1986, y=bP, color=name)) +
          geom_point(data=y.gof.A.F, aes(x=year+1986, y=bP, color=name),
                     size=1) +
          scale_color_manual(labels=c("y.fit.A.F"="Female",
                                      "y.fit.A.M"="Male"),
                             values=c("y.fit.A.F"="black",
                                      "y.fit.A.M"="darkgray")) +
          guides(color=guide_legend(title=element_blank())) +
          scale_y_continuous(limits=c(0, 1)) +
          scale_x_continuous(limits=c(1986.5, 2010.5),
                             breaks=seq(1987, 2010, by=1),
                             labels=c(1987, "", 1989, "", 1991,
                                      "", 1993, "", 1995, "",
                                      1997, "", 1999, "", 2001, 
                                      "", 2003, "", 2005, "", 
                                      2007, "", 2009, "")) +
          ylab(expression(italic(PPP)*paste("-value"))) +
          xlab("Year") +
          ggtitle("Figure 6 in Appendix A") +
          theme(title=element_text(size=12), 
                legend.text=element_text(size=10),
                legend.position=c(0.67, 0.83),
                axis.title.y=element_text(size=10,
                                          margin=margin(r=4), 
                                          vjust=0.5),
                axis.title.x=element_text(size=10, 
                                          margin=margin(t=4), 
                                          hjust=0.5),
                axis.text=element_text(size=6.5),
                panel.grid=element_blank(), 
                panel.border=element_blank(),
                plot.margin = unit(c(0, 0, 0.05, 0.05), "in"))

ggsave("Output/Figure_6.png", plot=fig6, 
       width=3.5, height=3.5, dpi=600, units="in")
ggsave("Output/PDFs/Figure_6.pdf", plot=fig6, device=cairo_pdf, 
        width=3.5, height=3.5, dpi=600, units="in")

# Figure 7 (Appendix B)--------------------------------------------------------

cont_appendixPlots <- list()

cont_appendixPlots[[1]] <- ggdraw() + draw_label("Figure 7 in Appendix B", fontface='bold')

cont_appendixPlots[[2]] <- ggplot()


cont_appendixPlots[[3]] <- ggplot()

# Clutch size
cont_appendixPlots[[4]] <- ggplot(subset(yearly_cont_summary, key == "cs")) +
                            geom_hline(aes(yintercept=0), 
                                       colour="lightgray", 
                                       alpha=0.5) +
                            geom_col(aes(x=year, y=mean), fill="black") +
                            geom_col(data=diff_lams, 
                                     aes(x=year, y=diff), 
                                     fill=alpha("white", 0), 
                                     colour=alpha("darkgray", 0.6)) +
                            annotate(geom="text", 
                                     label="Clutch size", 
                                     x=1986.5, y=0.60, 
                                     hjust=0, size=2) +
                            scale_x_continuous(limits=c(1986.5, 2010.5),
                                               breaks=seq(1987, 2010, by=1),
                                               labels=c(1987, "", 1989, "", 
                                                        1991, "", 1993, "",
                                                        1995, "", 1997, "",
                                                        1999, "", 2001, "",
                                                        2003, "", 2005, "", 
                                                        2007, "", 2009, "")) +
                            guides(fill=FALSE) +
                            xlab("Year") +
                            ylab("") +
                            theme(title=element_text(size=12), 
                                  axis.title.y=element_text(size=10, 
                                                            margin=margin(r=8),
                                                            vjust=0.5),
                                  axis.title.x=element_blank(), 
                                  axis.text.x=element_blank(), 
                                  axis.line.x=element_blank(), 
                                  axis.ticks.x=element_blank(),
                                  axis.text=element_text(size=9),
                                  panel.grid=element_blank(), 
                                  panel.border=element_blank(),
                                  plot.margin = unit(c(.05, 0, 0, .05), "in"))

# Hatching success
cont_appendixPlots[[5]] <- ggplot(subset(yearly_cont_summary, key == "hs")) +
                            geom_hline(aes(yintercept=0), 
                                       colour="lightgray", 
                                       alpha=0.5) +
                            geom_col(aes(x=year, y=mean), fill="black") +
                            geom_col(data=diff_lams, 
                                     aes(x=year, y=diff), 
                                     fill=alpha("white", 0), 
                                     colour=alpha("darkgray", 0.6)) +
                            annotate(geom="text", 
                                     label="Hatching success", 
                                     x=1986.5, y=0.60, 
                                     hjust=0, size=2) +
                            scale_x_continuous(limits=c(1986.5, 2010.5),
                                               breaks=seq(1987, 2010, by=1),
                                               labels=c(1987, "", 1989, "", 
                                                        1991, "", 1993, "",
                                                        1995, "", 1997, "",
                                                        1999, "", 2001, "",
                                                        2003, "", 2005, "", 
                                                        2007, "", 2009, "")) +
                            guides(fill=FALSE) +
                            xlab("Year") +
                            ylab("") +
                            theme(title=element_text(size=12), 
                                  axis.title.y=element_text(size=10, 
                                                            margin=margin(r=8),
                                                            vjust=0.5),
                                  axis.title.x=element_blank(), 
                                  axis.text.x=element_blank(), 
                                  axis.line.x=element_blank(), 
                                  axis.ticks.x=element_blank(),
                                  axis.text=element_text(size=9),
                                  panel.grid=element_blank(), 
                                  panel.border=element_blank(),
                                  plot.margin = unit(c(0, 0, 0, 0.00), "in"))

# fledging success
cont_appendixPlots[[6]] <- ggplot(subset(yearly_cont_summary, key == "fs")) +
                            geom_hline(aes(yintercept=0), 
                                       colour="lightgray", 
                                       alpha=0.5) +
                            geom_col(aes(x=year, y=mean), fill="black") +
                            geom_col(data=diff_lams, 
                                     aes(x=year, y=diff), 
                                     fill=alpha("white", 0), 
                                     colour=alpha("darkgray", 0.6)) +
                            annotate(geom="text", 
                                     label="Fledging success", 
                                     x=1986.5, y=0.60, 
                                     hjust=0, size=2) +
                            scale_x_continuous(limits=c(1986.5, 2010.5),
                                               breaks=seq(1987, 2010, by=1),
                                               labels=c(1987, "", 1989, "", 
                                                        1991, "", 1993, "",
                                                        1995, "", 1997, "",
                                                        1999, "", 2001, "",
                                                        2003, "", 2005, "", 
                                                        2007, "", 2009, "")) +
                            guides(fill=FALSE) +
                            xlab("Year") +
                            ylab("") +
                            theme(title=element_text(size=12), 
                                  axis.title.y=element_text(size=10, 
                                                            margin=margin(r=8),
                                                            vjust=0.5),
                                  axis.title.x=element_blank(), 
                                  axis.text.x=element_blank(), 
                                  axis.line.x=element_blank(), 
                                  axis.ticks.x=element_blank(),
                                  axis.text=element_text(size=9),
                                  panel.grid=element_blank(), 
                                  panel.border=element_blank(),
                                  plot.margin = unit(c(0, 0, 0, 0.05), "in"))

# Juvenile survival (FEMALE)
cont_appendixPlots[[7]] <- ggplot(subset(yearly_cont_summary, 
                                         key == "phi.J.F")) +
                            geom_hline(aes(yintercept=0), 
                                       colour="lightgray", 
                                       alpha=0.5) +
                            geom_col(aes(x=year, y=mean), fill="black") +
                            geom_col(data=diff_lams, aes(x=year, y=diff), 
                                     fill=alpha("white", 0), 
                                     colour=alpha("darkgray", 0.6)) +
                            annotate(geom="text", 
                                     label="Juvenile survival", 
                                     x=1986.5, y=0.60, 
                                     hjust=0, size=2) +
                            annotate(geom="text", label="(Female)", 
                                     x=1986.5, y=0.45, 
                                     hjust=0, size=2) +
                            scale_x_continuous(limits=c(1986.5, 2010.5),
                                               breaks=seq(1987, 2010, by=1),
                                               labels=c(1987, "", 1989, "", 
                                                        1991, "", 1993, "",
                                                        1995, "", 1997, "",
                                                        1999, "", 2001, "",
                                                        2003, "", 2005, "", 
                                                        2007, "", 2009, "")) +
                            guides(fill=FALSE) +
                            xlab("Year") +
                            ylab("") +
                            theme(title=element_text(size=12), 
                                  axis.title.y=element_text(size=10, 
                                                            margin=margin(r=8),
                                                            vjust=0.5),
                                  axis.title.x=element_blank(), 
                                  axis.text.x=element_blank(), 
                                  axis.line.x=element_blank(), 
                                  axis.ticks.x=element_blank(),
                                  axis.text=element_text(size=9),
                                  panel.grid=element_blank(), 
                                  panel.border=element_blank(),
                                  plot.margin = unit(c(0, 0, 0, 0.05), "in"))

# Juvenile survival (MALE)
cont_appendixPlots[[8]] <- ggplot(subset(yearly_cont_summary, 
                                         key == "phi.J.M")) +
                            geom_hline(aes(yintercept=0), 
                                       colour="lightgray", 
                                       alpha=0.5) +
                            geom_col(aes(x=year, y=mean), fill="black") +
                            geom_col(data=diff_lams, 
                                     aes(x=year, y=diff), 
                                     fill=alpha("white", 0), 
                                     colour=alpha("darkgray", 0.6)) +
                            annotate(geom="text", 
                                     label="Juvenile survival", 
                                     x=1986.5, y=0.60, 
                                     hjust=0, size=2) +
                            annotate(geom="text", 
                                     label="(Male)", 
                                     x=1986.5, y=0.45,
                                     hjust=0, size=2) +
                            scale_x_continuous(limits=c(1986.5, 2010.5),
                                               breaks=seq(1987, 2010, by=1),
                                               labels=c(1987, "", 1989, "", 
                                                        1991, "", 1993, "",
                                                        1995, "", 1997, "",
                                                        1999, "", 2001, "",
                                                        2003, "", 2005, "", 
                                                        2007, "", 2009, "")) +
                            guides(fill=FALSE) +
                            xlab("Year") +
                            ylab("") +
                            theme(title=element_text(size=12), 
                                  axis.title.y=element_text(size=10, 
                                                            margin=margin(r=8),
                                                            vjust=0.5),
                                  axis.title.x=element_blank(), 
                                  axis.text.x=element_blank(), 
                                  axis.line.x=element_blank(), 
                                  axis.ticks.x=element_blank(),
                                  axis.text=element_text(size=9),
                                  panel.grid=element_blank(), 
                                  panel.border=element_blank(),
                                  plot.margin = unit(c(0, 0, 0, 0.05), "in"))

# Adult survival (FEMALE)
cont_appendixPlots[[9]] <- ggplot(subset(yearly_cont_summary, 
                                  key == "phi.A.F")) +
                            geom_hline(aes(yintercept=0), 
                                       colour="lightgray", 
                                       alpha=0.5) +
                            geom_col(aes(x=year, y=mean), fill="black") +
                            geom_col(data=diff_lams, 
                                     aes(x=year, y=diff), 
                                     fill=alpha("white", 0), 
                                     colour=alpha("darkgray", 0.6)) +
                            annotate(geom="text", 
                                     label="Adult survival", 
                                     x=1986.5, y=0.60, 
                                     hjust=0, size=2) +
                            annotate(geom="text", 
                                     label="(Female)", 
                                     x=1986.5, y=0.45,
                                     hjust=0, size=2) +
                            scale_x_continuous(limits=c(1986.5, 2010.5),
                                               breaks=seq(1987, 2010, by=1),
                                               labels=c(1987, "", 1989, "", 
                                                        1991, "", 1993, "",
                                                        1995, "", 1997, "",
                                                        1999, "", 2001, "",
                                                        2003, "", 2005, "", 
                                                        2007, "", 2009, "")) +
                            guides(fill=FALSE) +
                            xlab("Year") +
                            ylab("") +
                            theme(title=element_text(size=12), 
                                  axis.title.y=element_text(size=10, 
                                                            margin=margin(r=8),
                                                            vjust=0.5),
                                  axis.title.x=element_blank(), 
                                  axis.text.x=element_blank(), 
                                  axis.line.x=element_blank(), 
                                  axis.ticks.x=element_blank(),
                                  axis.text=element_text(size=9),
                                  panel.grid=element_blank(), 
                                  panel.border=element_blank(),
                                  plot.margin = unit(c(0, 0, 0, 0), "in"))

# Adult survival (MALE)
cont_appendixPlots[[10]] <- ggplot(subset(yearly_cont_summary, 
                                        key == "phi.A.M")) +
                            geom_hline(aes(yintercept=0), 
                                       colour="lightgray", 
                                       alpha=0.5) +
                            geom_col(aes(x=year, y=mean), fill="black") +
                            geom_col(data=diff_lams, 
                                     aes(x=year, y=diff), 
                                     fill=alpha("white", 0), 
                                     colour=alpha("darkgray", 0.6)) +
                            annotate(geom="text", 
                                     label="Adult survival", 
                                     x=1986.5, y=0.60, 
                                     hjust=0, size=2) +
                            annotate(geom="text", 
                                     label="(Male)", 
                                     x=1986.5, y=0.45, 
                                     hjust=0, size=2) +
                            scale_x_continuous(limits=c(1986.5, 2010.5),
                                               breaks=seq(1987, 2010, by=1),
                                               labels=c(1987, "", 1989, "", 
                                                        1991, "", 1993, "",
                                                        1995, "", 1997, "",
                                                        1999, "", 2001, "",
                                                        2003, "", 2005, "", 
                                                        2007, "", 2009, "")) +
                            guides(fill=FALSE) +
                            xlab("Year") +
                            ylab("") +
                            theme(title=element_text(size=12), 
                                  axis.title.y=element_text(size=10, 
                                                            margin=margin(r=8),
                                                            vjust=0.5),
                                  axis.title.x=element_blank(), 
                                  axis.text.x=element_blank(), 
                                  axis.line.x=element_blank(), 
                                  axis.ticks.x=element_blank(),
                                  axis.text=element_text(size=9),
                                  panel.grid=element_blank(), 
                                  panel.border=element_blank(),
                                  plot.margin = unit(c(0, 0, 0, 0.05), "in"))

# Immigration (FEMALE)
cont_appendixPlots[[11]] <- ggplot(subset(yearly_cont_summary, 
                                         key == "omega.F")) +
                            geom_hline(aes(yintercept=0), 
                                       colour="lightgray", 
                                       alpha=0.5) +
                            geom_col(aes(x=year, y=mean), fill="black") +
                            geom_col(data=diff_lams, 
                                     aes(x=year, y=diff), 
                                     fill=alpha("white", 0), 
                                     colour=alpha("darkgray", 0.6)) +
                            annotate(geom="text", 
                                     label="Immigration", 
                                     x=1986.5, y=0.60, 
                                     hjust=0, size=2) +
                            annotate(geom="text", 
                                     label="(Female)", 
                                     x=1986.5, y=0.45, 
                                     hjust=0, size=2) +
                            scale_x_continuous(limits=c(1986.5, 2010.5),
                                               breaks=seq(1987, 2010, by=1),
                                               labels=c(1987, "", 1989, "", 
                                                        1991, "", 1993, "",
                                                        1995, "", 1997, "",
                                                        1999, "", 2001, "",
                                                        2003, "", 2005, "", 
                                                        2007, "", 2009, "")) +
                            guides(fill=FALSE) +
                            xlab("Year") +
                            ylab("") +
                            theme(title=element_text(size=12), 
                                  axis.title.y=element_text(size=10, 
                                                            margin=margin(r=8),
                                                            vjust=0.5),
                                  axis.title.x=element_blank(), 
                                  axis.text.x=element_blank(), 
                                  axis.line.x=element_blank(), 
                                  axis.ticks.x=element_blank(),
                                  axis.text=element_text(size=9),
                                  panel.grid=element_blank(), 
                                  panel.border=element_blank(),
                                  plot.margin = unit(c(0, 0, 0, 0), "in"))

# Immigration (MALE)
cont_appendixPlots[[12]] <- ggplot(subset(yearly_cont_summary, 
                                         key == "omega.M")) +
                            geom_hline(aes(yintercept=0), 
                                       colour="lightgray", 
                                       alpha=0.5) +
                            geom_col(aes(x=year, y=mean), fill="black") +
                            geom_col(data=diff_lams, 
                                     aes(x=year, y=diff), 
                                     fill=alpha("white", 0), 
                                     colour=alpha("darkgray", 0.6)) +
                            annotate(geom="text", 
                                     label="Immigration", 
                                     x=1986.5, y=0.60, 
                                     hjust=0, size=2) +
                            annotate(geom="text", 
                                     label="(Male)", 
                                     x=1986.5, y=0.45, 
                                     hjust=0, size=2) +
                            scale_x_continuous(limits=c(1986.5, 2010.5),
                                               breaks=seq(1987, 2010, by=1),
                                               labels=c(1987, "", 1989, "", 
                                                        1991, "", 1993, "",
                                                        1995, "", 1997, "",
                                                        1999, "", 2001, "",
                                                        2003, "", 2005, "", 
                                                        2007, "", 2009, "")) +
                            guides(fill=FALSE) +
                            xlab("Year") +
                            ylab("") +
                            theme(title=element_text(size=12), 
                                  axis.title.y=element_text(size=10, 
                                                            margin=margin(r=8),
                                                            vjust=0.5),
                                  axis.title.x=element_blank(), 
                                  axis.text.x=element_blank(), 
                                  axis.line.x=element_blank(), 
                                  axis.ticks.x=element_blank(),
                                  axis.text=element_text(size=9),
                                  panel.grid=element_blank(), 
                                  panel.border=element_blank(),
                                  plot.margin = unit(c(0, 0, 0, 0.05), "in"))

# Proportional abundance (FEMALE)
cont_appendixPlots[[13]] <- ggplot(subset(yearly_cont_summary, 
                                          key == "prop.F")) +
                            geom_hline(aes(yintercept=0), 
                                       colour="lightgray", alpha=0.5) +
                            geom_col(aes(x=year, y=mean), fill="black") +
                            geom_col(data=diff_lams, 
                                     aes(x=year, y=diff), 
                                     fill=alpha("white", 0), 
                                     colour=alpha("darkgray", 0.6)) +
                            annotate(geom="text", 
                                     label="Prop. abundance",
                                     x=1986.5, y=0.60, 
                                     hjust=0, size=2) +
                            annotate(geom="text", 
                                     label="(Female)", 
                                     x=1986.5, y=0.45, 
                                     hjust=0, size=2) +
                            scale_x_continuous(limits=c(1986.5, 2010.5),
                                               breaks=seq(1987, 2010, by=1),
                                               labels=c("87", "", "89", "",
                                                        "91", "", "93", "",
                                                        "95", "", "97", "",
                                                        "99", "", "01", "",
                                                        "03", "", "05", "",
                                                        "07", "", "09", "")) +
                            guides(fill=FALSE) +
                            xlab("Year") +
                            ylab(bquote(.("Contribution to") ~ Delta * lambda[t])) +
                            theme(title=element_text(size=12), 
                                axis.title.y=element_text(size=10, 
                                                          margin=margin(r=8),
                                                          vjust=0.5),
                                axis.title.x=element_text(size=10, 
                                                          margin=margin(t=8),
                                                          hjust=0.5),
                                axis.text=element_text(size=9),
                                panel.grid=element_blank(), 
                                panel.border=element_blank(),
                                plot.margin = unit(c(0, 0, .05, .05), "in"))


# Proportional abundance (MALE)
cont_appendixPlots[[14]] <- ggplot(subset(yearly_cont_summary, 
                                          key == "prop.M")) +
                            geom_hline(aes(yintercept=0), 
                                       colour="lightgray", 
                                       alpha=0.5) +
                            geom_col(aes(x=year, y=mean), fill="black") +
                            geom_col(data=diff_lams, 
                                     aes(x=year, y=diff), 
                                     fill=alpha("white", 0), 
                                     colour=alpha("darkgray", 0.6)) +
                            annotate(geom="text", 
                                     label="Prop. abundance", 
                                     x=1986.5, y=0.60, 
                                     hjust=0, size=2) +
                            annotate(geom="text", 
                                     label="(Male)", 
                                     x=1986.5, y=0.45, 
                                     hjust=0, size=2) +
                            scale_x_continuous(limits=c(1986.5, 2010.5),
                                               breaks=seq(1987, 2010, by=1),
                                               labels=c("87", "", "89", "",
                                                        "91", "", "93", "",
                                                        "95", "", "97", "",
                                                        "99", "", "01", "",
                                                        "03", "", "05", "",
                                                        "07", "", "09", "")) +
                            guides(fill=FALSE) +
                            xlab("Year") +
                            ylab("") + 
                            theme(title=element_text(size=12), 
                                  axis.title.y=element_text(size=10, 
                                                            margin=margin(r=8),
                                                            vjust=0.5),
                                  axis.title.x=element_text(size=10, 
                                                          margin=margin(t=8),
                                                          hjust=0.5),
                                  axis.text=element_text(size=9),
                                  panel.grid=element_blank(), 
                                  panel.border=element_blank(),
                                  plot.margin = unit(c(0, 0, 0.0, 0.00), "in"))

fig7 <- plot_grid(plotlist=cont_appendixPlots, align="hv", nrow=7, ncol=2)

ggsave("Output/Figure_7.png", fig7, 
       width=7, height=10, dpi=600, units="in")
ggsave("Output/PDFs/Figure_7.pdf", fig7, device=cairo_pdf, 
       width=7, height=10, dpi=600, units="in")
