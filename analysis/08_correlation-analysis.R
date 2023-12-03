
# build wide data for correlation analysis --------------------------------

#widen database - only treated @ days 3,5 and 7
correl.df <- data.unvax |> 
  filter(anytreat==1 & day %in% c(3,5,7)) |> 
  select(paper,firstauthor,treatment,group,trttype,n.virol,day,trteffect,sem.diff,agent) |> 
  pivot_wider(
    names_from = day,
    names_prefix = "d",
    names_vary = "slowest",
    values_from = c(trteffect,sem.diff)
  ) 

# Pearson correlation calculations ----------------------------------------


c35<-cor.test(correl.df$trteffect_d3,correl.df$trteffect_d5,method = "pearson")
c37<-cor.test(correl.df$trteffect_d3,correl.df$trteffect_d7,method = "pearson")
c57<-cor.test(correl.df$trteffect_d5,correl.df$trteffect_d7,method = "pearson")



# Plots comparing timepoints ----------------------------------------------


# common to all 3 plots ---------------------------------------------------


#set axis ticks
breaks <- seq(0,1.25,0.25)
labels <- breaks



# day 3 vs day 5 plot -----------------------------------------------------


plot.title <- "Day 3 and 5"
x.title <- "Excess clearance to day 3"
y.title <- "Excess clearance to day 5"


plot35 <- ggplot(subset(correl.df,!is.na(trteffect_d3)&!is.na(trteffect_d5)),aes(x = trteffect_d3, y = trteffect_d5, colour=paper,shape=agent)) +
  geom_point(size = 4,stroke=1.5) + 
  labs(title = plot.title,
       x = x.title, y = y.title) +
  scale_colour_manual(values=papercols, guide = "none")+
  scale_shape_manual(values=agentshape, guide = "none")+
  scale_x_continuous(breaks = breaks,labels = labels,limits = c(0,1.25))+
  scale_y_continuous(breaks = breaks,labels = labels,limits = c(0,1.25))+
  theme_classic()+
  theme(legend.direction = "vertical", 
        legend.box = "horizontal",
        legend.text = element_text(size=16),
        axis.line = element_line(linewidth = 0.85),
        axis.text = element_text(colour="black", size = 16),
        axis.title = element_text(colour = "black",size = 18),
        text = element_text(size = 18))+
  annotate("text", x = 0.2, y = 1.1, size = 6,label = glue("r = {round(c35$estimate,2)}, p = {round(c35$p.value ,3)}"))


# Day 3 vs Day 7 plot -----------------------------------------------------



plot.title <- "Day 3 and 7"
x.title <- "Excess clearance to day 3"
y.title <- "Excess clearance to day 7"



plot37 <- ggplot(subset(correl.df,!is.na(trteffect_d3)&!is.na(trteffect_d7)),aes(x = trteffect_d3, y = trteffect_d7, colour=paper,shape=agent)) +
  geom_point(size = 4,stroke=1.5) + 
  labs(title = plot.title,
       x = x.title, y = y.title) +
  scale_colour_manual(values=papercols,guide = "none")+
  scale_shape_manual(values=agentshape,guide = "none" )+
  scale_x_continuous(breaks = breaks,labels = labels,limits = c(0,1.25))+
  scale_y_continuous(breaks = breaks,labels = labels,limits = c(NA,1.25))+
  theme_classic()+
  theme(axis.line = element_line(linewidth = 0.85),
        axis.text = element_text(colour="black", size = 16),
        axis.title = element_text(colour = "black",size = 18),
        text = element_text(size = 18))+
  annotate("text", x = 0.2, y = 1.1, size = 6, label = glue("r = {round(c37$estimate,2)}, p = {round(c37$p.value ,4)}"))


# day 5 vs Day 7 plot -----------------------------------------------------

plot.title <- "Day 5 and 7"
x.title <- "Excess clearance to day 5"
y.title <- "Excess clearance to day 7"

plot57 <- ggplot(subset(correl.df,!is.na(trteffect_d5)&!is.na(trteffect_d7)),aes(x = trteffect_d5, y = trteffect_d7, colour=paper,shape=agent)) +
  geom_point(size = 4, stroke=1.5) + 
  labs(title = plot.title,
       x = x.title, y = y.title) +
  scale_colour_manual(values=papercols,guide = "none")+
  scale_shape_manual(values=agentshape,guide = "none" )+
  scale_x_continuous(breaks = breaks,labels = labels,limits = c(0,1.25))+
  scale_y_continuous(breaks = breaks,labels = labels,limits = c(0,1.25))+
  theme_classic()+
  theme(axis.line = element_line(linewidth = 0.85),
        axis.text = element_text(colour="black", size = 16),
        axis.title = element_text(colour = "black",size = 18),
        text = element_text(size = 18))+
  annotate("text", x = 0.2, y = 1.1,size = 6, label = glue("r = {round(c57$estimate,2)}, p = {round(c57$p.value ,2)}"))

# Plot to make legend...

correl.df  <- correl.df |> 
  mutate(first = coalesce(trteffect_d3,trteffect_d5,trteffect_d7))

plot.legend <- ggplot(data=subset(correl.df))+
  geom_point(size= 4,alpha = 1,aes(x=first, y=first,colour=paper,shape=agent)) +
  scale_colour_manual(values = papercols,
                      labels = paperlabels,
                      name = "Study")+
  scale_shape_manual( values = agentshape,
                      labels = agentlabels,
                      name = "Agent")+
  theme_classic()+
  theme(legend.direction = "vertical", 
        legend.box = "horizontal",
        legend.text = element_text(size=16),
        legend.key.height = unit(1.1,"cm"),
        text = element_text(size = 18)
  )+
  guides(col = guide_legend(keyheight = unit(0.5,"cm")))


# build 2x2 panel plot ----------------------------------------------------

#get legend from legend plot
legend<-get_legend(plot.legend + theme(legend.box.margin = margin(0,0,0,0)))

#build up 2x2 panel. Put a row of small NULL plots in between for spacing
panel <- plot_grid(plot35+theme(legend.position = "none",plot.margin = margin(0,10,3,0)),
                       plot37+theme(plot.margin = margin(0,10,3,0)), 
                       NULL,
                       NULL,
                       plot57+theme(plot.margin = margin(0,10,3,0)),
                       legend, 
                       labels = c("A","B","","","C",""),
                       label_size = 18,
                       ncol=2,rel_widths = c(1,1),rel_heights = c(1,0.05,1))


#save without title
ggsave(file='output/figureS3.pdf',panel,width=17,height=15)
ggsave(file='output/figureS3.png',panel,width=17,height=15)



# Compare day 3 and 5 models using common data ----------------------------

 
#start from 3 day dataframe, then cbind day 5 virology and delete any with missing data
compare.day.df <- unvax.day3 |> 
  select(paper,treatment,group,anytreat,trttype,n.virol,numevents,pcb.events,pcb.atrisk,numatrisk,day,trteffect,sem.diff) |> 
  rename(trteffect_d3 = trteffect,
         sem.diff_d3 = sem.diff) |> 
  filter(anytreat==1)

tmp <- unvax.day5 |> 
  filter(anytreat==1) |> 
  select(paper,group,trteffect,sem.diff) |> 
  rename(trteffect_d5 = trteffect,
         sem.diff_d5 = sem.diff)

compare.day.df <- compare.day.df |> 
  left_join(tmp) |> 
  filter(!is.na(trteffect_d3)&!is.na(trteffect_d5))


#model at day 3
compare.day.df <- escalc(measure="RR", ai=numevents, bi=numatrisk - numevents,
                ci=pcb.events, di=pcb.atrisk - pcb.events, data=compare.day.df)

subset.d3 <- rma.mv(yi, vi, mods = ~ trteffect_d3, method="ML", random = ~ 1 | paper, data=compare.day.df)


subset.results.d3 <- as.data.frame(cbind(summary(subset.d3)$beta,
                                  summary(subset.d3)$se,
                                  summary(subset.d3)$zval,
                                  summary(subset.d3)$pval,
                                  exp(summary(subset.d3)$beta),
                                  exp(summary(subset.d3)$ci.lb),
                                  exp(summary(subset.d3)$ci.ub),
                                  summary(subset.d3)$fit.stats[3,2]))




subset.results.d3 <- rownames_to_column(subset.results.d3,var = "variable")
colnames(subset.results.d3)<- c("variable","Beta est","Std. Err","z value","p value","RR","low.ci","upp.ci","AIC")

#model at day 5

subset.d5 <- rma.mv(yi, vi, mods = ~ trteffect_d5, method="ML", random = ~ 1 | paper, data=compare.day.df)


subset.results.d5 <- as.data.frame(cbind(summary(subset.d5)$beta,
                                  summary(subset.d5)$se,
                                  summary(subset.d5)$zval,
                                  summary(subset.d5)$pval,
                                  exp(summary(subset.d5)$beta),
                                  exp(summary(subset.d5)$ci.lb),
                                  exp(summary(subset.d5)$ci.ub),
                                  summary(subset.d5)$fit.stats[3,2]))




subset.results.d5 <- rownames_to_column(subset.results.d5,var = "variable")
colnames(subset.results.d5)<- c("variable","Beta est","Std. Err","z value","p value","RR","low.ci","upp.ci","AIC")


list_of_datasets <- list("day3" = subset.results.d3, "day5" = subset.results.d5)
write.xlsx(list_of_datasets, file = "output/common-data-subset-models.xlsx")


# clean out environment ---------------------------------------------------

rm("list_of_datasets","tmp")
rm("plot35","plot37","plot57")
rm("subset.d3","subset.d5")
rm("subset.results.d3","subset.results.d5")
rm("c35","c37","c57")
rm("compare.day.df","correl.df")
rm("legend","panel")
rm("breaks","labels","plot.title","plot.legend")
rm("x.title","y.title")

