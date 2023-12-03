
# 3 day plot --------------------------------------------------------------

# OBSERVED values plot data

#filter down to one row for each included study
plotdata.3day.obs <- unvax.day3 |> 
  select(paper,firstauthor,trteffect,anytreat,efficacy,ci.eff.low,ci.eff.upp,sem.diff,agent) |> 
  filter(anytreat==1)

#generate actual lower and upper bounds for horizontal CIs
plotdata.3day.obs$ci.hor.low <- (plotdata.3day.obs$trteffect-1.96*plotdata.3day.obs$sem.diff)
plotdata.3day.obs$ci.hor.upp <- (plotdata.3day.obs$trteffect+1.96*plotdata.3day.obs$sem.diff)

# divide all negative values for both point estimates and lower bounds of CIs

#y axis compression - divide all negative values by 50
plotdata.3day.obs$efficacy[plotdata.3day.obs$efficacy<0] <- plotdata.3day.obs$efficacy[plotdata.3day.obs$efficacy<0]/50
plotdata.3day.obs$ci.eff.low[plotdata.3day.obs$ci.eff.low<0] <- plotdata.3day.obs$ci.eff.low[plotdata.3day.obs$ci.eff.low<0]/50

# x axis compression - divide all negative values by 10
plotdata.3day.obs$trteffect[plotdata.3day.obs$trteffect<0] <- plotdata.3day.obs$trteffect[plotdata.3day.obs$trteffect<0]/10
plotdata.3day.obs$ci.hor.low[!is.na(plotdata.3day.obs$ci.hor.low) & plotdata.3day.obs$ci.hor.low<0] <- plotdata.3day.obs$ci.hor.low[!is.na(plotdata.3day.obs$ci.hor.low) & plotdata.3day.obs$ci.hor.low<0]/10

#shift trteffect by .0005 for OTH07 so vert CI is visible for both it and AV05
plotdata.3day.obs$trteffect[plotdata.3day.obs$paper=="OTH07"] <- 0.005


# MODEL values plot data

#put metafor prediction output into dataframe name used by plots
plotdata.3day.model <- effpred.d3

# suppress model fit and CI plots for negative efficacies
plotdata.3day.model$eff[plotdata.3day.model$eff<0] <- NA
plotdata.3day.model$eff.low[plotdata.3day.model$eff.low<0] <- 0

day = 3
x.title <- glue("Virological effect of treatment to day {day} (log10 copies/mL)")

#set breaks if base10:
x.breaks <- c((-0.05),seq(0,1.0,0.25))
x.labels <- c("-0.5",seq(0,1.0,0.25))
x.shade1 <- as.data.frame(seq(-0.05,0,length.out = 1e2))
names(x.shade1)<-"x"

#draw plot
plot.3day.drop <- ggplot(data=subset(plotdata.3day.obs))+
  geom_point(size= 4,alpha = 1,aes(x=trteffect , y=efficacy*100,colour=paper,shape = agent)) +
  labs(title=paste( "Day 3" ),
       x=x.title,y="Efficacy (%)")+
  scale_y_continuous(minor_breaks = c(seq(-10,-2,2),seq(0, 100, 20)), breaks=c(seq(-10,-2,2),seq(0, 100, 20)),
                     labels=c("-500","","","","",seq(0, 100, 20)),expand = expansion(mult = c(0.01,0.05)))+
  scale_x_continuous(minor_breaks = x.breaks, breaks=x.breaks,
                     labels=x.labels,expand = expansion(mult = c(0,0)))+
  scale_colour_manual(values=papercols,guide = "none")+
  scale_shape_manual(values=agentshape, guide = "none")+
  geom_errorbar(aes(x=trteffect , y=efficacy*100,ymin=ci.eff.low*100,ymax=ci.eff.upp*100,colour=paper),linewidth = .65, width=0.05) +
  geom_errorbarh(aes(y=efficacy*100,xmin=ci.hor.low , xmax=ci.hor.upp , colour=paper),linewidth = .65,height=4) +
  geom_line(data=(plotdata.3day.model),colour="purple3", aes(x=X.trteffect ,y=100*(eff)))+
  geom_ribbon(data=plotdata.3day.model,aes(x=X.trteffect , ymax=100*(plotdata.3day.model[,10]), ymin=100*(plotdata.3day.model[,9])), fill="purple3", alpha=0.2) +
  geom_ribbon(data=plotdata.3day.model,aes(x=X.trteffect ,ymax=0, ymin=-Inf), fill="grey", alpha=0.5)+
  geom_ribbon(data=x.shade1,aes(x=x,ymax=Inf, ymin=-Inf), fill="grey", alpha=0.5)+
  theme_classic()+
  theme(legend.direction = "vertical", 
        legend.box = "horizontal",
        legend.text = element_text(size=16),
        axis.text = element_text(colour = "black",size = 16),
        axis.line = element_line(linewidth = 0.85),
        axis.title = element_text(colour = "black",size = 18),
        text = element_text(size = 18)
        )+
  annotate("text", x = 0.75, y = 30, size = 6, label = glue("RR = {round(results.d3[2,6],2)}, p = {round(results.d3[2,5],4)}"))+
  annotate("text",x=-0.05,y=0,size = 6, label = "=", angle = 20)+
  annotate("text",x= 0,y=-11,size = 6, label = "=", angle = 70)+
  coord_cartesian(clip = "off")



# 5 day plot --------------------------------------------------------------

day = 5
x.title <- glue("Virological effect of treatment to day {day} (log10 copies/mL)")


# OBSERVED values plot data

#filter down to one row for each included study
plotdata.5day.obs <- unvax.day5 |> 
  select(paper,firstauthor,trteffect,anytreat,efficacy,ci.eff.low,ci.eff.upp,sem.diff,agent) |> 
  filter(anytreat==1)

#generate actual lower and upper bounds for horizontal CIs
plotdata.5day.obs$ci.hor.low <- (plotdata.5day.obs$trteffect-1.96*plotdata.5day.obs$sem.diff)
plotdata.5day.obs$ci.hor.upp <- (plotdata.5day.obs$trteffect+1.96*plotdata.5day.obs$sem.diff)

#y axis compression - divide all negative values by 50
plotdata.5day.obs$efficacy[plotdata.5day.obs$efficacy<0] <- plotdata.5day.obs$efficacy[plotdata.5day.obs$efficacy<0]/50
plotdata.5day.obs$ci.eff.low[plotdata.5day.obs$ci.eff.low<0] <- plotdata.5day.obs$ci.eff.low[plotdata.5day.obs$ci.eff.low<0]/50

# x axis compression - divide all negative values by 10
plotdata.5day.obs$trteffect[plotdata.5day.obs$trteffect<0] <- plotdata.5day.obs$trteffect[plotdata.5day.obs$trteffect<0]/10
plotdata.5day.obs$ci.hor.low[!is.na(plotdata.5day.obs$ci.hor.low) & plotdata.5day.obs$ci.hor.low<0] <- plotdata.5day.obs$ci.hor.low[!is.na(plotdata.5day.obs$ci.hor.low) & plotdata.5day.obs$ci.hor.low<0]/10

# switch lower CI for Gupta from -0.002 to +0.002 so end is visible
plotdata.5day.obs$ci.hor.low[plotdata.5day.obs$paper == "MAB15"] <- 0.005


#put metafor prediction output into dataframe name used by plots
plotdata.5day.model <- effpred.d5


# suppress model fit and CI plots for negative efficacies
plotdata.5day.model$eff[plotdata.5day.model$eff<0] <- NA
plotdata.5day.model$eff.low[plotdata.5day.model$eff.low<0] <- 0


#set breaks etc if base10:
x.breaks <- c(seq(0,1.5,0.5))
x.labels <- c(seq(0,1.5,0.5))

#draw plot
plot.5day.drop <- ggplot(data=subset(plotdata.5day.obs))+
  geom_point(size= 4,alpha = 1,aes(x=trteffect , y=efficacy*100,colour=paper,shape = agent)) +
  labs(title=paste( "Day 5" ),
       x=x.title,y="")+
  scale_y_continuous(minor_breaks = c(seq(-10,-2,2),seq(0, 100, 20)), breaks=c(seq(-10,-2,2),seq(0, 100, 20)),
                     labels=c("-500","","","","",seq(0, 100, 20)),expand = expansion(mult = c(0.01,0.05)))+
  scale_x_continuous(minor_breaks = x.breaks, breaks=x.breaks,
                     labels=x.labels,expand = expansion(mult = c(0,0)))+
  scale_colour_manual(values=papercols,guide = "none")+
  scale_shape_manual(values=agentshape,guide="none")+
  geom_errorbar(aes(x=trteffect , y=efficacy*100,ymin=ci.eff.low*100,ymax=ci.eff.upp*100,colour=paper),linewidth = 0.65, width=0.05) +
  geom_errorbarh(aes(y=efficacy*100,xmin=ci.hor.low , xmax=ci.hor.upp , colour=paper),linewidth = 0.65, height=4) +
  geom_line(data=(plotdata.5day.model),colour="purple3", aes(x=X.trteffect ,y=100*(eff)))+
  geom_ribbon(data=plotdata.5day.model,aes(x=X.trteffect , ymax=100*(plotdata.5day.model[,10]), ymin=100*(plotdata.5day.model[,9])), fill="purple3", alpha=0.2)+
  geom_ribbon(data=plotdata.5day.model,aes(x=X.trteffect ,ymax=0, ymin=-Inf), fill="grey", alpha=0.5)+
 # geom_ribbon(data=x.shade2,aes(x=x,ymax=Inf, ymin=-Inf), fill="grey", alpha=0.5)+
  theme_classic()+
  theme(axis.text = element_text(colour = "black", size = 16),
        axis.line = element_line(linewidth = 0.85),
        axis.title = element_text(colour = "black",size = 18),
        text = element_text(size = 18)
  )+
  annotate("text", x = 1.25, y = 30,size = 6, label = glue("RR = {round(results.d5[2,6],2)}, p = {round(results.d5[2,5],3)}"))+
  annotate("text",x=0,y=0,size = 6, label = "=", angle = 20)+
  #annotate("text",x= 0,y=-12,size = 6, label = "=", angle = 70)+
  coord_cartesian(clip = "off")
  

# 7 day plot --------------------------------------------------------------

day = 7
x.title <- glue("Virological effect of treatment to day {day} (log10 copies/mL)")


# OBSERVED values plot data

#filter down to one row for each included study
plotdata.7day.obs <- unvax.day7 |> 
  select(paper,firstauthor,trteffect,anytreat,efficacy,ci.eff.low,ci.eff.upp,sem.diff,agent) |> 
  filter(anytreat==1)

#generate actual lower and upper bounds for horizontal CIs
plotdata.7day.obs$ci.hor.low <- (plotdata.7day.obs$trteffect-1.96*plotdata.7day.obs$sem.diff)
plotdata.7day.obs$ci.hor.upp <- (plotdata.7day.obs$trteffect+1.96*plotdata.7day.obs$sem.diff)

#y axis compression - divide all negative values by 50
plotdata.7day.obs$efficacy[plotdata.7day.obs$efficacy<0] <- plotdata.7day.obs$efficacy[plotdata.7day.obs$efficacy<0]/50
plotdata.7day.obs$ci.eff.low[plotdata.7day.obs$ci.eff.low<0] <- plotdata.7day.obs$ci.eff.low[plotdata.7day.obs$ci.eff.low<0]/50

# x axis compression - divide all negative values by 5
plotdata.7day.obs$trteffect[plotdata.7day.obs$trteffect<0] <- plotdata.7day.obs$trteffect[plotdata.7day.obs$trteffect<0]/5
plotdata.7day.obs$ci.hor.low[!is.na(plotdata.7day.obs$ci.hor.low) & plotdata.7day.obs$ci.hor.low<0] <- plotdata.7day.obs$ci.hor.low[!is.na(plotdata.7day.obs$ci.hor.low) & plotdata.7day.obs$ci.hor.low<0]/5

#shift lower horizontal CI by .0005 for OTH07 so end doesn't hide behind axis
plotdata.7day.obs$ci.hor.low[plotdata.7day.obs$paper=="OTH07"] <- plotdata.7day.obs$ci.hor.low[plotdata.7day.obs$paper=="OTH07"]+0.005


#put metafor prediction output into dataframe name used by plots
plotdata.7day.model <- effpred.d7


# suppress model fit and CI plots for negative efficacies
plotdata.7day.model$eff[plotdata.7day.model$eff<0] <- NA
plotdata.7day.model$eff.low[plotdata.7day.model$eff.low<0] <- 0


#generate plot with observed and model values

#set breaks etc if base10:
x.breaks <- c((-0.2),seq(0,1.5,0.5))
x.labels <- c("-2.0",seq(0,1.5,0.5))
x.shade2 <- as.data.frame(seq(-0.2,0,length.out = 1e2))
names(x.shade2)<-"x"


#draw plot
plot.7day.drop <- ggplot(data=subset(plotdata.7day.obs))+
  geom_point(size= 4,alpha = 1,aes(x=trteffect , y=efficacy*100,colour=paper,shape=agent)) +
  labs(title=paste( "Day 7" ),
       x=x.title,y="")+
  scale_y_continuous(minor_breaks = c(seq(-10,-2,2),seq(0, 100, 20)), breaks=c(seq(-10,-2,2),seq(0, 100, 20)),
                     labels=c("-500","","","","",seq(0, 100, 20)),expand = expansion(mult = c(0.01,.05)))+
  scale_x_continuous(minor_breaks = x.breaks, breaks=x.breaks,
                     labels=x.labels,expand = expansion(mult = c(0,0)))+
  scale_colour_manual(values=papercols, guide = "none")+
  scale_shape_manual(values=agentshape, guide = "none")+
  geom_errorbar(aes(x=trteffect , y=efficacy*100,ymin=ci.eff.low*100,ymax=ci.eff.upp*100,colour=paper),linewidth = 0.65, width=0.05) +
  geom_errorbarh(aes(y=efficacy*100,xmin=ci.hor.low , xmax=ci.hor.upp , colour=paper),linewidth = 0.65, height=4) +
  geom_line(data=(plotdata.7day.model),colour="purple3", aes(x=X.trteffect ,y=100*(eff)))+
  geom_ribbon(data=plotdata.7day.model,aes(x=X.trteffect , ymax=100*(plotdata.7day.model[,10]), ymin=100*(plotdata.7day.model[,9])), fill="purple3", alpha=0.2)+
  geom_ribbon(data=plotdata.7day.model,aes(x=X.trteffect ,ymax=0, ymin=-Inf), fill="grey", alpha=0.5)+
  geom_ribbon(data=x.shade2,aes(x=x,ymax=Inf, ymin=-Inf), fill="grey", alpha=0.5)+
  theme_classic()+
  theme(axis.text = element_text(colour = "black",size = 16),
        axis.line = element_line(linewidth = 0.85),
        axis.title = element_text(colour = "black",size = 18),
        text = element_text(size = 18)
  )+
  annotate("text", x = 1.4, y = 30, size = 6, label = glue("RR = {round(results.d7[2,6],2)}, p = {round(results.d7[2,5],3)}"))+
  annotate("text",x=-0.2,y=0,size = 6, label = "=", angle = 20)+
  annotate("text",x= 0,y=-12,size = 6, label = "=", angle = 70)+
  coord_cartesian(clip = "off")



# Plot to make legend...
agg.tmp <- rbind(plotdata.3day.obs,plotdata.5day.obs,plotdata.7day.obs)

agg.tmp <- agg.tmp |> 
  group_by(paper,agent) |> 
  slice_head(n = 1) |> 
  ungroup()
  

plot.legend <- ggplot(data=subset(agg.tmp))+
  geom_point(size= 4,alpha = 1,aes(x=trteffect , y=efficacy*100,colour=paper,shape=agent)) +
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



# daily models 2x2 plot grid ----------------------------------------------


#get legend from legend plot
legend<-get_legend(plot.legend + theme(legend.box.margin = margin(0,0,0,0)))

#build up 2x2 panel. Put a row of small NULL plots in between for spacing
panel <- plot_grid(plot.3day.drop+theme(legend.position = "none",plot.margin = margin(0,10,3,0)),
                       plot.5day.drop+theme(plot.margin = margin(0,10,3,0)), 
                       NULL,
                       NULL,
                       plot.7day.drop+theme(plot.margin = margin(0,10,3,0)),
                       legend, 
                       labels = c("A","B","","","C",""),
                       label_size = 18,
                       ncol=2,rel_widths = c(1,1),rel_heights = c(1,0.05,1))
                 
                     

#save panel plot
ggsave(file='output/figure4.pdf',panel,width=17,height=15)
ggsave(file='output/figure4.png',panel,width=17,height=15)



# clean out environment ---------------------------------------------------

rm(legend,panel,x.shade1,x.shade2,plot.3day.drop,plot.5day.drop,plot.7day.drop)
rm(x.breaks,x.labels,x.title,agg.tmp,plot.legend)
