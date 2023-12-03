
# AUC plot ----------------------------------------------------------------


# OBSERVED values plot data

#filter down to one row for each included study
plotdata.auc.obs <- composite.model |> 
  select(paper,firstauthor,auc,anytreat,efficacy,ci.eff.low,ci.eff.upp,agent) |> 
  filter(anytreat==1)

# divide all negative values for both point estimates and lower bounds of CIs

#y values - divide negative values by 50
plotdata.auc.obs$efficacy[plotdata.auc.obs$efficacy<0] <- plotdata.auc.obs$efficacy[plotdata.auc.obs$efficacy<0]/50
plotdata.auc.obs$ci.eff.low[plotdata.auc.obs$ci.eff.low<0] <- plotdata.auc.obs$ci.eff.low[plotdata.auc.obs$ci.eff.low<0]/50

# x values - divide by 10
plotdata.auc.obs$auc[plotdata.auc.obs$auc<0] <- plotdata.auc.obs$auc[plotdata.auc.obs$auc<0]/10
#plotdata.auc.obs$ci.hor.low[!is.na(plotdata.auc.obs$ci.hor.low) & plotdata.auc.obs$ci.hor.low<0] <- plotdata.auc.obs$ci.hor.low[!is.na(plotdata.auc.obs$ci.hor.low) & plotdata.auc.obs$ci.hor.low<0]/10


# MODEL values plot data

plotdata.auc.model <- effpred.auc

# suppress model fit and CI plots for negative efficacies
plotdata.auc.model$eff[plotdata.auc.model$eff<0] <- NA
plotdata.auc.model$eff.low[plotdata.auc.model$eff.low<0] <- 0


#generate plot with observed and model values

x.title <- "Time weighted excess drop in VL"

#set breaks if base10:

x.breaks <- c(-0.2, seq(0,4.5,0.5))
x.labels <- c("-2", seq(0,4.5,0.5))
x.shade1 <- as.data.frame(seq(-0.2,0,length.out = 1e2))
names(x.shade1)<-"x"



plot.auc.drop <- ggplot(data=subset(plotdata.auc.obs))+
  geom_point(size= 4,alpha = 1,aes(x=auc, y=efficacy*100,colour=paper,shape = agent)) +
  labs(x=x.title,y="Efficacy (%)")+
  scale_y_continuous(minor_breaks = c(seq(-10,-2,2),seq(0, 100, 20)), breaks=c(seq(-10,-2,2),seq(0, 100, 20)),
                     labels=c("-500","","","","",seq(0, 100, 20)),expand = expansion(mult = c(0,.05)))+
  scale_x_continuous(minor_breaks = x.breaks, breaks=x.breaks,
                     labels=x.labels,expand = expansion(mult = c(0,0.01)))+
  scale_colour_manual(values=papercols,labels = paperlabels, name = "Study")+
  scale_shape_manual(values=agentshape, labels = agentlabels, name = "Agent")+
  geom_errorbar(aes(x=auc, y=efficacy*100,ymin=ci.eff.low*100,ymax=ci.eff.upp*100,colour=paper),linewidth = 0.65, width=0.02) +
  geom_line(data=(plotdata.auc.model),colour="purple3", aes(x=X.auc,y=100*(eff)))+
  geom_ribbon(data=plotdata.auc.model,aes(x=X.auc, ymax=100*(plotdata.auc.model[,10]), ymin=100*(plotdata.auc.model[,9])), fill="purple3", alpha=0.2) +
  geom_ribbon(data=plotdata.auc.model,aes(x=X.auc,ymax=0, ymin=-Inf), fill="grey", alpha=0.5)+
  geom_ribbon(data=x.shade1,aes(x=x,ymax=Inf, ymin=-Inf), fill="grey", alpha=0.5)+
  theme_classic()+
  theme(axis.line = element_line(linewidth = 0.8),
        axis.text = element_text(colour="black", size = 16),
        axis.title = element_text(colour = "black", size = 18),
        text = element_text(size = 18),
        legend.direction = "vertical", 
        legend.box = "horizontal",
        legend.text = element_text(size=16),
        legend.key.height = unit(1.1,"cm"),
  )+
  guides(col = guide_legend(ncol = 1,keyheight = unit(0.5,"cm")))+
  annotate("text", x = 2.1, y = 95,size = 5, label = glue("RR = {round(results.auc[2,6],2)}, p = {round(results.auc[2,5],3)}"))+
  annotate("text",x=-0.2,y=0,size = 6, label = "=", angle = 20)+
  annotate("text",x= 0,y=-11,size = 6, label = "=", angle = 70)+
  coord_cartesian(clip = "off")

#plot.auc.drop

ggsave(file='output/figureS5.pdf',plot.auc.drop,width=18,height=10)
ggsave(file='output/figureS5.png',plot.auc.drop,width=18,height=10)




# clean out environment ---------------------------------------------------


rm(x.shade1, x.breaks,x.labels,x.title,plot.auc.drop,plotdata.auc.model)
