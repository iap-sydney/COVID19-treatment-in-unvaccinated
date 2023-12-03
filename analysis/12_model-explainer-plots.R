# model explainer using day 3 model ---------------------------------------

#calc data (base10) for 50% and 70% efficacy

# 50% efficacy
eff50.3day <- plotdata.3day.model[which(plotdata.3day.model$eff.low>0.5, arr.ind=TRUE)[1],] 
eff50.3day <- eff50.3day |> 
  select(X.trteffect:eff.high)
colnames(eff50.3day) <- c("x","y_est","y_low","y_upp")


# 70% efficacy
eff70.3day <- plotdata.3day.model[which(plotdata.3day.model$eff.low>0.7, arr.ind=TRUE)[1],]
eff70.3day <- eff70.3day |> 
  select(X.trteffect:eff.high)
colnames(eff70.3day) <- c("x","y_est","y_low","y_upp")


# plot for 3 day model ----------------------------------------------------



x.title <- "Virological effect of treatment (log10 copies/mL)"

#set breaks if base10:

x.breaks <- c(seq(0,1.0,0.25))
x.labels <- c(seq(0,1.0,0.25))


plot.3day.model <- ggplot(data=subset(plotdata.3day.model))+
  labs(title=paste( "Day 3" ),
       x=x.title,y="Efficacy (%)")+
  scale_y_continuous(minor_breaks = seq(0, 100, 20), breaks=c(seq(0, 100, 20)),
                     labels=c(seq(0, 100, 20)),expand = expansion(mult = c(0,0.10)))+
  scale_x_continuous(minor_breaks = x.breaks, breaks=x.breaks,
                     labels=x.labels,expand = expansion(mult = c(0,0)))+
  geom_line(data=(plotdata.3day.model),colour="purple3", aes(x=X.trteffect,y=100*(eff)))+
  geom_ribbon(data=plotdata.3day.model,aes(X.trteffect, ymax=100*(plotdata.3day.model[,10]), ymin=100*(plotdata.3day.model[,9])), fill="purple3", alpha=0.2) +
  # 50% efficacy
  geom_segment(x = 0, y = eff50.3day$y_low*100, xend = eff50.3day$x, yend = eff50.3day$y_low*100, linetype=2, colour = "black")+
  geom_segment(x = eff50.3day$x, y = 0, xend = eff50.3day$x, yend = eff50.3day$y_low*100,linetype = 2, colour = "black")+
  # 70% efficacy
  geom_segment(x = 0, y = eff70.3day$y_low*100, xend = eff70.3day$x, yend = eff70.3day$y_low*100, linetype=2, colour = "red")+
  geom_segment(x = eff70.3day$x, y = 0, xend = eff70.3day$x, yend = eff70.3day$y_low*100,linetype = 2, colour = "red")+
  theme_classic()+
  theme(axis.line = element_line(linewidth = 0.8),
        axis.text = element_text(colour="black", size = 16),
        axis.title = element_text(colour = "black", size = 18),
        text = element_text(size = 18)
        )
  


# model explainer using day 5 model ---------------------------------------


# calcs for 50% and 70% efficacy - 5 day model ----------------------------



# suppress model fit and CI plots for negative efficacies
effpred.d5$eff[effpred.d5$eff<0] <- NA
effpred.d5$eff.low[effpred.d5$eff.low<0] <- 0

# 50% efficacy
eff50.5day <- effpred.d5[which(effpred.d5$eff.low>0.5, arr.ind=TRUE)[1],] 
eff50.5day <- eff50.5day |> 
  select(X.trteffect:eff.high)
colnames(eff50.5day) <- c("x","y_est","y_low","y_upp")


# 70% efficacy
eff70.5day <- effpred.d5[which(effpred.d5$eff.low>0.7, arr.ind=TRUE)[1],] 
eff70.5day <- eff70.5day |> 
  select(X.trteffect:eff.high)
colnames(eff70.5day) <- c("x","y_est","y_low","y_upp")


# plot using 5 day model --------------------------------------------------



x.title <- "Virological effect of treatment (log10 copies/mL)"

#set breaks (base10):

x.breaks <- c(seq(0,2.25,0.5))
x.labels <- c(seq(0,2.25,0.5))


plot.5day.model <- ggplot(data=subset(effpred.d5))+
  labs(title=paste( "Day 5" ),
       x=x.title,y="")+
  scale_y_continuous(minor_breaks = seq(0, 100, 20), breaks=c(seq(0, 100, 20)),
                     labels=c(seq(0, 100, 20)),expand = expansion(mult = c(0,.05)))+
  scale_x_continuous(minor_breaks = x.breaks, breaks=x.breaks,
                     labels=x.labels,expand = expansion(mult = c(0,0)))+
  geom_line(data=(effpred.d5),colour="purple3", aes(x=X.trteffect,y=100*(eff))) +
  geom_ribbon(data=effpred.d5,aes(X.trteffect, ymax=100*(effpred.d5[,10]), ymin=100*(effpred.d5[,9])), fill="purple3", alpha=0.2) +
  # 50% efficacy
  geom_segment(x = 0, y = eff50.5day$y_low*100, xend = eff50.5day$x, yend = eff50.5day$y_low*100, linetype=2, colour = "black")+
  geom_segment(x = eff50.5day$x, y = 0, xend = eff50.5day$x, yend = eff50.5day$y_low*100,linetype = 2, colour = "black")+
  # 70% efficacy
  geom_segment(x = 0, y = eff70.5day$y_low*100, xend = eff70.5day$x, yend = eff70.5day$y_low*100, linetype=2, colour = "red")+
  geom_segment(x = eff70.5day$x, y = 0, xend = eff70.5day$x, yend = eff70.5day$y_low*100,linetype = 2, colour = "red")+
  theme_classic()+
  theme(axis.line = element_line(linewidth = 0.8),
        axis.text = element_text(colour="black", size = 16),
        axis.title = element_text(colour = "black", size = 18),
        text = element_text(size = 18)
  )




# build 2x1 panel plot ----------------------------------------------------

panel <- plot_grid(plot.3day.model + theme(plot.margin = margin(0,10,0,0)),plot.5day.model+theme(plot.margin = margin(0,10,0,0)), labels = "AUTO",label_size = 18, ncol=2,rel_widths = c(1,1))


#save panel plot
ggsave(file='output/figureS6.pdf',panel,width=15,height=6)
ggsave(file='output/figureS6.png',panel,width=15,height=6)

# clean out environment ---------------------------------------------------

rm(x.breaks,x.labels,x.title,panel,plot.3day.model,plot.5day.model)
rm(eff50.3day,eff50.5day,eff70.3day,eff70.5day)

