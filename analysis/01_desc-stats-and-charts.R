
# Calc efficacy and CIs overall and by treat type -------------------------



overall.eff <- unvax.day1 |> 
  filter(anytreat==1)

overall.eff <- escalc(measure="RR", ai=numevents, bi=numatrisk - numevents,
                ci=pcb.events, di=pcb.atrisk - pcb.events, data=overall.eff)


#use these RRs in rma.mv

overall.model <- rma.mv(yi, vi, method="ML", random = ~ 1 | paper, data=overall.eff)

# Calculate I^2
overall.model.i2 <- calc.i2(overall.model)



#Collate results
results.overall <- as.data.frame(cbind(summary(overall.model)$beta,
                                  summary(overall.model)$se,
                                  summary(overall.model)$zval,
                                  summary(overall.model)$pval,
                                  exp(summary(overall.model)$beta),
                                  exp(summary(overall.model)$ci.lb),
                                  exp(summary(overall.model)$ci.ub)))




results.overall <- rownames_to_column(results.overall,var = "variable")
colnames(results.overall)<- c("variable","Beta est","Std. Err","z value","p value","RR","low.ci","upp.ci")

results.overall$eff <- 1- results.overall$RR
results.overall$eff.low <- 1 - results.overall$upp.ci
results.overall$eff.upp <- 1 - results.overall$low.ci


# Absolute risk from stratified model


# a.) mabs
#old glmer model (just used to get abs risk in each group for figure 2)
mab.out <-
  glmer(
    cbind(numevents, numatrisk - numevents) ~ anytreat + (1 | paper),
    data = subset(unvax.day1, treatment.type == "mAb"),
    family = binomial,
    na.action = na.omit
  )

#new rma.mv model

mab.model <- rma.mv(yi, vi, method="ML", random = ~ 1 | paper, data=subset(overall.eff, treatment.type == "mAb"))


#Collate results
results.mab <- as.data.frame(cbind(summary(mab.model)$beta,
                                       summary(mab.model)$se,
                                       summary(mab.model)$zval,
                                       summary(mab.model)$pval,
                                       exp(summary(mab.model)$beta),
                                       exp(summary(mab.model)$ci.lb),
                                       exp(summary(mab.model)$ci.ub)))




results.mab <- rownames_to_column(results.mab,var = "variable")
colnames(results.mab)<- c("variable","Beta est","Std. Err","z value","p value","RR","low.ci","upp.ci")

results.mab$eff <- 1- results.mab$RR
results.mab$eff.low <- 1 - results.mab$upp.ci
results.mab$eff.upp <- 1 - results.mab$low.ci



#abs risk in controls and treated - used in plot
mab.ctrl <- odds.to.prob(exp(fixef(mab.out)[1]))
mab.trt <- odds.to.prob(exp(fixef(mab.out)[1] + fixef(mab.out)[2]))


# b.) smmols

#old glmer model (just used to get abs risk in each group for Figure 2 black line)
smmol.out <-
  glmer(
    cbind(numevents, numatrisk - numevents) ~  anytreat + (1 | paper),
    data = subset(unvax.day1, treatment.type != "mAb"),
    family = binomial,
    na.action = na.omit
  )

#new rma.mv model
smmol.model <- rma.mv(yi, vi, method="ML", random = ~ 1 | paper, data=subset(overall.eff, treatment.type != "mAb"))


#Collate results
results.smmol <- as.data.frame(cbind(summary(smmol.model)$beta,
                                   summary(smmol.model)$se,
                                   summary(smmol.model)$zval,
                                   summary(smmol.model)$pval,
                                   exp(summary(smmol.model)$beta),
                                   exp(summary(smmol.model)$ci.lb),
                                   exp(summary(smmol.model)$ci.ub)))




results.smmol <- rownames_to_column(results.smmol,var = "variable")
colnames(results.smmol)<- c("variable","Beta est","Std. Err","z value","p value","RR","low.ci","upp.ci")

results.smmol$eff <- 1- results.smmol$RR
results.smmol$eff.low <- 1 - results.smmol$upp.ci
results.smmol$eff.upp <- 1 - results.smmol$low.ci



#abs risk in controls and treated - used in plot
smmol.ctrl <- odds.to.prob(exp(fixef(smmol.out)[1]))
smmol.trt <-
  odds.to.prob(exp(fixef(smmol.out)[1] + fixef(smmol.out)[2]))



#save result output
list_of_datasets <- list("mab" = results.mab, "smmol" = results.smmol,"overall" = results.overall)
write.xlsx(list_of_datasets, file = "output/efficacy-results.xlsx")



# Figure 2 absolute risk plot ---------------------------------------------



#build temp data for abs risk plot
temp.data <- unvax.day1

# use paper2 variable to handle multiple treatment arms in MAB06 and MAB08
temp.data <- temp.data  |>
  mutate(paper2 = paper) |>
  filter(paper %in% c("MAB06", "MAB08") & group == "Placebo") |>
  mutate(paper2 = paste0(paper, "a")) |>
  bind_rows(temp.data) |>
  mutate(paper2 = ifelse((paper == "MAB06" & group == "BAM Comb dose"),
                         paste0(paper, "a"), paper2)) |>
  mutate(paper2 = ifelse((paper == "MAB08" & group == "BEB"), 
                         paste0(paper, "a"), paper2)) |>
  mutate(paper2 = ifelse(is.na(paper2), as.character(paper), paper2))



#join on rows to take data for aggregate black line info
temp.data <- temp.data |>
  add_row(
    paper = "AABLACK",
    paper2 = "AABLACK",
    anytreat = "0",
    treatment.type = "mAb",
    absrisk = mab.ctrl
  ) |>
  add_row(
    paper = "AABLACK",
    paper2 = "AABLACK",
    anytreat = "1",
    treatment.type = "mAb",
    absrisk = mab.trt
  ) |>
  add_row(
    paper = "AABLACK",
    paper2 = "AABLACK",
    anytreat = "0",
    treatment.type = "Small mol",
    absrisk = smmol.ctrl
  ) |>
  add_row(
    paper = "AABLACK",
    paper2 = "AABLACK",
    anytreat = "1",
    treatment.type = "Small mol",
    absrisk = smmol.trt
  )

#use agent2 variable to map correct agent shape for placebo arms
temp.data <- temp.data |>
  mutate(agent2 = ifelse(agent == "Placebo", NA, agent)) |> 
  group_by(paper2) |>
  fill(agent2, .direction = "downup") |>
  ungroup()




# Panel per treatment type plot -------------------------------------------


#labels for facet wrap
trt.labs <- c("Passive antibodies", "Small molecules")
names(trt.labs) <- c("mAb", "Small mol")

plot.absrisk.trt <- ggplot(data = subset(temp.data,),
        aes(
           x = anytreat,
           y = absrisk * 100,
           colour = paper,
           shape = agent2
         )) +
  geom_point(size = 5,stroke=1.2) +
  geom_point(data = filter(temp.data, paper == "AABLACK"), size = 5,shape = 19) +
  geom_line(linewidth = 0.75,aes(group = paper2)) +
  geom_line(data = filter(temp.data, paper == "AABLACK"),linewidth = 1.0,aes(group = paper)) +
  scale_colour_manual(
    values = c(papercols),
    labels = c(paperlabels),
    name = "Study"
  ) +
  scale_shape_manual(
    values = agentshape,
    labels = agentlabels,
    name = "Agent"
  ) +
  labs(x = "Group", y = "Absolute risk of progression (%)") +
  scale_x_discrete(labels = c("Control", "Treatment")) +
  theme_classic()+
  theme(
    panel.border = element_rect(linewidth = NA, fill = NA),
    text = element_text(size = 24),
    strip.background = element_rect(linewidth = NA, fill = NA),
    strip.text = element_text(hjust=0.5,vjust=0,size=24),
    axis.line = element_line(linewidth = 0.95),
    axis.title = element_text(colour = "black", size = 24),
    axis.text = element_text(colour = "black", size = 24),
    panel.spacing.x = unit(-0.5,"cm"),
    legend.direction = "vertical",
    legend.box = "horizontal",
    legend.text = element_text(size=20),
    legend.key.height = unit(1.2,"cm")
  ) +
  guides(col = guide_legend(ncol = 1,keyheight = unit(0.5,"cm")))+
  facet_rep_wrap( ~ treatment.type,
              ncol = 4,
              labeller = labeller(treatment.type = trt.labs))


plot.absrisk.trt



ggsave(file = 'output/figure2.png',
       plot.absrisk.trt,
       width = 20,
       height = 9)
ggsave(file = 'output/figure2.pdf',
       plot.absrisk.trt,
       width = 20,
       height = 9)





# max drop by day X working -----------------------------------------------

tmp <- unvax.day7 |>
  filter(anytreat==1)


# Figure 3 VL drop over time by study -------------------------------------

tmp <- data.unvax |>
  distinct(paper, treatment)


#labels for facet wrap
fig3.labs <- c(tmp$treatment)
names(fig3.labs) <- c(tmp$paper)

fig3.labs["AV01"] <- "Nirmatrelvir +\nRitonivar"
fig3.labs["MAB04"] <- "Tixagevimab +\nCilgavimab"
fig3.labs["MAB06"] <- "Bamlanivimab +\nEtesevimab"
fig3.labs["MAB07"] <- "Bamlanivimab +\nEtesevimab"
fig3.labs["MAB13"] <- "Bamlanivimab +\nEtesevimab"
fig3.labs["MAB08"] <- "Bebtelovimab +\nBamlanivimab +\nEtesevimab"
fig3.labs["OTH07"] <- "Hydroxychloroquine"
fig3.labs["MAB03"] <- "Casirivimab +\nImdevimab"
fig3.labs["MAB02"] <- "Casirivimab +\nImdevimab"
fig3.labs["MAB16"] <- "Tixagevimab +\nCilgavimab"
fig3.labs["OTH09"] <- "Convalescent plasma"



plot.title <- "Drop in viral load (log10 copies/mL) from baseline for treatment and control arms of each study"
y.title <- "Change in log10 viral load"

#set breaks/labels for base 10
y.labels <- c(-5,-4,-3,-2,-1,0)



#set order for panels (mabs before smmols, longer text first within category )

order_vector<-c("MAB08","MAB07","MAB13","MAB06","MAB02","MAB03","MAB04","MAB16",
                "MAB14","MAB12","MAB17","MAB15","MAB20","MAB19","OTH09","AV01","OTH07","AV03","AV05","AV02","OTH05","AV11")
order_vector<-c(order_vector,levels(subset(data.unvax)$paper)[!(levels(subset(data.unvax)$paper) %in% order_vector)])
data.unvax$paper=factor(data.unvax$paper,levels=order_vector)
#this shows drop in vl at each observed timepoint by study

plot.vldrop.study <-
  ggplot(data = subset(data.unvax), aes(x = day,y = vlchange,colour = paper,shape = anytreat)) +
  geom_point(size=3) +
  geom_line(aes(linetype = anytreat, group = plotgroup),linewidth=1) +
  labs(#title = plot.title ,
       x = "Days", y = y.title) +
  scale_x_continuous(breaks = c(1, 3, 5, 7, 9,11)) +
  scale_y_continuous(labels = y.labels) +
  scale_shape_discrete(
    name = "Group",
    breaks = c(0, 1),
    labels = c("Placebo", "Treatment")
  ) +
  scale_linetype_discrete(
    name = "Group",
    breaks = c(0, 1),
    labels = c("Placebo", "Treatment")
  ) +
  scale_colour_manual(
    values = c(papercols),
    labels = c(paperlabels),
    name = "Study"
  ) +
  theme_classic()+
  theme(panel.border = element_rect(linewidth = NA, fill = NA),
        text = element_text(size = 18),
        strip.background = element_rect(linewidth=NA,fill=NA),
        strip.text = element_text(hjust=0.5,vjust=0,size=16),
        axis.text = element_text(color="black",size=16),
        axis.line = element_line(linewidth=0.85),
        axis.title = element_text(color="black",size=18),
        panel.spacing.y = unit(-1.3,"cm"),
        panel.spacing.x = unit(-0.5,"cm"),
        legend.box.margin = margin(t=-200),
        legend.text= element_text(size=16)
        ) +
  guides(col = guide_legend(ncol = 1))+
  facet_rep_wrap( ~ paper,
              ncol = 4,
              labeller = labeller(paper = fig3.labs))


#plot.vldrop.study

ggsave(file = 'output/figure3.png',
       plot.vldrop.study,
       width = 16,
       height = 14)

ggsave(file = 'output/figure3.pdf',
       plot.vldrop.study,
       width = 16,
       height = 14)






# Figure S2 Available timepoints - Venn diagram --------------------------------

day3 <- unvax.day3 |>
  distinct(paper)

day4 <- unvax.day4 |>
  distinct(paper)

day5 <- unvax.day5 |>
  distinct(paper)

day7 <- unvax.day7 |>
  distinct(paper)



vennlist <- list(
  "Day 3" = as.character(day3$paper),
  "Day 4" = as.character(day4$paper),
  "Day 5" = as.character(day5$paper),
  "Day 7" = as.character(day7$paper)
)

vennplot <-
  ggVennDiagram(
    vennlist,
    label = "count",
    label_size = 5,
    set_color = c("black", "blue", "red", "purple"),
    set_size = 5
  ) +
  scale_fill_gradient(low = "lightblue", high = "lightblue4") +
  scale_color_manual (values = c("black", "blue", "red", "purple"))


ggsave(file = 'output/figureS2.png',
       vennplot,
       width = 6,
       height = 4)
ggsave(file = 'output/figureS2.pdf',
       vennplot,
       width = 6,
       height = 4)




#max treat effect day working --------------------------------------------

tmp <- data.unvax |>
  filter(anytreat==1) |>
  select(paper, group,day, trteffect) |>
  group_by(paper,group) |>
  mutate(max_days = day[which.max(day)]) |>
  mutate(day_of_max = day[which.max(trteffect)]) |>
  ungroup()

t <- tmp |>
  distinct(paper,group,day_of_max)

# clean out environment ---------------------------------------------------


rm(day3,day4,day5,day7,temp.data,tmp,vennlist,vennplot,t)
rm(mab.out,smmol.out,mab.ctrl,mab.trt,smmol.ctrl,smmol.trt)
rm(plot.title,y.title,plot.absrisk.trt,plot.vldrop.study)
rm(list_of_datasets,fig3.labs,order_vector,y.labels,trt.labs)
rm(mab.model,overall.model,smmol.model,results.mab,results.overall,results.smmol)


