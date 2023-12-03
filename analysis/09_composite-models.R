
# Build composite model dataframe -----------------------------------------

#include all days <=7
composite.model <- data.unvax |>
  filter(day <=7) |>
  select(paper,firstauthor,treatment,group,anytreat,trttype,n.virol,numevents,numatrisk,pcb.events,pcb.atrisk,
         day,trteffect,efficacy,ci.eff.low,ci.eff.upp,sem.diff,agent,treatment.type,variant,treat.lag) |>
  pivot_wider(
    names_from = day,
    names_prefix = "d",
    names_vary = "slowest",
    values_from = c(trteffect,sem.diff)
  ) |>
  relocate(ends_with("_d2"),.after = sem.diff_d1) |>
  relocate(ends_with("_d3"),.after = sem.diff_d2) |>
  relocate(ends_with("_d4"),.after = sem.diff_d3) |>
  relocate(ends_with("_d5"),.after = sem.diff_d4) |>
  relocate(ends_with("_d6"),.after = sem.diff_d5)

#adds extra rows and sets up paper2 column to handle MAB06 and MAB08

composite.model <- composite.model  |>
  mutate(paper2 = paper) |>
  filter(paper %in% c("MAB06","MAB08") & group == "Placebo") |>
  mutate(paper2 = paste0(paper,"a")) |>
  bind_rows(composite.model) |>
  mutate(paper2 = ifelse((paper == "MAB06" & group == "BAM Comb dose"),paste0(paper,"a"),paper2)) |>
  mutate(paper2 = ifelse((paper == "MAB08" & group == "BEB"),paste0(paper,"a"),paper2)) |>
  mutate(paper2 = ifelse(is.na(paper2),as.character(paper),paper2)) |>
  arrange(paper2) |>
  relocate(paper2,.after=last_col())




# building dataframe for AUC model ----------------------------------------


auc.wide <- data.unvax |> 
  filter(day <=14) |> 
  select(paper,firstauthor,treatment,group,anytreat,trttype,n.virol,numevents,numatrisk,pcb.events,pcb.atrisk,
         day,trteffect,efficacy,ci.eff.low,ci.eff.upp,sem.diff,agent,treatment.type,variant,treat.lag) |> 
  pivot_wider(
    names_from = day,
    names_prefix = "d",
    names_vary = "slowest",
    values_from = c(trteffect,sem.diff)
  ) |> 
  relocate(ends_with("_d2"),.after = sem.diff_d1) |> 
  relocate(ends_with("_d3"),.after = sem.diff_d2) |>
  relocate(ends_with("_d4"),.after = sem.diff_d3) |>
  relocate(ends_with("_d5"),.after = sem.diff_d4) |>
  relocate(ends_with("_d6"),.after = sem.diff_d5) |> 
  relocate(ends_with("_d7"),.after = sem.diff_d6) |> 
  relocate(ends_with("_d8"),.after = sem.diff_d7) |> 
  relocate(ends_with("_d10"),.after = sem.diff_d8) |> 
  relocate(ends_with("_d11"),.after = sem.diff_d10) 

#populate 0 treat effect at day 7 for placebo rows
auc.wide$trteffect_d7[is.na(auc.wide$trteffect_d7) & auc.wide$anytreat==0] <- 0 

#populate day 7 trteffect for treated groups
auc.wide$trteffect_d7 <- ifelse(auc.wide$paper %in% c("AV01","AV03","AV05") & auc.wide$anytreat==1,(auc.wide$trteffect_d10 - auc.wide$trteffect_d5)*2/5 + auc.wide$trteffect_d5 ,
                        ifelse(auc.wide$paper == "MAB14" & auc.wide$anytreat==1,(auc.wide$trteffect_d8 - auc.wide$trteffect_d4)*3/4 + auc.wide$trteffect_d4 ,
                        ifelse(auc.wide$paper == "MAB15" & auc.wide$anytreat==1,(auc.wide$trteffect_d8 - auc.wide$trteffect_d5)*2/3 + auc.wide$trteffect_d5,
                        ifelse(auc.wide$paper == "MAB19" & auc.wide$anytreat==1,(auc.wide$trteffect_d8 - auc.wide$trteffect_d3)*4/5 + auc.wide$trteffect_d3, 
                        ifelse(auc.wide$paper == "OTH05" & auc.wide$anytreat==1,(auc.wide$trteffect_d10 - auc.wide$trteffect_d4)*3/6 + auc.wide$trteffect_d4,
                        ifelse(auc.wide$paper == "MAB04" & auc.wide$anytreat==1, (0.05-auc.wide$trteffect_d6)*1/9 + auc.wide$trteffect_d6,
                        ifelse(auc.wide$paper == "MAB16" & auc.wide$anytreat==1, (0.35-auc.wide$trteffect_d4)*3/4 + auc.wide$trteffect_d4,auc.wide$trteffect_d7) ))))))

#turn back into a long dataframe
auc.long <- auc.wide |> 
  pivot_longer(
    cols = trteffect_d1:sem.diff_d11,
    names_to = c(".value","day"),
    names_sep = "_d",
    values_drop_na = TRUE
  )

auc.long$day <- as.numeric(auc.long$day)

#remove everything >7 days
auc.long <- auc.long |> 
  filter(day<=7)

#calculate each piece of area
auc.long$slice <- ifelse(auc.long$day==1,0,
                         (auc.long$trteffect + lag(auc.long$trteffect))/2 * (auc.long$day-lag(auc.long$day)))

auc.results <- auc.long |> 
  group_by(paper, group, anytreat,treatment.type,variant,treat.lag) |>
  summarise(auc = sum(slice)) |> 
  ungroup() 

composite.model <- composite.model |> 
  left_join(auc.results) |> 
  filter(anytreat==1)


#calculate rrs and sample variances
composite.model <- escalc(measure="OR", ai=numevents, bi=numatrisk - numevents,
                             ci=pcb.events, di=pcb.atrisk - pcb.events, data=composite.model)


# AUC unadjusted model  --------------------------------------------------------------

model.auc <- rma.mv(yi, vi, mods = ~ auc, method="ML", random = ~ 1 | paper, data=subset(composite.model))

confint(model.auc)

# Calculate I^2
model.auc.i2 <- calc.i2(model.auc)

#Collate results
results.auc <- collate.results(model.auc)
colnames(results.auc)<- c("variable","Beta est","Std. Err","z value","p value","RR","low.ci","upp.ci")


#calculate efficacies for various virological effects
effpred.auc <- gen.effpred(model.auc,0,4.5)

# AUC adjusted model ------------------------------------------------------

model.auc.adj <- rma.mv(yi, vi, mods = ~ auc + treatment.type + treat.lag + variant, method="ML", random = ~ 1 | paper, data=subset(composite.model))

# Calculate I^2
model.auc.adj.i2 <- calc.i2(model.auc.adj)

#Collate results
results.auc.adj <- collate.results(model.auc.adj)
colnames(results.auc.adj)<- c("variable","Beta est","Std. Err","z value","p value","RR","low.ci","upp.ci")

#save result output
list_of_datasets <- list("auc" = results.auc,"auc.adj" = results.auc.adj)
write.xlsx(list_of_datasets, file = "output/composite-models.xlsx")



# # clean out environment ---------------------------------------------------

rm(list_of_datasets)
rm(auc.long,auc.wide,auc.results,model.auc,model.auc.adj)
