# Day 3 model  ------------------------------------------

# Generate RR data using escalc

rr.d3 <- unvax.day3 |> 
  filter(anytreat==1)

rr.d3 <- escalc(measure="RR", ai=numevents, bi=numatrisk - numevents,
                ci=pcb.events, di=pcb.atrisk - pcb.events, data=rr.d3)


#use these RRs in rma.mv
full.model.d3 <- rma.mv(yi, vi, mods = ~ trteffect, method="ML", random = ~ 1 | paper, data=rr.d3)
red.model.d3 <- rma.mv(yi, vi, method="ML", random = ~ 1 | paper, data=rr.d3)
confint(full.model.d3)

#Calculate I^2
red.model.d3.i2 <- calc.i2(red.model.d3)
full.model.d3.i2 <- calc.i2(full.model.d3)

#Collate results
results.d3 <- collate.results(full.model.d3)
colnames(results.d3)<- c("variable","Beta est","Std. Err","z value","p value","RR","low.ci","upp.ci")


#calculate efficacies for various virological effects
effpred.d3 <- gen.effpred(full.model.d3,0,1.0)


# Day 5 model  ------------------------------------------

rr.d5 <- unvax.day5 |> 
  filter(anytreat==1)

rr.d5 <- escalc(measure="RR", ai=numevents, bi=numatrisk - numevents,
                ci=pcb.events, di=pcb.atrisk - pcb.events, data=rr.d5)


#use these RRs in rma.mv
full.model.d5 <- rma.mv(yi, vi, mods = ~ trteffect, method="ML", random = ~ 1 | paper, data=rr.d5)
red.model.d5 <- rma.mv(yi, vi, method="ML", random = ~ 1 | paper, data=rr.d5)
confint(full.model.d5)

# Calculate I^2
red.model.d5.i2 <- calc.i2(red.model.d5)
full.model.d5.i2 <- calc.i2(full.model.d5)

#Collate results
results.d5 <- collate.results(full.model.d5)
colnames(results.d5)<- c("variable","Beta est","Std. Err","z value","p value","RR","low.ci","upp.ci")


#calculate efficacies for various virological effects
effpred.d5 <- gen.effpred(full.model.d5,0,1.5)


#  Day 7 model ------------------------------------------------------------

rr.d7 <- unvax.day7 |> 
  filter(anytreat==1)

rr.d7 <- escalc(measure="RR", ai=numevents, bi=numatrisk - numevents,
                ci=pcb.events, di=pcb.atrisk - pcb.events, data=rr.d7)


#use these RRs in rma.mv
full.model.d7 <- rma.mv(yi, vi, mods = ~ trteffect, method="ML", random = ~ 1 | paper, data=rr.d7)
red.model.d7 <- rma.mv(yi, vi, method="ML", random = ~ 1 | paper, data=rr.d7)
confint(full.model.d7)

# Calculate I^2
red.model.d7.i2 <- calc.i2(red.model.d7)
full.model.d7.i2 <- calc.i2(full.model.d7)

#Collate results
results.d7 <- collate.results(full.model.d7)
colnames(results.d7)<- c("variable","Beta est","Std. Err","z value","p value","RR","low.ci","upp.ci")

#calculate efficacies for various virological effects
effpred.d7 <- gen.effpred(full.model.d7,0,1.7)


# save results down -------------------------------------------------------

#save model output
list_of_datasets <- list("day3" = results.d3, "day5" = results.d5,"day7" = results.d7)
write.xlsx(list_of_datasets, file = glue("output/single-day-modelsRR.xlsx"))

#save funnel plots

#need to play around with width, height, units and resolution to get this to save properly

png(file='output/funneld3.png', width = 900, height = 600 ) # Open PNG device with specific file name
funnel(full.model.d3, label = "out", slab = firstauthor) # Plot the forest
dev.off()

pdf(file='output/funneld3.pdf', width = 9, height = 6 ) # Open PDF device with specific file name
funnel(full.model.d3, label = "out", slab = firstauthor) # Plot the forest
dev.off()

png(file='output/funneld5.png', width = 900, height = 600 ) # Open PNG device with specific file name
funnel(full.model.d5, label = "out", slab = firstauthor) # Plot the forest
dev.off()

pdf(file='output/funneld5.pdf', width = 9, height = 6 ) # Open PDF device with specific file name
funnel(full.model.d5, label = "out", slab = firstauthor) # Plot the forest
dev.off()

png(file='output/funneld7.png', width = 900, height = 600 ) # Open PNG device with specific file name
funnel(full.model.d7, label = "out", slab = firstauthor) # Plot the forest
dev.off()

pdf(file='output/funneld7.pdf', width = 9, height = 6 ) # Open PDF device with specific file name
funnel(full.model.d7, label = "out", slab = firstauthor) # Plot the forest
dev.off()




# clean out environment ---------------------------------------------------

rm(list_of_datasets, full.model.d3,full.model.d5,full.model.d7,red.model.d3,red.model.d5,red.model.d7)


