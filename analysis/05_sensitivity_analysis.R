# model adjusting for treatment type ---------------------------------------------

# Day 3
bytrt.d3 <- rma.mv(yi, vi, mods = ~ trteffect + treatment.type, method="ML", random = ~ 1 | paper, data=rr.d3)


#Collate results
results.bytrt.d3 <- collate.results(bytrt.d3)
colnames(results.bytrt.d3)<- c("variable","Beta est","Std. Err","z value","p value","RR","low.ci","upp.ci")

# Day 5
bytrt.d5 <- rma.mv(yi, vi, mods = ~ trteffect + treatment.type, method="ML", random = ~ 1 | paper, data=rr.d5)

#Collate results
results.bytrt.d5 <- collate.results(bytrt.d5)
colnames(results.bytrt.d5)<- c("variable","Beta est","Std. Err","z value","p value","RR","low.ci","upp.ci")

# Day 7
bytrt.d7 <- rma.mv(yi, vi, mods = ~ trteffect + treatment.type, method="ML", random = ~ 1 | paper, data=rr.d7)

#Collate results
results.bytrt.d7 <- collate.results(bytrt.d7)
colnames(results.bytrt.d7)<- c("variable","Beta est","Std. Err","z value","p value","RR","low.ci","upp.ci")


# save down results
list_of_datasets <- list("day3" = results.bytrt.d3, "day5" = results.bytrt.d5,"day7" = results.bytrt.d7)
write.xlsx(list_of_datasets, file = "output/bytrtmodels.xlsx")


#  model adjusting for variant ---------------------------------------------------

# Day 3
byvariant.d3 <- rma.mv(yi, vi, mods = ~ trteffect + variant, method="ML", random = ~ 1 | paper, data=rr.d3)

#Collate results
results.byvariant.d3 <- collate.results(byvariant.d3)
colnames(results.byvariant.d3)<- c("variable","Beta est","Std. Err","z value","p value","RR","low.ci","upp.ci")

# Day 5
byvariant.d5 <- rma.mv(yi, vi, mods = ~ trteffect + variant, method="ML", random = ~ 1 | paper, data=rr.d5)

#Collate results
results.byvariant.d5 <- collate.results(byvariant.d5)
colnames(results.byvariant.d5)<- c("variable","Beta est","Std. Err","z value","p value","RR","low.ci","upp.ci")

# Day 7
byvariant.d7 <- rma.mv(yi, vi, mods = ~ trteffect + variant, method="ML", random = ~ 1 | paper, data=rr.d7)

#Collate results
results.byvariant.d7 <- collate.results(byvariant.d7)
colnames(results.byvariant.d7)<- c("variable","Beta est","Std. Err","z value","p value","RR","low.ci","upp.ci")

#save by variant
list_of_datasets <- list("day3" = results.byvariant.d3, "day5" = results.byvariant.d5,"day7" = results.byvariant.d7)
write.xlsx(list_of_datasets, file = "output/byvariantmodels.xlsx")


# model adjusting for treatment timing --------------------------------------------------------

# Day 3
bytrtlag.d3 <- rma.mv(yi, vi, mods = ~ trteffect + treat.lag, method="ML", random = ~ 1 | paper, data=rr.d3)

#Collate results
results.bytrtlag.d3 <- collate.results(bytrtlag.d3)
colnames(results.bytrtlag.d3)<- c("variable","Beta est","Std. Err","z value","p value","RR","low.ci","upp.ci")

# Day 5
bytrtlag.d5 <- rma.mv(yi, vi, mods = ~ trteffect + treat.lag, method="ML", random = ~ 1 | paper, data=rr.d5)

#Collate results
results.bytrtlag.d5 <- collate.results(bytrtlag.d5)
colnames(results.bytrtlag.d5)<- c("variable","Beta est","Std. Err","z value","p value","RR","low.ci","upp.ci")

# Day 7
bytrtlag.d7 <- rma.mv(yi, vi, mods = ~ trteffect + treat.lag, method="ML", random = ~ 1 | paper, data=rr.d7)

#Collate results
results.bytrtlag.d7 <- collate.results(bytrtlag.d7)
colnames(results.bytrtlag.d7)<- c("variable","Beta est","Std. Err","z value","p value","RR","low.ci","upp.ci")

#save by treatment lag

list_of_datasets <- list("day3" = results.bytrtlag.d3, "day5" = results.bytrtlag.d5,"day7" = results.bytrtlag.d7)
write.xlsx(list_of_datasets, file = "output/bytrtlagmodels.xlsx")


# model adjusting for all 3 ------------------------------------------------

# Day 3
adjusted.d3 <- rma.mv(yi, vi, mods = ~ trteffect + treatment.type + treat.lag + variant, method="ML", random = ~ 1 | paper, data=rr.d3)
confint(adjusted.d3)

# Calculate I^2
adjusted.d3.i2 <- calc.i2(adjusted.d3)

#Collate results
results.adjusted.d3 <- collate.results(adjusted.d3)
colnames(results.adjusted.d3)<- c("variable","Beta est","Std. Err","z value","p value","RR","low.ci","upp.ci")

# Day 5
adjusted.d5 <- rma.mv(yi, vi, mods = ~ trteffect + treatment.type + treat.lag + variant, method="ML", random = ~ 1 | paper, data=rr.d5)
confint(adjusted.d5)

# Calculate I^2
adjusted.d5.i2 <- calc.i2(adjusted.d5)

#Collate results
results.adjusted.d5 <- collate.results(adjusted.d5)
colnames(results.adjusted.d5)<- c("variable","Beta est","Std. Err","z value","p value","RR","low.ci","upp.ci")


# Day 7

adjusted.d7 <- rma.mv(yi, vi, mods = ~ trteffect + treatment.type + treat.lag + variant, method="ML", random = ~ 1 | paper, data=rr.d7)
confint(adjusted.d7)

# Calculate I^2
adjusted.d7.i2 <- calc.i2(adjusted.d7)

#Collate results
results.adjusted.d7 <- collate.results(adjusted.d7)
colnames(results.adjusted.d7)<- c("variable","Beta est","Std. Err","z value","p value","RR","low.ci","upp.ci")


#save adjusted model

list_of_datasets <- list("day3" = results.adjusted.d3, "day5" = results.adjusted.d5,"day7" = results.adjusted.d7)
write.xlsx(list_of_datasets, file = glue("output/adjustedmodels.xlsx"))


# Excluding high RoB studies ----------------------------------------------

# Day 3
rob.d3 <- rma.mv(yi, vi, mods = ~ trteffect, method="ML", random = ~ 1 | paper, data=subset(rr.d3, robhigh != 1))

#Collate results
results.rob.d3 <- collate.results(rob.d3)
colnames(results.rob.d3)<- c("variable","Beta est","Std. Err","z value","p value","RR","low.ci","upp.ci")

# Day 5

rob.d5 <- rma.mv(yi, vi, mods = ~ trteffect, method="ML", random = ~ 1 | paper, data=subset(rr.d5, robhigh != 1))

#Collate results
results.rob.d5 <- collate.results(rob.d5)
colnames(results.rob.d5)<- c("variable","Beta est","Std. Err","z value","p value","RR","low.ci","upp.ci")

# Day 7

rob.d7 <- rma.mv(yi, vi, mods = ~ trteffect , method="ML", random = ~ 1 | paper, data=subset(rr.d7, robhigh != 1))

#Collate results
results.rob.d7 <- collate.results(rob.d7)
colnames(results.rob.d7)<- c("variable","Beta est","Std. Err","z value","p value","RR","low.ci","upp.ci")

#save excluding high RoB
list_of_datasets <- list("day3" = results.rob.d3, "day5" = results.rob.d5,"day7" = results.rob.d7)
write.xlsx(list_of_datasets, file = glue("output/exhighrob.xlsx"))


# stratified by treatment type --------------------------------------------

# Day 3
mab.d3 <- rma.mv(yi, vi, mods = ~ trteffect , method="ML", random = ~ 1 | paper, data=subset(rr.d3,treatment.type == "mAb"))
smmol.d3 <- rma.mv(yi, vi, mods = ~ trteffect , method="ML", random = ~ 1 | paper, data=subset(rr.d3,treatment.type != "mAb"))

#Collate results
results.mab.d3 <- collate.results(mab.d3)
colnames(results.mab.d3)<- c("variable","Beta est","Std. Err","z value","p value","RR","low.ci","upp.ci")
results.smmol.d3 <- collate.results(smmol.d3)
colnames(results.smmol.d3)<- c("variable","Beta est","Std. Err","z value","p value","RR","low.ci","upp.ci")


# Day 5 

mab.d5 <- rma.mv(yi, vi, mods = ~ trteffect , method="ML", random = ~ 1 | paper, data=subset(rr.d5,treatment.type == "mAb"))
smmol.d5 <- rma.mv(yi, vi, mods = ~ trteffect , method="ML", random = ~ 1 | paper, data=subset(rr.d5,treatment.type != "mAb"))

#Collate results
results.mab.d5 <- collate.results(mab.d5)
colnames(results.mab.d5)<- c("variable","Beta est","Std. Err","z value","p value","RR","low.ci","upp.ci")
results.smmol.d5 <- collate.results(smmol.d5)
colnames(results.smmol.d5)<- c("variable","Beta est","Std. Err","z value","p value","RR","low.ci","upp.ci")

# Day 7

mab.d7 <- rma.mv(yi, vi, mods = ~ trteffect , method="ML", random = ~ 1 | paper, data=subset(rr.d7,treatment.type == "mAb"))
smmol.d7 <- rma.mv(yi, vi, mods = ~ trteffect , method="ML", random = ~ 1 | paper, data=subset(rr.d7,treatment.type != "mAb"))

#Collate results
results.mab.d7 <- collate.results(mab.d7)
colnames(results.mab.d7)<- c("variable","Beta est","Std. Err","z value","p value","RR","low.ci","upp.ci")
results.smmol.d7 <- collate.results(smmol.d7)
colnames(results.smmol.d7)<- c("variable","Beta est","Std. Err","z value","p value","RR","low.ci","upp.ci")

#save stratified by treatment type
list_of_datasets <- list("day3mab" = results.mab.d3, "day5mab" = results.mab.d5,"day7mab" = results.mab.d7,
                         "day3smmol" =   results.smmol.d3, "day5smmol" = results.smmol.d5,"day7smmol" = results.smmol.d7 )
write.xlsx(list_of_datasets, file = glue("output/treatstrat.xlsx"))



# leave out one analysis --------------------------------------------------

# Day 7
p <- unique(as.character(rr.d7$paper))
lo1.d7.slope <- matrix(0,length(p),8)
lo1.d7.intercept <- matrix(0,length(p),8)


for(i in 1:length(p)){
  t <- rr.d7 |> 
    filter(paper != p[i])
  
  lo1.d7 <- rma.mv(yi, vi, mods = ~ trteffect, method="ML", random = ~ 1 | paper, data=t)
  lo1.d7.slope[i,] <- c(p[i],
                      summary(lo1.d7)$beta[2,],
                      summary(lo1.d7)$se[2],
                      summary(lo1.d7)$zval[2],
                      summary(lo1.d7)$pval[2],
                      exp(summary(lo1.d7)$beta[2,]),
                      exp(summary(lo1.d7)$ci.lb[2]),
                      exp(summary(lo1.d7)$ci.ub[2])
                      )
  lo1.d7.intercept[i,] <- c(p[i],
                        summary(lo1.d7)$beta[1,],
                        summary(lo1.d7)$se[1],
                        summary(lo1.d7)$zval[1],
                        summary(lo1.d7)$pval[1],
                        exp(summary(lo1.d7)$beta[1,]),
                        exp(summary(lo1.d7)$ci.lb[1]),
                        exp(summary(lo1.d7)$ci.ub[1])
  )
}

lo1.d7.slope <- as.data.frame(lo1.d7.slope)
colnames(lo1.d7.slope)<- c("ex. paper","Beta est","Std. Err","z value","p value","RR","low.ci","upp.ci")


lo1.d7.intercept <- as.data.frame(lo1.d7.intercept)
colnames(lo1.d7.intercept)<- c("ex. paper","Beta est","Std. Err","z value","p value","RR","low.ci","upp.ci")

for (i in 2:8) {
  lo1.d7.slope[,i] <- as.numeric(lo1.d7.slope[,i])
  lo1.d7.intercept[,i] <- as.numeric(lo1.d7.intercept[,i])
}

# Day 5

p <- unique(as.character(rr.d5$paper))
lo1.d5.slope <- matrix(0,length(p),8)
lo1.d5.intercept <- matrix(0,length(p),8)


for(i in 1:length(p)){
  t <- rr.d5 |> 
    filter(paper != p[i])
  
  lo1.d5 <- rma.mv(yi, vi, mods = ~ trteffect, method="ML", random = ~ 1 | paper, data=t)
  lo1.d5.slope[i,] <- c(p[i],
                        summary(lo1.d5)$beta[2,],
                        summary(lo1.d5)$se[2],
                        summary(lo1.d5)$zval[2],
                        summary(lo1.d5)$pval[2],
                        exp(summary(lo1.d5)$beta[2,]),
                        exp(summary(lo1.d5)$ci.lb[2]),
                        exp(summary(lo1.d5)$ci.ub[2])
  )
  lo1.d5.intercept[i,] <- c(p[i],
                            summary(lo1.d5)$beta[1,],
                            summary(lo1.d5)$se[1],
                            summary(lo1.d5)$zval[1],
                            summary(lo1.d5)$pval[1],
                            exp(summary(lo1.d5)$beta[1,]),
                            exp(summary(lo1.d5)$ci.lb[1]),
                            exp(summary(lo1.d5)$ci.ub[1])
  )
}

lo1.d5.slope <- as.data.frame(lo1.d5.slope)
colnames(lo1.d5.slope)<- c("ex. paper","Beta est","Std. Err","z value","p value","RR","low.ci","upp.ci")


lo1.d5.intercept <- as.data.frame(lo1.d5.intercept)
colnames(lo1.d5.intercept)<- c("ex. paper","Beta est","Std. Err","z value","p value","RR","low.ci","upp.ci")

for (i in 2:8) {
  lo1.d5.slope[,i] <- as.numeric(lo1.d5.slope[,i])
  lo1.d5.intercept[,i] <- as.numeric(lo1.d5.intercept[,i])
}


# Day 3

p <- unique(as.character(rr.d3$paper))
lo1.d3.slope <- matrix(0,length(p),8)
lo1.d3.intercept <- matrix(0,length(p),8)


for(i in 1:length(p)){
  t <- rr.d3 |> 
    filter(paper != p[i])
  
  lo1.d3 <- rma.mv(yi, vi, mods = ~ trteffect, method="ML", random = ~ 1 | paper, data=t)
  lo1.d3.slope[i,] <- c(p[i],
                        summary(lo1.d3)$beta[2,],
                        summary(lo1.d3)$se[2],
                        summary(lo1.d3)$zval[2],
                        summary(lo1.d3)$pval[2],
                        exp(summary(lo1.d3)$beta[2,]),
                        exp(summary(lo1.d3)$ci.lb[2]),
                        exp(summary(lo1.d3)$ci.ub[2])
  )
  lo1.d3.intercept[i,] <- c(p[i],
                            summary(lo1.d3)$beta[1,],
                            summary(lo1.d3)$se[1],
                            summary(lo1.d3)$zval[1],
                            summary(lo1.d3)$pval[1],
                            exp(summary(lo1.d3)$beta[1,]),
                            exp(summary(lo1.d3)$ci.lb[1]),
                            exp(summary(lo1.d3)$ci.ub[1])
  )
}

lo1.d3.slope <- as.data.frame(lo1.d3.slope)
colnames(lo1.d3.slope)<- c("ex. paper","Beta est","Std. Err","z value","p value","RR","low.ci","upp.ci")


lo1.d3.intercept <- as.data.frame(lo1.d3.intercept)
colnames(lo1.d3.intercept)<- c("ex. paper","Beta est","Std. Err","z value","p value","RR","low.ci","upp.ci")

for (i in 2:8) {
  lo1.d3.slope[,i] <- as.numeric(lo1.d3.slope[,i])
  lo1.d3.intercept[,i] <- as.numeric(lo1.d3.intercept[,i])
}


#save leave out one results
list_of_datasets <- list("day3intercept" = lo1.d3.intercept, "day3slope" = lo1.d3.slope,
                         "day5intercept" = lo1.d5.intercept, "day5slope" = lo1.d5.slope,
                         "day7intercept" = lo1.d7.intercept, "day7slope" = lo1.d7.slope)
write.xlsx(list_of_datasets, file = glue("output/leaveoutone.xlsx"))



# clean out environment ---------------------------------------------------

rm(list_of_datasets, bytrt.d3,bytrt.d5,bytrt.d7, results.bytrt.d3, results.bytrt.d5, results.bytrt.d7)
rm(byvariant.d3,byvariant.d5,byvariant.d7,results.byvariant.d3,results.byvariant.d5,results.byvariant.d7)
rm(bytrtlag.d3,bytrtlag.d5,bytrtlag.d7, results.bytrtlag.d3,results.bytrtlag.d5,results.bytrtlag.d7)
rm(rob.d3,rob.d5,rob.d7,results.rob.d3,results.rob.d5,results.rob.d7)
rm(mab.d3,mab.d5,mab.d7,results.mab.d3,results.mab.d5,results.mab.d7,smmol.d3,smmol.d5,smmol.d7)
rm(results.smmol.d3,results.smmol.d5,results.smmol.d7,results.adjusted.d3,results.adjusted.d5,results.adjusted.d7)
rm(lo1.d3, lo1.d3.intercept,lo1.d3.slope,lo1.d5,lo1.d5.intercept,lo1.d5.slope,lo1.d7,lo1.d7.intercept,lo1.d7.slope)
rm(adjusted.d3,adjusted.d5,adjusted.d7,t)



