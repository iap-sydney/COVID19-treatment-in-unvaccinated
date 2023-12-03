# Data set up script -------------------------------------------

#loads csv from all data tab
#manipulates then saves down csv to use for analysis


# Data load and manipulation ----------------------------------------------

#load data file

data_file <- "raw-data/trt_data_doseagg.csv"

if(!file.exists(data_file)){
  stop(glue("{data_file} does not exist."))
}

trtdata <- read_csv(data_file)



colnames(trtdata)<-tolower(colnames(trtdata))

#drop columns not used at all
trtdata <- trtdata |>
  select(!c(maxdayssincepos,maxdayssincesymp,daysrelto)) 



# basic new variable set up -----------------------------------------------


#set up dichotomous anytreat var
trtdata$anytreat <- as.factor(ifelse(trtdata$trttype=="Placebo",0,1))

#set up pcbchange var (in preparation for trteffect). This copies vlchange into
#pcbchange for pcb rows, and populates that column with 0 for treated rows. Then
#we enter the sum of those 2 values for each distinct paper and day value. For
#paper p and day d, we put the sum of the pcbchange and - into both the pcb and
#trt row for that day.

trtdata <- trtdata |> 
        mutate(
          pcbchange = ifelse(anytreat==0,vlchange,0)
        ) |> 
    group_by(paper, day) |>
    mutate(pcbchange = sum(pcbchange)) |> 
  ungroup()

# Add trteffect variable (excess drop in VL in treated compared to PCB)
#on log10 scale
trtdata$trteffect <- trtdata$pcbchange - trtdata$vlchange

#add trt.perday
trtdata <- trtdata |> 
  mutate(
    trt.perday = ifelse(day==1,NA,trteffect/(day-1))
  )

# cat vars Factors and ref levels -----------------------------------------


trtdata$trttype <- as.factor(trtdata$trttype)
trtdata$trttype <- relevel(trtdata$trttype, ref = "Placebo") 

trtdata$risk <- as.factor(trtdata$risk)
trtdata$risk <- relevel(trtdata$risk, ref = "Low") 

#set up new binary risk variable - 0 for Low
# 1 for Med or High
trtdata$riskbin <- as.factor(ifelse(trtdata$risk == "Low",0,1))


trtdata$variant <- as.factor(trtdata$variant)
trtdata$variant <- relevel(trtdata$variant, ref = "WT") 

trtdata$vaccstatus <- as.factor(trtdata$vaccstatus)
trtdata$vaccstatus <- relevel(trtdata$vaccstatus, ref = "Unvaccinated")

trtdata$paper <- as.factor(trtdata$paper)



# Calculate risk of progression -------------------------------------------


#1. absolute risk of progression

trtdata$absrisk <- trtdata$numevents/trtdata$numatrisk

#2. Relative risk

#populates pcb values for each study
trtdata <- trtdata |> 
  mutate(
    pcb.events = ifelse(anytreat==0 & day==1,numevents,0),
    pcb.atrisk = ifelse(anytreat==0 & day==1,numatrisk,0),
    pcb.absrisk = ifelse(anytreat==0 & day==1,absrisk,0),
  ) |> 
  group_by(paper) |>
  mutate(pcb.events = sum(pcb.events),
         pcb.atrisk = sum(pcb.atrisk),
         pcb.absrisk = sum(pcb.absrisk),
         ) |> 
  ungroup()

#populate relrisk and efficacy for treated

trtdata$relrisk <- ifelse(trtdata$anytreat==1,trtdata$absrisk/trtdata$pcb.absrisk,NA)
trtdata$efficacy <- 1-trtdata$relrisk

# 3. populate drops as -1*change
trtdata$vldrop <- -trtdata$vlchange
trtdata$pcbdrop <- -trtdata$pcbchange



# Calculate 95%CIs for efficacy and RR -------------------------------


#first set up temp columns with 0.5 added to zero numbers of events.
trtdata <- trtdata |> 
  mutate(
    tmp.pcb.events = ifelse(anytreat==1 & pcb.events==0,0.5,
                            ifelse(anytreat==1 & pcb.events!=0,pcb.events,NA)),
    tmp.trt.events = ifelse(anytreat==1 & numevents==0,0.5,
                            ifelse(anytreat==1 & numevents!=0,numevents,NA))
    )

#use these tmp columns to calculate efficacy cis
#then remove tmp cols


trtdata <- trtdata |> 
  mutate(
    ci.eff.low = 1-((tmp.trt.events/numatrisk)/(tmp.pcb.events/pcb.atrisk))*exp(qnorm(0.975)*sqrt(1/tmp.trt.events+1/tmp.pcb.events-1/numatrisk-1/pcb.atrisk)),
    ci.eff.upp = 1-((tmp.trt.events/numatrisk)/(tmp.pcb.events/pcb.atrisk))*exp(qnorm(0.025)*sqrt(1/tmp.trt.events+1/tmp.pcb.events-1/numatrisk-1/pcb.atrisk))
  ) |> 
  select(-tmp.pcb.events, -tmp.trt.events) 

#cut off all lower bound negative efficacies at -550%
trtdata$ci.eff.low[trtdata$ci.eff.low< -5.5] <- -5.5


#calculate RR CIs (1-effCI)
trtdata$ci.rr.low = 1 - trtdata$ci.eff.upp
trtdata$ci.rr.upp = 1 - trtdata$ci.eff.low




# Calc 95% CIs for virological treatment effect---------------------------------------------

# 1. create column sem.group. If given 95%CI by group / 1.96 to get s.e., if given sem by group copy it across  
trtdata$sem.group <- ifelse (trtdata$rangetype=="95CI GROUP",trtdata$rangesize/1.96,
                                ifelse(trtdata$rangetype=="SEM GROUP",trtdata$rangesize,NA))

# 2. calculate sem.diff. If have sem by group use these to calculate sem
# of their difference. If given sem of the difference, copy it across.

#create column for sem.pcb.group 

trtdata <- trtdata |> 
  mutate(
    sem.pcb.group = ifelse(anytreat==0,sem.group,0)
  ) |> 
  group_by(paper, day) |>
  mutate(sem.pcb.group = sum(sem.pcb.group)) |> 
  mutate(
    sem.pcb.group = ifelse(anytreat==0,NA,sem.pcb.group)
  ) |>
  ungroup()

#calculate se(diff) when we have se for each group
trtdata$sem.diff <- sqrt(trtdata$sem.group^2 + trtdata$sem.pcb.group^2)

#copy across ones given as se of difference
trtdata$sem.diff <- ifelse(trtdata$rangetype=="SEM DIFF",trtdata$rangesize,trtdata$sem.diff)

#populate sem.diff with cidiff/1.96 where that is given
trtdata$sem.diff <- ifelse(trtdata$rangetype=="95CI DIFF",trtdata$rangesize/1.96,trtdata$sem.diff)


#add plotgroup to get multiple lines working correctly on plots

trtdata$plotgroup <- paste(trtdata$paper,trtdata$group, sep = ":")


#save csv (in output folder)
write_csv(trtdata,"output/trtdata-clean.csv")

data.unvax <- trtdata

rm(trtdata)

#split into seperate days
unvax.day1 <- subset(data.unvax,day==1)
unvax.day3 <- subset(data.unvax,day==3)
unvax.day4 <- subset(data.unvax,day==4)
unvax.day5 <- subset(data.unvax,day==5)
unvax.day7 <- subset(data.unvax,day==7)


# Clean out environment ---------------------------------------------------

rm("data_file")


