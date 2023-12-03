# -------------------------------------------------------------------------
#' This file is used to set up everything that's needed across the project
#' It loads libraries, creates functions, sets themes and defaults
#' 
#
# -------------------------------------------------------------------------


# load packages -----------------------------------------------------------

library(lemon)
library(tidyverse)
library(lme4) 
library(mvtnorm)
library(ggVennDiagram)
library(metafor)
library(glue)
library(openxlsx) 
library(cowplot)

# load local functions ----------------------------------------------------

source("RFunctions/result_formats.R")
source("RFunctions/i2_fns.R")

# create output folder (if it does not exist already)
if(!dir.exists("output")){
  dir.create("output")
}


# set defaults ------------------------------------------------------------

papercols <- c("AV01"="#a50026","AV03"= "#543005","AV05" = "#d73027","AV11" = "#c51b7d", 
               "MAB02"="#8e0152","MAB04" = "#8c510a", "MAB06" = "#f46d43","MAB07"= "#de77ae",
               "MAB08" = "#bf812d","MAB12"= "#fdae61","MAB13"= "#b8e186","MAB14" = "#80cdc1",
               "OTH05" = "#6a287e" , "OTH07"= "#7fbc41","AV02" = "#35978f", "MAB03" = "#74add1", "MAB15" = "#4d9221", 
               "MAB16" = "#01665e", "OTH09" = "#4575b4","MAB17" = "#276419","MAB19" = "#313695", "MAB20" = "#003c30",  "AABLACK" = "black","dummy"= "#003c30") 

paperlabels <- c("AV01"="Hammond et al. (2022)","AV03"= "Jayk Bernal et al. (2021)","AV05" = "Caraco et al. (2022)","AV11" = "Gottlieb et al. (2021b)", 
                 "MAB02"="Norton et al. (2022)","MAB04" = "Montgomery et al. (2022)", "MAB06" = "Gottlieb et al. (2021a)",
                 "MAB07"= "Dougan et al. (2021)","MAB08" = "Dougan et al. (2022a)","MAB12"= "Streinu-Cercel et al. (2022)",
                 "MAB13"= "Dougan et al. (2022b)","MAB14" = "Chew et al. (2022)" ,"OTH05" = "Rossignol et al. (2022)" , "OTH07"= "Mitja et al. (2021)",
                 "AV02" = "Fisher et al. (2022)", "MAB03" = "Weinrich et al.(2021)", "MAB15" = "Gupta et al. (2021)", "MAB16" = "Bender Ignacio et al. (2023)",
                 "OTH09" = "Gharbharan et al. (2023)", "MAB17" = "Kim et al. (2022)", "MAB19" = "Vega et al. (2023)","MAB20" = "Ison et al. (2023)", "AABLACK" = "Aggregate") 


agentshape <- c("BAM"=1,"BAM + ETE" = 0,"BEB" = 2,"BEB + BAM + ETE"=4,"Hydroxychloroquine"=18,"Molnupiravir"=16,"Paxlovid"=15,"Regdanvimab"=11,"Regen-Cov"=5,
               "Remdesivir"=17,"TIX + CIL"=7, "Sotrovimab" = 10,"CP" = 12,"BGB-DXP593" = 13,"Nitazoxanide" =8, "Adintrevimab" = 14) 
agentlabels <- c("BAM"="Bamlanivimab" ,"BAM + ETE" = "Bamlanivimab + \nEtesevimab" ,"BEB" = "Bebtelovimab" ,"BEB + BAM + ETE"= "Bebtelovimab + \nBamlanivimab + \nEtesevimab",
                "Hydroxychloroquine"= "Hydroxychloroquine" ,"Molnupiravir"= "Molnupiravir" ,"Paxlovid"= "Nirmatrelvir + \nRitonivar" ,"Regdanvimab"= "Regdanvimab",
                "Regen-Cov"= "Casirivimab + \nImdevimab","Remdesivir"= "Remdesivir","TIX + CIL"= "Tixagevimab + \nCilgavimab","Sotrovimab" = "Sotrovimab",
                "CP" = "Convalescent Plasma","BGB-DXP593" = "BGB-DXP593","Nitazoxanide"="Nitazoxanide")


#full paper list
fulllist <- c("AV01","AV03","AV05","AV11", "MAB02","MAB04", "MAB06", "MAB07","MAB08","MAB12","MAB13","MAB14","OTH05","OTH07","AV02","MAB03","MAB15","MAB16","OTH09","MAB17","MAB19","MAB20")

#extra paper list (for MAB06a and MAB08a)
extralist <- c(fulllist,"MAB06a","MAB08a")


#ggplot - set default theme for all charts
theme_set(
  theme_bw(base_size=14,
           base_line_size = 0.5,
           base_rect_size = 0.5
))

num_x_evals <- 1e3



