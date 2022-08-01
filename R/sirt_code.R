#############################################################################
# R script for processing CJ decision data from https://www.nomoremarking.com/
# July2022
#############################################################################

  #clear variable list if needed
  #rm(list = ls())

#############################################################################
# sirt is the package that contains the btm commands
# also need dplyr for transforming data
# cocor useful for comparing correlations
# ggpubr useful for normality checks
# and we need the sirt functions
#############################################################################
  library(sirt)
  require(dplyr)
  require(cocor)
  library("ggpubr")
  source("sirt_functions.R")

  

#############################################################################
# get the decision data, and change column headings if required
# the sample data sampleNMMdecisionData.csv needs to be in the same folder as this R script
#############################################################################
  decisions <- read.csv('sampleNMMdecisionData.csv', header=T, stringsAsFactors=F)
  # change 'sampleNMMdecisionData.csv' to your own decisions file as needed
  
  colnames(decisions)[c(1,3,4)] <- c("judge_id", "won", "lost")

  
#############################################################################  
# number of scripts, number of judges
#############################################################################
  no_scripts <- length(unique(c(decisions$won,decisions$lost)))  
  no_judges <- length(unique(decisions$judge_id))
  
#############################################################################  
# fit to BT model and get scale values and SSR and interrater reliability
#############################################################################
  mod1 <- btm_with_judges(decisions)
  SSR <- mod1$mle.rel 
  interrater <- interrater_rel(decisions)

  # calculate misfit
  judgefit <- judge_misfit(mod1$fit_judges)
  scriptfit <- script_misfit(mod1$residuals)
  
  #############################################################################  
  # export results
  #############################################################################
  write.csv(mod1$effects, 'scalevalues.csv', row.names = F)
  