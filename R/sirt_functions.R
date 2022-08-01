#vehicles in the park functions

#############################################################################
# fit btm
############################################################################
btm_with_judges <- function(judgement_pairs){
  players <- unique(c(judgement_pairs$won,judgement_pairs$lost))
  df <- data.frame(id1=judgement_pairs$won, id2=judgement_pairs$lost,  result=1, judge=judgement_pairs$judge_id)
  df$id1 <- factor(df$id1,levels=players)
  df$id2 <- factor(df$id2,levels=players)
  mod1 <- sirt::btm(df[,1:3]   , judge = df$judge, maxit=400 , fix.eta=0 , ignore.ties=TRUE )
  return(mod1)
}

#############################################################################
# calculate judge misfit (if you must!)
#############################################################################
judge_misfit <- function(judge_fit){
  judge_thresh <- mean(judge_fit$infit) + 2*sd(judge_fit$infit)
  judge_fits <- cbind(judge_fit, threshold=judge_thresh, misfit=as.numeric(judge_fit$infit>judge_thresh)) #view this table to see which judges are misfits, if any
  return(judge_fits)
}

#############################################################################
# calculate script misfit (if you must!)
#############################################################################
script_misfit <- function(resids){
  resid <- resids[,c(1,2,4,5)]
  colnames(resid)[3:4] <- c("res","stdres")
  script_res <- data.frame(id=c(resid$id1,resid$id2),
                         probs=c(resid$res^2/resid$stdres^2,resid$res^2/resid$stdres^2),
                         sqrstdres=c(resid$stdres^2,resid$stdres^2)
  )
  script_fit = script_res %>%
    group_by(id) %>%
    summarise(infit = sum(probs * sqrstdres)/sum(probs)) %>% as.data.frame

    script_thresh <- mean(script_fit$infit) + 2*sd(script_fit$infit)
  script_fit <- cbind(script_fit, script_thresh, misfit=as.numeric(script_fit$infit>script_thresh)) #view this table to see which scripts are misfits, if any
  return(script_fit)
}


#############################################################################
# estimate inter-rater reliability using the split-halves technique
#############################################################################
  
interrater_rel <- function(decisions, num_of_iterations = 100){
  mod1 <- btm_with_judges(decisions)
  iterations = num_of_iterations #100 is usually fine, but change if you wish
  ir = data.frame(iteration = rep(0,iterations), pearson = rep(0,iterations), SSR1 = rep(0,iterations), SSR2 = rep(0,iterations), num_cand = rep(0,iterations)) #stores the result of each iteration
  ip = as.data.frame(matrix(data=0,nrow=nrow(mod1$effects),ncol=2*iterations)) #store all theta values in case of interest
  colnames(ip) = as.character(c(1:(2*iterations)))
  noj = length(unique(decisions$judge_id)) #number of judges
  judges = data.frame(row = 1:noj, judge_id = unique(decisions$judge_id), rand = rep(0,noj), group = rep(0,noj)) #list of unique judge nmaes

  #############################################################################
  # loop 'iterations' times
  #############################################################################
  for(i in 1:iterations) 
  { 
  ir[i,1] = i # fill iteration column
  
  #############################################################################
  #randomise judges to two groups
  #############################################################################
  judges$rand = runif(noj, -1, 1)  
  judges <- judges[order(judges$rand),]
  judges$row = 1:noj
  judges$group = 2
  judges$group[1:(noj/2)] <-1
  groups = merge(decisions,judges) #now split judgements corresponding to judge groups
  g1 = groups[groups$group==1,]
  g2 = groups[groups$group==2,]
  
  #############################################################################
  #calculate scale values for each group
  #############################################################################
  modg1 <- btm_with_judges(g1)
  modg2 <- btm_with_judges(g2)
  modg12 = merge(modg1$effects, modg2$effects, by="individual") #bodge in case different number of candidates in each rank
  
  #############################################################################
  #calculate Pearson correlation and store results
  #############################################################################
  ir[i,2] =  cor(modg12$theta.x,modg12$theta.y, method="pearson") # fill correlation column
  ir[i,3] =  modg1$mle.rel
  ir[i,4] =  modg2$mle.rel 
  ir[i,5] = nrow(modg12)
  ip[,2*i-1] = c(modg12$theta.x, rep(0,nrow(mod1$effects)-nrow(modg12)))
  ip[,2*i] = c(modg12$theta.y, rep(0,nrow(mod1$effects)-nrow(modg12)))
  
  #############################################################################
  #display iteration number so user knows it's not crashed
  #############################################################################
  print(i)
  } # end of loop

  #############################################################################
  #take median value
  #############################################################################
  interrater <- median(ir$pearson)
  return(interrater)
  }
