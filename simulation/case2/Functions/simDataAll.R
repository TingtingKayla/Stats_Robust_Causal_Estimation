
##sample size, numPred-number of predictors in the dataset
###overlapL-confounder level (coefficients in the propensity model)
###out of the total number of predictors numPred-x1, x2 true confounders, x3,x4 predictors of outcome only, x5,x6 predictors of treatment only
###rho-correlation between predictors
###outcomes under treatment and control are parallel
##treatEff=true treatment effect
#sampleSize=200; numPred=20; overlapL="high"; seed.num=1; rho=0; treatEff=0;

#sampleSize=200; numPred=20; overlapL="low"; seed.num=1; rho=0; treatEff=0

simulateDate=function(sampleSize=200, numPred=20, overlapL="high", seed.num=1, rho=0, treatEff=0) {
  
  library("MASS") 
  set.seed(seed.num)
  
  ###simulate the dataset
  ###each x is standard normal N(0, 1)
  mean_x = 0 ###
  sig_x = 1
  
  pC = pI = pP = 2
  pS = numPred - (pC+pI+pP)
  var.list = c(paste("Xc",1:pC,sep=""),paste("Xp",1:pP,sep=""),paste("Xi",1:pI,sep=""),paste("Xs",1:pS,sep=""))
  
  # Set strength of relationship between covariates and outcome
  beta_v =  NULL
  
  # Set strength of relationship between covariates and treatment
  alpha_v=NULL
  if(overlapL=="moderate"){
    
    alpha_v = c( 1.0, 1.0,  0,  0,  1, 1,  rep(0, numPred - 6) )
    beta_v =  c( 0.6, 0.6, 0.6, 0.6, 0, 0, rep(0, numPred - 6) )
    
  } else if(overlapL=="low") {
    
    alpha_v = c( 1.0, 1.0,  0,  0,  1, 1,  rep(0, numPred - 6) )
    beta_v =  c( 0.2, 0.2, 0.6, 0.6, 0, 0, rep(0, numPred - 6) )
    
  } else if(overlapL=="high") {  ###weak confounders
    
    alpha_v = c( 0.4, 0.4,  0,  0,  1, 1,  rep(0, numPred - 6) )
    beta_v =  c( 0.6, 0.6, 0.6, 0.6, 0, 0, rep(0, numPred - 6) )
    
  }
  
  names(beta_v) = names(alpha_v) = var.list
  
  
  ### simulate data
  Sigma_x = matrix(rho * sig_x^2, nrow=length(var.list), ncol=length(var.list)) 
  diag(Sigma_x) = sig_x^2
  Mean_x = rep(mean_x, length(var.list))
  
  simdat = as.data.frame(mvrnorm(n = sampleSize, mu=Mean_x, Sigma = Sigma_x, empirical = FALSE))
  names(simdat) = var.list
  
  gA_x = rowSums(simdat[,var.list] * matrix(alpha_v, nrow=sampleSize, ncol=length(var.list), byrow=TRUE))
  pA = expit( gA_x )
  
  ###treatment assignment
  simdat$A <- rbinom(sampleSize, 1, prob = pA)###treatment status
  mean(simdat$A)
  
  gY_xA = rowSums(simdat[,var.list] * matrix(beta_v, nrow=sampleSize, ncol=length(var.list),byrow=TRUE))   
  simdat$Y = gY_xA + rnorm(n=sampleSize, mean=0, sd=sig_x)
  simdat$Y = simdat$Y + simdat$A * treatEff
  
  
  simdat$id2=1:dim(simdat)[1]
  simdat$pa = pA
  simdat$id=1
  
  return(list(simdat, var.list) )
  
}




###measure the amount of overlaps between the treated and control propensity distributions
overlapMeasure=function(data, alpha.level=0.05, treat.varname, sampleSize){
  
  ####calculate percent of treated inside 5% and 95% of control distributions and the other way around
  propTreat=data$pa
  propControl=1-data$pa
  treatInd=data[, treat.varname]
  
  qtreat=quantile(propTreat[treatInd==1], probs = c(alpha.level, 1-alpha.level))
  qcontrol=quantile(propControl[treatInd==0], probs = c(alpha.level, 1-alpha.level))
  
  
  plot(density(propTreat[treatInd==1]))
  lines(density(propTreat[treatInd==0]))
  abline(v=qtreat[1])
  abline(v=qtreat[2])
  
  ######first time point 
  set1a=which(propTreat >= qtreat[1] & propTreat <= qtreat[2])
  set1b=which(propControl >= qcontrol[1] & propControl <= qcontrol[2])
  setInt1=set1a[set1a %in% set1b]
  
  return( c(length(setInt1)/sampleSize, sum(treatInd[setInt1]),  sum( (treatInd[setInt1])==0 ) ) )
  
}



