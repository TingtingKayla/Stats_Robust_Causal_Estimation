
##define some functions for generating data, ATE estimates, and the wAMD,
expit = function(x){ 
  pr = ( exp(x) / (1+exp(x)) ) 
  return(pr)
}




########################IPTW and AIPTW estimates######################
########################IPTW estimator################################
IPTW_est = function(Yobs, weight, treatInd){
  
  ATE_iptw= ( sum(weight * Yobs * (treatInd==1))/sum(weight * (treatInd==1)) ) - ( sum(weight * Yobs * (treatInd==0))/sum(weight * (treatInd==0)) )
  
  return( ATE_iptw )
  
}


###AIPTW estimator
AIPTW_est = function(Yobs, propenScore, treatInd, pred1, pred0){
  
  
  ###doubly robust weighted estimator
  ATE_aiptw=sum( (Yobs * treatInd) / propenScore - pred1*(treatInd - propenScore) / propenScore) / length(Yobs) -
    sum( (Yobs*(1-treatInd) ) / (1 - propenScore) + pred0*(treatInd - propenScore) / (1-propenScore) ) / length(Yobs)
  
  return(ATE_aiptw)
  
  
}


########### create weight for each subject
create_weights = function(propenScore, treatInd){
  
  fw = (propenScore)^(-1)
  fw[treatInd==0] = (1 - propenScore[treatInd==0])^(-1)
  return(fw)
  
}




###########weighted mean difference#############
wAMD_function = function(DataM, varlist, trt.var, wgt, beta){
  
  trt = untrt = diff_vec = rep(NA,length(beta)) 
  names(trt) = names(untrt) = names(diff_vec) = varlist
  
  for(jj in 1:length(varlist)){ 
    
    this.var = paste("w",varlist[jj],sep="") 
    DataM[,this.var] = DataM[,varlist[jj]] * DataM[,wgt] 
    
    trt[jj] = sum( DataM[DataM[,trt.var]==1, this.var ]) / sum(DataM[DataM[,trt.var]==1, wgt]) 
    untrt[jj] = sum(DataM[DataM[,trt.var]==0, this.var]) / sum(DataM[DataM[,trt.var]==0, wgt]) 
    
    diff_vec[jj] = abs( trt[jj] - untrt[jj] ) 
    
  } 
  
  wdiff_vec = diff_vec * abs(beta) 
  wAMD = c( sum(wdiff_vec) )
  ret = list( diff_vec = diff_vec, wdiff_vec = wdiff_vec, wAMD = wAMD )
  return(ret) 
  
}







################generating a bootstrap sample from the original dataset
###### stratified bootstraps ##########################################
genSim=function(inputData, setNum) {
  
  set.seed(setNum)
  inputData$id2=1:dim(inputData)[1]
  inputData$id=1
  tempD=inputData[sample(1:dim(inputData)[1],replace=T),]
  return(tempD)
  
}



##########################process the bootstrap estimates ##############
processPENCOMP=function(mulResult){
  
  x=mulResult[1,]
  y=mulResult[2,]
  x=x[!is.na(x)]
  y=y[!is.na(y)]
  
  numT=length(x)
  theta_d=mean(x) 
  Wd=mean(y)  ### within imputation variance
  
  Bd=(1/(numT-1))*(sum((x-mean(x))^2))###between imputation variance
  Td=Wd+(1+1/numT)*Bd ###total variability associated with mean
  
  v=(numT-1)*(1+(1/(1/numT+1))*(Wd/Bd))^2 ##degree of freedom
  
  return( c(theta_d[1], sqrt(Td[1]),theta_d[1] + c(-1,1)*qt(0.975,v[1])*sqrt(Td[1]))  )
  
}



#dataOR=simdat;pspp0=pspp0; pspp0Sd=sd(pspp0$residuals);
#pspp1=pspp1; pspp1Sd=sd(pspp1$residuals); propenScore=simdat[,paste("f.pA",lil,sep="")];
#treat.varname="A"; treatInd=simdat$A; Yobs=simdat$Y

#########################################################################
pencomp_est=function(dataOR, pspp0, pspp0Sd, pspp1, pspp1Sd, propenScore, treat.varname, treatInd, Yobs){
  
  #########################################
  ###imputing the missing potential outcomes under control with draws
  newData0=dataOR
  newData0[, treat.varname]=0
  predict2a=1-propenScore
  newData0$pslogit=log(predict2a/(1-predict2a))
  
  fv <- predict.gam(pspp0, newData0, se.fit=TRUE)
  imputed0 = fv$fit + rnorm(dim(newData0)[1], 0, pspp0Sd)
  mean(imputed0)
  
  ####################################################################
  ####imputing the missing potential outcomes under treatment#########
  newData1=dataOR
  newData1[, treat.varname]=1
  predict2a=propenScore
  newData1$pslogit=log(predict2a/(1-predict2a))
  
  fv <- predict.gam(pspp1, newData1, se.fit=TRUE)
  imputed1 = fv$fit + rnorm(dim(newData1)[1], 0, pspp1Sd)
  mean(imputed1)
  
  ##############keep the observed outcome the same#####################
  imputed1[treatInd==1]=Yobs[treatInd==1]
  imputed0[treatInd==0]=Yobs[treatInd==0]

  ATE_pencomp=mean(imputed1 - imputed0)
  ATE_pencompVar=var(imputed1 - imputed0)/length(imputed1)
  
  return( c(ATE_pencomp, ATE_pencompVar) )
}





############### the bagging estimator
bagging=function(countMat, estimate, sampleSize){
  
  countj=colSums(countMat)
  countMat_s = countMat-matrix(countj, nrow=dim(countMat)[1], ncol=sampleSize, byrow = T)
  estimate_s = matrix(estimate - mean(estimate), nrow = dim(countMat)[1], ncol=sampleSize, byrow = F)
  countMat_s2= countMat_s * estimate_s / dim(countMat)[1]
  varTemp=colSums(countMat_s2)
  sdEst=sqrt(sum(varTemp*varTemp))
  
  theta_d=mean(estimate[estimate!=0]) 
  return( c(theta_d[1], sdEst, theta_d[1] + c(-1,1)*1.96*sdEst) )
  
}



############### the bagging estimator
bagging2=function(countMat, estimate, sampleSize){  ###need to be careful about the dimension of countMat here is numBoot by sampleSize

  if(sum(is.na(estimate) !=0)){
    
    naIndex = which(is.na(estimate))
    estimate = estimate[-c(naIndex)]
    countMat = countMat[-c(naIndex), ]
    
  }
  
  sdEst = sqrt( sum(sapply(1:sampleSize, function(k){ cov(countMat[,k], estimate)^2 })) )

  theta_d=mean(estimate[estimate!=0]) 
  return( c(theta_d[1], sdEst, theta_d[1] + c(-1,1)*1.96*sdEst) )
  
}






############### the bagging estimator
percentile=function(estimate){
  
  return( c( mean(estimate, na.rm = T), sd(estimate, na.rm = T),
            quantile(estimate, probs = c(0.025, 0.975), na.rm = T) ) )
  
}




############### the bagging estimator
bagging=function(countMat, estimate, sampleSize){
  
  countj=colSums(countMat)
  countMat_s = countMat-matrix(countj, nrow=dim(countMat)[1], ncol=sampleSize, byrow = T)
  estimate_s = matrix(estimate - mean(estimate), nrow = dim(countMat)[1], ncol=sampleSize, byrow = F)
  countMat_s2= countMat_s * estimate_s / dim(countMat)[1]
  varTemp=colSums(countMat_s2)
  sdEst=sqrt(sum(varTemp^2))
  
  theta_d=mean(estimate[estimate!=0]) 
  return( c(theta_d[1], sdEst, theta_d[1] + c(-1,1)*1.96*sdEst) )
  
}






###########################################################################################################
######function to calculate standardized difference in term of propensity score between treated and control 

#DataM=inputData;varlist=var.list; trt.var=treat.varname; propenScore=inputData[,paste("f.pA",lil,sep="")]; beta=betaXY

stdDiff <- function(DataM, varlist, trt.var, propenScore, beta) {  ### 
  
  diff_vec2 = rep(NA,length(beta)) 
  names(diff_vec2) = varlist
  
  treatInd = DataM[, trt.var]
  meanTreat=mean(propenScore[treatInd==1] ) 
  meanControl=mean(propenScore[treatInd==0] )
  
  s2_T=var(propenScore[treatInd==1] )  
  s2_C=var(propenScore[treatInd==0] ) 
  
  diffProp=abs((meanTreat - meanControl)/sqrt( (s2_T + s2_C)/2 ))
  
  for(jj in 1:length(varlist)){ 
    
    ####################
    meanTreat=NULL
    meanControl=NULL
    s2_T = NULL
    s2_C = NULL
    
    meanTreat=mean(DataM[treatInd==1, varlist[jj]] ) 
    meanControl=mean(DataM[treatInd==0, varlist[jj]] ) 
    
    s2_T=var(DataM[treatInd==1, varlist[jj]] )  
    s2_C=var(DataM[treatInd==0, varlist[jj]] ) 
    
    diff_vec2[jj]=abs((meanTreat - meanControl)/sqrt( (s2_T + s2_C)/2 ))
    
  } 
  
  #wdiff_vec = diff_vec2 * abs(beta) 
  #wAMD = c( sum(wdiff_vec) )
  
  wdiff_vec = diff_vec2
  wAMD = c( sum(wdiff_vec) )
  
  ret = list( diffProp=diffProp, wdiff_vec = wdiff_vec, wAMD = wAMD )
  return(ret) 
  
}



