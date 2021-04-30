##############################################################################################
##############################################################################################
##### CREATED 1/28/2018 #####
#install.packages("lqa")
###predictors correlation of 0.2
rm(list=ls())
library("lqa") # version 1.0-3
library(MASS) # version 3.3.1
library("mgcv")
require(glmnet)

#########################################################
#########################################################
DIREC="/home/tkzhou/PSPP_Project/PENCOMP_OneTimePoint/variableSelection5/case2/"
DIRECOUT="/home/tkzhou/PSPP_Project/PENCOMP_OneTimePoint/variableSelection5/case2/sampleSize200"

funLoc = "Functions/"
  
source(paste0(DIREC, funLoc, "addFun.R"))
source(paste0(DIREC, funLoc, "formulaConstruct.R"))
source(paste0(DIREC, funLoc, "simDataAll.R"))
source(paste0(DIREC, funLoc, "variableSelectY.R"))
source(paste0(DIREC, funLoc, "variableSelectT.R"))
source(paste0(DIREC, funLoc, "variableSelectT2.R"))
source(paste0(DIREC, funLoc, "allLasso.R"))
source(paste0(DIREC, funLoc, "pencompFit.R"))

numRun=500

############IPTW, AIPTW and PENCOMP estimators###############
###standard CI
estFinal_iptw=matrix(NA, nrow=numRun, ncol=4)
estFinal_aiptw=matrix(NA, nrow=numRun, ncol=4)
estFinal_pencomp=matrix(NA, nrow=numRun, ncol=4)

###bagging estimator
estFinal_iptw_bag=matrix(NA, nrow=numRun, ncol=4)
estFinal_aiptw_bag=matrix(NA, nrow=numRun, ncol=4)
estFinal_pencomp_bag=matrix(NA, nrow=numRun, ncol=4)

###percentiles
estFinal_iptw_per=matrix(NA, nrow=numRun, ncol=4)
estFinal_aiptw_per=matrix(NA, nrow=numRun, ncol=4)
estFinal_pencomp_per=matrix(NA, nrow=numRun, ncol=4)

###Rubin's combining rule
estFinal_pencomp_rubin=matrix(NA, nrow=numRun, ncol=4)
varSelPropY=matrix(NA, nrow=numRun, ncol = 20)
varSelPropT=matrix(NA, nrow=numRun, ncol = 20)



start=201
end=300
for(d in start:end)
  {
tryCatch (
  {
  
  numT=1000
  
  
sampleSize=200
  numPred=20 ##number of predictors
level="high"
  
simdatG=simulateDate(sampleSize=sampleSize, numPred=numPred, overlapL=level, seed.num=d, rho=0, treatEff=2)
  simdat=simdatG[[1]]
  varList=simdatG[[2]]
  
  outcome.varname="Y"
  treat.varname="A"
  splineTerm="s(pslogit, bs=\"ps\", k=15)"  ###
    
  firstNum="treat" ###adaptive lasso on the outcome first and then propensity score model
  Method="REML"
  
  ##for both propensity and prediction models
  modelType="allLasso" ###seperate adaptive lasso on the propensity and prediction models
  outcomeVarList0=NULL
  outcomeVarList1=NULL
  propenVarList=NULL
  
  
  ##############################################################################################################################
  ####################################################################################################################
  # print out IPTW, AIPTW and PENCOMP estimates corresponding to smallest wAMD value
  estimate.out=NULL
  estimate.out=pencompAllLasso(dataOR=simdat, data=simdat, varList=varList, propenVarList=propenVarList,
                              outcomeVarList0=outcomeVarList0, outcomeVarList1=outcomeVarList1,
                              treat.varname=treat.varname, outcome.varname=outcome.varname) 
  
  
 if( typeof(estimate.out) == "list" ){ ###if output is list, should be right

  estFinal_iptw[d,1]=estimate.out$out[1]
  estFinal_aiptw[d,1]=estimate.out$out[2]
  estFinal_pencomp[d,1]=estimate.out$out[3]

  }


  

  estimate.boot=matrix(NA, nrow=numT, ncol=3)  ###IPTW, AIPTW and PENCOMP estimates from each bootstrap sample
  pencomp.rubin=matrix(NA, nrow=numT, ncol=2)  ###variance of PENCOMP
  pencomp.numKnot=matrix(NA, nrow=numT, ncol=2)  ###number of knots in PENCOMP
  
  varSelectTreat=matrix( NA, nrow=numT, ncol=length(varList) )  ### coefficients of variables in propensity model
  varSelectY1=matrix( NA, nrow=numT, ncol=length(varList) )  ### coefficients of variables in the outcome model Y0
  varSelectY0=matrix( NA, nrow=numT, ncol=length(varList) )  ### coefficients of variables in the outcome modely1
  
  countMat=matrix(NA, ncol=nrow(simdat), nrow=numT)
  
  
  for(ind in 1:numT){
   tryCatch (
   {
    
    set.seed(ind)
    
    bootSample = simdat[sample(1:nrow(simdat),replace=T),]  ###random bootstraps

    
    tempCount=numeric(nrow(bootSample))
    for(countIndex in 1:length(tempCount)){
      tempCount[countIndex] = sum(bootSample$id2==countIndex)
    }
    
    mulResult = pencompAllLasso(dataOR=simdat, data=bootSample, varList=varList, propenVarList=propenVarList,
                 outcomeVarList0=outcomeVarList0, outcomeVarList1=outcomeVarList1,
                 treat.varname=treat.varname, outcome.varname=outcome.varname ) 
    
    if( typeof(mulResult) == "list" ){ ###if output is list, should be right
      
      estimate.boot[ind,] = (mulResult$out)[c(1, 2, 3)]
      pencomp.rubin[ind,] = (mulResult$out)[c(4, 5)]
      varSelectTreat[ind,]=mulResult$varTreat
      varSelectY1[ind,]=mulResult$varY1
      varSelectY0[ind,]=mulResult$varY0
      countMat[ind,]=tempCount
      pencomp.numKnot[ind,]=mulResult$numK  ###number of knots in PENCOMP
      
    }
  
 }
,
error=function(e) { }
 )
    
    
    
  }
  
  
  if(d < 10){
  ####store bootstrap estimates 
  bootResult=cbind(estimate.out$out[1], estimate.out$out[2], estimate.out$out[3], estimate.boot, pencomp.rubin, pencomp.numKnot)
  write.table(bootResult, paste(DIRECOUT, "/bootResults/sample", d, modelType, "_", level, ".txt",sep=""), row.name=F, quote=F, sep="\t", 
              col.names = c("iptwOR", "aiptwOR","pencompOR", "iptw", "aiptw","pencompBoot", "pencompRubin", "pencompRubinVar", "K0", "K1"))
  }
  
  ####store counts of each datapoint in each bootstrap (for calculating Brad Efron's CI)
  #write.table(countMat, paste(DIRECOUT, "/bootResults/sample", d, modelType, "_", level, "countMat.txt",sep=""), row.name=F, quote=F, sep="\t")
  
  
varSelPropY[d,]=colMeans(varSelectY0, na.rm = T)
varSelPropT[d,]=colMeans(varSelectTreat, na.rm = T)


write.table(varSelPropY, paste(DIRECOUT, "/Results/", modelType, "_", level, "_start_", start, "varSelectY.txt",sep=""), row.name=F, quote=F, col.names = varList,
           sep="\t")
write.table(varSelPropT, paste(DIRECOUT, "/Results/", modelType, "_", level, "_start_", start, "varSelectTreat.txt",sep=""), row.name=F, quote=F, col.names = varList,
           sep="\t")
  
  ####store coefficients of outcome model Y1
 # write.table(varSelectY1, paste(DIRECOUT, "/bootResults/sample", d, modelType, "_", level, "varSelectY1.txt",sep=""), row.name=F, quote=F, col.names = varList,
 #             sep="\t")
  
  
  #########standard confidence interval, Rubin's combining rule for PENCOMP
  estFinal_pencomp_rubin[d,]=processPENCOMP(t(pencomp.rubin))
  
  estFinal_iptw[d, 2:4]=c( sd(estimate.boot[,1], na.rm = T), estFinal_iptw[d,1] + c(-1, 1)*1.96*sd(estimate.boot[, 1], na.rm = T) )
  estFinal_aiptw[d, 2:4]=c( sd(estimate.boot[,2], na.rm = T), estFinal_aiptw[d,1] + c(-1, 1)*1.96*sd(estimate.boot[,2], na.rm = T) )
  estFinal_pencomp[d, 2:4]=c( sd(estimate.boot[,3], na.rm = T), estFinal_pencomp[d,1] + c(-1, 1)*1.96*sd(estimate.boot[,3], na.rm = T) )
  
  
  ##############################################
  #### bagging estimator accounting for model selection
  estFinal_iptw_bag[d,]=bagging2(countMat=countMat, estimate=estimate.boot[,1], sampleSize=sampleSize)
  estFinal_aiptw_bag[d,]=bagging2(countMat=countMat, estimate=estimate.boot[,2], sampleSize=sampleSize)
  estFinal_pencomp_bag[d,]=bagging2(countMat=countMat, estimate=estimate.boot[,3], sampleSize=sampleSize)
  
  
  ##############################################
  #### confidence interval based on quantiles
  estFinal_iptw_per[d,]=percentile(estimate=estimate.boot[,1])
  estFinal_aiptw_per[d,]=percentile(estimate=estimate.boot[,2])
  estFinal_pencomp_per[d,]=percentile(estimate=estimate.boot[,3])
  
  
  resultTable=NULL
  resultTable=data.frame(estFinal_pencomp, estFinal_pencomp_bag, estFinal_pencomp_per, estFinal_pencomp_rubin)
  write.table(resultTable, paste(DIRECOUT, "/Results/pencomp_", modelType, "_", level, "_start_", start, ".txt",sep=""), row.name=F, quote=F, sep="\t")
  
  resultTable=NULL
  resultTable=data.frame(estFinal_iptw, estFinal_iptw_bag, estFinal_iptw_per)
  write.table(resultTable, paste(DIRECOUT, "/Results/iptw_", modelType, "_", level, "_start_", start, ".txt",sep=""), row.name=F, quote=F, sep="\t")
  
  resultTable=NULL
  resultTable=data.frame(estFinal_aiptw, estFinal_aiptw_bag, estFinal_aiptw_per)
  write.table(resultTable, paste(DIRECOUT, "/Results/aiptw_", modelType, "_", level, "_start_", start, ".txt",sep=""), row.name=F, quote=F, sep="\t")
  
    
 }
,
error=function(e) { }
 )
}




