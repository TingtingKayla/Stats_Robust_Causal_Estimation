##############################################################################################
##############################################################################################
################PENCOMP #########################################################################

rm(list=ls())
library(splines)
library(nlme)
require(stats)
require(graphics)
#library("rootSolve")
library("mgcv")
require(glmnet)
library("lqa")

#########################################################
#########################################################

#########################################################
DIREC="M:/Private/overlap/Application/"
DIRECOUT="M:/Private/overlap/Application/version4"

#DIREC="/home/tkzhou/PSPP_Project/applicationChapter2/"
#DIRECOUT="/home/tkzhou/PSPP_Project/applicationChapter2/version4"

funLoc = "version4/Functions/"

source(paste0(DIREC, funLoc, "addFun.R"))
source(paste0(DIREC, funLoc, "formulaConstruct.R"))
source(paste0(DIREC, funLoc, "simDataAll.R"))
source(paste0(DIREC, funLoc, "variableSelectY.R"))
source(paste0(DIREC, funLoc, "variableSelectT.R"))
source(paste0(DIREC, funLoc, "variableSelectT2.R"))
source(paste0(DIREC, funLoc, "LassoStepwise.R"))
source(paste0(DIREC, funLoc, "pencompFit.R"))

###import the dataset
simdat2=read.csv(paste(DIREC, "dataset/data.csv", sep=""), header=T)

dim(simdat2)
table(simdat2$yearPos)
table(simdat2$visit)

summary(simdat2$age)
hist(simdat2$age)
table(simdat2$white)
table(simdat2$college)

################################################################################
###the start and the end of the visits considered in this analysis
start= 7
end= 21

numRun=end
numT=1000

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



simdat2=simdat2[simdat2$hivVisit40 == 1, ] #### keep only the hiv+ in the baseline of each 3 time point window

table(simdat2$hivVisit40)  ###1641 subjects


for (k in start:end) {
  
  print(k)
  
  set.seed(k)
  
  ####every one time point
  #####here I use the blood count measures and treatment history during the past two visits as baseline covariates ###
  ####the outcome of interest is CD4 counts after 6 months
  
  ############for every 3-visit moving window, run the following script
  ########################################################
  simdat=data.frame(CASEID=simdat2$CASEID)
  
  ###treatment indicator at first time point A1, at second time point A2
  simdat$A1=simdat2[,which(names(simdat2)==paste("TreatVisit", (k-2)*10, sep=""))] 
  simdat$A2=simdat2[,which(names(simdat2)==paste("TreatVisit", (k-1)*10, sep=""))] 
  simdat$A3=simdat2[,which(names(simdat2)==paste("TreatVisit", (k)*10, sep=""))] 
  simdat$A4=simdat2[,which(names(simdat2)==paste("TreatVisit", (k+1)*10, sep=""))] 
  
  
  ###CD4 count, LEU3N1 at first time point, LEU3N2 outcome of interest after treatment A1
  simdat$LEU3N1=simdat2[,which(names(simdat2)==paste("LEU3N.", (k-3)*10, sep=""))]  ###
  simdat$LEU3N2=simdat2[,which(names(simdat2)==paste("LEU3N.", (k-2)*10, sep=""))]
  simdat$LEU3N3=simdat2[,which(names(simdat2)==paste("LEU3N.", (k-1)*10, sep=""))]  ###
  simdat$LEU3N4=simdat2[,which(names(simdat2)==paste("LEU3N.", (k)*10, sep=""))]  ###
  simdat$LEU3N5=simdat2[,which(names(simdat2)==paste("LEU3N.", (k+1)*10, sep=""))]  ###outcome
  
  ###########CD8 counts
  simdat$LEU2N1=simdat2[,which(names(simdat2)==paste("LEU2N.", (k-3)*10, sep=""))]  ###
  simdat$LEU2N2=simdat2[,which(names(simdat2)==paste("LEU2N.", (k-2)*10, sep=""))]
  simdat$LEU2N3=simdat2[,which(names(simdat2)==paste("LEU2N.", (k-1)*10, sep=""))]  ###
  simdat$LEU2N4=simdat2[,which(names(simdat2)==paste("LEU2N.", (k)*10, sep=""))]  ###
  simdat$LEU2N5=simdat2[,which(names(simdat2)==paste("LEU2N.", (k+1)*10, sep=""))]  ###outcome
  
  ###white blood cell counts
  simdat$WBC1=simdat2[,which(names(simdat2)==paste("WBC.", (k-3)*10, sep=""))]
  simdat$WBC2=simdat2[,which(names(simdat2)==paste("WBC.", (k-2)*10, sep=""))]
  simdat$WBC3=simdat2[,which(names(simdat2)==paste("WBC.", (k-1)*10, sep=""))]
  simdat$WBC4=simdat2[,which(names(simdat2)==paste("WBC.", (k)*10, sep=""))]
  simdat$WBC5=simdat2[,which(names(simdat2)==paste("WBC.", (k+1)*10, sep=""))]
  
  
  ###Red blood cell counts
  simdat$RBC1=simdat2[,which(names(simdat2)==paste("RBC.", (k-3)*10, sep=""))]
  simdat$RBC2=simdat2[,which(names(simdat2)==paste("RBC.", (k-2)*10, sep=""))]
  simdat$RBC3=simdat2[,which(names(simdat2)==paste("RBC.", (k-1)*10, sep=""))]
  simdat$RBC4=simdat2[,which(names(simdat2)==paste("RBC.", (k)*10, sep=""))]
  simdat$RBC5=simdat2[,which(names(simdat2)==paste("RBC.", (k+1)*10, sep=""))]
  
  
  ###platelet counts
  simdat$PLATE1=simdat2[,which(names(simdat2)==paste("PLATE.", (k-3)*10, sep=""))]
  simdat$PLATE2=simdat2[,which(names(simdat2)==paste("PLATE.", (k-2)*10, sep=""))]
  simdat$PLATE3=simdat2[,which(names(simdat2)==paste("PLATE.", (k-1)*10, sep=""))]
  simdat$PLATE4=simdat2[,which(names(simdat2)==paste("PLATE.", (k)*10, sep=""))]
  simdat$PLATE5=simdat2[,which(names(simdat2)==paste("PLATE.", (k+1)*10, sep=""))]
  
  ##viral load
  simdat$VLOAD1=simdat2[,which(names(simdat2)==paste("VLOAD.", (k-3)*10, sep=""))]
  simdat$VLOAD2=simdat2[,which(names(simdat2)==paste("VLOAD.", (k-2)*10, sep=""))]
  simdat$VLOAD3=simdat2[,which(names(simdat2)==paste("VLOAD.", (k-1)*10, sep=""))]
  simdat$VLOAD4=simdat2[,which(names(simdat2)==paste("VLOAD.", (k)*10, sep=""))]
  simdat$VLOAD5=simdat2[,which(names(simdat2)==paste("VLOAD.", (k+1)*10, sep=""))]
  
  
  ####demographic information
  simdat$age=simdat2[,which(names(simdat2)=="age")]
  simdat$white=simdat2[,which(names(simdat2)=="white")]
  simdat$college=simdat2[,which(names(simdat2)=="college")]
  simdat$deathYear=simdat2[,which(names(simdat2)=="DTHDATEyy")]
  
  ####transform the blood values by taking the square root, distributions of raw values are highly skewed
  simdat$LEU3N1=sqrt(simdat$LEU3N1)
  simdat$LEU2N1=sqrt(simdat$LEU2N1) 
  simdat$WBC1=sqrt(simdat$WBC1)  
  simdat$RBC1=sqrt(simdat$RBC1)
  simdat$PLATE1=sqrt(simdat$PLATE1)
  
  simdat$LEU3N2=sqrt(simdat$LEU3N2)
  simdat$LEU2N2=sqrt(simdat$LEU2N2) 
  simdat$WBC2=sqrt(simdat$WBC2)  
  simdat$RBC2=sqrt(simdat$RBC2)
  simdat$PLATE2=sqrt(simdat$PLATE2)
  
  
  simdat$LEU3N3=sqrt(simdat$LEU3N3)
  simdat$LEU2N3=sqrt(simdat$LEU2N3) 
  simdat$WBC3=sqrt(simdat$WBC3)  
  simdat$RBC3=sqrt(simdat$RBC3)
  simdat$PLATE3=sqrt(simdat$PLATE3)
  
  simdat$LEU3N4=sqrt(simdat$LEU3N4)
  simdat$LEU2N4=sqrt(simdat$LEU2N4) 
  simdat$WBC4=sqrt(simdat$WBC4)  
  simdat$RBC4=sqrt(simdat$RBC4)
  simdat$PLATE4=sqrt(simdat$PLATE4)
  
  simdat$LEU3N5=sqrt(simdat$LEU3N5)
  simdat$LEU2N5=sqrt(simdat$LEU2N5) 
  simdat$WBC5=sqrt(simdat$WBC5)  
  simdat$RBC5=sqrt(simdat$RBC5)
  simdat$PLATE5=sqrt(simdat$PLATE5)
  
  
  dim(simdat)
  
  sum(is.na(simdat$LEU3N1))
  sum(is.na(simdat$PLATE1))  
  sum(is.na(simdat$WBC1))
  sum(is.na(simdat$RBC1))  
  sum(is.na(simdat$RBC2))
  sum(is.na(simdat$LEU3N2))
  
  
  ######for our analysis, we looked only the complete data for each 3-visit window, avoid dealing with lost to followup/death topic of future research
  simdatFit=simdat[which(!is.na(simdat$LEU3N1) & !is.na(simdat$LEU2N1) & !is.na(simdat$WBC1) & 
                           !is.na(simdat$RBC1) & !is.na(simdat$PLATE1) & !is.na(simdat$A1) &
                           
                           !is.na(simdat$LEU3N2) & !is.na(simdat$LEU2N2) & !is.na(simdat$WBC2) & 
                           !is.na(simdat$RBC2) & !is.na(simdat$PLATE2) & 
                           
                           
                           !is.na(simdat$LEU3N3) & !is.na(simdat$LEU2N3) & !is.na(simdat$WBC3) & 
                           !is.na(simdat$RBC3) & !is.na(simdat$PLATE3) & 
                           
                           !is.na(simdat$LEU3N4) & !is.na(simdat$LEU2N4) & !is.na(simdat$WBC4) & 
                           !is.na(simdat$RBC4) & !is.na(simdat$PLATE4) & 
                           
                           
                           !is.na(simdat$LEU3N5) & !is.na(simdat$LEU2N5) & !is.na(simdat$WBC5) & 
                           !is.na(simdat$RBC5) & !is.na(simdat$PLATE5) & 
                           
                           !is.na(simdat$A1)  & !is.na(simdat$A2) & !is.na(simdat$A3)  & !is.na(simdat$A4) & !is.na(simdat$white)
                         & !is.na(simdat$college) & !is.na(simdat$age)),]
  
  dim(simdatFit)
  sampleSize=nrow(simdatFit)
  
  #simdatFit$A0=as.factor(simdatFit$A0)
  simdatFit$id2=1:dim(simdatFit)[1]
  simdatFit$id=1
  
  sum(simdatFit$A4==1)
  sum(simdatFit$A4==0)
  
  mean(simdatFit$LEU3N2[(simdatFit$A4==1)], na.rm=T)
  mean(simdatFit$LEU3N2[(simdatFit$A4==0)], na.rm=T)
  
  
  varList=NULL
  ############################################
  if(k==7){
    
    varList = c(paste0("LEU3N", 1:4), paste0("LEU2N", 1:4), paste0("WBC", 1:4), paste0("RBC", 1:4), paste0("PLATE", 1:4),
                "age", "white", "college") 
    outcomeVarList0=NULL
    outcomeVarList1=NULL
    propenVarList=NULL
    
  } else if(k==8){
    
    varList = c(paste0("LEU3N", 1:4), paste0("LEU2N", 1:4), paste0("WBC", 1:4), paste0("RBC", 1:4), paste0("PLATE", 1:4),
                "age", "white", "college", paste0("A", 3)) 
    outcomeVarList0=NULL
    outcomeVarList1=NULL
    propenVarList=NULL
    
    
  } else if(k==9){
    varList = c(paste0("LEU3N", 1:4), paste0("LEU2N", 1:4), paste0("WBC", 1:4), paste0("RBC", 1:4), paste0("PLATE", 1:4),
                "age", "white", "college", paste0("A", c(2,3))) 
    outcomeVarList0=NULL
    outcomeVarList1=NULL
    propenVarList=NULL
    
    
  } else if (k>=10){
    varList = c(paste0("LEU3N", 1:4), paste0("LEU2N", 1:4), paste0("WBC", 1:4), paste0("RBC", 1:4), paste0("PLATE", 1:4),
                "age", "white", "college", paste0("A", 1:3)) 
    outcomeVarList0=NULL
    outcomeVarList1=NULL
    propenVarList=NULL
  }
  
  
  ##for both propensity and prediction models
  modelType="lassoStepY"
  treat.varname="A4"
  outcome.varname="LEU3N5" 
  firstNum="outcome"

  splineTerm="s(pslogit, bs=\"ps\", k=15)"  ###
  
  ##############################################################################################################################
  ####################################################################################################################
  # print out IPTW, AIPTW and PENCOMP estimates corresponding to smallest wAMD value
  estimate.out=NULL
  estimate.out=pencompLassoStepwise(dataOR=simdatFit, data=simdatFit, varList=varList, propenVarList=propenVarList,
                                    outcomeVarList0=outcomeVarList0, outcomeVarList1=outcomeVarList1,
                                    treat.varname=treat.varname, outcome.varname=outcome.varname, first=firstNum ) 
  
  
  
  if( typeof(estimate.out) == "list" ){ ###if output is list, should be right
    
    estFinal_iptw[k,1]=estimate.out$out[1]
    estFinal_aiptw[k,1]=estimate.out$out[2]
    estFinal_pencomp[k,1]=estimate.out$out[3]
    
  }
  
  
  
  estimate.boot=matrix(NA, nrow=numT, ncol=3)  ###IPTW, AIPTW and PENCOMP estimates from each bootstrap sample
  pencomp.rubin=matrix(NA, nrow=numT, ncol=2)  ###variance of PENCOMP
  pencomp.numKnot=matrix(NA, nrow=numT, ncol=2)  ###number of knots in PENCOMP
  
  varSelectTreat=matrix( NA, nrow=numT, ncol=length(varList) )  ### coefficients of variables in propensity model
  varSelectY1=matrix( NA, nrow=numT, ncol=length(varList) )  ### coefficients of variables in the outcome model Y0
  varSelectY0=matrix( NA, nrow=numT, ncol=length(varList) )  ### coefficients of variables in the outcome modely1
  
  countMat=matrix(NA, ncol=nrow(simdatFit), nrow=numT)
  
  
  for(ind in 1:numT){
    tryCatch (
      {
        print(ind)
        set.seed(ind)
        
        bootSample = simdatFit[sample(1:nrow(simdatFit),replace=T),]  ###random bootstraps
        
        
        tempCount=numeric(nrow(bootSample))
        for(countIndex in 1:length(tempCount)){
          tempCount[countIndex] = sum(bootSample$id2==countIndex)
        }
        
        mulResult = pencompLassoStepwise(dataOR=simdatFit, data=bootSample, varList=varList, propenVarList=propenVarList,
                                         outcomeVarList0=outcomeVarList0, outcomeVarList1=outcomeVarList1,
                                         treat.varname=treat.varname, outcome.varname=outcome.varname, first=firstNum ) 
        
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
  
  
  ####store bootstrap estimates 
  bootResult=cbind(estimate.out$out[1], estimate.out$out[2], estimate.out$out[3], estimate.boot, pencomp.rubin, pencomp.numKnot)
  write.table(bootResult, paste(DIRECOUT, "/bootResults/sample", k, modelType, ".txt",sep=""), row.name=F, quote=F, sep="\t", 
              col.names = c("iptwOR", "aiptwOR","pencompOR", "iptw", "aiptw","pencompBoot", "pencompRubin", "pencompRubinVar", "K0", "K1"))
  
  ####store counts of each datapoint in each bootstrap (for calculating Brad Efron's CI)
  #write.table(countMat, paste(DIRECOUT, "/bootResults/sample", k, modelType, "_", "countMat.txt",sep=""), row.name=F, quote=F, sep="\t")
  
  ####store coefficients of propensity model
  write.table(varSelectTreat, paste(DIRECOUT, "/bootResults/sample", k, modelType, "_", "varSelectTreat.txt",sep=""), row.name=F, quote=F, col.names = varList,
              sep="\t")
  
  ####store coefficients of outcome model Y0
  write.table(varSelectY0, paste(DIRECOUT, "/bootResults/sample", k, modelType, "_", "varSelectY0.txt",sep=""), row.name=F, quote=F, col.names = varList,
              sep="\t")
  
  ####store coefficients of outcome model Y1
  #write.table(varSelectY1, paste(DIRECOUT, "/bootResults/sample", k, modelType, "_", "varSelectY1.txt",sep=""), row.name=F, quote=F, col.names = varList,
  #            sep="\t")
  
  
  #########standard confidence interval, Rubin's combining rule for PENCOMP
  estFinal_pencomp_rubin[k,]=processPENCOMP(t(pencomp.rubin))
  
  estFinal_iptw[k, 2:4]=c( sd(estimate.boot[,1], na.rm = T), estFinal_iptw[k,1] + c(-1, 1)*1.96*sd(estimate.boot[, 1], na.rm = T) )
  estFinal_aiptw[k, 2:4]=c( sd(estimate.boot[,2], na.rm = T), estFinal_aiptw[k,1] + c(-1, 1)*1.96*sd(estimate.boot[,2], na.rm = T) )
  estFinal_pencomp[k, 2:4]=c( sd(estimate.boot[,3], na.rm = T), estFinal_pencomp[k,1] + c(-1, 1)*1.96*sd(estimate.boot[,3], na.rm = T) )
  
  
  ##############################################
  #### bagging estimator accounting for model selection
  estFinal_iptw_bag[k,]=bagging2(countMat=countMat, estimate=estimate.boot[,1], sampleSize=sampleSize)
  estFinal_aiptw_bag[k,]=bagging2(countMat=countMat, estimate=estimate.boot[,2], sampleSize=sampleSize)
  estFinal_pencomp_bag[k,]=bagging2(countMat=countMat, estimate=estimate.boot[,3], sampleSize=sampleSize)
  
  
  ##############################################
  #### confidence interval based on quantiles
  estFinal_iptw_per[k,]=percentile(estimate=estimate.boot[,1])
  estFinal_aiptw_per[k,]=percentile(estimate=estimate.boot[,2])
  estFinal_pencomp_per[k,]=percentile(estimate=estimate.boot[,3])
  
  
  resultTable=NULL
  resultTable=data.frame(estFinal_pencomp, estFinal_pencomp_bag, estFinal_pencomp_per, estFinal_pencomp_rubin)
  write.table(resultTable, paste(DIRECOUT, "/Results/pencomp_", modelType, ".txt",sep=""), row.name=F, quote=F, sep="\t")
  
  resultTable=NULL
  resultTable=data.frame(estFinal_iptw, estFinal_iptw_bag, estFinal_iptw_per)
  write.table(resultTable, paste(DIRECOUT, "/Results/iptw_", modelType, ".txt",sep=""), row.name=F, quote=F, sep="\t")
  
  resultTable=NULL
  resultTable=data.frame(estFinal_aiptw, estFinal_aiptw_bag, estFinal_aiptw_per)
  write.table(resultTable, paste(DIRECOUT, "/Results/aiptw_", modelType, ".txt",sep=""), row.name=F, quote=F, sep="\t")
  
  
  
}







