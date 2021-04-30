########created 5/27/2017
###different way of formating the tables in the manuscript

###modified 7/14/2018,
###excluding multiple versions of stepwise, vs stepwise lasso with regular adaptive lasso and outcome adaptive lasso
rm(list=ls())


DIRECOUT="C:/Users/Tingting/Desktop/paper3/variableSelection/case2Results/"
measure1=c("bias", "sd","sdBoot", "RMSE", "coverage", "widthCI")

###functions
selectFuc=function(mydata, methods, modelType, way){
  
  index=NULL
  for(j in 1:length(measure1)){
    index=c(index, which(names(mydata)==measure1[j]))
  }
  temp=mydata[which(row.names(mydata)==paste0(methods,"_", modelType, "_", way)), index]
  
  return(temp)
  
}

modelTypeVal=c("allPot","truePropen","trueOutcomePred","trueConf", "stepwise", "lassoSep", "allLasso", "lassoOutcome", "lassoStepwiseT", "lassoStepwiseY")


for(sampleSize in c(200, 1000)){
  
    ############
    result1=NULL
    result1=read.table(paste0(DIRECOUT, "high_sampleSize", sampleSize, ".txt"), header=T, sep="\t")
    
    ###rowwise-correct, misPred, misWeight, columnwise-high, moderate, low
    #########IPTW###############
    output1=NULL
    for(k in 1:length(modelTypeVal)){
      temp=NULL
      temp=rbind(temp, 
                 selectFuc(result1, methods="iptw", modelType=modelTypeVal[k], way="standard"),
                 selectFuc(result1, methods="iptw", modelType=modelTypeVal[k], way="bagging"))
      output1 = rbind(output1, temp)
      
    }
    iptw_s2=output1  ###scenario 2
    
    
    ############
    result1=NULL
    result1=read.table(paste0(DIRECOUT, "moderate_sampleSize", sampleSize, ".txt"), header=T, sep="\t")
    
    ############################
    #########IPTW###############
    output1=NULL
    for(k in 1:length(modelTypeVal)){
      temp=NULL
      temp=rbind(temp, 
                 selectFuc(result1, methods="iptw", modelType=modelTypeVal[k], way="standard"),
                 selectFuc(result1, methods="iptw", modelType=modelTypeVal[k], way="bagging"))
      output1 = rbind(output1, temp)
      
    }
    
    iptw_s1=output1  ###scenario 1
    
    
    #################################
    result1=NULL
    result1=read.table(paste0(DIRECOUT, "low_sampleSize", sampleSize, ".txt"), header=T, sep="\t")
    
    ############################
    #########IPTW###############
    output1=NULL
    for(k in 1:length(modelTypeVal)){
      temp=NULL
      temp=rbind(temp, 
                 selectFuc(result1, methods="iptw", modelType=modelTypeVal[k], way="standard"),
                 selectFuc(result1, methods="iptw", modelType=modelTypeVal[k], way="bagging"))
      output1 = rbind(output1, temp)
      
    }
    
    iptw_s3=output1  ###scenario 1
    
    
    
    
    
    ###################################################################################################
    ############################## PENCOMP ############################################################
    
    ############
    result1=NULL
    result1=read.table(paste0(DIRECOUT, "high_sampleSize", sampleSize, ".txt"), header=T, sep="\t")
    
    ###rowwise-correct, misPred, misWeight, columnwise-high, moderate, low
    #########IPTW###############
    output1=NULL
    for(k in 1:length(modelTypeVal)){
      temp=NULL
      temp=rbind(temp, 
                 selectFuc(result1, methods="pencomp", modelType=modelTypeVal[k], way="rubin"),
                 selectFuc(result1, methods="pencomp", modelType=modelTypeVal[k], way="bagging"))
      output1 = rbind(output1, temp)
      
    }
    pencomp_s2=output1  ###scenario 2
    
    
    ############
    result1=NULL
    result1=read.table(paste0(DIRECOUT, "moderate_sampleSize", sampleSize, ".txt"), header=T, sep="\t")
    
    ############################
    #########IPTW###############
    output1=NULL
    for(k in 1:length(modelTypeVal)){
      temp=NULL
      temp=rbind(temp, 
                 selectFuc(result1, methods="pencomp", modelType=modelTypeVal[k], way="rubin"),
                 selectFuc(result1, methods="pencomp", modelType=modelTypeVal[k], way="bagging"))
      output1 = rbind(output1, temp)
      
    }
    
    pencomp_s1=output1  ###scenario 1
    
    
    #################################
    result1=NULL
    result1=read.table(paste0(DIRECOUT, "low_sampleSize", sampleSize, ".txt"), header=T, sep="\t")
    
    ############################
    #########IPTW###############
    output1=NULL
    for(k in 1:length(modelTypeVal)){
      temp=NULL
      temp=rbind(temp, 
                 selectFuc(result1, methods="pencomp", modelType=modelTypeVal[k], way="rubin"),
                 selectFuc(result1, methods="pencomp", modelType=modelTypeVal[k], way="bagging"))
      output1 = rbind(output1, temp)
      
    }
    
    pencomp_s3=output1  ###scenario 1
    
    
    ##############################################################################################
    ###################################### AIPTW ###################################################
    result1=NULL
    result1=read.table(paste0(DIRECOUT, "high_sampleSize", sampleSize, ".txt"), header=T, sep="\t")
    
    ###rowwise-correct, misPred, misWeight, columnwise-high, moderate, low
    #########IPTW###############
    output1=NULL
    for(k in 1:length(modelTypeVal)){
      temp=NULL
      temp=rbind(temp, 
                 selectFuc(result1, methods="aiptw", modelType=modelTypeVal[k], way="standard"),
                 selectFuc(result1, methods="aiptw", modelType=modelTypeVal[k], way="bagging"))
      output1 = rbind(output1, temp)
      
    }
    aiptw_s2=output1  ###scenario 2
    
    
    ############
    result1=NULL
    result1=read.table(paste0(DIRECOUT, "moderate_sampleSize", sampleSize, ".txt"), header=T, sep="\t")
    
    ############################
    #########IPTW###############
    output1=NULL
    for(k in 1:length(modelTypeVal)){
      temp=NULL
      temp=rbind(temp, 
                 selectFuc(result1, methods="aiptw", modelType=modelTypeVal[k], way="standard"),
                 selectFuc(result1, methods="aiptw", modelType=modelTypeVal[k], way="bagging"))
      output1 = rbind(output1, temp)
      
    }
    
    aiptw_s1=output1  ###scenario 1
    
    
    #################################
    result1=NULL
    result1=read.table(paste0(DIRECOUT, "low_sampleSize", sampleSize, ".txt"), header=T, sep="\t")
    
    ############################
    #########IPTW###############
    output1=NULL
    for(k in 1:length(modelTypeVal)){
      temp=NULL
      temp=rbind(temp, 
                 selectFuc(result1, methods="aiptw", modelType=modelTypeVal[k], way="standard"),
                 selectFuc(result1, methods="aiptw", modelType=modelTypeVal[k], way="bagging"))
      output1 = rbind(output1, temp)
      
    }
    
    aiptw_s3=output1  ###scenario 1
    
    
    
    #################################table for empirical bias##########
    varName="bias"
    bias=cbind(pencomp_s1[,varName], pencomp_s2[, varName], pencomp_s3[, varName],
              aiptw_s1[,varName], aiptw_s2[, varName], aiptw_s3[, varName],
              iptw_s1[,varName], iptw_s2[, varName], iptw_s3[, varName])
    bias=format(1000*bias, digits = 0)
    
  
    
    biasAll=cbind(rep("&", dim(bias)[1]), bias[,1], rep("&", dim(bias)[1]), bias[,2],  rep("&", dim(bias)[1]), bias[,3], rep("&", dim(bias)[1]),
                  rep("&", dim(bias)[1]), bias[,4], rep("&", dim(bias)[1]), bias[,5], 
                  rep("&", dim(bias)[1]), bias[,6], rep("&", dim(bias)[1]), rep("&", dim(bias)[1]), bias[,7], rep("&", dim(bias)[1]), bias[,8],
                  rep("&", dim(bias)[1]), bias[,9],
                  rep("\\\\", dim(bias)[1]))
    
    modelTypeOut=c("allPotent","true", "outcomePred","trueConf", "SW","AL", "allLasso", "OAL",
                   "Step-ALT","Step-ALY")
    
    
    biasAll=cbind(rep( c(rep(modelTypeOut[1], 2), rep(modelTypeOut[2], 2),
                         rep(modelTypeOut[3], 2),
                         rep(modelTypeOut[4], 2),
                         rep(modelTypeOut[5], 2), rep(modelTypeOut[6], 2),
                         rep(modelTypeOut[7], 2), rep(modelTypeOut[8], 2),rep(modelTypeOut[9], 2), rep(modelTypeOut[10], 2)), 1), biasAll)
    
    biasAll=cbind(rep(c("Standard/Rubin &", "Bagging &"), 10), biasAll)
    
    write.table(biasAll, paste(DIRECOUT, "sampleSize", sampleSize, "Paper_Table_", varName, ".txt",sep=""),  quote=F, col.names=F, row.names = F, sep="\t")
    
    
    
    #################################table for empirical bias##########
    varName="RMSE"
    bias=NULL
    bias=cbind(pencomp_s1[,varName], pencomp_s2[, varName], pencomp_s3[, varName],
               aiptw_s1[,varName], aiptw_s2[, varName], aiptw_s3[, varName],
               iptw_s1[,varName], iptw_s2[, varName], iptw_s3[, varName])
    bias=format(100*bias, digits = 0)
    
    
    biasAll=cbind(rep("&", dim(bias)[1]), bias[,1], rep("&", dim(bias)[1]), bias[,2],  rep("&", dim(bias)[1]), bias[,3], rep("&", dim(bias)[1]),
                  rep("&", dim(bias)[1]), bias[,4], rep("&", dim(bias)[1]), bias[,5], 
                  rep("&", dim(bias)[1]), bias[,6], rep("&", dim(bias)[1]), rep("&", dim(bias)[1]), bias[,7], rep("&", dim(bias)[1]), bias[,8],
                  rep("&", dim(bias)[1]), bias[,9],
                  rep("\\\\", dim(bias)[1]))
    
    
    biasAll=cbind(rep( c(rep(modelTypeOut[1], 2), rep(modelTypeOut[2], 2),
                         rep(modelTypeOut[3], 2),
                         rep(modelTypeOut[4], 2),
                         rep(modelTypeOut[5], 2), rep(modelTypeOut[6], 2),
                         rep(modelTypeOut[7], 2), rep(modelTypeOut[8], 2),rep(modelTypeOut[9], 2), rep(modelTypeOut[10], 2)), 1), biasAll)
    
    biasAll=cbind(rep(c("Standard/Rubin &", "Bagging &"), 10), biasAll)
    
    write.table(biasAll, paste(DIRECOUT, "sampleSize", sampleSize, "Paper_Table_", varName, ".txt",sep=""),  quote=F, col.names=F, row.names = F, sep="\t")
    
    
    
    #################################table for empirical bias##########
    varName="coverage"
    bias=NULL
    bias=cbind(pencomp_s1[,varName], pencomp_s2[, varName], pencomp_s3[, varName],
               aiptw_s1[,varName], aiptw_s2[, varName], aiptw_s3[, varName],
               iptw_s1[,varName], iptw_s2[, varName], iptw_s3[, varName])
    bias=1-bias
    bias=format(100*bias, digits = 0)
    
    
    biasAll=cbind(rep("&", dim(bias)[1]), bias[,1], rep("&", dim(bias)[1]), bias[,2],  rep("&", dim(bias)[1]), bias[,3], rep("&", dim(bias)[1]),
                  rep("&", dim(bias)[1]), bias[,4], rep("&", dim(bias)[1]), bias[,5], 
                  rep("&", dim(bias)[1]), bias[,6], rep("&", dim(bias)[1]), rep("&", dim(bias)[1]), bias[,7], rep("&", dim(bias)[1]), bias[,8],
                  rep("&", dim(bias)[1]), bias[,9],
                  rep("\\\\", dim(bias)[1]))
    
    
    biasAll=cbind(rep( c(rep(modelTypeOut[1], 2), rep(modelTypeOut[2], 2),
                         rep(modelTypeOut[3], 2),
                         rep(modelTypeOut[4], 2),
                         rep(modelTypeOut[5], 2), rep(modelTypeOut[6], 2),
                         rep(modelTypeOut[7], 2), rep(modelTypeOut[8], 2),rep(modelTypeOut[9], 2), rep(modelTypeOut[10], 2)), 1), biasAll)
    
    biasAll=cbind(rep(c("Standard/Rubin &", "Bagging &"), 10), biasAll)
    
    write.table(biasAll, paste(DIRECOUT, "sampleSize", sampleSize, "Paper_Table_", varName, ".txt",sep=""),  quote=F, col.names=F, row.names = F, sep="\t")
    
    
    #################################table for empirical mean confidence interval width ##########
    varName="widthCI"
    bias=NULL
    bias=cbind(pencomp_s1[,varName], pencomp_s2[, varName], pencomp_s3[, varName],
               aiptw_s1[,varName], aiptw_s2[, varName], aiptw_s3[, varName],
               iptw_s1[,varName], iptw_s2[, varName], iptw_s3[, varName])
    bias=format(10*bias, digits = 0)
    
    
    biasAll=cbind(rep("&", dim(bias)[1]), bias[,1], rep("&", dim(bias)[1]), bias[,2],  rep("&", dim(bias)[1]), bias[,3], rep("&", dim(bias)[1]),
                  rep("&", dim(bias)[1]), bias[,4], rep("&", dim(bias)[1]), bias[,5], 
                  rep("&", dim(bias)[1]), bias[,6], rep("&", dim(bias)[1]), rep("&", dim(bias)[1]), bias[,7], rep("&", dim(bias)[1]), bias[,8],
                  rep("&", dim(bias)[1]), bias[,9],
                  rep("\\\\", dim(bias)[1]))
    
    
    biasAll=cbind(rep( c(rep(modelTypeOut[1], 2), rep(modelTypeOut[2], 2),
                         rep(modelTypeOut[3], 2),
                         rep(modelTypeOut[4], 2),
                         rep(modelTypeOut[5], 2), rep(modelTypeOut[6], 2),
                         rep(modelTypeOut[7], 2), rep(modelTypeOut[8], 2),rep(modelTypeOut[9], 2), rep(modelTypeOut[10], 2)), 1), biasAll)
    
    biasAll=cbind(rep(c("Standard/Rubin &", "Bagging &"), 10), biasAll)
    
    write.table(biasAll, paste(DIRECOUT, "sampleSize", sampleSize,  "Paper_Table_", varName, ".txt",sep=""),  quote=F, col.names=F, row.names = F, sep="\t")
    
    
    
  }
