
rm(list=ls())

###where simulation results are stored; the outputs from this R file are stored in folder case2Results
DIREC_main="C:/Users/Tingting/Desktop/paper3/variableSelection/"  ###change the directory

############################################################################################################
processResult=function(result, truth11){
  
  result=result[!is.na(result[,2]),]
  
  total_ps11=numeric(dim(result)[1])
  for (g in 1:dim(result)[1]) {
    total_ps11[g]=as.numeric(result[g,3] <= truth11 & result[g,4] >= truth11)
  }
  coverage_ps11=sum(total_ps11)/length(total_ps11)
  coverage_ps11
  
  bias11=mean(result[,1]-(truth11))
  estimate11=mean(result[,1])
  temp11=(result[,1]-(truth11))^2
  RMSE11=sqrt(mean(temp11))
  
  sd11=sd(result[,1])
  width11=mean(abs(result[,4]-result[,3]))
  
  sd11Boot=mean(result[,2]) 
  
  finalOut=c(estimate11, bias11, sd11, sd11Boot, RMSE11, coverage_ps11, width11, dim(result)[1], truth11)  
  names(finalOut)=c("estimate", "bias",  "sd", "sdBoot", "RMSE", "coverage", "widthCI", "num.sim",  "truth")  
  return(finalOut)
  
}

truth=2


#######################################################
#######################################################
for(sampleSize in c("200", "1000")){
  
  for (level in c("high", "moderate", "low" ) ) {
    
    resultAll=NULL
    temp_pencomp=NULL
    temp_aiptw=NULL
    temp_iptw=NULL
    
    ###########################################################################
    ############################################################################
    for(modelType in c("trueOutcomePred","trueConf", "truePropen", "allPot","lassoOutcome", "lassoSep", "lassoStepwiseT", "lassoStepwiseY", "stepwise", "allLasso", "stepwiseWOT") ) {
      if(modelType=="allLasso"){ ###everything except allLasso results are in the folder case2
        DIREC=paste0(DIREC_main, "case2b", "/sampleSize", sampleSize, "/Results/")  ###where allLasso results are stored
      } else if (modelType=="stepwiseWOT"){
        DIREC=paste0(DIREC_main, "case2c", "/sampleSize", sampleSize, "/Results/")  ###where all other results are stored
      } else{
        DIREC=paste0(DIREC_main, "case2", "/sampleSize", sampleSize, "/Results/")  ###where all other results are stored
      }
      
      fileNames=list.files(DIREC, pattern=paste("aiptw_", modelType, "_", level,"_start_", sep=""))
      
      aiptw=NULL
      for(k in 1:length(fileNames)) {
        if(sampleSize==200 & level=="high" & modelType=="stepwiseWOT"){
          tempResult=read.table(paste(DIREC, fileNames[k], sep=""),header=T, sep="\t")
          tempResult=tempResult[which(tempResult[,1] != 0),]
          if(k==1){
            tempResult=tempResult[1:66,]
          }
          aiptw=rbind(aiptw,tempResult) 
        } else {
          tempResult=read.table(paste(DIREC, fileNames[k], sep=""),header=T, sep="\t")
          tempResult=tempResult[which(tempResult[,1] != 0),]
          aiptw=rbind(aiptw,tempResult)
        }

      }
      
      
      temp=NULL
      temp=rbind(temp, processResult(result = aiptw[,(1:4)+4*0], truth11 = truth),
                 processResult(result = aiptw[,(1:4)+4*1], truth11 = truth)) 
      
      row.names(temp)=paste("aiptw", modelType, c("standard", "bagging"), sep="_")
      
      temp_aiptw=rbind(temp_aiptw, temp)
    }
    
    
    ###########################################################################
    ############################################################################
    for(modelType in c("trueOutcomePred","trueConf", "truePropen", "allPot","lassoOutcome", "lassoSep", "lassoStepwiseT", "lassoStepwiseY", "stepwise", "allLasso", "stepwiseWOT") ) {
      
      if(modelType=="allLasso"){ ###everything except allLasso results are in the folder case2
        DIREC=paste0(DIREC_main, "case2b", "/sampleSize", sampleSize, "/Results/")  ###where allLasso results are stored
      } else if(modelType=="stepwiseWOT"){
        DIREC=paste0(DIREC_main, "case2c", "/sampleSize", sampleSize, "/Results/")  ###where all other results are stored
      } else{
        DIREC=paste0(DIREC_main, "case2", "/sampleSize", sampleSize, "/Results/")  ###where all other results are stored
      }
      
      fileNames=list.files(DIREC, pattern=paste("iptw_", modelType, "_", level,"_start_", sep=""))
      fileNames=fileNames[grep("^iptw",fileNames)]
      
      iptw=NULL
      for(k in 1:length(fileNames)) {
        if(sampleSize==200 & level=="high" & modelType=="stepwiseWOT"){
            tempResult=read.table(paste(DIREC, fileNames[k], sep=""),header=T, sep="\t")
            tempResult=tempResult[which(tempResult[,1] != 0),]
            if(k==1){
              tempResult=tempResult[1:66,]
            } else if(k==2){
              tempResult=tempResult[67:500,]
            }
            iptw=rbind(iptw,tempResult)
          } else {
            tempResult=read.table(paste(DIREC, fileNames[k], sep=""),header=T, sep="\t")
            tempResult=tempResult[which(tempResult[,1] != 0),]
            iptw=rbind(iptw,tempResult)
          }
      }
      
      
      #################
      temp=NULL
      temp=rbind(temp, processResult(result = iptw[,(1:4)+4*0], truth11 = truth),
                 processResult(result = iptw[,(1:4)+4*1], truth11 = truth)) 
      
      row.names(temp)=paste("iptw", modelType, c("standard", "bagging"),  sep="_")
      
      temp_iptw=rbind(temp_iptw, temp)
      
    }
    
    
    
    ###########################################################################
    ############################################################################
    for(modelType in c("trueOutcomePred","trueConf", "truePropen", "allPot","lassoOutcome", "lassoSep", "lassoStepwiseT", "lassoStepwiseY", "stepwise", "allLasso", "stepwiseWOT") ) {
      
      if(modelType=="allLasso"){ ###everything except allLasso results are in the folder case2
        DIREC=paste0(DIREC_main, "case2b", "/sampleSize", sampleSize, "/Results/")  ###where allLasso results are stored
      } else if (modelType=="stepwiseWOT"){
        DIREC=paste0(DIREC_main, "case2c", "/sampleSize", sampleSize, "/Results/")  ###where all other results are stored
      } else{
        DIREC=paste0(DIREC_main, "case2", "/sampleSize", sampleSize, "/Results/")  ###where all other results are stored
      }
      
      fileNames=list.files(DIREC, pattern=paste("pencomp_", modelType, "_", level,"_start_", sep=""))
      
      pencomp=NULL
      for(k in 1:length(fileNames)) {
        if (sampleSize==200 & level=="high" & modelType=="stepwiseWOT"){
          tempResult=read.table(paste(DIREC, fileNames[k], sep=""),header=T, sep="\t")
          tempResult=tempResult[which(tempResult[,1] != 0),]
          if(k==1){
            tempResult=tempResult[1:66,]
          } else if(k==2){
            tempResult=tempResult[67:500,]
          }
          pencomp=rbind(pencomp,tempResult)
        } else {
          tempResult=read.table(paste(DIREC, fileNames[k], sep=""),header=T, sep="\t")
          tempResult=tempResult[which(tempResult[,1] != 0),]
          pencomp=rbind(pencomp,tempResult)
        }
      }
      
      #################
      temp=NULL
      temp=rbind(temp, processResult(result = pencomp[,(1:4)+4*1], truth11 = truth),
                 processResult(result = pencomp[,(1:4)+4*3], truth11 = truth) ) 
      
      row.names(temp)=paste("pencomp", modelType, c( "bagging", "rubin"), sep="_")
      
      temp_pencomp=rbind(temp_pencomp, temp) 
    }
    
    
    
    
    resultAll=rbind(temp_pencomp, temp_aiptw, temp_iptw)
    
    write.table(resultAll, paste0(DIREC_main,"case2Results/", level,"_",  
                                  "sampleSize", sampleSize, ".txt"), 
                sep="\t", quote =F)
    
  }
  
}





######