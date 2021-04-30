
####looking at proportions that each variable was selected across all 500 simulated datasets


for(txtName in c("varSelectTreat", "varSelectY")){
  
  for(sampleSize in c(200, 1000)){
    
    
    for(level in c("high", "moderate", "low")){
      
      varProp_lassoOutcome=NULL
      varProp_lassoSep=NULL
      varProp_lassoStepwiseY=NULL
      varProp_lassoStepwiseT=NULL
      varProp_stepwise=NULL
      varProp_allLasso=NULL
      
      #######################################
      DIRECRoot="C:/Users/Tingting/Desktop/paper3/variableSelection/"
      
      modelType = "allLasso"
      
      if(modelType=="allLasso"){
        DIREC=paste0(DIRECRoot, "case2b", "/sampleSize", sampleSize, "/", "Results/")
      } else {
        DIREC=paste0(DIRECRoot, "case2", "/sampleSize", sampleSize, "/", "Results/")
      }
      
      fileNames=list.files(DIREC, pattern=paste(modelType, "_", level, sep=""))
      fileNames=fileNames[grep(txtName, fileNames)]
      
      varProp_allLasso=NULL
      for(k in 1:length(fileNames)) {
        tempResult=read.table(paste(DIREC, fileNames[k], sep=""),header=T, sep="\t")
        tempResult=tempResult[which(tempResult[,1] != 0),]
        varProp_allLasso=rbind(varProp_allLasso,tempResult)
      }
      
      
      #######################################
      modelType = "stepwise"
      if(modelType=="allLasso"){
        DIREC=paste0(DIRECRoot, "case2b", "/sampleSize", sampleSize, "/", "Results/")
      } else {
        DIREC=paste0(DIRECRoot, "case2", "/sampleSize", sampleSize, "/", "Results/")
      }
      
      fileNames=list.files(DIREC, pattern=paste(modelType, "_", level, sep=""))
      fileNames=fileNames[grep(txtName, fileNames)]
      
      varProp_stepwise=NULL
      for(k in 1:length(fileNames)) {
        tempResult=read.table(paste(DIREC, fileNames[k], sep=""),header=T, sep="\t")
        tempResult=tempResult[which(tempResult[,1] != 0),]
        varProp_stepwise=rbind(varProp_stepwise,tempResult)
      }
      
      
      
      ############################################
      modelType ="lassoOutcome"
      if(modelType=="allLasso"){
        DIREC=paste0(DIRECRoot, "case2b", "/sampleSize", sampleSize, "/", "Results/")
      } else {
        DIREC=paste0(DIRECRoot, "case2", "/sampleSize", sampleSize, "/", "Results/")
      }
      
      fileNames=list.files(DIREC, pattern=paste(modelType, "_", level, sep=""))
      fileNames=fileNames[grep(txtName, fileNames)]
      
      
      varProp_lassoOutcome=NULL
      for(k in 1:length(fileNames)) {
        tempResult=read.table(paste(DIREC, fileNames[k], sep=""),header=T, sep="\t")
        tempResult=tempResult[which(tempResult[,1] != 0),]
        varProp_lassoOutcome=rbind(varProp_lassoOutcome,tempResult)
      }
      
      
      
      
      
      ######################################################################
      modelType = "lassoSep"
      if(modelType=="allLasso"){
        DIREC=paste0(DIRECRoot, "case2b", "/sampleSize", sampleSize, "/", "Results/")
      } else {
        DIREC=paste0(DIRECRoot, "case2", "/sampleSize", sampleSize, "/", "Results/")
      }
      
      fileNames=list.files(DIREC, pattern=paste(modelType, "_", level, sep=""))
      fileNames=fileNames[grep(txtName, fileNames)]
      
      varProp_lassoSep=NULL
      for(k in 1:length(fileNames)) {
        tempResult=read.table(paste(DIREC, fileNames[k], sep=""),header=T, sep="\t")
        tempResult=tempResult[which(tempResult[,1] != 0),]
        varProp_lassoSep=rbind(varProp_lassoSep,tempResult)
      }
      
      
      
      #######################################
      modelType = "lassoStepwiseT"
      if(modelType=="allLasso"){
        DIREC=paste0(DIRECRoot, "case2b", "/sampleSize", sampleSize, "/", "Results/")
      } else {
        DIREC=paste0(DIRECRoot, "case2", "/sampleSize", sampleSize, "/", "Results/")
      }
      
      fileNames=list.files(DIREC, pattern=paste(modelType, "_", level, sep=""))
      fileNames=fileNames[grep(txtName, fileNames)]
      
      varProp_lassoStepwiseT=NULL
      for(k in 1:length(fileNames)) {
        tempResult=read.table(paste(DIREC, fileNames[k], sep=""),header=T, sep="\t")
        tempResult=tempResult[which(tempResult[,1] != 0),]
        varProp_lassoStepwiseT=rbind(varProp_lassoStepwiseT,tempResult)
      }
      
      
      #######################################
      modelType = "lassoStepwiseY"
      if(modelType=="allLasso"){
        DIREC=paste0(DIRECRoot, "case2b", "/sampleSize", sampleSize, "/", "Results/")
      } else {
        DIREC=paste0(DIRECRoot, "case2", "/sampleSize", sampleSize, "/", "Results/")
      }
      
      fileNames=list.files(DIREC, pattern=paste(modelType, "_", level, sep=""))
      fileNames=fileNames[grep(txtName, fileNames)]
      
      
      varProp_lassoStepwiseY=NULL
      for(k in 1:length(fileNames)) {
        tempResult=read.table(paste(DIREC, fileNames[k], sep=""),header=T, sep="\t")
        tempResult=tempResult[which(tempResult[,1] != 0),]
        varProp_lassoStepwiseY=rbind(varProp_lassoStepwiseY,tempResult)
      }
      
      
      
      variableProp=rbind(colMeans(varProp_lassoOutcome, na.rm = T),colMeans(varProp_lassoSep, na.rm = T),
                         colMeans(varProp_lassoStepwiseY, na.rm = T),colMeans(varProp_lassoStepwiseT, na.rm = T),
                         colMeans(varProp_allLasso, na.rm = T),
                         colMeans(varProp_stepwise, na.rm = T))
      
      
      write.table(variableProp, paste0(DIRECRoot, "case2Results/",
                                       "variablePropSum_", level, "_sampleSize", sampleSize,"_", txtName, ".txt"), sep="\t",
                  row.names = c("lassoOutcome", "lassoSep", "lassoStepwiseY", "lassoStepwiseT", "allLasso", "stepwise"), 
                  col.names = colnames(varProp_lassoStepwiseY), quote = F)
      
      
    }
    
  }
}

