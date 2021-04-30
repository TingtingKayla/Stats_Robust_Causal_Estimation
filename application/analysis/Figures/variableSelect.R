
####looking at proportions that each variable was selected across all 500 simulated datasets
varList = c(paste0("LEU3N", 1:4), paste0("LEU2N", 1:4), paste0("WBC", 1:4), paste0("RBC", 1:4), paste0("PLATE", 1:4),
            "age", "white", "college", paste0("A", 1:3)) 

#fileName="varSelectTreat"

for(fileName in c("varSelectTreat", "varSelectY0")){
  
      varProp_lassoOutcome=matrix(NA, nrow = 21, ncol=length(varList))
      varProp_lassoSep=matrix(NA, nrow = 21, ncol=length(varList))
      varProp_lassoStepwiseY=matrix(NA, nrow = 21, ncol=length(varList))
      varProp_lassoStepwiseT=matrix(NA, nrow = 21, ncol=length(varList))
      varProp_stepwise=matrix(NA, nrow = 21, ncol=length(varList))
      
      DIRECRoot="M:/Private/overlap/Application/version4/"
      DIREC=paste0(DIRECRoot, "bootResults/")
      
      sampleIDVal=c(12)
      for(sampleID in sampleIDVal){
        tryCatch({
          
          print(sampleID)
          
          ########################
          lassoSep=read.table(paste0(DIREC,"sample", sampleID, "lasso_", fileName, ".txt"), header=T, sep="\t")
          
          varProp_lassoSep[sampleID,]=colMeans(lassoSep, na.rm = T)
          
        }, error=function(e){}
        ) 
      }
      
      #######################################
      for(sampleID in sampleIDVal){
        tryCatch({
          
          print(sampleID)
          
          ########################
          lassoOutcome=read.table(paste0(DIREC,"sample", sampleID, "lassoOutcome_", fileName, ".txt"), header=T, sep="\t")
          
          varProp_lassoOutcome[sampleID,]=colMeans(lassoOutcome, na.rm = T)
          
        }, error=function(e){}
        ) 
      }
      
      
      #######################################
      for(sampleID in sampleIDVal){
        tryCatch({
          
          print(sampleID)
          
          ########################
          lassoStepwiseY=read.table(paste0(DIREC,"sample", sampleID, "lassoStepY_", fileName, ".txt"), header=T, sep="\t")
          
          varProp_lassoStepwiseY[sampleID,]=colMeans(lassoStepwiseY, na.rm = T)
          
        }, error=function(e){}
        ) 
      }
      
      
      #######################################
      for(sampleID in sampleIDVal){
        tryCatch({
          
          print(sampleID)
          
          ########################
          lassoStepwiseT=read.table(paste0(DIREC,"sample", sampleID, "lassoStepT_", fileName, ".txt"), header=T, sep="\t")
          
          varProp_lassoStepwiseT[sampleID,]=colMeans(lassoStepwiseT, na.rm = T)
          
        }, error=function(e){}
        ) 
      }
      
      
      
      #######################################
      for(sampleID in sampleIDVal){
        tryCatch({
          
          print(sampleID)
          
          ########################
          stepwise=read.table(paste0(DIREC,"sample", sampleID, "stepwise_", fileName, ".txt"), header=T, sep="\t")
          
          varProp_stepwise[sampleID,]=colMeans(stepwise, na.rm = T)
          
        }, error=function(e){}
        ) 
      }
      
      
      write.table(format(100*varProp_lassoOutcome, digits=0), paste0(DIRECRoot,"Figures/variable_lassoOutcome_", fileName, ".txt"),
                  sep="\t", row.names = F,col.names = varList, quote = F)
      
      
      write.table(format(100*varProp_lassoSep, digits=0), paste0(DIRECRoot,"Figures/variable_lassoSep_", fileName, ".txt"),
                  sep="\t", row.names = F, col.names = varList, quote = F)
      
      
      write.table(format(100*varProp_lassoStepwiseT, digits=0), paste0(DIRECRoot,"Figures/variable_lassoStepwiseT_", fileName, ".txt"),
                  sep="\t", row.names = F, col.names = varList, quote = F)
      
      
      write.table(format(100*varProp_lassoStepwiseY, digits=0), paste0(DIRECRoot,"Figures/variable_lassoStepwiseY_", fileName, ".txt"),
                  sep="\t", row.names = F, col.names = varList, quote = F)
      
      write.table(format(100*varProp_stepwise, digits=0), paste0(DIRECRoot,"Figures/variable_stepwise_", fileName, ".txt"),
                  sep="\t", row.names = F, col.names = varList, quote = F)
      
    
}

