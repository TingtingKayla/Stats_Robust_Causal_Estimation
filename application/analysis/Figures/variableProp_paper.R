#######plots used in the paper###############
#############################################

rm(list=ls())

sampleID=12

####looking at proportions that each variable was selected across all 500 simulated datasets
varList = c(paste0("LEU3N", 1:4), paste0("LEU2N", 1:4), paste0("WBC", 1:4), paste0("RBC", 1:4), paste0("PLATE", 1:4),
            "age", "white", "college", paste0("A", 1:3)) 

####################################################################################
####################################################################################

DIRECRoot="C:/Users/Tingting.Zhou/Desktop/paper3/biomlatex/Application/version4/"
DIREC=paste0(DIRECRoot, "bootResults/")

fileName="varSelectTreat"

varProp_lassoOutcome=read.table(paste0(DIRECRoot,"Figures/variable_lassoOutcome_", fileName, ".txt"),sep="\t", header = T)
varProp_lassoSep=read.table(paste0(DIRECRoot,"Figures/variable_lassoSep_", fileName, ".txt"), sep="\t", header=T)
varProp_lassoStepwiseT=read.table(paste0(DIRECRoot,"Figures/variable_lassoStepwiseT_", fileName, ".txt"),sep="\t", header=T)
varProp_lassoStepwiseY=read.table(paste0(DIRECRoot,"Figures/variable_lassoStepwiseY_", fileName, ".txt"), sep="\t", header=T)
varProp_stepwise=read.table(paste0(DIRECRoot,"Figures/variable_stepwise_", fileName, ".txt"), sep="\t", header=T)

varProp_lassoOutcome=t(varProp_lassoOutcome[sampleID,])
varProp_lassoSep=t(varProp_lassoSep[sampleID,])
varProp_lassoStepwiseT=t(varProp_lassoStepwiseT[sampleID,])
varProp_lassoStepwiseY=t(varProp_lassoStepwiseY[sampleID,])
varProp_stepwise=t(varProp_stepwise[sampleID,])


################prediction model
fileName="varSelectY0"

varProp_lassoOutcome_Y=read.table(paste0(DIRECRoot,"Figures/variable_lassoOutcome_", fileName, ".txt"),sep="\t", header = T)
varProp_lassoSep_Y=read.table(paste0(DIRECRoot,"Figures/variable_lassoSep_", fileName, ".txt"), sep="\t", header=T)
varProp_lassoStepwiseT_Y=read.table(paste0(DIRECRoot,"Figures/variable_lassoStepwiseT_", fileName, ".txt"),sep="\t", header=T)
varProp_lassoStepwiseY_Y=read.table(paste0(DIRECRoot,"Figures/variable_lassoStepwiseY_", fileName, ".txt"), sep="\t", header=T)
varProp_stepwise_Y=read.table(paste0(DIRECRoot,"Figures/variable_stepwise_", fileName, ".txt"), sep="\t", header=T)

varProp_lassoOutcome_Y=t(varProp_lassoOutcome_Y[sampleID,])
varProp_lassoSep_Y=t(varProp_lassoSep_Y[sampleID,])
varProp_lassoStepwiseT_Y=t(varProp_lassoStepwiseT_Y[sampleID,])
varProp_lassoStepwiseY_Y=t(varProp_lassoStepwiseY_Y[sampleID,])
varProp_stepwise_Y=t(varProp_stepwise_Y[sampleID,])



all=cbind(varProp_stepwise_Y, varProp_lassoSep_Y, varProp_stepwise, varProp_lassoOutcome, varProp_lassoStepwiseT, varProp_lassoStepwiseY)
all=all[order(all[,1], decreasing = T),]

biasAll=cbind("&", all[,1], "&", all[,2], "&", "&", all[,3], "&", all[,4], "&", all[,5], "&", all[,6], "\\\\") 

columnNames=c("SW", "AL","", "SW", "OAL", "Step_ALT", "Step_ALY")

#rowNames=c("CD4 t=-3", "CD4 t=-2","CD4 t=-1","CD4 t=1",
#  "CD8 t=-3", "CD8 t=-2","CD8 t=-1","CD8 t=1",
#  "WBC t=-3", "WBC t=-2","WBC t=-1","WBC t=1",
#  "RBC t=-3", "RBC t=-2","RBC t=-1","RBC t=1",
#  "Platelet t=-3", "platelet t=-2","platelet t=-1",
#  "platelet t=1", "age", "white", "college", 
#  "treat at t=-3", "treat at t=-2","treat at t=-1")

rowNames=c("CD4 t=-1", "CD4 t=1" ,"CD8 t=-1", "RBC t=1", "RBC t=-2", "WBC t=1", "college" ,"CD4 t=-2","platelet t=-1" ,"CD8 t=1", "treat t=-3", "treat t=-1", "treat t=-2",   
  "platelet t=-3", "WBC t=-1" , "age","CD8 t=-2" ,"RBC t=-1","white", "platelet t=1", "CD4 t=-3", "CD8 t=-3", "WBC t=-2","WBC t=-3" ,"platelet t=-2","RBC t=-3")  


write.table(biasAll, paste(DIRECRoot, "Figures/Paper_Table_variableSelect.txt",sep=""),  quote=F, col.names=F,
            row.names = rowNames, sep="\t")


