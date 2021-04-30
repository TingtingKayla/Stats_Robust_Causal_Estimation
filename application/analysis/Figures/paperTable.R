########created 7/21/2018

rm(list=ls())


times=c(12)
modelTypeVal=c("lassoOutcome", "lassoSep", "lassoStepwiseT", "lassoStepwiseY",  "allPotent")

############
DIRECRoot="C:/Users/Tingting.Zhou/Desktop/paper3/biomlatex/Application/version4/"
DIREC=paste0(DIRECRoot, "Results/")

result=NULL
 
for(way in c( "iptw_","aiptw_", "pencomp_") ){
  
  lassoOutcome=NULL
  lassoOutcome=read.table(paste(DIREC, way, "lassoOutcome.txt", sep=""),header=T, sep="\t")
  if(way=="pencomp_"){
    lassoOutcome=lassoOutcome[times, c((1:4)+4*3, (1:4)+4*1) ]
  } else {
    lassoOutcome=lassoOutcome[times, c((1:4)+4*0, (1:4)+4*1) ]
  }
  
  ########################################
  lassoSep=read.table(paste(DIREC, way, "lasso.txt", sep=""),header=T, sep="\t")
  if(way=="pencomp_"){
    lassoSep=lassoSep[times, c((1:4)+4*3, (1:4)+4*1) ]
  } else {
    lassoSep=lassoSep[times, c((1:4)+4*0, (1:4)+4*1) ]
  }
  
  ########################################
  lassoStepwiseT=read.table(paste(DIREC, way, "lassoStepT.txt", sep=""),header=T, sep="\t")
  if(way=="pencomp_"){
    lassoStepwiseT=lassoStepwiseT[times, c((1:4)+4*3, (1:4)+4*1) ]
  } else {
    lassoStepwiseT=lassoStepwiseT[times, c((1:4)+4*0, (1:4)+4*1) ]
  }
  
  ########################################
  lassoStepwiseY=read.table(paste(DIREC, way, "lassoStepY.txt", sep=""),header=T, sep="\t")
  if(way=="pencomp_"){
    lassoStepwiseY=lassoStepwiseY[times, c((1:4)+4*3, (1:4)+4*1) ]
  } else {
    lassoStepwiseY=lassoStepwiseY[times, c((1:4)+4*0, (1:4)+4*1) ]
  }
  
  ########################################
  allPotent=read.table(paste(DIREC, way, "allPotent.txt", sep=""),header=T, sep="\t")
  if(way=="pencomp_"){
    allPotent=allPotent[times, c((1:4)+4*3, (1:4)+4*1) ]
  } else {
    allPotent=allPotent[times, c((1:4)+4*0, (1:4)+4*1) ]
  }
  
  ########################################
  stepwise=read.table(paste(DIREC, way, "stepwise.txt", sep=""),header=T, sep="\t")
  if(way=="pencomp_"){
    stepwise=stepwise[times, c((1:4)+4*3, (1:4)+4*1) ]
  } else {
    stepwise=stepwise[times, c((1:4)+4*0, (1:4)+4*1) ]
  }
  
  

  temp=rbind(c("&", paste0(format(allPotent[1,1], digits=2, nsmall=1)," (", format(allPotent[1,3], digits=2, nsmall=1), ", ", format(allPotent[1,4], digits=2, nsmall=1), ")")),
        c("&", paste0(format(allPotent[1,5], digits=2, nsmall=1)," (", format(allPotent[1,7], digits=2, nsmall=1), ", ", format(allPotent[1,8], digits=2, nsmall=1), ")")),
        
        c("&", paste0(format(stepwise[1,1], digits=2, nsmall=1)," (", format(stepwise[1,3], digits=2, nsmall=1), ", ", format(stepwise[1,4], digits=2, nsmall=1), ")")),
        c("&", paste0(format(stepwise[1,5], digits=2, nsmall=1)," (", format(stepwise[1,7], digits=2, nsmall=1), ", ", format(stepwise[1,8], digits=2, nsmall=1), ")")),
        
        c("&", paste0(format(lassoSep[1,1], digits=2, nsmall=1)," (", format(lassoSep[1,3], digits=2, nsmall=1), ", ", format(lassoSep[1,4], digits=2, nsmall=1), ")")),
        c("&", paste0(format(lassoSep[1,5], digits=2, nsmall=1)," (", format(lassoSep[1,7], digits=2, nsmall=1), ", ", format(lassoSep[1,8], digits=2, nsmall=1), ")")),
        
        c("&", paste0(format(lassoOutcome[1,1], digits=2, nsmall=1)," (", format(lassoOutcome[1,3], digits=2, nsmall=1), ", ", format(lassoOutcome[1,4], digits=2, nsmall=1), ")")),
        c("&", paste0(format(lassoOutcome[1,5], digits=2, nsmall=1)," (", format(lassoOutcome[1,7], digits=2, nsmall=1), ", ", format(lassoOutcome[1,8], digits=2, nsmall=1), ")")),
        
        c("&", paste0(format(lassoStepwiseT[1,1], digits=2, nsmall=1)," (", format(lassoStepwiseT[1,3], digits=2, nsmall=1), ", ", format(lassoStepwiseT[1,4], digits=2, nsmall=1), ")")),
        c("&", paste0(format(lassoStepwiseT[1,5], digits=2, nsmall=1)," (", format(lassoStepwiseT[1,7], digits=2, nsmall=1), ", ", format(lassoStepwiseT[1,8], digits=2, nsmall=1), ")")),
        
        c("&", paste0(format(lassoStepwiseY[1,1], digits=2, nsmall=1)," (", format(lassoStepwiseY[1,3], digits=2, nsmall=1), ", ", format(lassoStepwiseY[1,4], digits=2, nsmall=1), ")")),
        c("&", paste0(format(lassoStepwiseY[1,5], digits=2, nsmall=1)," (", format(lassoStepwiseY[1,7], digits=2, nsmall=1), ", ", format(lassoStepwiseY[1,8], digits=2, nsmall=1), ")")) )
        
  
  
  result=cbind(result, temp)  
  
  }


  biasAll=NULL
  biasAll=cbind(result, "\\\\") 
  
  biasAll=cbind(c(rep(c("Rubin/standard ", "Bagging "), 6)), biasAll)
  biasAll=cbind(c("allPotent","", "SW","", "AL","", "OAL","", "Step-ALT","", "Step-ALY", ""), rep("&", nrow(result)), biasAll)

  write.table(biasAll, paste(DIRECRoot, "Figures/Paper_Table.txt",sep=""),  quote=F, col.names=F, row.names = F, sep="\t")

