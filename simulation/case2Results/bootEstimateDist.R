

####looking at distributions of the estimates
sampleID=3
sampleSize=200

DIRECRoot="C:/Users/Tingting/Desktop/paper3/variableSelection/"
DIREC=paste0(DIRECRoot,"case2/sampleSize", sampleSize, "/bootResults/")


level="high"
lassoOutcome200H=read.table(paste0(DIREC,"sample", sampleID, "lassoOutcome_", level, ".txt"), header=T, sep="\t")
lassoSep200H=read.table(paste0(DIREC,"sample", sampleID, "lassoSep_", level, ".txt"), header=T, sep="\t")
lassoStepwiseY200H=read.table(paste0(DIREC,"sample", sampleID, "lassoStepwiseY_", level, ".txt"), header=T, sep="\t")
trueConf200H=read.table(paste0(DIREC,"sample", sampleID, "trueConf_", level, ".txt"), header=T, sep="\t")
lassoStepwiseT200H=read.table(paste0(DIREC,"sample", sampleID, "lassoStepwiseT_", level, ".txt"), header=T, sep="\t")
truePropen200H=read.table(paste0(DIREC,"sample", sampleID, "truePropen_", level, ".txt"), header=T, sep="\t")
stepwise200H=read.table(paste0(DIREC,"sample", sampleID, "stepwise_", level, ".txt"), header=T, sep="\t")
all200H=read.table(paste0(DIREC,"sample", sampleID, "allPot_", level, ".txt"), header=T, sep="\t")
outPred200H=read.table(paste0(DIREC,"sample", sampleID, "trueOutcomePred_", level, ".txt"), header=T, sep="\t")



#######################################################
sampleSize=1000
DIREC=paste0(DIRECRoot,"/sampleSize", sampleSize, "/bootResults/")

lassoOutcome1000H=read.table(paste0(DIREC,"sample", sampleID, "lassoOutcome_", level, ".txt"), header=T, sep="\t")
lassoSep1000H=read.table(paste0(DIREC,"sample", sampleID, "lassoSep_", level, ".txt"), header=T, sep="\t")
lassoStepwiseY1000H=read.table(paste0(DIREC,"sample", sampleID, "lassoStepwiseY_", level, ".txt"), header=T, sep="\t")
trueConf1000H=read.table(paste0(DIREC,"sample", sampleID, "trueConf_", level, ".txt"), header=T, sep="\t")
lassoStepwiseT1000H=read.table(paste0(DIREC,"sample", sampleID, "lassoStepwiseT_", level, ".txt"), header=T, sep="\t")
truePropen1000H=read.table(paste0(DIREC,"sample", sampleID, "truePropen_", level, ".txt"), header=T, sep="\t")
stepwise1000H=read.table(paste0(DIREC,"sample", sampleID, "stepwise_", level, ".txt"), header=T, sep="\t")
all1000H=read.table(paste0(DIREC,"sample", sampleID, "allPot_", level, ".txt"), header=T, sep="\t")
outPred1000H=read.table(paste0(DIREC,"sample", sampleID, "trueOutcomePred_", level, ".txt"), header=T, sep="\t")


#########################################
sampleSize=200
DIREC=paste0(DIRECRoot,"/sampleSize", sampleSize, "/bootResults/")

level="moderate"
lassoOutcome200M=read.table(paste0(DIREC,"sample", sampleID, "lassoOutcome_", level, ".txt"), header=T, sep="\t")
lassoSep200M=read.table(paste0(DIREC,"sample", sampleID, "lassoSep_", level, ".txt"), header=T, sep="\t")
lassoStepwiseY200M=read.table(paste0(DIREC,"sample", sampleID, "lassoStepwiseY_", level, ".txt"), header=T, sep="\t")
trueConf200M=read.table(paste0(DIREC,"sample", sampleID, "trueConf_", level, ".txt"), header=T, sep="\t")
lassoStepwiseT200M=read.table(paste0(DIREC,"sample", sampleID, "lassoStepwiseT_", level, ".txt"), header=T, sep="\t")
truePropen200M=read.table(paste0(DIREC,"sample", sampleID, "truePropen_", level, ".txt"), header=T, sep="\t")
stepwise200M=read.table(paste0(DIREC,"sample", sampleID, "stepwise_", level, ".txt"), header=T, sep="\t")
all200M=read.table(paste0(DIREC,"sample", sampleID, "allPot_", level, ".txt"), header=T, sep="\t")
outPred200M=read.table(paste0(DIREC,"sample", sampleID, "trueOutcomePred_", level, ".txt"), header=T, sep="\t")



#######################################################
sampleSize=1000
DIREC=paste0(DIRECRoot,"/sampleSize", sampleSize, "/bootResults/")

lassoOutcome1000M=read.table(paste0(DIREC,"sample", sampleID, "lassoOutcome_", level, ".txt"), header=T, sep="\t")
lassoSep1000M=read.table(paste0(DIREC,"sample", sampleID, "lassoSep_", level, ".txt"), header=T, sep="\t")
lassoStepwiseY1000M=read.table(paste0(DIREC,"sample", sampleID, "lassoStepwiseY_", level, ".txt"), header=T, sep="\t")
trueConf1000M=read.table(paste0(DIREC,"sample", sampleID, "trueConf_", level, ".txt"), header=T, sep="\t")
lassoStepwiseT1000M=read.table(paste0(DIREC,"sample", sampleID, "lassoStepwiseT_", level, ".txt"), header=T, sep="\t")
truePropen1000M=read.table(paste0(DIREC,"sample", sampleID, "truePropen_", level, ".txt"), header=T, sep="\t")
stepwise1000M=read.table(paste0(DIREC,"sample", sampleID, "stepwise_", level, ".txt"), header=T, sep="\t")
all1000M=read.table(paste0(DIREC,"sample", sampleID, "allPot_", level, ".txt"), header=T, sep="\t")
outPred1000M=read.table(paste0(DIREC,"sample", sampleID, "trueOutcomePred_", level, ".txt"), header=T, sep="\t")



#########################################
sampleSize=200
DIREC=paste0(DIRECRoot,"/sampleSize", sampleSize, "/bootResults/")

level="low"
lassoOutcome200L=read.table(paste0(DIREC,"sample", sampleID, "lassoOutcome_", level, ".txt"), header=T, sep="\t")
lassoSep200L=read.table(paste0(DIREC,"sample", sampleID, "lassoSep_", level, ".txt"), header=T, sep="\t")
lassoStepwiseY200L=read.table(paste0(DIREC,"sample", sampleID, "lassoStepwiseY_", level, ".txt"), header=T, sep="\t")
trueConf200L=read.table(paste0(DIREC,"sample", sampleID, "trueConf_", level, ".txt"), header=T, sep="\t")
lassoStepwiseT200L=read.table(paste0(DIREC,"sample", sampleID, "lassoStepwiseT_", level, ".txt"), header=T, sep="\t")
truePropen200L=read.table(paste0(DIREC,"sample", sampleID, "truePropen_", level, ".txt"), header=T, sep="\t")
stepwise200L=read.table(paste0(DIREC,"sample", sampleID, "stepwise_", level, ".txt"), header=T, sep="\t")
all200L=read.table(paste0(DIREC,"sample", sampleID, "allPot_", level, ".txt"), header=T, sep="\t")
outPred200L=read.table(paste0(DIREC,"sample", sampleID, "trueOutcomePred_", level, ".txt"), header=T, sep="\t")



#######################################################
sampleSize=1000
DIREC=paste0(DIRECRoot,"/sampleSize", sampleSize, "/bootResults/")

lassoOutcome1000L=read.table(paste0(DIREC,"sample", sampleID, "lassoOutcome_", level, ".txt"), header=T, sep="\t")
lassoSep1000L=read.table(paste0(DIREC,"sample", sampleID, "lassoSep_", level, ".txt"), header=T, sep="\t")
lassoStepwiseY1000L=read.table(paste0(DIREC,"sample", sampleID, "lassoStepwiseY_", level, ".txt"), header=T, sep="\t")
trueConf1000L=read.table(paste0(DIREC,"sample", sampleID, "trueConf_", level, ".txt"), header=T, sep="\t")
lassoStepwiseT1000L=read.table(paste0(DIREC,"sample", sampleID, "lassoStepwiseT_", level, ".txt"), header=T, sep="\t")
truePropen1000L=read.table(paste0(DIREC,"sample", sampleID, "truePropen_", level, ".txt"), header=T, sep="\t")
stepwise1000L=read.table(paste0(DIREC,"sample", sampleID, "stepwise_", level, ".txt"), header=T, sep="\t")
all1000L=read.table(paste0(DIREC,"sample", sampleID, "allPot_", level, ".txt"), header=T, sep="\t")
outPred1000L=read.table(paste0(DIREC,"sample", sampleID, "trueOutcomePred_", level, ".txt"), header=T, sep="\t")


methodName=c("allPotent",  "true", "trueConf", "outcomePred", "SW", "AL", "OAL", "Step-ALT","Step-ALY")

################################################################

pdf(paste0(DIRECRoot, "/case2Results/bootEstimateDist_IPTW.pdf"))


par(mfrow = c(3, 2))
par(cex = 0.6)
par(mar = c(5.5, 2.1, 1.1, 1.1), oma = c(2.5, 2.5, 2.5, 2.5))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0))


val=c(trueConf200H$iptw, truePropen200H$iptw, stepwise200H$iptw, lassoOutcome200H$iptw, lassoSep200H$iptw, 
      lassoStepwiseT200H$iptw, lassoStepwiseY200H$iptw, outPred200H$iptw, all200H$iptw,
      
      trueConf1000H$iptw, truePropen1000H$iptw, stepwise1000H$iptw, lassoOutcome1000H$iptw, lassoSep1000H$iptw, 
      lassoStepwiseT1000H$iptw, lassoStepwiseY1000H$iptw, outPred1000H$iptw, all1000H$iptw)


boxplot(all200H$iptw, truePropen200H$iptw, trueConf200H$iptw, outPred200H$iptw, stepwise200H$iptw, lassoSep200H$iptw, lassoOutcome200H$iptw, 
        lassoStepwiseT200H$iptw, lassoStepwiseY200H$iptw, 
        names=methodName, horizontal = F, las=2, ylim=c(min(val, na.rm = T), max(val, na.rm = T)),
        main="N=200")
mtext("scenario 1", side = 2, line=2)


boxplot(all1000H$iptw, truePropen1000H$iptw, trueConf1000H$iptw, outPred1000H$iptw, stepwise1000H$iptw, lassoSep1000H$iptw, lassoOutcome1000H$iptw,  
        lassoStepwiseT1000H$iptw, lassoStepwiseY1000H$iptw, names=methodName, horizontal = F, las=2, ylim=c(min(val, na.rm = T), max(val, na.rm = T)),
        main="N=1000")




#######################################
val=c(trueConf200M$iptw, truePropen200M$iptw, stepwise200M$iptw, lassoOutcome200M$iptw, lassoSep200M$iptw, 
      lassoStepwiseT200M$iptw, lassoStepwiseY200M$iptw, outPred200M$iptw, all200M$iptw,
      
      trueConf1000M$iptw, truePropen1000M$iptw, stepwise1000M$iptw, lassoOutcome1000M$iptw, lassoSep1000M$iptw, 
      lassoStepwiseT1000M$iptw, lassoStepwiseY1000M$iptw, outPred1000M$iptw, all1000M$iptw)


boxplot(all200M$iptw, truePropen200M$iptw, trueConf200M$iptw, outPred200M$iptw,  stepwise200M$iptw, lassoSep200M$iptw, lassoOutcome200M$iptw, 
        lassoStepwiseT200M$iptw, lassoStepwiseY200M$iptw, names=methodName, horizontal = F, las=2, ylim=c(min(val, na.rm = T), max(val, na.rm = T)),
        main="N=200")
mtext("scenario 2", side = 2, line=2)


boxplot(all1000M$iptw, truePropen1000M$iptw, trueConf1000M$iptw, outPred1000M$iptw, stepwise1000M$iptw, lassoSep1000M$iptw, lassoOutcome1000M$iptw, 
        lassoStepwiseT1000M$iptw, lassoStepwiseY1000M$iptw, names=methodName, horizontal = F, las=2, ylim=c(min(val, na.rm = T), max(val, na.rm = T)),
        main="N=1000")


############################################################

val=c(trueConf200L$iptw, truePropen200L$iptw, stepwise200L$iptw, lassoOutcome200L$iptw, lassoSep200L$iptw, 
      lassoStepwiseT200L$iptw, lassoStepwiseY200L$iptw, outPred200L$iptw, all200L$iptw,
      
      trueConf1000L$iptw, truePropen1000L$iptw, stepwise1000L$iptw, lassoOutcome1000L$iptw, lassoSep1000L$iptw, 
      lassoStepwiseT1000L$iptw, lassoStepwiseY1000L$iptw, outPred1000L$iptw, all1000L$iptw)


boxplot(all200L$iptw, truePropen200L$iptw, trueConf200L$iptw, outPred200L$iptw, stepwise200L$iptw, lassoSep200L$iptw, lassoOutcome200L$iptw, 
        lassoStepwiseT200L$iptw, lassoStepwiseY200L$iptw, 
        names=methodName, horizontal = F, las=2, ylim=c(min(val, na.rm = T), max(val, na.rm = T)),
        main="N=200")
mtext("scenario 3", side = 2, line=2)


boxplot(all1000L$iptw, truePropen1000L$iptw, trueConf1000L$iptw, outPred1000L$iptw, stepwise1000L$iptw, lassoSep1000L$iptw, lassoOutcome1000L$iptw,  
        lassoStepwiseT1000L$iptw, lassoStepwiseY1000L$iptw, names=methodName, horizontal = F, las=2, ylim=c(min(val, na.rm = T), max(val, na.rm = T)),
        main="N=1000")





dev.off()

#########################################################################################################
##########################################################################################################
#################################looking at pencomp bootstrap estimates #################################

pdf(paste0(DIRECRoot, "/case2Results/bootEstimateDist_pencomp.pdf"))


par(mfrow = c(3, 2))
par(cex = 0.6)
par(mar = c(5.5, 2.1, 1.1, 1.1), oma = c(2.5, 2.5, 2.5, 2.5))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0))


val=c(trueConf200H$pencompBoot, truePropen200H$pencompBoot, stepwise200H$pencompBoot, lassoOutcome200H$pencompBoot, lassoSep200H$pencompBoot, 
      lassoStepwiseT200H$pencompBoot, lassoStepwiseY200H$pencompBoot, outPred200H$pencompBoot, all200H$pencompBoot,
      
      trueConf1000H$pencompBoot, truePropen1000H$pencompBoot, stepwise1000H$pencompBoot, lassoOutcome1000H$pencompBoot, lassoSep1000H$pencompBoot, 
      lassoStepwiseT1000H$pencompBoot, lassoStepwiseY1000H$pencompBoot, outPred1000H$pencompBoot, all1000H$pencompBoot)


boxplot(trueConf200H$pencompBoot, truePropen200H$pencompBoot, outPred200H$pencompBoot, all200H$pencompBoot, stepwise200H$pencompBoot, 
        lassoSep200H$pencompBoot, 
        lassoOutcome200H$pencompBoot,
        lassoStepwiseT200H$pencompBoot, lassoStepwiseY200H$pencompBoot, 
        names=methodName, horizontal = F, las=2,ylim=c(min(val, na.rm = T), max(val, na.rm = T)),
        main="N=200")
mtext("scenario 1", side = 2, line=2)


boxplot(trueConf1000H$pencompBoot, truePropen1000H$pencompBoot, outPred1000H$pencompBoot, all1000H$pencompBoot, stepwise1000H$pencompBoot, 
        lassoSep1000H$pencompBoot, lassoOutcome1000H$pencompBoot,  
        lassoStepwiseT1000H$pencompBoot, lassoStepwiseY1000H$pencompBoot, names=methodName, horizontal = F, las=2, ylim=c(min(val, na.rm = T), max(val, na.rm = T)),
        main="N=1000")




#######################################
val=c(trueConf200M$pencompBoot, truePropen200M$pencompBoot, stepwise200M$pencompBoot, lassoOutcome200M$pencompBoot, lassoSep200M$pencompBoot, 
      lassoStepwiseT200M$pencompBoot, lassoStepwiseY200M$pencompBoot, outPred200M$pencompBoot, all200M$pencompBoot,
      
      trueConf1000M$pencompBoot, truePropen1000M$pencompBoot, stepwise1000M$pencompBoot, lassoOutcome1000M$pencompBoot, lassoSep1000M$pencompBoot, 
      lassoStepwiseT1000M$pencompBoot, lassoStepwiseY1000M$pencompBoot, outPred1000M$pencompBoot, all1000M$pencompBoot)


boxplot(trueConf200M$pencompBoot, truePropen200M$pencompBoot, outPred200M$pencompBoot, all200M$pencompBoot, stepwise200M$pencompBoot,
        lassoSep200M$pencompBoot, lassoOutcome200M$pencompBoot, 
        lassoStepwiseT200M$pencompBoot, lassoStepwiseY200M$pencompBoot, names=methodName, horizontal = F, las=2, ylim=c(min(val, na.rm = T), max(val, na.rm = T)),
        main="N=200")
mtext("scenario 2", side = 2, line=2)


boxplot(trueConf1000M$pencompBoot, truePropen1000M$pencompBoot, outPred1000M$pencompBoot, all1000M$pencompBoot, stepwise1000M$pencompBoot, 
        lassoSep1000M$pencompBoot, lassoOutcome1000M$pencompBoot, 
        lassoStepwiseT1000M$pencompBoot, lassoStepwiseY1000M$pencompBoot, names=methodName, horizontal = F, las=2, ylim=c(min(val, na.rm = T), max(val, na.rm = T)),
        main="N=1000")




#######################################
val=c(trueConf200L$pencompBoot, truePropen200L$pencompBoot, stepwise200L$pencompBoot, lassoOutcome200L$pencompBoot, lassoSep200L$pencompBoot, 
      lassoStepwiseT200L$pencompBoot, lassoStepwiseY200L$pencompBoot, outPred200L$pencompBoot, all200L$pencompBoot,
      
      trueConf1000L$pencompBoot, truePropen1000L$pencompBoot, stepwise1000L$pencompBoot, lassoOutcome1000L$pencompBoot, lassoSep1000L$pencompBoot, 
      lassoStepwiseT1000L$pencompBoot, lassoStepwiseY1000L$pencompBoot, outPred1000L$pencompBoot, all1000L$pencompBoot)


boxplot(trueConf200L$pencompBoot, truePropen200L$pencompBoot, outPred200L$pencompBoot, all200L$pencompBoot, stepwise200L$pencompBoot, 
        lassoSep200L$pencompBoot, lassoOutcome200L$pencompBoot, 
        lassoStepwiseT200L$pencompBoot, lassoStepwiseY200L$pencompBoot, names=methodName, horizontal = F, las=2, ylim=c(min(val, na.rm = T), max(val, na.rm = T)),
        main="N=200")
mtext("scenario 3", side = 2, line=2)


boxplot(trueConf1000L$pencompBoot, truePropen1000L$pencompBoot, outPred1000L$pencompBoot, all1000L$pencompBoot, stepwise1000L$pencompBoot, 
        lassoSep1000L$pencompBoot, lassoOutcome1000L$pencompBoot, 
        lassoStepwiseT1000L$pencompBoot, lassoStepwiseY1000L$pencompBoot, names=methodName, horizontal = F, las=2, ylim=c(min(val, na.rm = T), max(val, na.rm = T)),
        main="N=1000")



dev.off()


############################################################################################

pdf(paste0(DIRECRoot, "/case2Results/bootEstimateDist_AIPTW.pdf"))


par(mfrow = c(3, 2))
par(cex = 0.6)
par(mar = c(5.5, 2.1, 1.1, 1.1), oma = c(2.5, 2.5, 2.5, 2.5))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0))


val=c(trueConf200H$aiptw, truePropen200H$aiptw, stepwise200H$aiptw, lassoOutcome200H$aiptw, lassoSep200H$aiptw, 
      lassoStepwiseT200H$aiptw, lassoStepwiseY200H$aiptw, outPred200H$aiptw, all200H$aiptw,
      
      trueConf1000H$aiptw, truePropen1000H$aiptw, stepwise1000H$aiptw, lassoOutcome1000H$aiptw, lassoSep1000H$aiptw, 
      lassoStepwiseT1000H$aiptw, lassoStepwiseY1000H$aiptw, outPred1000H$aiptw, all1000H$aiptw)


boxplot(all200H$aiptw, truePropen200H$aiptw, trueConf200H$aiptw, outPred200H$aiptw, stepwise200H$aiptw, lassoSep200H$aiptw, lassoOutcome200H$aiptw, 
        lassoStepwiseT200H$aiptw, lassoStepwiseY200H$aiptw, 
        names=methodName, horizontal = F, las=2, ylim=c(min(val, na.rm = T), max(val, na.rm = T)),
        main="N=200")
mtext("scenario 1", side = 2, line=2)


boxplot(all1000H$aiptw, truePropen1000H$aiptw, trueConf1000H$aiptw, outPred1000H$aiptw, stepwise1000H$aiptw, lassoSep1000H$aiptw, lassoOutcome1000H$aiptw,  
        lassoStepwiseT1000H$aiptw, lassoStepwiseY1000H$aiptw, names=methodName, horizontal = F, las=2, ylim=c(min(val, na.rm = T), max(val, na.rm = T)),
        main="N=1000")




#######################################
val=c(trueConf200M$aiptw, truePropen200M$aiptw, stepwise200M$aiptw, lassoOutcome200M$aiptw, lassoSep200M$aiptw, 
      lassoStepwiseT200M$aiptw, lassoStepwiseY200M$aiptw, outPred200M$aiptw, all200M$aiptw,
      
      trueConf1000M$aiptw, truePropen1000M$aiptw, stepwise1000M$aiptw, lassoOutcome1000M$aiptw, lassoSep1000M$aiptw, 
      lassoStepwiseT1000M$aiptw, lassoStepwiseY1000M$aiptw, outPred1000M$aiptw, all1000M$aiptw)


boxplot(all200M$aiptw, truePropen200M$aiptw, trueConf200M$aiptw, outPred200M$aiptw,  stepwise200M$aiptw, lassoSep200M$aiptw, lassoOutcome200M$aiptw, 
        lassoStepwiseT200M$aiptw, lassoStepwiseY200M$aiptw, names=methodName, horizontal = F, las=2, ylim=c(min(val, na.rm = T), max(val, na.rm = T)),
        main="N=200")
mtext("scenario 2", side = 2, line=2)


boxplot(all1000M$aiptw, truePropen1000M$aiptw, trueConf1000M$aiptw, outPred1000M$aiptw, stepwise1000M$aiptw, lassoSep1000M$aiptw, lassoOutcome1000M$aiptw, 
        lassoStepwiseT1000M$aiptw, lassoStepwiseY1000M$aiptw, names=methodName, horizontal = F, las=2, ylim=c(min(val, na.rm = T), max(val, na.rm = T)),
        main="N=1000")


############################################################

val=c(trueConf200L$aiptw, truePropen200L$aiptw, stepwise200L$aiptw, lassoOutcome200L$aiptw, lassoSep200L$aiptw, 
      lassoStepwiseT200L$aiptw, lassoStepwiseY200L$aiptw, outPred200L$aiptw, all200L$aiptw,
      
      trueConf1000L$aiptw, truePropen1000L$aiptw, stepwise1000L$aiptw, lassoOutcome1000L$aiptw, lassoSep1000L$aiptw, 
      lassoStepwiseT1000L$aiptw, lassoStepwiseY1000L$aiptw, outPred1000L$aiptw, all1000L$aiptw)


boxplot(all200L$aiptw, truePropen200L$aiptw, trueConf200L$aiptw, outPred200L$aiptw, stepwise200L$aiptw, lassoSep200L$aiptw, lassoOutcome200L$aiptw, 
        lassoStepwiseT200L$aiptw, lassoStepwiseY200L$aiptw, 
        names=methodName, horizontal = F, las=2, ylim=c(min(val, na.rm = T), max(val, na.rm = T)),
        main="N=200")
mtext("scenario 3", side = 2, line=2)


boxplot(all1000L$aiptw, truePropen1000L$aiptw, trueConf1000L$aiptw, outPred1000L$aiptw, stepwise1000L$aiptw, lassoSep1000L$aiptw, lassoOutcome1000L$aiptw,  
        lassoStepwiseT1000L$aiptw, lassoStepwiseY1000L$aiptw, names=methodName, horizontal = F, las=2, ylim=c(min(val, na.rm = T), max(val, na.rm = T)),
        main="N=1000")





dev.off()



