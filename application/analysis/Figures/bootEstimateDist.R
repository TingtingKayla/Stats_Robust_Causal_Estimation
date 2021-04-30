

####looking at distributions of the estimates across the bootstraps
DIREC="M:/Private/overlap/Application/version4/bootResults/"

methodName=c("allPotent", "SW", "AL", "OAL", "Step-ALT","Step-ALY")

####looking at proportions that each variable was selected across all 500 simulated datasets
varList = c(paste0("LEU3N", 1:4), paste0("LEU2N", 1:4), paste0("WBC", 1:4), paste0("RBC", 1:4), paste0("PLATE", 1:4),
            "age", "white", "college", paste0("A", 1:3)) 
################################################################

pdf("M:/Private/overlap/Application/version4/Figures/bootstrapEstimates.pdf")

sampleID=12

par(mfrow = c(2, 1))
par(cex = 0.6)
par(mar = c(5.5, 2.1, 1.1, 1.1), oma = c(2.5, 2.5, 2.5, 2.5))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0))



lassoOutcome=read.table(paste0(DIREC,"sample", sampleID, "lassoOutcome.txt"), header=T, sep="\t")
lassoSep=read.table(paste0(DIREC,"sample", sampleID, "lasso.txt"), header=T, sep="\t")
lassoStepwiseY=read.table(paste0(DIREC,"sample", sampleID, "lassoStepY.txt"), header=T, sep="\t")
lassoStepwiseT=read.table(paste0(DIREC,"sample", sampleID, "lassoStepT.txt"), header=T, sep="\t")
stepwise=read.table(paste0(DIREC,"sample", sampleID, "stepwise.txt"), header=T, sep="\t")
allPotent=read.table(paste0(DIREC,"sample", sampleID, "allPotent.txt"), header=T, sep="\t")


val=c(allPotent$iptw, stepwise$iptw, lassoSep$iptw, lassoOutcome$iptw, lassoStepwiseT$iptw, lassoStepwiseY$iptw,
      allPotent$pencompBoot, stepwise$pencompBoot, lassoSep$pencompBoot, lassoOutcome$pencompBoot, lassoStepwiseT$pencompBoot,
      lassoStepwiseY$pencompBoot)

boxplot(allPotent$iptw, stepwise$iptw, lassoSep$iptw, lassoOutcome$iptw, lassoStepwiseT$iptw, lassoStepwiseY$iptw, 
        names=methodName, horizontal = F, las=2, ylim=c(min(val, na.rm = T), max(val, na.rm = T)),
        main=paste0("IPTW Estimates"))

boxplot(allPotent$pencompBoot, stepwise$pencompBoot, lassoSep$pencompBoot, lassoOutcome$pencompBoot, lassoStepwiseT$pencompBoot,
        lassoStepwiseY$pencompBoot, 
        names=methodName, horizontal = F, las=2, ylim=c(min(val, na.rm = T), max(val, na.rm = T)),
        main=paste0("PENCOMP Estimates"))


dev.off()




