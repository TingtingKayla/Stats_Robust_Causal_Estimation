#######plots used in the paper###############
#############################################
#################here I plot the propensity score distributions be

#rm(list=ls())

###first load the data 

sampleID=12

####looking at proportions that each variable was selected across all 500 simulated datasets
varList = c(paste0("LEU3N", 1:4), paste0("LEU2N", 1:4), paste0("WBC", 1:4), paste0("RBC", 1:4), paste0("PLATE", 1:4),
            "age", "white", "college", paste0("A", 1:3)) 

####################################################################################
####################################################################################

DIRECRoot="M:/Private/overlap/Application/version4/"
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


row.names(varProp_stepwise)[which(varProp_stepwise>=20)]
row.names(varProp_lassoSep)[which(varProp_lassoSep>=20)]
row.names(varProp_lassoStepwiseT)[which(varProp_lassoStepwiseT>=20)]
row.names(varProp_lassoStepwiseY)[which(varProp_lassoStepwiseY>=20)]


treat.varname="A4"
outcome.varname="LEU3N5" 


pdf("M:/Private/overlap/Application/version4/Figures/propensityDist.pdf")

par(mfrow = c(2, 2))
par(cex = 0.6)
par(mar = c(5.5, 2.1, 1.1, 1.1), oma = c(2.5, 2.5, 2.5, 2.5))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0))


#################################################
propenVarList=varList
propen.model=formulaF(varList=c(1,propenVarList), y.name=treat.varname)
treatBoot=simdatFit[, treat.varname]

######## propensity score model ########################################################### 
model2a=glm(propen.model, data=simdatFit, family="binomial", control = list(maxit = 50))
probT=predict(model2a, type="response")

#########these codes are for checking overlap distributions ##########
###########looking the overlap regions when all the covariates were included vs with the selected models##########
#propenVarList=varList
#propen.model=formulaF(varList=c(1,propenVarList), y.name=treat.varname)

#plot(density(probT[treatBoot==0], from=0, 
#             to=1), lty=1, lwd=2, col="black", xlab="Propensity Score")
#lines(density(probT[treatBoot==1],from=0, to=1), lty=2, lwd=2, col="red")
#legend("center", c("control","treated"), cex=1.2, lty=1:2, col=c("black", "red"))


# Histogram Grey Color
hist(probT[treatBoot==0], col=rgb(0.1,0.1,0.1,0.5), ylim=c(0, 220), main="A", xlab="")
hist(probT[treatBoot==1], col=rgb(0.8,0.8,0.8,0.5), add=T)
box()


#######proportion of treated that is inside the 5% and 95% quantile of propensity score distribution of the control
cutoff=quantile(probT[simdatFit[, treat.varname]==0], probs = c(0.05, 0.95))
prop1=(sum(probT[simdatFit[, treat.varname]==1] >= cutoff[1]  & probT[simdatFit[, treat.varname]==1] <= cutoff[2]))/sum(simdatFit[, treat.varname]==1)

#######proportion of treated that is inside the 5% and 95% quantile of propensity score distribution of the treated
cutoff=quantile(probT[simdatFit[, treat.varname]==1], probs = c(0.05, 0.95))
prop0=(sum(probT[simdatFit[, treat.varname]==0] >= cutoff[1]  & probT[simdatFit[, treat.varname]==0] <= cutoff[2]))/sum(simdatFit[, treat.varname]==0)


prop1
prop0

#> prop1
#[1] 0.179661
#> prop0
#[1] 0.2238806
#> 


######################################################################################
######################################################################################
propenVarList=row.names(varProp_lassoStepwiseT)[which(varProp_lassoStepwiseT>=20)]
propen.model=formulaF(varList=c(1,propenVarList), y.name=treat.varname)
treatBoot=simdatFit[, treat.varname]

######## propensity score model ########################################################### 
model2a=glm(propen.model, data=simdatFit, family="binomial", control = list(maxit = 50))
probT=predict(model2a, type="response")

# Histogram Grey Color
hist(probT[treatBoot==0], col=rgb(0.1,0.1,0.1,0.5), ylim=c(0, 220), main="B", xlab="")
hist(probT[treatBoot==1], col=rgb(0.8,0.8,0.8,0.5), add=T)
legend("center", c("control","treated"), cex=1.2, lty=1:2, col=c("black", "grey"))
box()


#######proportion of treated that is inside the 5% and 95% quantile of propensity score distribution of the control
cutoff=quantile(probT[simdatFit[, treat.varname]==0], probs = c(0.05, 0.95))
prop1=(sum(probT[simdatFit[, treat.varname]==1] >= cutoff[1]  & probT[simdatFit[, treat.varname]==1] <= cutoff[2]))/sum(simdatFit[, treat.varname]==1)

#######proportion of treated that is inside the 5% and 95% quantile of propensity score distribution of the treated
cutoff=quantile(probT[simdatFit[, treat.varname]==1], probs = c(0.05, 0.95))
prop0=(sum(probT[simdatFit[, treat.varname]==0] >= cutoff[1]  & probT[simdatFit[, treat.varname]==0] <= cutoff[2]))/sum(simdatFit[, treat.varname]==0)


prop1
prop0

#> prop1
#[1] 0.3322034
#> prop0
#[1] 0.4925373


dev.off()




######################################################################################
######################################################################################
propenVarList=row.names(varProp_stepwise)[which(varProp_stepwise>=20)]
propen.model=formulaF(varList=c(1,propenVarList), y.name=treat.varname)
treatBoot=simdatFit[, treat.varname]

######## propensity score model ########################################################### 
model2a=glm(propen.model, data=simdatFit, family="binomial", control = list(maxit = 50))
probT=predict(model2a, type="response")

# Histogram Grey Color
hist(probT[treatBoot==0], col=rgb(0.1,0.1,0.1,0.5), ylim=c(0, 220), main="B", xlab="")
hist(probT[treatBoot==1], col=rgb(0.8,0.8,0.8,0.5), add=T)
legend("center", c("control","treated"), cex=1.2, lty=1:2, col=c("black", "grey"))
box()



#######proportion of treated that is inside the 5% and 95% quantile of propensity score distribution of the control
cutoff=quantile(probT[simdatFit[, treat.varname]==0], probs = c(0.05, 0.95))
prop1=(sum(probT[simdatFit[, treat.varname]==1] >= cutoff[1]  & probT[simdatFit[, treat.varname]==1] <= cutoff[2]))/sum(simdatFit[, treat.varname]==1)

#######proportion of treated that is inside the 5% and 95% quantile of propensity score distribution of the treated
cutoff=quantile(probT[simdatFit[, treat.varname]==1], probs = c(0.05, 0.95))
prop0=(sum(probT[simdatFit[, treat.varname]==0] >= cutoff[1]  & probT[simdatFit[, treat.varname]==0] <= cutoff[2]))/sum(simdatFit[, treat.varname]==0)


prop1
prop0

#> prop1
#[1] 0.1762712
#> prop0
#[1] 0.2288557



######################################################################################
######################################################################################
propenVarList=row.names(varProp_lassoOutcome)[which(varProp_lassoOutcome>=20)]
propen.model=formulaF(varList=c(1,propenVarList), y.name=treat.varname)
treatBoot=simdatFit[, treat.varname]

######## propensity score model ########################################################### 
model2a=glm(propen.model, data=simdatFit, family="binomial", control = list(maxit = 50))
probT=predict(model2a, type="response")

# Histogram Grey Color
hist(probT[treatBoot==0], col=rgb(0.1,0.1,0.1,0.5), ylim=c(0, 220), main="B", xlab="")
hist(probT[treatBoot==1], col=rgb(0.8,0.8,0.8,0.5), add=T)
legend("center", c("control","treated"), cex=1.2, lty=1:2, col=c("black", "grey"))
box()



#######proportion of treated that is inside the 5% and 95% quantile of propensity score distribution of the control
cutoff=quantile(probT[simdatFit[, treat.varname]==0], probs = c(0.05, 0.95))
prop1=(sum(probT[simdatFit[, treat.varname]==1] >= cutoff[1]  & probT[simdatFit[, treat.varname]==1] <= cutoff[2]))/sum(simdatFit[, treat.varname]==1)

#######proportion of treated that is inside the 5% and 95% quantile of propensity score distribution of the treated
cutoff=quantile(probT[simdatFit[, treat.varname]==1], probs = c(0.05, 0.95))
prop0=(sum(probT[simdatFit[, treat.varname]==0] >= cutoff[1]  & probT[simdatFit[, treat.varname]==0] <= cutoff[2]))/sum(simdatFit[, treat.varname]==0)


prop1
prop0

#> prop1
#[1] 0.1728814
#> prop0
#[1] 0.2736318

