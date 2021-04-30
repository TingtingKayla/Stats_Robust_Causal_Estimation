

#y.varname=outcome.varname;x.varnames=outcomeVarList0; propen.score=data0$pslogit;
#data=data0; num.knot=numKnot; basisType="tp"; spacing="quantile";


#########################################################
###assume a truncated linear basis
### right now it only supports truncated linear basis with equally spaced knot
### will expand on this later
###basisType="tp" (low rank thin plate spline), "lb" (truncated linear basis)
###spacing="quantile", or "equal" spacing
pencompFit=function(y.varname, x.varnames, propen.score, data, num.knot) {
  
  #####knot locations################
  space=(max(propen.score)-min(propen.score))/(num.knot+1)
  knots=(min(propen.score)+space*(1:num.knot))
  
  ###########################################################
  linearB=NULL  ###associated with random coefficients
  linearB =outer(propen.score, knots, "-")
  linearB =linearB * (linearB > 0)

  
  ####################################################
  ########### matrix design for fixed effects
  response=data[, y.varname]
  covariateX=NULL
  for(i in 1:length(x.varnames)){
    temp = NULL
    temp = cbind(temp, data[, x.varnames[i] ])
    colnames(temp) = x.varnames[i]
    covariateX=cbind(covariateX, temp)
  }
  covariateX=cbind(rep(1,nrow(data)), covariateX, propen.score)
  
  
  all=rep(1, dim(data)[1])
  psppM=lme(response ~ 0+covariateX, method="REML", random=list(all=pdIdent(~0+linearB)) ) 
  fixCoef=psppM$coefficients$fixed
  names(fixCoef)=c("intercept", x.varnames, "propen.score")
  randCoef=psppM$coefficients$random$all
  
  return(list(fixed=fixCoef, random=randCoef, knot.loc=knots, sigmaRes=psppM$sigma))
  
}






######################################################################
##########prediction values############################################ 
###assume a truncated linear basis
##model is fitted model from lmeFit
###imputing missing potential outcomes 
###basisType="tp" (low rank thin plate spline), "lb" (truncated linear basis)

imputeF=function(newdata, model, x.varnames, propen.score.new) {
  
  knots=model$knot.loc
  
  ###########################################################
  linearB=NULL  ###associated with random coefficients
  linearB =outer(propen.score.new, knots, "-")
  linearB =linearB * (linearB > 0)

  
  covariateX=NULL
  for(i in 1:length(x.varnames)){
    temp = NULL
    temp = cbind(temp, newdata[, x.varnames[i] ])
    colnames(temp) = x.varnames[i]
    covariateX=cbind(covariateX, temp)
  }
  
  designM=cbind(rep(1,nrow(newdata)), covariateX, propen.score.new)
  
  designM=as.matrix(designM)
  predicted = designM %*% model$fixed + as.matrix(linearB) %*% as.vector(unlist(model$random)) + rnorm(nrow(newdata), 0, model$sigmaRes)

  return(predicted)
  
}


