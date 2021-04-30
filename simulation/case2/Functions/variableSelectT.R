######################################################################################
##############variable selection on the propensity model ############
#####################################################################################################
#############including the variables selected by outcome adaptive lasso and traditional adaptive lasso 
#######propensity scores including variables associated with treatment and outcome #############
allLasso=function(data, varList, treat.varname, outcome.varname) {
  

    ###step 1--variable selection on the propensity model 
    ###in this step, I use outcome adaptive lasso so variables not predictors of treatment but of outcome are more likely to be included
    varFilter = outcomeAdapLasso(Data=data, varList=varList, treat.varname=treat.varname, outcome.varname=outcome.varname, threshold=10^-8)
    
    ###step 2 adaptive lasso on the outcome model using the variables selected at step 1
    selectedVar = adapLassoY(data=data, varList=varList, treat.varname=treat.varname, outcome.varname=outcome.varname)
    
    selectedVar=c(selectedVar, varFilter)
    selectedVar=unique(selectedVar)
    
  return(selectedVar)
  
}



#####################################################################################################
######first perform variable selection on the outcome model,##########################################
###then perform variable selection on the propensity model, excluding variables not selected at the first stage
###7/12/2018 Here I changed this ### CHANGE THIS COMPARED TO CASE3
stepwiseLasso=function(data, varList, treat.varname, outcome.varname, first="outcome") {
  
  #require(glmnet)
  if(first=="outcome") { ###variable selection on the outcome model first
    
    ###step 1-filter out variables not associated with outcome 
    varFilter = adapLassoY(data=data, varList=varList, treat.varname=treat.varname, outcome.varname=outcome.varname)
    
    ###reason if predictors of the outcome have efficiency gains, why not include them all in the propensity model
    ## step 2-perform outcome adaptive lasso for the propensity model including only the variables selected at the step 1
    #selectedVar = outcomeAdapLasso(data=data, varList=varFilter, treat.varname=treat.varname, outcome.varname=outcome.varname)
    selectedVar=varFilter
    
  } else {  ###variable selection on the propensity model first
    

    ###step 1--variable selection on the propensity model 
    ###in this step, I use outcome adaptive lasso so variables not predictors of treatment but of outcome are more likely to be included
    varFilter = outcomeAdapLasso(Data=data, varList=varList, treat.varname=treat.varname, outcome.varname=outcome.varname, threshold=10^-8)
    
    ###step 2 adaptive lasso on the outcome model using the variables selected at step 1
    selectedVar = adapLassoY(data=data, varList=varFilter, treat.varname=treat.varname, outcome.varname=outcome.varname)

    
  }
  
  return(selectedVar)
  
}


##########################################################################
##########stepwise variable selection for the outcome Y###################
stepX=function(data, varList, outcome.varname) {
  
  ########################variable selection on the prediction model #########################
  modelForm.1 = as.formula(paste(outcome.varname, " ~ ", paste(c(1), collapse = "+")))
  model.1=glm(modelForm.1 , data=data, family = "binomial") 
  
  modelForm.2 = as.formula(paste(outcome.varname, " ~ ", paste(c(1, varList), collapse = "+")))
  model.2=glm(modelForm.2, data=data, family = "binomial") 
  
  stepMod <- step(model.1, scope = list(lower = model.1, upper = model.2), direction = "both", trace = 0, steps = 1000, criterion = "BIC")  # perform step-wise algorithm
  shortlistedVars <- names(unlist(stepMod[[1]])) # get the shortlisted variable.
  shortlistedVars <- shortlistedVars[!shortlistedVars %in% "(Intercept)"]  # remove intercept 
  return(shortlistedVars)
  
}



