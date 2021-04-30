
#######################################################################################
#######################adaptive lasso on the outcome model ############################
#data=simdat
adapLassoY=function(data, varList, treat.varname, outcome.varname) {
  

  # Normlize coviarates to have mean 0 and standard deviation 1
  temp.mean = colMeans(data[,varList])
  Temp.mean = matrix(temp.mean,ncol=length(varList),nrow=nrow(data),byrow=TRUE)
  data[,varList] = data[,varList] - Temp.mean
  temp.sd = apply(data[varList], FUN=sd, MARGIN=2)
  Temp.sd = matrix(temp.sd,ncol=length(varList), nrow=nrow(data), byrow=TRUE)
  data[varList] = data[,varList] / Temp.sd
  rm(list=c("temp.mean","Temp.mean","temp.sd","Temp.sd"))
  
  #require(glmnet)
  ## ridge regression on the outcome model
  x <- as.matrix(data[,c(treat.varname, varList)])
  y <- as.double(as.matrix(data[, outcome.varname]))
  
  
  ## Perform ridge regression with 10-fold CV
  ridge1_cv <- cv.glmnet(x = x, y = y, type.measure = "mse",
                         ## K = 10 is the default.
                         nfold = 10,
                         alpha = 0)
  ## Penalty vs CV MSE plot
  #plot(ridge1_cv)
  ## The intercept estimate should be dropped.
  best_ridge_coef <- as.numeric(coef(ridge1_cv, s = ridge1_cv$lambda.min))[-c(1)]
  
  x = NULL
  y = NULL
  x <- as.matrix(data[, c(treat.varname, varList)])
  y <- as.double(as.matrix(data[, outcome.varname]))
  ## Perform adaptive LASSO with 10-fold CV
  alasso1_cv <- cv.glmnet(x = x, y = y,
                          ## type.measure: loss to use for cross-validation.
                          type.measure = "mse",
                          ## K = 10 is the default.
                          nfold = 10,
                          ## 'alpha = 1' is the lasso penalty, and 'alpha = 0' the ridge penalty.
                          alpha = 1,
                          ##
                          ## penalty.factor: Separate penalty factors can be applied to each
                          ##           coefficient. This is a number that multiplies 'lambda' to
                          ##           allow differential shrinkage. Can be 0 for some variables,
                          ##           which implies no shrinkage, and that variable is always
                          ##           included in the model. Default is 1 for all variables (and
                          ##           implicitly infinity for variables listed in 'exclude'). Note:
                          ##           the penalty factors are internally rescaled to sum to nvars,
                          ##           and the lambda sequence will reflect this change.
                          penalty.factor = 1 / abs(best_ridge_coef),
                          ## prevalidated array is returned
                          keep = TRUE)
  ## Penalty vs CV MSE plot
  #plot(alasso1_cv)
  #best_alasso_coef1 <- coef(alasso1_cv, s = alasso1_cv$lambda.min)
  best_alasso_coef1 <- coef(alasso1_cv, s = alasso1_cv$lambda.1se)
  best_alasso_coef1 = best_alasso_coef1[-c(1, 2),]
  
  selectedVar <- names(best_alasso_coef1 )[which( best_alasso_coef1  != 0) ]## Consideri
  selectedVar
  return( selectedVar )
  #return(list(selectedVar,best_alasso_coef1) )
  
}



##########################################################################
##########stepwise variable selection for the outcome Y###################
######this should be in case 3, the stepwise with BIC in case 3 should be case 4 and case4 should be case 3
####similar to the other methods, here I include treatment indicator in the model selection
stepY=function(data, varList, treat.varname, outcome.varname) {
  
  ########################variable selection on the prediction model #########################
  modelForm.1 = as.formula(paste(outcome.varname, " ~ ", paste(c(1), collapse = "+")))
  model.1=lm(modelForm.1 , data=data) 
  
  modelForm.2 = as.formula(paste(outcome.varname, " ~ ", paste(c(1, treat.varname, varList), collapse = "+")))
  model.2=lm(modelForm.2, data=data) 
  
  stepMod <- step(model.1, scope = list(lower = model.1, upper = model.2), direction = "both", trace = 0, steps = 1000, criterion = "BIC")  # perform step-wise algorithm
  shortlistedVars <- names(unlist(stepMod[[1]])) # get the shortlisted variable.
  shortlistedVars <- shortlistedVars[!shortlistedVars %in% c("(Intercept)", treat.varname)]  # remove intercept and treatment 
  return(shortlistedVars)
  
}


