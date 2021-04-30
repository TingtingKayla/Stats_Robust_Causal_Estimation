######data current dataset
####dataOR original dataset
###adaptive lasso on the prediction model, and then the propensity model 
#dataOR=simdat
#propenVarList=NULL; outcomeVarList0=NULL; outcomeVarList1=NULL;
#treat.varname=treat.varname; outcome.varname=outcome.varname;

###dataOR-original dataset
###data-bootstrap sample

#dataOR=simdat; data=bootSample; varList=varList; propenVarList=propenVarList;
#outcomeVarList0=outcomeVarList0; outcomeVarList1=outcomeVarList1;
#treat.varname=treat.varname; outcome.varname=outcome.varname; first=firstNum; splineTerm = splineTerm


####dataOR-original dataset
####data-bootstrap sample

pencompAllLasso=function(dataOR, data, varList, propenVarList, outcomeVarList0, outcomeVarList1, treat.varname, outcome.varname) {
  tryCatch ( 
    {
      
      treatInd = dataOR[, treat.varname]  ###indicator of treated subjects in the original dataset
      Yobs = dataOR[, outcome.varname]
      
      treatBoot = data[, treat.varname]  ###indicator of treated subjects in the bootstrap sample
      YobsBoot = data[, outcome.varname]
      
      ######## propensity score model ########################################################### 
      ###fit the propensity, use them for all the estimates
      ##for propensity model
      propen.model=NULL
      if ( length(propenVarList)==0 ){
        
        propenVarList=allLasso(data=data, varList=varList, treat.varname = treat.varname, outcome.varname=outcome.varname)
      } 
      

      propenVarList=unique(propenVarList)
      propen.model=formulaF(varList=c(1,propenVarList), y.name=treat.varname)
      
      
      ######## propensity score model ########################################################### 
      model2a=glm(propen.model, data=data, family="binomial", control = list(maxit = 50))
      prob.boot=predict(model2a, newdata=data, type="response")
      temp2a=NULL
      temp2a=as.numeric(treatBoot==1)*prob.boot + as.numeric(treatBoot==0)*(1-prob.boot)  ###estimate propensity of observed treatment
      data$pslogit = log(temp2a/(1-temp2a))   ####here the name pslogit is probability of observed treatment
      probT = predict(model2a, newdata = data, type = "response")
      
      
      ####fit the prediction model for all the models
      ###for the outcome model separate model y0 and y1
      modely0=NULL
      modely1=NULL
      
      if ( length(outcomeVarList0)==0 & length(outcomeVarList1)==0 ){
        
        outcomeVarList0=adapLassoY(data=data, varList=varList, treat.varname=treat.varname, outcome.varname=outcome.varname)
        outcomeVarList1=outcomeVarList0
      } 
      
      #########store the variables before removing variables to 
      outcomeVarList0Comp=outcomeVarList0
      outcomeVarList1Comp=outcomeVarList1
      
      
      ###construct outcome models for AIPTW
      modely0 = formulaF(varList=c(1, outcomeVarList0), y.name=outcome.varname)
      modely1 = formulaF(varList=c(1, outcomeVarList1), y.name=outcome.varname)
      
      ##############################################################
      ###prediction model for Y under control using observed data
      y0.model <- lm(modely0, data=data[treatBoot==0,])
      #######################################################################
      ###prediction model for Y under treatment using treated data
      y1.model <- lm(modely1, data=data[treatBoot==1,])
      
      pred0=predict(y0.model, newdata=data)
      pred1=predict(y1.model, newdata=data)
      
      
      
      ###########IPTW and AIPTW##################
      weightTop=rep(1, length(probT))
      weight = weightTop/( treatBoot*probT + (1-treatBoot)*(1-probT) )
      
      IPTW=sum( (YobsBoot * weight)[which(treatBoot==1)] )/sum( weight[which(treatBoot==1)] ) -
        sum( (YobsBoot * weight)[which(treatBoot==0)] )/sum( weight[which(treatBoot==0)] )
      
      AIPTW=sum( weightTop * (pred1 - pred0) ) / sum(weightTop) + sum( weight * treatBoot * (YobsBoot - pred1) ) / sum(weight * treatBoot) -
        sum( weight * (1-treatBoot) * (YobsBoot - pred0) ) / sum(weight * (1-treatBoot))
      
      
      
      
      
      ##############################################################
      ###prediction model for Y under control using observed data
      ###using mgcv package with B splines
      ##############################################################
      
      ##########to avoid multicollinearity issue, remove 
      ######## propensity score model ########################################################### 
      
      if(length(propenVarList)!=0 & (sum(propenVarList %in% outcomeVarList0) == length(propenVarList))){  ###if all the selected variables in outcome are in the propensity
        removeIndex=which(outcomeVarList0==propenVarList[length(propenVarList)])
        outcomeVarList0 = outcomeVarList0[-c(removeIndex)]                      ###then remove one, not if otherwise
      } else {
        outcomeVarList0=outcomeVarList0  
      }
      
      ##########################
      if(length(propenVarList)!=0 & (sum(propenVarList %in% outcomeVarList1) == length(propenVarList))){  ###if all the selected variables in outcome are in the propensity
        removeIndex=which(outcomeVarList1==propenVarList[length(propenVarList)])
        outcomeVarList1 = outcomeVarList1[-c(removeIndex)]                      ###then remove one, not if otherwise
      } else {
        outcomeVarList1=outcomeVarList1  
      }
      

      
      ##################################################using truncated linear basis##################################
      ################################################################################################################
      ################################################################################################################
      ###prediction model for Y under control using observed data
      ###assume a truncated linear basis
      data0=data[treatBoot==0,] 
      data1=data[treatBoot==1,]
      
      if(length(propenVarList) !=0){
        
        K0=round(min(length(unique(data0$pslogit))/4, 35))
        pspp0=pencompFit(y.varname=outcome.varname, x.varnames=outcomeVarList0, propen.score=data0$pslogit, data=data0, num.knot=K0)
        
        ####################################################################
        ####imputing the missing potential outcomes under treatment#########
        K1=round(min(length(unique(data1$pslogit))/4, 35))
        pspp1=pencompFit(y.varname=outcome.varname, x.varnames=outcomeVarList1, propen.score=data1$pslogit, data=data1, num.knot=K1)
        
      } else {
        
        pspp0=y0.model 
        pspp1=y1.model 
        
      }
      
      
      #########################################
      ###imputing the missing potential outcomes under control with draws
      newData0=dataOR
      newData0[, treat.varname]=0
      predict2a=1 - predict(model2a, newdata=newData0, type="response")
      newData0$pslogit=log(predict2a/(1-predict2a))
      
      
      
      #########################################
      ###imputing the missing potential outcomes under control with draws
      newData1=dataOR
      newData1[, treat.varname]=1
      predict2a=predict(model2a, newdata=newData1, type="response")
      newData1$pslogit=log(predict2a/(1-predict2a))
      
      
      if(length(propenVarList) !=0){
        
        imputed0=as.vector( imputeF(newdata=newData0, model=pspp0, x.varnames=outcomeVarList0, propen.score.new=newData0$pslogit) )
        imputed1=as.vector( imputeF(newdata=newData1, model=pspp1, x.varnames=outcomeVarList1, propen.score.new=newData1$pslogit) )
        
      } else{
        
        imputed0=as.vector( predict(pspp0, newdata=newData0) + rnorm(nrow(newData0), 0, summary(pspp0)$sigma) )
        imputed1=as.vector( predict(pspp1, newdata=newData1) + rnorm(nrow(newData1), 0, summary(pspp1)$sigma) )
        
      }
      
      ##############keep the observed outcome the same#####################
      imputed1[treatInd==1]=Yobs[treatInd==1]
      imputed0[treatInd==0]=Yobs[treatInd==0]
      
      #######include everyone
      ATE=c(mean(imputed1-imputed0), var(imputed1-imputed0)/nrow(dataOR) )
      
      
      
      #############################################################
      #############imputing on each bootstrap sample######################
      ###imputing the missing potential outcomes under control with draws
      imputed1=NULL
      imputed0=NULL
      newData0=data
      newData0[, treat.varname]=0
      predict2a=1 - predict(model2a, newdata=newData0, type="response")
      newData0$pslogit=log(predict2a/(1-predict2a))
      
      
      
      ####################################################################
      ####imputing the missing potential outcomes under treatment#########
      newData1=data
      newData1[, treat.varname]=1
      predict2a=predict(model2a, newdata=newData1, type="response")
      newData1$pslogit=log(predict2a/(1-predict2a))
      
      
      if(length(propenVarList) !=0){
        
        imputed0=as.vector( imputeF(newdata=newData0, model=pspp0, x.varnames=outcomeVarList0, propen.score.new=newData0$pslogit) )
        imputed1=as.vector( imputeF(newdata=newData1, model=pspp1, x.varnames=outcomeVarList1, propen.score.new=newData1$pslogit) )
        
      } else{
        
        imputed0=as.vector( predict(pspp0, newdata=newData0) + rnorm(nrow(newData0), 0, summary(pspp0)$sigma) )
        imputed1=as.vector( predict(pspp1, newdata=newData1) + rnorm(nrow(newData1), 0, summary(pspp1)$sigma) )
        
      }
      
      ##############keep the observed outcome the same#####################
      imputed1[treatBoot==1]=YobsBoot[treatBoot==1]
      imputed0[treatBoot==0]=YobsBoot[treatBoot==0]
      
      ######
      ATE.boot=mean(imputed1-imputed0)
      
      
      ###########no variables were selected for propensity score model, do not use the splines
      #########CHANGE on 7/12/2018 
      if(length(propenVarList) !=0){
        
        K0 = K0 
        K1 = K1 
        
      } else {
        
        K0 = 0
        K1 = 0
      }

      
      
      return( list( out=c(IPTW, AIPTW, ATE.boot, ATE), varTreat=as.numeric(varList %in% propenVarList),
                    varY1=as.numeric(varList %in% outcomeVarList1Comp), 
                    varY0=as.numeric(varList %in% outcomeVarList0Comp), numK=c(K0, K1 ) ) )###estimate and variance of ATE
    }, error=function(e) return(NA) )
}


