# Logit models

# Libraries
library(boot)
library(glmnet)
library(ggplot2)
library(caret)
library(e1071)
library(data.table)
library(dplyr)

## For reproducibility, check without
set.seed(1569787)

## Loading data

Raw_Data<-readRDS("MIDA_4.2_Cleaned.rds")

## Problem 1 the excessive NAs

colSums(is.na(Raw_Data))

## Some quick pruning, probably should do more intenstive checks to determin what to remove
NNA_Data<-as.data.table(na.omit(Raw_Data[,c(4:5,7:9,13:19,23,27)]))

## What are the problems?

### Problem 1, correlated variables
#### Problem pairs (stmon,endmon), (maxdur,mindur), (hostlev,hiact)
#### Numeric var

NumericPred<-NNA_Data[,sapply(NNA_Data,class) %in% c("integer","double","numeric") & -14,with=FALSE]
cormat<-cor(NumericPred)
heatmap(cormat,symm = TRUE)

#### Cat/binary var

nonNumericPred<-NNA_Data[,-14 & !(sapply(NNA_Data,class) %in% c("integer","double","numeric")),with=FALSE]
nonNumericPred<-nonNumericPred[,-6]


## Create a crude logit model, mostly for comparison purposes
dlogit_intercept<-glm(deaths~1,data=NNA_Data,family = "binomial")
fullformula<-paste("deaths~",paste(colnames(NNA_Data[,-14]),collapse = "+"))
dlogit_step<-step(dlogit_intercept,scope = fullformula,direction = "both")

## Using more sophisticated approach (Lasso)
### Use parallel
x.mm<-model.matrix(~.,NNA_Data[,1:13])

cv.dlogit_lasso<-cv.glmnet(x.mm,NNA_Data$deaths,family="binomial",alpha = 1,nfolds = 10)

### Look at results (Use lambda.1se as it is more conservative/favors simpler models)
lambda_hat<-cv.dlogit_lasso$lambda.1se
plot(cv.dlogit_lasso)

coef(cv.dlogit_lasso,s="lambda.1se")

## Now without hiact and hostlevel

x.mm2<-model.matrix(~.,NNA_Data[,c(1:7,10:13)])
cv.dlogit_lasso_limited<-cv.glmnet(x.mm2,NNA_Data$deaths,family="binomial",alpha = 1,nfolds = 10)

# Comparison of results
## Can be made more efficient later but not really an issue, Might be able to add parallel

ResDat<-data.frame("Intercept"= predict(dlogit_intercept,type = "response"),
                   "Step"= predict(dlogit_step,type = "response"),
                   "Lasso"= unname(predict(cv.dlogit_lasso,newx = x.mm,s="lambda.1se",type="response")),
                   "Limited"= unname(predict(cv.dlogit_lasso_limited,newx = x.mm2,s="lambda.1se",type="response")))

# A function to calculate log likelihood b/c for some reason I couldn't find one
## perhaps do it more elegantly
logitLL<-function(fit,real){
  n<-length(real)
  indvll<-vector(length=n)
  for(i in 1:n){
    indvll[i]<-real[i]*log(fit[i])+(1-real[i])*log(1-fit[i])
  }
  return(sum(indvll))
}

AIChack<-function(cv.model,fit,real){
  p<-nnzero(coef(cv.model,s="lambda.1se"))
  return(-2*logitLL(fit,real)+2*p)
}

extractcoef<-function(cv.model){
  allcoef<-coef(cv.model,s="lambda.1se")
  res<-allcoef[which(allcoef !=0)]
  names(res)<-dimnames(allcoef)[[1]][which(allcoef !=0)]
  return(res)
}


## Misclass

transFitted<-function(fittedresults,divide=.5){
  n<-length(fittedresults)
  res<-rep(FALSE,n)
  for(i in 1:n){
    if(fittedresults[i]>divide){
      res[i]<-TRUE
    }
  }
  return(res)
}

confAll<-function(oddsDat,trueDat){
  N<-dim(oddsDat)[2]
  res<-vector(mode="list",length=N)
  for(i in 1:N){
    res[[i]]<-confusionMatrix(as.factor(transFitted(oddsDat[,i])),
                            as.factor(trueDat),
                            positive = "TRUE")
  }
  return(res)
}

confusion_mats<-confAll(ResDat,NNA_Data$deaths)

getVfromLiL<-function(x,l1,l2) x[[l1]][[l2]]

coeflist<-vector(mode="list",length=4)
coeflist[[1]]<-coef(dlogit_intercept)
coeflist[[2]]<-coef(dlogit_step)
coeflist[[3]]<-extractcoef(cv.dlogit_lasso)
coeflist[[4]]<-extractcoef(cv.dlogit_lasso_limited)

coefmerger <-function(listofcoef){
  n<-length(listofcoef)
  res<-data.frame("Coef"=factor())
  for(i in 1:n){
    temp<-data.frame("Coef"=names(listofcoef[[i]]),unname(listofcoef[[i]]))
    res<-full_join(res,temp,by="Coef")
  }
  return(res)
}
# The warnings are proof that it works

coefmat<-coefmerger(coeflist)
colnames(coefmat)[-1]<-c("Intercept","Stepwise","Lasso","Reduced Lasso")
rownames(coefmat)<-coefmat[,1]
coefmat[]<-round(coefmat[,-1],4)

saveRDS(coefmat,file="coefficients.rds")

resMeasures<-data.table("Model"=c("Intercept","Stepwise","Lasso","Reduced Lasso"),
                        "Accuracy"=as.numeric(lapply(confusion_mats,getVfromLiL,"overall","Accuracy")),
                        "Acc p-Val"=as.numeric(lapply(confusion_mats,getVfromLiL,"overall","AccuracyPValue")),
                        "Sensitivity"=as.numeric(lapply(confusion_mats,getVfromLiL,"byClass","Sensitivity")),
                        "Specificity"=as.numeric(lapply(confusion_mats,getVfromLiL,"byClass","Specificity")),
                        "AIC"=c(AIC(dlogit_intercept),
                                AIC(dlogit_step),
                                AIChack(cv.dlogit_lasso,ResDat$Lasso,as.numeric(NNA_Data$deaths)),
                                AIChack(cv.dlogit_lasso_limited,ResDat$Limited,as.numeric(NNA_Data$deaths))))
resMeasures[,Model := as.factor(Model)]
dispRes<-resMeasures
dispRes[,c("Accuracy","Acc p-Val","Sensitivity","Specificity") := round(resMeasures[,-1],4)]


# Creating and outputing charts/figures for report
heatmap(cormat,Rowv= NA,Colv = "Rowv", symm = TRUE, main = "Heatmap of correlation for numerical predictors")

AICvSens<-ggplot(data=dispRes,aes(x=AIC,y=Sensitivity,color=Model))+
  geom_point()+
  geom_text(aes(label=Model),vjust=1.5)

ggsave("AICvSens.png",plot = AICvSens)

plot(cv.dlogit_lasso$glmnet.fit,"lambda",main="Coefficient Shrinkage")
abline(v=log(cv.dlogit_lasso$lambda.min))
abline(v=log(cv.dlogit_lasso$lambda.1se))

## to-do (improve datahandling)
