# Logit models

# Libraries
library(boot)
library(glmnet)
library(ggplot2)
library(caret)
library(e1071)
library(data.table)

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

# Comparison of results
## Can be made more efficient later but not really an issue, Might be able to add parallel

ResDat<-data.frame("Intercept"= predict(dlogit_intercept,type = "response"),
                   "Step"= predict(dlogit_step,type = "response"),
                   "Lasso"= unname(predict(cv.dlogit_lasso,newx = x.mm,s="lambda.1se",type="response")))


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
                            as.factor(trueDat))
  }
  return(res)
}

confusion_mats<-confAll(ResDat,NNA_Data$deaths)

getVfromLiL<-function(x,l1,l2) x[[l1]][l2]

resMeasures<-data.table("Accuracy"=lapply(confusion_mats,getVfromLiL,"overall","Accuracy"),
                        "Acc p-Val"=lapply(confusion_mats,getVfromLiL,"overall","AccuracyPValue"),
                        "Sensitivity"=lapply(confusion_mats,getVfromLiL,"byClass","Sensitivity"),
                        "Specificity"=lapply(confusion_mats,getVfromLiL,"byClass","Specificity")) # need to clean up w/ rounding

# Creating and outputing charts/figures for report
heatmap(cormat,Rowv= NA,Colv = "Rowv", symm = TRUE, main = "Heatmap of correlation for numerical predictors")


## to-do (improve datahandling)
