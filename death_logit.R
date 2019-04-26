# Logit models

# Libraries
library(boot)
library(glmnet)
library(ggplot2)
library(caret)
library(e1071)

## For reproducibility, check without
set.seed(1569787)

## Loading data

Raw_Data<-fread("MIDA_4.2_Cleaned.csv")

## Problem 1 the excessive NAs

colSums(is.na(Raw_Data))

## Some quick pruning, probably should do more intenstive checks to determin what to remove
NNA_Data<-na.omit(Raw_Data[,c(4:5,7:9,13:19,23,27)])

## First crude logit model, mostly for comparison purposes
dlogit_fc1<-glm(deaths~.,data = NNA_Data, family = "binomial")
cv.dlogit_fc1<-cv.glm(NNA_Data,dlogit_fc1, K=10)

## Try simple stepwise selection first

dlogit_step<-step(dlogit_fc1,direction="both")
cv.dlogit_step<-cv.glm(NNA_Data,dlogit_step, K=10)

## Using more sophisticated approach (Lasso)
### Should probably address co-variance issues first
x.mm<-model.matrix(~.,NNA_Data[,1:13])

cv.dlogit_lasso<-cv.glmnet(x.mm,NNA_Data$deaths,family="binomial",alpha = 1,nfolds = 10)

### Look at results (Use lambda.1se as it is more conservative/favors simpler models)
lambda_hat<-cv.dlogit_lasso$lambda.1se
plot(cv.dlogit_lasso)

coef(cv.dlogit_lasso,s="lambda.1se")

# Comparison of results
## Can be made more efficient later but not really an issue, Might be able to add parallel

ResDat<-data.frame("Full"= predict(dlogit_fc1,type = "response"),
                   "Step"= predict(dlogit_step,type = "response"),
                   "Lasso"= unname(predict(cv.dlogit_lasso,newx = x.mm,s="lambda.1se",type="response")))

DevDat<-data.frame("Full"=cv.dlogit_fc1$delta[[1]],
                   "Step"=cv.dlogit_step$delta[[1]],
                   "Lasso" = cv.dlogit_lasso$cvm[match(lambda_hat,cv.dlogit_lasso$lambda)])


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
# Maybe do manually later

## Note to self get ROC curve

## What are the problems?

### Problem 1, correlated variables
#### Problem pairs (styera,endyear), (maxdur,mindur), (hostlev,hiact)

heatmap(cor(NNA_Data[,-14]),symm = TRUE)


## Problem 2 factors being treated improperly (arguably not issue for the ordered ones)

dlogit_ffact<-glm(deaths~factor(outcome)+.,data = NNA_Data, family = "binomial")
dlogit_stepfact<-step(dlogit_ffact,direction="both")


# Creating and outputing charts/figures for report

