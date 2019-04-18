# Logit models

# Libraries
library(boot)

## For reproducibility, check without
set.seed(1569787)

## Loading data

Raw_Data<-fread("MIDA_4.2_Cleaned.csv")

## Problem 1 the excessive NAs

colSums(is.na(Raw_Data))

## Some quick pruning, probably should do more intenstive checks to determin what to remove
NNA_Data<-na.omit(Raw_Data[,c(4:5,7:9,13:19,23,27)])

## First crude logit model, mostly for comparison purposes
death_logit_fc1<-glm(deaths~.,data = NNA_Data, family = "binomial")
cv.death_logit_fc1<-cv.glm(NNA_Data,death_logit_fc1, K=10)
