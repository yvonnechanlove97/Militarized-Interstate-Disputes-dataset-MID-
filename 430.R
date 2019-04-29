if(!require('pacman')) install.packages('pacman')
pacman::p_load(
  'dplyr',
  'tidyr',
  'zoo',
  'data.table',
  'caret',
  'ROCR'
)


#drop dispnum4,revtype2 because they contain too many missing values
#drop fatalpre because it contians missing values and highly correlated to fatality 
#drop the last 4 variables which is unrelated
MID_Actor=data %>% select(-c('dispnum4','revtype2',"fatalpre", "version", "changes_1", "changes_2", "changes_3"))

#record the exact dates in case
dates=MID_Actor %>% select(stday:endyear)


#let the missing start day and end day be the first day of the month 
MID_Actor$stday[MID_Actor$stday==-9]=1
MID_Actor$stday[MID_Actor$endday==-9]=1

#calculate the lasting days of the dispute
MID_Actor=tidyr::unite(MID_Actor,'startdate',c("styear","stmon","stday"),sep='-')
MID_Actor=tidyr::unite(MID_Actor,'enddate',c("endyear","endmon","endday"),sep='-')
MID_Actor=MID_Actor %>% 
  mutate_at(c('startdate','enddate'),function(x){as.Date(x)}) %>%
  mutate(last_days=enddate-startdate)
MID_Actor$last_days[MID_Actor$last_days==0]=1
MID_Actor$last_days=as.numeric(MID_Actor$last_days)

#predict 
fwrite(MID_Actor,file="MIDB_4.2_Cleaned.csv")

#let fatality==-9 be NA
#fatality>0=1
MID_Actor$fatality[MID_Actor$fatality==-9]=NA
MID_Actor$fatality[MID_Actor$fatality>0]=1
#deAL with nas
MID_Actor=MID_Actor %>% drop_na()
library(caret)

#split
set.seed(69)
testindex=sample(dim(MID_Actor)[1],dim(MID_Actor)[1]*0.3,replace = F)
train=MID_Actor[-testindex,]
test=MID_Actor[testindex,]
train=train %>% select(-c("dispnum3","stabb","ccode", "startdate","enddate"))
test=test %>% select(-c("dispnum3","stabb","ccode", "startdate","enddate"))

#logistics
model_n=glm(train$fatality~.,data=train,binomial(link='logit'))
n_y_hat1=predict(model_n,type = "response",newdata =test)


install.packages("pROC")
library(pROC)
r3=roc(predictor=n_y_hat1,response=test$fatality)
auc(r3)
#Find t that minimizes error
e <- cbind(r3$thresholds,r3$sensitivities+r3$specificities)
opt_t <- subset(e,e[,2]==max(e[,2]))[,1]

#Plot ROC Curve
plot(1-r3$specificities,r3$sensitivities,type="l",
     ylab="Sensitiviy",xlab="1-Specificity",col="black",lwd=2,
     main = "ROC Curve for MIB data")
abline(a=0,b=1)
abline(v = opt_t) #add optimal t to ROC curve


n_y_hat_class=ifelse(n_y_hat1>opt_t,1,0)
n_class=as.data.frame(cbind(seq(dim(test)[1]),n_y_hat_class,test$fatality))
ggplot(n_class,aes(V1,n_y_hat_class))+
  geom_point(aes(color=as.factor(V3)),shape=1)+
  labs(title ="Misclassification Plot", x = "Obs", y = "Predicted Class") +
  scale_color_discrete(name = "True class")
#calculate log-loss
logLoss = function(y, p){
  if (length(p) != length(y)){
    stop('Lengths of prediction and labels do not match.')
  }
  
  if (any(p < 0)){
    stop('Negative probability provided.')
  }
  
  p = pmax(pmin(p, 1 - 10^(-15)), 10^(-15))
  mean(ifelse(y == 1, -log(p), -log(1 - p)))
}

logLoss(test$fatality, y_hat1)
#0.2621403

#transfer to factor
cate_name=c("sidea","revstate","revtype1","hiact","hostlev","orig")
train_f=train %>% mutate_at(cate_name,function(x){as.factor(x)})
test_f=test %>% mutate_at(cate_name,function(x){as.factor(x)})
train_f$last_days=as.numeric(train_f$last_days)
test_f$last_days=as.numeric(test_f$last_days)

model_f=glm(train_f$fatality~.,data=train_f,binomial(link='logit'))
f_y_hat1=predict(model,type = "response",newdata=test_f)
logLoss(test_f$fatality, f_y_hat1)
#0.2185222

#misclassification visualization
predict(model_f,newdata =test_f)
ggplot(geom_point()



#glm for lasting days
day_model=lm(last_days~.,data=train)
#there are some outliers but overall the residuals are constant variance 
plot(day_model$residuals)
#not very normal
qqnorm(day_model$residuals)

#try log transformation
day_model2=lm(log(last_days)~.,data=train)
#homogeneous
plot(day_model2$residuals)
#normal distributed
qqnorm(day_model2$residuals)

day_yhat_log=predict(day_model2,newdata=test)
#rmse is 0.0491256 which is very good
rmse=sqrt(mean(day_yhat_log - log(test$last_days))^2)

#categorical variables association: frequency table

