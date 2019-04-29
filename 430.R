if(!require('pacman')) install.packages('pacman')
pacman::p_load(
  'dplyr',
  'tidyr',
  'zoo',
  'data.table',
  'caret',
  'pROC'
)

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

#AUC
r3=roc(predictor=n_y_hat1,response=test$fatality)
auc(r3)
#Find t that minimizes error
e <- cbind(r3$thresholds,r3$sensitivities+r3$specificities)
opt_t <- subset(e,e[,2]==max(e[,2]))[,1]

#Plot ROC Curve
plot(1-r3$specificities,r3$sensitivities,type="l",
     ylab="Sensitiviy",xlab="1-Specificity",col="black",lwd=2,
     main = "ROC Curve for MIB fatality")
abline(a=0,b=1)
abline(v = opt_t) #add optimal t to ROC curve

#misclassification visualization
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

#transfer to factor recive a lower log-loss
cate_name=c("sidea","revstate","revtype1","hiact","hostlev","orig")
train_f=train %>% mutate_at(cate_name,function(x){as.factor(x)})
test_f=test %>% mutate_at(cate_name,function(x){as.factor(x)})
train_f$last_days=as.numeric(train_f$last_days)
test_f$last_days=as.numeric(test_f$last_days)

model_f=glm(train_f$fatality~.,data=train_f,binomial(link='logit'))
f_y_hat1=predict(model_f,type = "response",newdata=test_f)
logLoss(test_f$fatality, f_y_hat1)
#0.2185222

#AUC:0.9421
r4=roc(predictor=f_y_hat1,response=test_f$fatality)
auc(r4)
#Find t that minimizes error
f <- cbind(r4$thresholds,r4$sensitivities+r4$specificities)
opt_t_f <- subset(f,f[,2]==max(f[,2]))[,1]

#Plot ROC Curve
plot(1-r4$specificities,r4$sensitivities,type="l",
     ylab="Sensitiviy",xlab="1-Specificity",col="black",lwd=2,
     main = "ROC Curve for MIB fatality with factors")
abline(a=0,b=1)
abline(v = opt_t_f) 

#misclassification visualization
f_y_hat_class=ifelse(f_y_hat1>opt_t_f,1,0)
f_class=as.data.frame(cbind(seq(dim(test_f)[1]),f_y_hat_class,test_f$fatality))
ggplot(f_class,aes(V1,f_y_hat_class))+
  geom_point(aes(color=as.factor(V3)),shape=1)+
  labs(title ="Misclassification Plot", x = "Obs", y = "Predicted Class") +
  scale_color_discrete(name = "True class")
#we can see for class 0, the accurate is pretty high,but for class 1 misclassification rate gets higher


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

