Final_Pres
========================================================
title: Predicting Deaths in Interstate Militarized Disputes
author: Yong, Koruna & Cleary
date: 
autosize: true

Introduction
========================================================

This projects looks at the militarized interstate dispute (MID) data compiled by the Correlates of War Project. 

- “Militarized interstate disputes are united historical cases of conflict in which the threat, display or use of military force short of war by one member state is explicitly directed towards the government, official representatives, official forces, property, or territory of another state. Disputes are composed of incidents that range in intensity from threats to use force to actual combat short of war” (Jones et al. 1996: 163).

Primary objective is to predict if a conflict resulted in any deaths.
- Number of challenges including need to clean data.

David's Slide
========================================================

![Histogram](Final_Pres-figure/unnamed-chunk-1-1.png)
![MIDA Corrplot](Final_Pres-figure/unnamed-chunk-2-1.png)
![MIDB Corrplot](Final_Pres-figure/unnamed-chunk-2-2.png)



Zach's logit
========================================================

- Base is a logit regression
- Used lasso to improve results
- Classic trade-off of complexity vs potency
- Lasso also introduces bias/variance

***

![A suprising result](Final_Pres-figure/AICvSens.png)

![I think it is pretty](Final_Pres-figure/shrinkplot.png)


Modeling on MIDB
========================================================
- A.Logistic model with numeric variables  
- B.Logistic model with factor variables  
- C.Linear regression model to predict lasting days of a MID

***

![MIDB](Final_Pres-figure/MIDB.png)


Conclusion
========================================================

Ultimately model outperformed expectations. Combination of better data-cleaning and lasso has produced impressive results.




|Model         | Accuracy| Acc p-Val| Sensitivity| Specificity|      AIC|
|:-------------|--------:|---------:|-----------:|-----------:|--------:|
|Intercept     |    0.792|     0.513|       0.000|       1.000| 2076.748|
|Stepwise      |    0.926|     0.000|       0.799|       0.960|  822.502|
|Lasso         |    0.925|     0.000|       0.787|       0.961|  861.132|
|Reduced Lasso |    0.834|     0.000|       0.244|       0.989| 1412.988|

Model        | AUC   | Log-loss | Misclassification rate
-------------|-------|----------|------------------------
Logi numeric |0.9249 |  0.2621  |  0.1399   
Logi factor  |0.9421 |  0.2185  |  0.0979


