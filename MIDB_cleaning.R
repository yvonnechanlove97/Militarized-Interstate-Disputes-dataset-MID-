#MIDB describes the participants in each of those disputes.

#get rid of the missing, lasting days, FatalPre

library(dplyr)
library(tidyr)
library(zoo)
#drop dispnum4,revtype2 because they contain too many missing values
#drop fatalpre because it contians missing values and highly correlated to fatality 
#drop the last 4 variables which is unrelated
data_clean=data %>% select(-c("stday","endday", 'dispnum4','revtype2',"fatalpre", "version", "changes_1", "changes_2", "changes_3"))

dates=data %>% select(stday:endyear)

#calculate the lasting days of the dispute
#since start and end days contain certain amount of missing value, we used the first day of each month as the measurement
data_clean=tidyr::unite(data_clean,'startdate',c("styear","stmon"),sep='-')
data_clean=tidyr::unite(data_clean,'enddate',c("endyear","endmon"),sep='-')
data_clean=data_clean %>% 
  mutate_at(c('startdate','enddate'),function(x){as.Date(as.yearmon(x))}) %>%
  select(-c("stday","endday"))
data_clean$last_days=data_clean$enddate-data_clean$startdate


#does hostlev has anything to do with fatality level
#probably change to only use the first day of a month for missing values on start and end days                     