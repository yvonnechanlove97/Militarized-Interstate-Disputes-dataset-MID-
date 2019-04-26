
#MIDB describes the participants in each of those disputes.
if(!require('pacman')) install.packages('pacman')
pacman::p_load(
  'dplyr',
  'tidyr',
  'zoo',
  'data.table'
)


#drop dispnum4,revtype2 because they contain too many missing values
#drop fatalpre because it contians missing values and highly correlated to fatality 
#drop the last 4 variables which is unrelated
MID_Actor=MID_Actor %>% select(-c('dispnum4','revtype2',"fatalpre", "version", "changes_1", "changes_2", "changes_3"))

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

#does hostlev has anything to do with fatality level
fwrite(MID_Actor,file="MIDB_4.2_Cleaned.csv")
