# This document contains all the universal data cleaning done so there is a record of what changes have been made.

library(data.table)

## Loading in data
MID_Dispute<-fread("MIDA_4.2.csv")
MID_Actor<-fread("MIDB_4.2.csv")

#MIDA

# Cleaning data
## Recoding NA values
### There has to be a better way of doing this

MID_Dispute[dispnum4==-9,dispnum4 := NA]
MID_Dispute[stday==-9,stday := NA]
MID_Dispute[endday==-9,endday := NA]
MID_Dispute[outcome==-9,outcome := NA]
MID_Dispute[settle==-9,settle := NA]
MID_Dispute[fatality==-9,fatality := NA]
MID_Dispute[fatalpre==-9,fatalpre := NA]
MID_Dispute[hiact==-9,hiact := NA]

# Coding factor variables properly

MID_Dispute[, outcome := factor(outcome, labels = c("A victory", "B victory","A yield","B yield","Stalemate","Compromise","Released","Unclear","Joins ongoing war"))]
MID_Dispute[, settle := factor(settle, labels = c("Negotiated","Imposed","None","Unclear"))]
MID_Dispute[, fatality := factor(fatality, ordered = TRUE)]
MID_Dispute[, hiact := factor(hiact, ordered = TRUE)] # Add labels later
MID_Dispute[, hostlev := factor(hostlev, ordered = TRUE)] # Add labels later
MID_Dispute[, recip := as.logical(recip)]
MID_Dispute[, ongo2010 := as.logical(ongo2010)]

# Little bit of spare code that shows that for all values fatality is NA fatalpre is NA
sum(is.na(MID_Dispute[is.na(fatality)==TRUE,fatalpre])==FALSE)

# Create new logical variable that measures if there were any deaths

MID_Dispute[, deaths := as.logical(fatality != 0)]

# Write results

saveRDS(MID_Dispute,file="MIDA_4.2_Cleaned.rds")


#MIDB
if(!require('pacman')) install.packages('pacman')
pacman::p_load(
  'dplyr',
  'tidyr',
  'zoo',
  'data.table'
)

#Drop dispnum4,revtype2 because they contain too many missing values
#Drop fatalpre because it contians missing values and highly correlated to fatality 
#Drop the last 4 variables which is unrelated

MID_Actor=MID_Actor %>% select(-c('dispnum4','revtype2',"fatalpre", "version", "changes_1", "changes_2", "changes_3"))

#Record the exact dates in case
dates=MID_Actor %>% select(stday:endyear)


#Let the missing start day and end day be the first day of the month 
MID_Actor$stday[MID_Actor$stday==-9]=1
MID_Actor$stday[MID_Actor$endday==-9]=1

#Calculate the lasting days of the dispute
MID_Actor=tidyr::unite(MID_Actor,'startdate',c("styear","stmon","stday"),sep='-')
MID_Actor=tidyr::unite(MID_Actor,'enddate',c("endyear","endmon","endday"),sep='-')
MID_Actor=MID_Actor %>% 
  mutate_at(c('startdate','enddate'),function(x){as.Date(x)}) %>%
  mutate(last_days=enddate-startdate)
MID_Actor$last_days[MID_Actor$last_days==0]=1

#let fatality==-9 be NA
#fatality>0=1
MID_Actor$fatality[MID_Actor$fatality==-9]=NA
MID_Actor$fatality[MID_Actor$fatality>0]=1
#Deal with nas
MID_Actor=MID_Actor %>% drop_na()

# Write results

saveRDS(MID_Actor,file="MIDB_4.2_Cleaned.rds")
