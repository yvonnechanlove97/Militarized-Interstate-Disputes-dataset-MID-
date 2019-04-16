# This document contains all the universal data cleaning done so there is a record of what changes have been made.

library(data.table)

## Loading in data
MID_Dispute<-fread("MIDA_4.2.csv")
MID_Actor<-fread("MIDB_4.2.csv")

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
MID_Dispute[, highact := factor(highact, ordered = TRUE)] # Add labels later
MID_Dispute[, hostlev := factor(hostlev, ordered = TRUE)] # Add labels later
MID_Dispute[, recip := as.logical(recip)]

# Little bit of spare code that shows that for all values fatality is NA fatalpre is NA
sum(is.na(MID_Dispute[is.na(fatality)==TRUE,fatalpre])==FALSE)

# Create new logical variable that measures if there were any deaths

MID_Dispute[, deaths := as.logical(fatality != 0)]

# Write results

fwrite(MID_Dispute,file="MIDA_4.2_Cleaned.csv")
