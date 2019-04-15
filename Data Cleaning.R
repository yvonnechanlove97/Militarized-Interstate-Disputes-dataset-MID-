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

## Coding factor variables properly