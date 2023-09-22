# clear
rm(list=ls())

# Packages
library(tidyverse)
library(lubridate)
library(psych)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(data.table)
library(naniar)
library(stringi)

# manually ID yelp/AHA matches --------------------------------------------


#import 
df <- read.csv(file="data/intermediate/YelpReview_HospitalLevel-OfficeRevise.csv", header=T, na.strings=c(""," ","NA"))

#### These were sent to this dataset because they were assumed to be doctors offices.
#### Thus, each of them need to be checked individually. The following lists the observations
#### that need to be dropped. 

# Hospitals to drop 
list = c('6740192','6142390','6431030','6410020','6232390','6230013','6520670','6530116','6432137',
         '6933910','6441550','6360895','6940058','6930823','6741483','6411180','6530160','6910125',
         '6380950','6231460','6930875','6390997','6933940','6910335','6420210','6442410','6840400',
         '6214990','6720486','6370030','6860595','6630770','6932595','6932123','6120300','6360008',
         '6932304','6880071','6820200','6740900','6731220','6214735','6720418','6140780','6120155',
         '6910480','6630008')

#drop these hospitals from df
for (i in list){
  df <-df[which(df$ID!= i),]
}

# dropping variables
df <- select(df, -c(st_name, st_name_yelp, st_num, st_num_yelp, st_suffix_yelp, st_suffix, X))


# export 
write.csv(df, file = "data/intermediate/YelpReview_HospitalLevel-OfficeRevise-CLEAN.csv")
