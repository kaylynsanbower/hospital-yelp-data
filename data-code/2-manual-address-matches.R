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


# import 
df <- read.csv(file="data/intermediate/YelpReview_HospitalLevel-ManualRevise.csv", header=T, na.strings=c(""," ","NA"))

# empty dataframe for saving matches
df_fs <- data.frame()

### Obs. where the spelling is incorrect/erroneous spacing ###
list = c('6230470', '6670460', '6742801', '6040002', '6540315', '6390253',
         '6390160', '6730051', '6370021', '6630065', '6950305', '6390489',
         '6850012', '6521210', '6520485', '6940115', '6740397', '6670050',
         '6390691', '6933915', '6210034', '6220070', '6211980', '6310011',
         '6870120', '6860570', '6740455', '6870011', '6231890', '6212925', 
         '6933505', '6210665', '6040054', '6380650', '6510042', '6860034',
         '6530025' )

for (i in list){
  df_add <- df[which(df$ID == i ),]
  df_fs <- rbind(df_add, df_fs)
} 
rm(df_add)

# drop the observations from the other data frame
for (i in list){
  df <- df[c(-which(df$ID == i )),]
} 

### Obs. for different street names but same street numbers ### 
### May be the case that the road is named differently - checked these with google maps ###
df_numb <- df[which(as.character(df$st_num) ==  as.character(df$st_num_yelp)),]
df <- df[c(-which(as.character(df$st_num) == as.character(df$st_num_yelp))),]

### The AHA address listed did not match to the website associated with the hospital for 
### hospital ID 6230456	and 6521060. The Yelp address listed was correct, according to the
### hospital's own website. 

# There was only one hospital that should not be included; dropped here
df_numb <- df_numb[which(df_numb$ID != '6211680'),]

# Combining the remaining observations in the numbers df to the data frame to save observations
df_fs <- rbind(df_numb, df_fs)
rm(df_numb)

##### Breaking the data subsets out by number of reviews #####
### manually review profiles with more than 10 reviews (more likely to be correctly matched) ###
df_10 <- df[which(df$rc >10),]
df <- df[c(-which(df$rc >10)),]

# List of hospitals to drop
list = c('6931392', '6390846', '6931570', '6932585', '6930068')

#drop these hospitals from df_10
for (i in list){
  df_10 <-df_10[which(df_10$ID!= i),]
}

# adding df_10 to the data to keep, then dropping df_10
df_fs <- rbind(df_10, df_fs)
rm(df_10)


### manually review profiles with more than 5 reviews ###
df_5 <- df[which(df$rc >5),]
df <- df[c(-which(df$rc >5)),]

# List of hospitals to drop
list = c('6911240', '6320180', '6390670', '6390680', '6340490')

#drop these hospitals from df_5
for (i in list){
  df_5 <-df_5[which(df_5$ID!= i),]
}

# adding df_5 to the data to keep, then dropping df_5
df_fs <- rbind(df_5, df_fs)
rm(df_5)


### manually review profiles where its the first yelp profile listed ###
### this means its likely that there isn't another profile already in the other data set ###
df_prof <- df[which(df$profiles == 1),]
df <- df[c(-which(df$profiles ==1)),]

# List of hospitals to drop
# Drop all hospitals whose Yelp profile addresses do not coincide with the AHA address. 
list = c('6233320', '6430870', '6340300','6710320','6932310', '6451580','6110330',
         '6520665', '6670780', '6840018', '6420740', '6214560', '6931360', '6931350',
         '6233100', '6731283', '6930041', '6840002', '6210545', '6120060', '6710510',
         '6720028', '6140430', '6441795', '6141020')

#drop these hospitals from df_prof
for (i in list){
  df_prof <-df_prof[which(df_prof$ID!= i),]
}

# adding df_5 to the data to keep, then dropping df_5
df_fs <- rbind(df_prof, df_fs)
rm(df_prof)


# Manually review the remaining observations in the df. 
# Found no hospitals to keep in the remaining 65.

# dropping variables
df_fs <- select(df_fs, -c(st_name, st_name_yelp, st_num, st_num_yelp, st_suffix_yelp, st_suffix, X))


# Export to csv
write.csv(df_fs, file = "data/intermediate/YelpReview_HospitalLevel-ManualRevise-CLEAN.csv")
