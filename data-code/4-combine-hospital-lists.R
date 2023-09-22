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


# combine all datasets ----------------------------------------------------


# import 
em <- read.csv(file="data/intermediate/YelpReview_HospitalLevel-CLEAN.csv", header=T, na.strings=c(""," ","NA"))
em_mr <- read.csv(file="data/intermediate/YelpReview_HospitalLevel-ManualRevise-CLEAN.csv", header=T, na.strings=c(""," ","NA"))
em_or <- read.csv(file="data/intermediate/YelpReview_HospitalLevel-OfficeRevise-CLEAN.csv", header=T, na.strings=c(""," ","NA"))

em <- select(em, -c(X))
em_mr <- select(em_mr, -c(X))
em_or <- select(em_or, -c(X))


# bind all datasets -------------------------------------------------------

# Indicators for which dataset the observation originated from
em$origin <- 1
em_mr$origin <-2
em_or$origin <-3

# Combining datasets
em_1 <- rbind(em_mr, em_or)
em_2 <- rbind(em_1, em)
rm(em_mr, em_or,em, em_1)
em <- em_2
rm(em_2)

# Variable for hospitals with more than 1 Exact match
em <- em %>% group_by(ID) %>% mutate(num_EM=n())

# Dropping any duplicates and eliminating variables 
em <- select(em, -c(profiles))
em <- distinct(em)

# Reviewing the hospitals that have multiple Yelp profiles from the exact match. 
em_mult <- em[which(em$num_EM>1),]
em <- em[c(-which(em$num_EM>1)),]

##### Parsing out the correct matches ##### 
# Keeping exact match addresses
em_add <- em_mult[c(which(as.character(em_mult$MLOCADDR)== as.character(em_mult$MLOCADDR_yelp))),]
em_mult <- em_mult[c(-which(as.character(em_mult$MLOCADDR)== as.character(em_mult$MLOCADDR_yelp))),]
em <- rbind(em, em_add)
rm(em_add)

# After reviewing the remaining 50 observations, it's clear that each observation with the same 
# numbers associated with the address are correct. So this part keeps the observations where the
# street number is correct.
# Making variables to move observations based on only the street number. 
em_mult$st_num <- gsub("([0-9A-Za-z]+).*", "\\1", em_mult$MLOCADDR)
em_mult$st_num_yelp <- gsub("([0-9A-Za-z]+).*", "\\1", em_mult$MLOCADDR_yelp)
# Moving observations based on street number
em_add <- em_mult[c(which(as.character(em_mult$st_num) == as.character(em_mult$st_num_yelp))),]
em_mult <- em_mult[c(-which(as.character(em_mult$st_num)== as.character(em_mult$st_num_yelp))),]
em_add <- select(em_add, -c(st_num, st_num_yelp))
em_mult <- select(em_mult, -c(st_num, st_num_yelp))
em <- rbind(em, em_add)
rm(em_add)


# Manually reviewing the remaining 26 hospitals.
hURL = c('https://www.yelp.com/biz/st-barnabas-hospital-bronx-6', 
         'https://www.yelp.com/biz/blessing-hospital-quincy-2',
         'https://www.yelp.com/biz/heart-of-florida-regional-medical-center-haines-city',
         'https://www.yelp.com/biz/northwest-community-hospital-arlington-heights-4')

# Create empty dataframe
em_addTot <- em_mult[FALSE,]

for (i in hURL){
  em_add <-em_mult[which(em_mult$url == i),]
  em_addTot <- rbind (em_add, em_addTot)
}

em <- rbind(em_addTot, em)

# Dropping unnecessary datasets and variables
rm(em_add, em_mult, em_addTot)
em <- select(em, -c(num_EM))


# limiting to hospitals that only have one profile matched ----------------

em <- em %>% group_by(ID) %>% mutate(num_EM=n()) %>% filter(num_EM==1)
em <- subset(em, select = -c(MLOCADDR_yelp, MNAME_yelp, num_EM, origin))



# export 
write.csv(em, file = "data/intermediate/yelp-matched-hospitals.csv")

