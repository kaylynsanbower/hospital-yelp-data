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


# Clean Yelp Hospital Data (i.e., the hospital profile information) -------

# import 
df <- read.csv(file="data/input/Yelp_Reviews_Hosp.csv", header=T, na.strings=c(""," ","NA")) %>% 
  setnames(old=c("review_count"), new=c("rc")) %>% 
  filter(rc>0) %>% # only keep hospitals that have at least one review 
  filter(complete.cases(MLOCADDR_yelp)) # keep hospitals with address information


# Modify Yelp addresses to match the conventions used in the AHA data

# Directional words
df$MLOCADDR_yelp <- str_replace_all(df$MLOCADDR_yelp, " E ", " East ")
df$MLOCADDR_yelp <- str_replace_all(df$MLOCADDR_yelp, " W ", " West ")
df$MLOCADDR_yelp <- str_replace_all(df$MLOCADDR_yelp, " N ", " North ")
df$MLOCADDR_yelp <- str_replace_all(df$MLOCADDR_yelp, " S ", " South ")

# Street abbreviations
df$MLOCADDR_yelp <- str_replace_all(df$MLOCADDR_yelp, " St ", " Street ")
df$MLOCADDR_yelp <- str_replace_all(df$MLOCADDR_yelp, " Ave ", " Avenue ")
df$MLOCADDR_yelp <- str_replace_all(df$MLOCADDR_yelp, " Av ", " Avenue ")
df$MLOCADDR_yelp <- str_replace_all(df$MLOCADDR_yelp, " Dr ", " Drive ")
df$MLOCADDR_yelp <- str_replace_all(df$MLOCADDR_yelp, " Blvd ", " Boulevard ")
df$MLOCADDR_yelp <- str_replace_all(df$MLOCADDR_yelp, " Ln ", " Lane ")
df$MLOCADDR_yelp <- str_replace_all(df$MLOCADDR_yelp, " Rd ", " Road ")
df$MLOCADDR_yelp <- str_replace_all(df$MLOCADDR_yelp, " Plz ", " Plaza ")
df$MLOCADDR_yelp <- str_replace_all(df$MLOCADDR_yelp, " Sq ", " Square ")
df$MLOCADDR_yelp <- str_replace_all(df$MLOCADDR_yelp, " Pl ", " Place ")
df$MLOCADDR_yelp <- str_replace_all(df$MLOCADDR_yelp, " Frwy ", " Freeway ")
df$MLOCADDR_yelp <- str_replace_all(df$MLOCADDR_yelp, " Fwy ", " Freeway ")
df$MLOCADDR_yelp <- str_replace_all(df$MLOCADDR_yelp, " Cir ", " Circle ")
df$MLOCADDR_yelp <- str_replace_all(df$MLOCADDR_yelp, " Pkwy ", " Parkway ")
df$MLOCADDR_yelp <- str_replace_all(df$MLOCADDR_yelp, " Hwy ", " Highway ")
df$MLOCADDR_yelp <- str_replace_all(df$MLOCADDR_yelp, " Rte ", " Route ")
df$MLOCADDR_yelp <- str_replace_all(df$MLOCADDR_yelp, " Rt ", " Route ")
df$MLOCADDR_yelp <- str_replace_all(df$MLOCADDR_yelp, " Expy ", " Expressway ")
df$MLOCADDR_yelp <- str_replace_all(df$MLOCADDR_yelp, "&amp;#39;","'")
df$MLOCADDR_yelp <- str_replace_all(df$MLOCADDR_yelp, "&amp;amp;;","")
df$MLOCADDR <- str_replace_all(df$MLOCADDR, "-", " ")
df$MLOCADDR <- str_replace_all(df$MLOCADDR, " U S ", " US ")


# Shortened numbers
df$MLOCADDR_yelp <- str_replace_all(df$MLOCADDR_yelp, " 1st ", " First ")
df$MLOCADDR_yelp <- str_replace_all(df$MLOCADDR_yelp, " 2nd ", " Second ")
df$MLOCADDR_yelp <- str_replace_all(df$MLOCADDR_yelp, " 3rd ", " Third ")
df$MLOCADDR_yelp <- str_replace_all(df$MLOCADDR_yelp, " 4th ", " Fourth ")
df$MLOCADDR_yelp <- str_replace_all(df$MLOCADDR_yelp, " 5th ", " Fifth ")
df$MLOCADDR_yelp <- str_replace_all(df$MLOCADDR_yelp, " 6th ", " Sixth ")
df$MLOCADDR_yelp <- str_replace_all(df$MLOCADDR_yelp, " 7th ", " Seventh ")
df$MLOCADDR_yelp <- str_replace_all(df$MLOCADDR_yelp, " 8th ", " Eigth ")
df$MLOCADDR_yelp <- str_replace_all(df$MLOCADDR_yelp, " 9th ", " Ninth ")
df$MLOCADDR_yelp <- str_replace_all(df$MLOCADDR_yelp, " 10th ", " Tenth ")

# numbers in digit v word format
df$MLOCADDR_yelp <- str_replace_all(df$MLOCADDR_yelp, "One ", "1 ")
df$MLOCADDR <- str_replace_all(df$MLOCADDR, "One ", "1 ")




# Compare addresses -------------------------------------------------------
# Create new variables to compare different aspects of the AHA v Yelp address 

# Count number of profiles per hospital ID  
df <- df %>% group_by(ID) %>% mutate(profiles=seq(n()))

# Extract street number, or the first word/number of the string
df$st_num <- gsub("([0-9A-Za-z]+).*", "\\1", df$MLOCADDR)
df$st_num_yelp <- gsub("([0-9A-Za-z]+).*", "\\1", df$MLOCADDR_yelp)

# Extract street name, or the second word of the string
df$st_name <- word(df$MLOCADDR, 2)
df$st_name_yelp <- word(df$MLOCADDR_yelp, 2)

# Extract third word of the string, which is often times "Road" or "Drive", etc. 
df$st_suffix <- word(df$MLOCADDR, 3)
df$st_suffix_yelp <- word(df$MLOCADDR_yelp, 3)

# trim whitespace from the yelp addresses
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
df$MLOCADDR_yelp <- trim(df$MLOCADDR_yelp)



# Create dataset of matched hospitals -------------------------------------

# This section uses the variables created above to compare the AHA and Yelp addresses for each of the observations. 
# Based on the criteria defined below, observations where the addresses match are kept in df_fs. 
# Observations that we can't match in this way we review manually.


## --- Exact Matches --- ##
# sending exact matched addresses to a new dataset 
df_fs <- df[which(df$MLOCADDR == df$MLOCADDR_yelp),]

# retains the inexact matches for further analysis
df <- df[which(df$MLOCADDR != df$MLOCADDR_yelp),]


## --- Match by first 3 words --- ##
# adds hospitals with almost exact address matches to the df_fs frame
df_add <- df[which(df$st_num == df$st_num_yelp & 
                     df$st_name == df$st_name_yelp & 
                     df$st_suffix==df$st_suffix_yelp),]
df_fs <- rbind(df_add, df_fs)

# drops hospitals with almost exact address matches from df frame
df <-  df[c(-which(df$st_num == df$st_num_yelp & 
                     df$st_name == df$st_name_yelp & 
                     df$st_suffix==df$st_suffix_yelp)),]


## --- Match Street Number, and second OR third word match --- ##
# add hospitals to df_fs
df_add <- df[which(df$st_num == df$st_num_yelp & 
                     (df$st_name == df$st_name_yelp | df$st_suffix==df$st_suffix_yelp)),]
df_fs <- rbind(df_add, df_fs)

# delete hospitals from df
df <- df[c(-which(df$st_num == df$st_num_yelp & 
                    (df$st_name == df$st_name_yelp | df$st_suffix==df$st_suffix_yelp))),]


## --- Matching street number and either the 2nd/3rd for AHA match either the 2nd/3rd word for Yelp --- ##
# add hospitals
df_add <- df[which(df$st_num == df$st_num_yelp & (df$st_name == df$st_name_yelp | 
                                                    df$st_suffix==df$st_suffix_yelp |
                                                    df$st_name == df$st_suffix_yelp |
                                                    df $ st_suffix == df$st_name_yelp)),]
df_fs <- rbind(df_add, df_fs)

# drop from df
df <- df[c(-which(df$st_num == df$st_num_yelp & (df$st_name == df$st_name_yelp | 
                                                   df$st_suffix==df$st_suffix_yelp |
                                                   df$st_name == df$st_suffix_yelp |
                                                   df$st_suffix == df$st_name_yelp))),]


## --- Drop observations that might be other offices, but keep them for further review --- ##
df_offices <- df[which(df$st_name == df$st_name_yelp & df$st_suffix==df$st_suffix_yelp),]
df <- df[c(-which(df$st_name == df$st_name_yelp & df$st_suffix==df$st_suffix_yelp)),]

### --- Starting a dataframe for observations that need to be manually sorted --- ###
## Observations that only match by street number or have other characteristics that the current process isn't catching ##
df_manual <- df[which(df$st_num == df$st_num_yelp),]
df_manual1 <- df[which(df$st_suffix == "&"),]
df_manual2 <- df[which(df$st_suffix_yelp =="And"),]

df_manual <- rbind(df_manual, df_manual1, df_manual2)
rm(df_manual1, df_manual2)

# drop observations
df <- df[c(-which(df$st_num == df$st_num_yelp)),]
df <- df[c(-which(df$st_suffix == "&")),]
df <- df[c(-which(df$st_suffix_yelp =="And")),]


## --- Cases where the AHA data does not have a street number, but the street names match --- ##
# add hospitals 
df_add <- df[which(df$st_num == df$st_name_yelp ),]
df_fs <- rbind(df_add, df_fs)

# drop observations
df <- df[c(-which(df$st_num == df$st_name_yelp )),]


## --- Cases where none of the 3 variables we have match --- ## 
df_drop <- df[which(df$st_num != df$st_num_yelp &
                      df$st_num != df$st_name_yelp &
                      df$st_num != df$st_suffix_yelp & 
                      df$st_name != df$st_name_yelp & 
                      df$st_name != df$st_num_yelp & 
                      df$st_suffix!=df$st_suffix_yelp &
                      df$st_name != df$st_suffix_yelp &
                      df$st_suffix != df$st_num_yelp &
                      df$st_suffix != df$st_name_yelp),]

# drop from remaining dataset
df <- df[c(-which(df$st_num != df$st_num_yelp &
                 df$st_num != df$st_name_yelp &
                 df$st_num != df$st_suffix_yelp & 
                 df$st_name != df$st_name_yelp & 
                 df$st_name != df$st_num_yelp & 
                 df$st_suffix!=df$st_suffix_yelp &
                 df$st_name != df$st_suffix_yelp &
                 df$st_suffix != df$st_num_yelp &
                 df$st_suffix != df$st_name_yelp)),]

### Combine all remaining observations to the "df_manual" dataframe ### 
df_manual <- rbind(df_manual, df)
rm(df_add, df_drop, df)

### Remove the unnecessary variables from the df_fs database
df_fs <- select(df_fs, -c(st_name, st_name_yelp, st_num, st_num_yelp, st_suffix_yelp, st_suffix))


# Export
write.csv(df_manual, file = "data/intermediate/YelpReview_HospitalLevel-ManualRevise.csv")
write.csv(df_offices, file = "data/intermediate/YelpReview_HospitalLevel-OfficeRevise.csv")
write.csv(df_fs, file = "data/intermediate/YelpReview_HospitalLevel-CLEAN.csv")