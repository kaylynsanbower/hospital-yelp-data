# clear
rm(list=ls())

# Load Packages -----------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, dplyr, lubridate) 

# Import Data -------------------------------------------------------------

hosp <- read.csv(file="data/intermediate/yelp-matched-hospitals.csv", header=T) %>% subset(select=-c(X))
ind_reviews <- read.csv(file="data/input/Yelp-Reviews_Ind.csv", header=T)

# Keep relevant variables
# identify hospitals that were found on Yelp
hosp <- hosp %>% subset(select= c('ID','MCRNUM'))

# only keep relevant variables from the individual reviews 
ind_reviews <- ind_reviews %>% subset(select= c('ID','MCRNUM','rating_date','rating_ind', 'user_id', "cool", 'funny', 'useful', 'review'))

# Only keep Yelp reviews from hospitals in the "approved" hospitals list 
ind <- ind_reviews[FALSE,]
for (i in hosp$ID){
  keep <-ind_reviews[which(ind_reviews$ID== i),]
  ind <- bind_rows(ind, keep)
}
rm(keep)

# Format and Rename Variables ---------------------------------------------
# rename rating and date variables 
ind <- ind %>% rename("date"=rating_date,
                      "rating"=rating_ind,
                      "user"= user_id)

# change date to date format and create year variable 
ind$date <- as.Date(ind$date, '%m/%d/%Y')
ind$year <-format(ind$date, '%Y')

# Create Running Variable and Observed Var  -------------------------------
# recode hospital identifiers as factors 
ind[,c('ID','MCRNUM')] <- lapply(ind[,c('ID','MCRNUM')] , factor)

# arrange by date and drop missing MCRNUM
ind <- dplyr::arrange(ind, date) %>% drop_na(MCRNUM)

# create running variable 
ind <- ind %>% group_by(MCRNUM) %>% 
  mutate(rv_last = (cumsum(rating)-rating)/(seq_along(rating)-1),
         rv_now = (cumsum(rating))/(seq_along(rating)))

ind <- ind %>% group_by(MCRNUM) %>% 
  mutate(count_last = seq_along(rating)-1,
         count_now = seq_along(rating))

# replace the NaN values for cumulative average with zeros
ind$rv_last[is.nan(ind$rv_last)]<-0
ind$rv_now[is.nan(ind$rv_now)]<-0

ind <- ind %>% mutate(first = ifelse(rv_last==0, 1,0))

# create variable for difference between last review and the running variable
ind <- ind %>% mutate(rev_minus_rv = rating - rv_now)


# create indicators to show whether or not a review caused that hospital to be rounded up
# define constants 
cutoff <- c(1.25, 1.75, 2.25, 2.75, 3.25, 3.75, 4.25, 4.75)
stars <- c(1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5)
bw = 0.1

# create observed ratings and variables for the cutoffs; baseline values 
ind <- ind %>% mutate(observed=1,cut=1.25) 

# loop through to update the variables 
for (i in cutoff) {
  ind <- ind %>% mutate(observed = ifelse(rv_now>=i & rv_now<=(i+0.25), (i+0.25), observed),
                                          cut = ifelse(rv_now>=i & rv_now<=(i+0.25), i, cut))
}

for (i in stars) {
  ind <- ind %>% mutate(observed = ifelse(rv_now>=i & rv_now<(i+0.25), i, observed),
                                          cut = ifelse(rv_now>i & rv_now<(i+0.25), i + 0.25, cut))
}

# create indicator for rounded 
ind <- ind %>% mutate(rounded = ifelse(rv_now>=cut & rv_now<= cut + bw, 1,0))

# create a subset of the variables to export 
indout <- ind %>% subset(select=c(MCRNUM, date, rating, user, year, rv_last, rv_now, rev_minus_rv, first, count_last, count_now, rounded))
write.csv(ind, file = "data/output/Yelp-Reviews-for-choice.csv")





