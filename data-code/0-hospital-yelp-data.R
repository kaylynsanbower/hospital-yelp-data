# clear
rm(list=ls())


if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, dplyr, lubridate, zoo, DescTools, psych, here, janitor, cobalt, tidyr, ggplot2,
               data.table, naniar, stringi) 

# Import cleaned data via "source" ----------------------------------------

source(here("data-code/1-clean-hospital-list.R"))
source(here("data-code/2-manual-address-matches.R"))
source(here("data-code/3-manual-offices-matches.R"))
source(here("data-code/4-combine-hospital-lists.R"))
source(here("data-code/5-combine-hospitals-and-reviews.R"))



