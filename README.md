# hospital-yelp-data

This repository contains the code to process a dataset of web scraped Yelp reviews.

## Raw Data 
The raw data used in these code files is pulled from Yelp using the code detailed in this [Jupiter Notebook] (link to pablo's notebook.)

## Code Files
The master code file (0-hospital-yelp-data.R) runs all of the subsequent raw code files (files number 1 through 6). The raw code files are written such that they can each be run independently, but one can rely on the master code file (0-hospital-yelp-data.R) to take the data from raw to final form. 

Collectively, the code files complete the following steps.
1.   **Match Yelp hospitals to those listed in AHA data** -- Hospital information from the AHA annual survey data was used to pull the hospital profiles and reviews on Yelp.com. However, to ensure that the profi

## Raw Code Files 
- In code file 4, we drop any hospitals that have more than one matched Yelp profile. This eliminates a total of 44 hospitals. 

