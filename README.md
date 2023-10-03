# hospital-yelp-data

This repository contains the code to process a dataset of web scraped Yelp reviews.

## Raw Data 
The raw data used in these code files was scraped from Yelp. The scraping occurred in early 2019, so when generating any datasets with these Yelp data, we stop a the end of 2018.

## Code Files
The master code file (0-hospital-yelp-data.R) runs all of the subsequent raw code files (files number 1 through 5). The raw code files are written such that they can each be run independently, but one can rely on the master code file (0-hospital-yelp-data.R) to take the data from raw to final form. 

Collectively, the code files complete the following steps.
1.   **Match Yelp hospitals to those listed in AHA data** -- Hospital information from the AHA annual survey data was used to pull the hospital profiles and reviews on Yelp.com. Files 1 through 3 use a series of processes to ensure that the profiles pulled from Yelp are attributed to the correct hospital.
2.   **Combine lists of Yelp/AHA hospital matches** -- After reviewing the hospitals, file 4 combines the lists of matched hospitals and then limits the data to hospitals that only have one matching Yelp profile. There were 44 hospitals that had multiple Yelp profiles and were eliminated from the dataset.
3. **Limit review data to matched hospitals** -- File 5 uses the list of matched hospitals to eliminate reviews for hospitals that were not matched. The file also generates new variables that are needed for analysis.
