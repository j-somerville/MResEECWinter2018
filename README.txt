# EEC2019
Code for Jasmine Somerville MREs Masters Thesis. Written by Georgina Halford and Jasmine Somerville 
sparrow_cage2.py and format_interactions.py were created by Alexander Flynn-Caroll and can be found here:https://github.com/aflynncarroll/CMEE17
├── sparrow_cage2.py      <-  A general use file to make interaction times from RFID data
│   ├── raw interaction data needed in csv file as one continuous column 
│   ├── complete for each date interaction data was taken for that network
│   ├── outputs sorted and overlap.csv files for each date
│   ├──sorted contains all entries into the cage and overlap contains all interactions between two individuals inside the cage 
├── format_interactions.py       <-  Formats RFID data into one data file 
│   ├── need overlap.csv files for each date data was taken within that network 
│   ├── creates one total.csv file that contains all interactions within that network 
├── sociality_scores.R    <-  Initial analysis of RFID data for gain sociality scores
│   ├── creates sociality scores from the interation data contained in total.csv
│   ├──files needed for each network are coh_sex.csv, validcolourcodes.csv, tblAllCodes.csv and total.scv
│   ├──coh_sex.csv<-contains a column of bird ID against a cloumn of sex and a column with their cohort year 
│   ├──validcolourcodes.csv<-contains a birdID column against a column with their ring colour codes 
│   ├──tblAllCodes.csv<-birdID column and a column with their transponder numbers
│   ├──creates a soc.csv file containing the sociality scores for degree, betweenness and closeness for individuals within the network as   well as background data such as cohort, age, sex and ring colour codes
│   ├──individuals that entered the cage but did not interact do not appear in the file but have sociality scores of 0
├── interaction data.R       <-  Analyses sociality scores and badge sizes for each individual per aviary, across the captive population
│   ├──uses raw data file of aviaries and indiviudals including transponders, aviary number, age, cohort+ social scores and badge size
├── SNAsimulations.R      <-  Randomised social network simulations to create null model 
│   ├──creates a distribution of correlations from randomised null models 
│   ├──needs a file containing all interactions from every occassion and aviary used in the initial anlysis, plus individuals that entered the cage but did not interact with others
│   ├──needs a file for badge data, which includes individual's transponder and average badge size and hidden badge size
