# Guide to the code


### 1_literature_values_covid.R

This script contains the values of different age-stratified IFR and IHR
estimates extracted from the literature, as well as the values of different
age-stratified hospital lethality reports. It puts the two kind of
data in table format and exports them to the csv files
*literature_rates_estimations.csv* and *hospitalized_patient_studies.csv*
respectively.


### 2_countries_values.R

This script contains data gathered about age-stratified seroprevalence
estimates or testing reports for several countries, and the corresponding
age-stratified numbers of cumulative hospitalized, critical care and
dead patients. It puts these data into the file 
*locations_serology_data.csv*


### 3_estimate_serology_outcome_rates.R

Fit a bayesian regression model to the proportion of outcomes
(severe disease, critical disease, deaths), taken over
the number of estimated infections from seroprevalence data
(or from testing in some cases). The model is defined
in the file **3_estimate_serology_outcome_rates.stan**.




