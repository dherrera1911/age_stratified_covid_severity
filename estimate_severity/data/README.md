# Data dictionary

## Collected_data

The *collected_data* directory contains the minimally
preprocessed data that we use as input.

### literature_rates_estimations.csv

Contains estimates of age-stratified outcome rates
among infected individuals from previous studies in the literature.
In particular, the three IFR estimates are meta-analyses based on
serology data.

| Variable | Description |
| -------- | ----------- |
| *Age* | Age stratum |
| *Proportion* | Estimated proportion of disease outcome among infected individuals for the age stratum |
| *Proportion_L* | Lower bound of the 95 confidence interval of the estimated proportion (if CI not reported, predictive interval is used instead) |
| *Proportion_H* | Same as above, but for the upper bound |
| *Study* | Variable identifying the study reporting the estimates |
| *Type* | Variable indicating type of outcome. Takes value of IFR for estimates of proportion of death, and IHR for estimate of proportion of severe disease


### hospitalized_patient_studies

Contains data reported on the outcomes of patients hospitalized or
in critical care, in studies from different locations. 

| Variable | Description |
| -------- | ----------- |
| *Age* | Age range of the group of patients |
| *Patients* | Number of patients included in the group |
| *Deaths* | Number of deaths among the group patients |
| *Letality* | Estimated letality for this group of patients |
| *LetalityL* | Lower bound of the 95CI for the estimated letality (binomial test) |
| *LetalityH* | Upper bound of the 95CI for the estimated letality (binomial test) |
| *Study* | Study from which data were extracted |
| *Type* | Type of medical atention received by patient group: *Hospital* or *ICU* |
| *Location* | Country or region where data were collected |
| *EndPoint* | Final date of the study (may vary between last endpoint date considered, or last date of hospital admission) |


### locations_data

Contains the age-stratified estimations of number of infections in
different locations, obtained from serology studies or from extensive
testing and tracing, and age-stratified counts of COVID disease outcomes.
Slight data processing was applied in some cases to match the age
stratifications of seroprevalence and disease outcome data sources.

| Variable | Description |
| -------- | ----------- |
| *Age* | Age range of population |
| *Cases* | Number of cases for the age group, obtained from testing or by multiplying the estimated seroprevalence by the population size of the age group |
| *CasesL* | Lower bound of the estimated number of cases. Obtained by multiplying lower bound of seroprevalence 95CI estimate by population size |
| *CasesH* | Same as above, but for the upper bound |
| *Hospitalized* | Number of hospitalized individuals reported for this age range |
| *ICU* | Number individuals admitted to ICU reported in this age range |
| *Deaths* | Number deaths reported in this age range |
| *Type* | Method used to obtain the estimate of cases: *Testing*, *Seroprevalence* or *Seroprevalence_convenience* |
| *Location* | Location where the reported data corresponds to |
| *EndPointOutcome* | Last day of the period in which outcomes were counted |
| *EndPointCases* | Last day of the period in which seroprevalence data were gathered |

