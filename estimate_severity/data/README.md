# Data dictionary

## Input data (collected_data)

The *collected_data* directory contains the minimally
preprocessed data that we use as input.

In the file *literature_rates_estimations.csv* we show
pre-existing estimates of age-stratified outcome rates from
different analyses in the literature. In particular, the three
IFR estimates are meta-analyses based on serology data. The
variables in the table contain the following:

| Variable | Description |
| -------- | ----------- |
| Age | Age stratum |
| Proportion | Estimated proportion of disease outcome among infected individuals for the age stratum |
| Proportion_L | Lower bound of the estimated proportion. Confidence interval if available, and predictive interval otherwise |
| Proportion_H | Same as above, but for the upper bound |
| Study | Variable identifying the study reporting the estimates |
| Type | Variable indicating type of outcome. Takes value of IFR for estimates of proportion of death, and IHR for estimate of proportion of severe disease







