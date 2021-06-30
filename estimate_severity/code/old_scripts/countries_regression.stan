data {
  int<lower=0> N;                     // number of observations
  int<lower=0> K;                     // number of locations
  int<lower=1, upper=K> country[N];   // location vector
  vector[N] ageVec;                   // predictor
  vector[N] proportionVec;            // response variable
}
parameters {
  // vector[K] countrySlope; 
  vector[K] countryIntercept; 
  real ageSlope;
  real intercept;
  // real<lower = 0> ageSlopeSigma;
  real<lower = 0> interceptSigma;
  real<lower=0> residualVar;
}
model {
  // Hyperpriors
  intercept ~ normal(0, 2);
  interceptSigma ~ normal(0, 2);
  countryIntercept ~ normal(intercept, interceptSigma);
  ageSlope ~ normal(0, 2);
  // ageSlopeSigma ~ normal(0, 2);
  // countrySlope ~ normal(ageSlope, ageSlopeSigma);
  // proportionVec ~ normal(countrySlope[country] .* ageVec + countryIntercept[country], residualVar);
  proportionVec ~ normal(ageSlope * ageVec + countryIntercept[country], residualVar);
}

