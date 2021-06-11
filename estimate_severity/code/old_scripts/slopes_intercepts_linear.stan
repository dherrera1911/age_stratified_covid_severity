data {
  int<lower=0> N;                     // number of observations
  int<lower=0> K;                     // number of groups
  int<lower=1, upper=K> group[N];     // location vector
  vector[N] ageVec;                   // predictor
  int successes[N];
  int totalCount[N];
  int<lower=0> Nages;
  vector[Nages] outputAges;
}
parameters {
  vector[K] groupSlope; 
  vector[K] groupIntercept; 
  real ageSlope;
  real intercept;
  //real<lower=0> ageSlopeSigma;
  real<lower=0> interceptSigma;
  // real<lower=0> residualSigma;
}
transformed parameters{
  vector<lower=0, upper=1>[N] successProb;
  successProb = inv_logit(groupIntercept[group] + ageSlope * ageVec);
}
model {
  // Priors
  intercept ~ normal(-4, 2);
  interceptSigma ~ exponential(0.5);
  ageSlope ~ normal(5, 3);
  //ageSlopeSigma ~ exponential(0.5);
  groupIntercept ~ normal(intercept, interceptSigma);
  //groupSlope ~ normal(ageSlope, ageSlopeSigma);
  successes ~ binomial(totalCount, successProb);
}
generated quantities{
  vector[Nages] outcomeProb;
  outcomeProb = inv_logit(intercept + ageSlope * outputAges);
}

