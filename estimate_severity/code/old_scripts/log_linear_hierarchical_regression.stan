data {
  int<lower=0> N;                     // number of observations
  int<lower=0> K;                     // number of groups
  int<lower=1, upper=K> group[N];     // location vector
  vector[N] ageVec;                   // predictor
  int successes[N];
  int totalCount[N];
}
parameters {
//  vector[K] groupSlope; 
  vector[K] groupIntercept; 
  real ageSlope;
  real intercept;
//  real<lower=0> ageSlopeSigma;
  real<lower=0> interceptSigma;
}
transformed parameters{
  vector[N] successProb;
  // successProb = Phi(groupIntercept[group] + groupSlope[group] .* ageVec);
  successProb = Phi(groupIntercept[group] + ageSlope * ageVec);
  // successProb = Phi(intercept + groupSlope[group] .* ageVec);
}
model {
  // Hyperpriors
  intercept ~ normal(0, 2);
  interceptSigma ~ exponential(1);
  ageSlope ~ normal(0, 1);
  // ageSlopeSigma ~ exponential(1);
  groupIntercept ~ normal(intercept, interceptSigma);
  // groupSlope ~ normal(ageSlope, ageSlopeSigma);
  successes ~ binomial(totalCount, successProb);
}

