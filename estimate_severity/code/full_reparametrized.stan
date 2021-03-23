data {
  int<lower=0> N;                     // number of observations
  int<lower=0> K;                     // number of groups
  int<lower=1, upper=K> group[N];     // location vector
  vector[N] ageVec;                   // predictor
  int outcomes[N];
  int totalCount[N];
  int<lower=0> ifrN;
//  int<lower=0> ifrK;
  vector[ifrN] ifr;
  vector[ifrN] ifr_sd;
  vector[ifrN] ifr_age;
//  int<lower=1, upper=ifrK> ifr_study[ifrN];
}
parameters {
  // outcome letality fit
  real ageSlope;
  real<lower=0> ageSlopeSigma;
  real intercept;
  real<lower=0> interceptSigma;
  vector[K] groupIntercept; 
  // outcome rate fit
  real ageSlopeOutcome;
  real interceptOutcome;
  //real<lower=0> interceptOutcomeSigma;
  //vector[ifrK] groupInterceptOutcome;
  vector[K] sigma_raw;
}
transformed parameters{
  vector<lower=0, upper=1>[N] letality;
  vector<lower=0, upper=1>[ifrN] newLetality;
  vector<lower=0, upper=1>[ifrN] outcomeRate;
  vector[K] groupSlope; 
  // fitted letality
  letality = inv_logit(groupIntercept[group] +
    groupSlope[group] .* ageVec);
  // estimated letality for ifr ages
   newLetality = inv_logit(intercept + ageSlope * ifr_age);
  // estimated rate of outcome
  outcomeRate = exp(interceptOutcome + ifr_age*ageSlopeOutcome);
  //outcomeRate = interceptOutcome + ifr_age*ageSlopeOutcome;
  groupSlope = ageSlope + ageSlopeSigma * sigma_raw;
}
model {
  // Outcome letality fit
  intercept ~ normal(0, 3);
  interceptSigma ~ exponential(1);
  groupIntercept ~ normal(intercept, interceptSigma);
  ageSlope ~ normal(0, 0.2);
  ageSlopeSigma ~ exponential(0.2);
  //groupSlope ~ normal(ageSlope, ageSlopeSigma);
  sigma_raw ~ std_normal();
  outcomes ~ binomial(totalCount, letality);
  // Outcome rate fit, individual studies
  ageSlopeOutcome ~ normal(0, 0.1);
  interceptOutcome ~ normal(0, 3);
  //groupInterceptOutcome ~ normal(interceptOutcome, interceptOutcomeSigma);
  ifr ~ normal(newLetality .* outcomeRate, ifr_sd);
}
//generated quantities{
//  vector[ifrN] outcomeRate;
//  outcomeRate = ifr ./ newLetality;
//}

