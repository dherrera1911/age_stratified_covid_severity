data {
  //          letality data
  int<lower=0> N;                     // number of observations
  int<lower=0> K;                     // number of groups
  int<lower=1, upper=K> group[N];     // location vector
  vector[N] ageVec;                   // predictor
  int outcomes[N];
  int totalCount[N];

  //          ifr data
  int<lower=0> ifrN;
  //int<lower=0> ifrK;
  vector[ifrN] ifr;
  vector[ifrN] ifr_sd;
  vector[ifrN] ifr_age;
  //int<lower=1, upper=ifrK> ifr_study[ifrN];
}

parameters {
  //          outcome letality fit
  real ageSlope;
  real<lower=0> ageSlopeSigma;
  vector[K] groupSlope; 
  real intercept;
  real<lower=0> interceptSigma;
  vector[K] groupIntercept; 

  //          outcome rate fit
  real ageSlopeOutcome;
  real interceptOutcome;
  //real<lower=0> slopeOutcomeSigma;
  //vector[ifrK] groupSlopeOutcome;
  //real<lower=0> interceptOutcomeSigma;
  //vector[ifrK] groupInterceptOutcome;
}

transformed parameters{
  vector<lower=0, upper=1>[N] letality;
  vector<lower=0, upper=1>[ifrN] outcomeRate;
  //vector<lower=0, upper=1>[ifrN] newLetality;
  // fitted letality
  letality = inv_logit(groupIntercept[group] + groupSlope[group] .* ageVec);
  //letality = exp(groupIntercept[group] + groupSlope[group] .* ageVec);

  // estimated rate of outcome
  //newLetality = inv_logit(intercept + ageSlope * ifr_age);
  outcomeRate = inv_logit(interceptOutcome + ifr_age * ageSlopeOutcome);
  //outcomeRate = exp(interceptOutcome + ifr_age * ageSlopeOutcome);
}

model {
  //            Outcome letality fit
  vector[ifrN] newLetality;
  ageSlope ~ normal(5, 3);
  ageSlopeSigma ~ exponential(0.5);
  groupSlope ~ normal(ageSlope, ageSlopeSigma);
  intercept ~ normal(-4, 2);
  interceptSigma ~ exponential(1);
  groupIntercept ~ normal(intercept, interceptSigma);
  outcomes ~ binomial(totalCount, letality);

  //            Outcome rate fit, individual studies
  newLetality = inv_logit(intercept + ageSlope * ifr_age);
  //newLetality = exp(intercept + ageSlope * ifr_age);
  ageSlopeOutcome ~ normal(5, 3);
  //slopeOutcomeSigma ~ exponential(0.2);
  //groupSlopeOutcome ~ normal(ageSlopeOutcome, slopeOutcomeSigma);
  interceptOutcome ~ normal(-3, 1);
  //interceptOutcomeSigma ~ exponential(1);
  //groupInterceptOutcome ~ normal(interceptOutcome, interceptOutcomeSigma);
  ifr ~ normal(newLetality .* outcomeRate, ifr_sd);
}

