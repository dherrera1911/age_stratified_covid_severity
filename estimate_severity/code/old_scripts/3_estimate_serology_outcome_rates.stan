data {
  int<lower=0> N;                       // number of observations
  int<lower=0> K;                       // number of locations
  int<lower=1, upper=K> location[N];    // location index vector
  vector[N] ageVec;                     // predictor
  int population[N];                    // population count
  vector[N] seroprevShape;              // Prevalence gamma shape
  vector[N] seroprevRate;           // Prevalence gamma rate
  int<lower=0> outcomes[N];                      // outcome count
}

parameters {
  // Countries outcome fit
  real<lower=0> ageSlope;                      // mean slope
  real<lower=0> ageSlopeSigma;        // sd of slope across locations
  vector[K] locationSlope;            // vector with slope of each location
  real intercept;                     // mean intercept
  real<lower=0> interceptSigma;       // sd of the intercept
  vector[K] locationIntercept;        // intercept of each location 
  vector[N] prevalence; 
}

transformed parameters{
  vector<lower=0, upper=1>[N] outcomeRate;  // variable to contain % outcome
  outcomeRate = inv_logit(locationIntercept[location] + locationSlope[location] .* ageVec);  // logistic regression model 
}

model {
  ageSlope ~ normal(1, 2); // normal(0, 0.1);
  //ageSlopeSigma ~ exponential(0.5);
  ageSlopeSigma ~ exponential(1);
  locationSlope ~ normal(ageSlope, ageSlopeSigma);
  intercept ~ normal(-5, 2);
  interceptSigma ~ exponential(0.5);
  locationIntercept ~ normal(intercept, interceptSigma);
  prevalence ~ gamma(seroprevShape, seroprevRate);
  outcomes ~ binomial(population, outcomeRate .* prevalence);
}

