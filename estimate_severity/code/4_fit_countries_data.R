library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(rstan)
library(bayesplot)
library(tidybayes)
source("./functions_auxiliary.R")
source("./stan_utility.R")

countryData <- read.csv("../data/2_countries_outcomes.csv",
                        stringsAsFactors=FALSE) %>%
  dplyr::mutate(., meanAge=mid_bin_age(age))

hospData <- dplyr::filter(countryData, !is.na(Hosp) & propHosp!=0) %>%
  dplyr::mutate(., countryNum=as.integer(factor(country)))

modelData <- list(N=nrow(hospData), K=length(unique(hospData$country)),
                  country=hospData$countryNum,
                  ageVec=hospData$meanAge,
                  proportionVec=log(hospData$propHosp))

outcome_reg <- rstan::stan_model("./countries_regression.stan")

hospFit <- rstan::sampling(outcome_reg, data=modelData,
                           chains=4, iter=10000, refresh=0)

write.csv(outcomeDistData, "../data/3_countries_outcome_dist.csv", row.names=FALSE)

