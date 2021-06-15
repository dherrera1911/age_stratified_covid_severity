library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(rstan)
library(bayesplot)
library(tidybayes)
source("./functions_auxiliary.R")
source("./stan_utility.R")

lethalityData <- read.csv("../data/collected_data/hospitalized_patient_studies.csv",
                        stringsAsFactors=FALSE) %>%
  as_tibble(.)

outcome_reg <- rstan::stan_model("./3_estimate_serology_outcome_rates.stan")
save('outcome_reg', file = '3_serology_model.RData')
#outcome_reg <- load("./3_serology_model.RData")

#############################
# Fit hospital lethality by age
#############################

# Select studies of hospitalization lethality
hospLeth <- dplyr::filter(lethalityData, Type=="Hospital") %>%
  #dplyr::filter(., Type != "Testing") %>%
  dplyr::mutate(., locationNum=as.integer(factor(Location)))

meanHospAge <- mean(hospLeth$meanAge)
sdHospAge <- sd(hospLeth$meanAge)
hospLeth$stdAge <- (hospLeth$meanAge-meanHospAge)/sdHospAge

# Do simple regression to use reasonable priors
priorHospReg <- hospLeth %>%
  glm(cbind(Deaths, Patients-Deaths) ~ stdAge, family="binomial", data=.)

# Put data input in list
hospLethList <- list(N=nrow(hospLeth),
                  K=length(unique(hospLeth$Location)),
                  location=hospLeth$locationNum,
                  ageVec=hospLeth$stdAge,
                  cases=hospLeth$Patients,
                  outcomes=hospLeth$Deaths,
                  slopePrior=priorHospReg$coefficients[2],
                  interceptPrior=priorHospReg$coefficients[1])

# Compile and run model
hospLethFit <- rstan::sampling(outcome_reg, data=hospLethList,
                               chains=4, iter=5000, refresh=0)


#############################
# Fit ICU lethality by age
#############################

# Select studies of hospitalization lethality
icuLeth <- dplyr::filter(lethalityData, Type=="ICU") %>%
  #dplyr::filter(., Type != "Testing") %>%
  dplyr::mutate(., locationNum=as.integer(factor(Location)))

meanICUAge <- mean(icuLeth$meanAge)
sdICUAge <- sd(icuLeth$meanAge)
icuLeth$stdAge <- (icuLeth$meanAge-meanICUAge)/sdICUAge

# Do simple regression to use reasonable priors
priorICUReg <- icuLeth %>%
  glm(cbind(Deaths, Patients-Deaths) ~ stdAge, family="binomial", data=.)

# Put data input in list
icuLethList <- list(N=nrow(icuLeth),
                  K=length(unique(icuLeth$Location)),
                  location=icuLeth$locationNum,
                  ageVec=icuLeth$stdAge,
                  cases=icuLeth$Patients,
                  outcomes=icuLeth$Deaths,
                  slopePrior=priorICUReg$coefficients[2],
                  interceptPrior=priorICUReg$coefficients[1])

# Compile and run model
icuLethFit <- rstan::sampling(outcome_reg, data=icuLethList,
                               chains=4, iter=5000, refresh=0)

models <- list(Hospital=hospLethFit,
               ICU=icuLethFit)
meanAge <- list(Hospital=meanHospAge,
                ICU=meanICUAge)
sdAge <- list(Hospital=sdHospAge,
                ICU=sdICUAge)
modelList <- list(model=models, meanAge=meanAge, sdAge=sdAge)

saveRDS(modelList, "../data/processed_data/4_hospital_lethality_fit.RDS")


