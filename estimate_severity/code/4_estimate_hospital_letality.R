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


#############################
# Fit hospital lethality by age
#############################

# Select studies of hospitalization lethality
hospLeth <- dplyr::filter(lethalityData, Type=="Hospital") %>%
  #dplyr::filter(., Type != "Testing") %>%
  dplyr::mutate(., locationNum=as.integer(factor(Location)))

meanHospAge <- mean(hospData$meanAge)
hospData$centeredAge <- hospData$meanAge-meanHospAge

# Do simple regression to use reasonable priors
priorHospReg <- hospData %>%
  glm(cbind(Hospitalized, Cases-Hospitalized) ~ centeredAge,
      family="binomial", data=.)

# Put data input in list
hospDataList <- list(N=nrow(hospData),
                  K=length(unique(hospData$Location)),
                  location=hospData$locationNum,
                  ageVec=hospData$centeredAge,
                  cases=hospData$Cases,
                  outcomes=hospData$Hospitalized,
                  slopePrior=priorHospReg$coefficients[2])

# Compile and run model
outcome_reg <- rstan::stan_model("./3_estimate_serology_outcome_rates.stan")
hospFit <- rstan::sampling(outcome_reg, data=hospDataList,
                           chains=4, iter=5000, refresh=0)

posteriorHosp <- extract(hospFit)
meanSlopeHosp <- mean(posteriorHosp$ageSlope)
meanInterceptHosp <- mean(posteriorHosp$intercept)
ageVec <- seq(2.5, 90, 5) - meanHospAge
linHosp <- meanInterceptHosp + meanSlopeHosp * ageVec
meanFitHosp <- exp(linHosp)/(1+exp(linHosp))
hospFitDf <- data.frame(meanAge=ageVec+meanHospAge, hospProp=meanFitHosp)

# Plot hospitalization data and fit
hospPlot <- dplyr::mutate(hospData, hospProp=Hospitalized/Cases,
                              hospPropL=Hospitalized/CasesH,
                              hospPropH=Hospitalized/CasesL) %>%
  ggplot(., aes(x=meanAge, y=hospProp*100, color=Location, linetype=Type)) +
       geom_line() +
       scale_y_continuous(trans = 'log10', labels=scaleFun) +
       geom_line(data=hospFitDf, color="black", linetype="solid", size=2) +
       theme_bw() +
       xlab("Age") +
       ylab("% Hospitalized")



#############################
# Fit ICU by age
#############################

# Select countries with hospitalization data
icuData <- dplyr::filter(countryData, !is.na(ICU)) %>%
  #dplyr::filter(., Type != "Testing") %>%
  dplyr::mutate(., locationNum=as.integer(factor(Location)))

meanICUAge <- mean(icuData$meanAge)
icuData$centeredAge <- icuData$meanAge-meanICUAge

# Do simple regression to use reasonable priors
priorICUReg <- icuData %>%
  glm(cbind(ICU, Cases-ICU) ~ centeredAge,
      family="binomial", data=.)

# Put data input in list
icuDataList <- list(N=nrow(icuData),
                  K=length(unique(icuData$Location)),
                  location=icuData$locationNum,
                  ageVec=icuData$centeredAge,
                  cases=icuData$Cases,
                  outcomes=icuData$ICU)

# Compile and run model
#outcome_reg <- rstan::stan_model("./3_estimate_serology_outcome_rates.stan")
icuFit <- rstan::sampling(outcome_reg, data=icuDataList,
                           chains=4, iter=5000, refresh=0)

posterior <- extract(icuFit)
meanSlope <- mean(posterior$ageSlope)
meanIntercept <- mean(posterior$intercept)
ageVec <- seq(2.5, 90, 5) - meanHospAge
lin <- meanIntercept + meanSlope * ageVec
meanFit <- exp(lin)/(1+exp(lin))
icuFitDf <- data.frame(meanAge=ageVec+meanHospAge, icuProp=meanFit)

# Plot icuitalization data and fit
icuPlot <- dplyr::mutate(icuData, icuProp=Hospitalized/Cases,
                              icuPropL=Hospitalized/CasesH,
                              icuPropH=Hospitalized/CasesL) %>%
  ggplot(., aes(x=meanAge, y=icuProp*100, color=Location, linetype=Type)) +
       geom_line() +
       scale_y_continuous(trans = 'log10', labels=scaleFun) +
       geom_line(data=icuFitDf, color="black", linetype="solid", size=2) +
       theme_bw() +
       xlab("Age") +
       ylab("% Hospitalized")








