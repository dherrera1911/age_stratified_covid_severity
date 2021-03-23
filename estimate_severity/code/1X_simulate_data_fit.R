library(dplyr)
library(tidyr)
library(ggplot2)
library(rstan)
library(bayesplot)
source("./functions_auxiliary.R")

meanAgeVec <- c(5, 15, 25, 35, 45, 55, 65, 75, 85)
normAge <- meanAgeVec/100

# make synthetic data to test fit

#####################
# Letality data
#####################
ageSlope <- 5
ageSlopeSigma <- 1
intercept <- -5
interceptSigma <- 0.3
K_letality <- 12
n_scales <- c(10, 30, 50, 70, 90, 100, 100, 150, 200, 300, 500, 700)

hospMult <- c(1, 1, 2, 2, 3, 4, 4, 3, 2)

letalityDf <- NULL
for (k in c(1:K_letality)) {
  totHosp <- round(n_scales[k]*hospMult*max(rnorm(length(meanAgeVec), 1, 0.2), 0.7))
  k_slope <- max(rnorm(1, ageSlope, ageSlopeSigma), 0.3)
  k_intercept <- min(rnorm(1, intercept, interceptSigma), -2)
  probVec <- 1/(1+exp(-k_slope*normAge-k_intercept))
  #probVec <- max(exp(k_slope*normAge+k_intercept), 1)
  deaths <- NULL
  for (l in c(1:length(probVec))) {
    deaths[l] <- rbinom(1, totHosp[l], probVec[l])
  }
  tempDf <- data.frame(studyNum=k, meanAge=meanAgeVec, Hosp=totHosp, Deaths=deaths,
                       prop=deaths/totHosp*100)
  letalityDf <- rbind(letalityDf, tempDf)
}

dataPlot <- ggplot(letalityDf, aes(x=meanAge, y=prop, color=study)) +
  geom_point()
dataPlot

 
#####################
# IFR data
#####################
ifrSlope <- 11
ifrIntercept <- -7
ifr <- exp(normAge * ifrSlope + ifrIntercept)
ifrSD <- ifr*0.1

ifrDf <- data.frame(meanAge=meanAgeVec, IFR=ifr, sd=ifrSD)

#####################
# proportion hospitalized
#####################
#meanLetality <- exp(ageSlope*normAge + intercept)
meanLetality <- 1/(1+exp(-(ageSlope*normAge + intercept)))
propOutcome <- ifr/meanLetality/100


###################
# Define IFR from prop hospitalized
###################
slopeOutcome <- 5.5
interceptOutcome <- -5.5
#propOutcome <- 1/(1+exp(-(slopeOutcome*normAge + interceptOutcome)))
propOutcome <- exp(slopeOutcome*normAge + interceptOutcome)

meanLetality <- 1/(1+exp(-(ageSlope*normAge + intercept)))
ifr <- propOutcome*meanLetality*100
ifrSD <- ifr*0.1
ifrDf <- data.frame(meanAge=meanAgeVec, IFR=ifr, sd=ifrSD)

###################
# Fit model
###################
full_model <- rstan::stan_model("./full_model.stan")

fullData <- list(N=length(letalityDf$studyNum),
                 K=length(unique(letalityDf$studyNum)),
                 group=letalityDf$studyNum,
                 ageVec=letalityDf$meanAge/100,
                 outcomes=letalityDf$Deaths,
                 totalCount=letalityDf$Hosp,
                 ifrN=nrow(ifrDf),
                 ifrK=length(unique(ifrDf$study)),
                 ifr=ifrDf$IFR/100,
                 ifr_sd=ifrDf$sd/100,
                 ifr_age=ifrDf$meanAge/100)

initFun <- function(){list(ageSlope=rnorm(1, 5, 2),
                           ageSlopeSigma=max(0, rnorm(1, 2, 2)),
                           intercept=rnorm(1, -4, 2),
                           ageSlopeOutcome=rnorm(1, 5, 2),
                           interceptOutcome=rnorm(1, -4, 2))}

fit <- rstan::sampling(full_model, data=fullData,
                       chains=3, iter=6000, refresh=0,
                       control=list(max_treedepth=15),
                       init=initFun)


estimatedParams <- summary(fit)[["summary"]]
propRows <- grep("outcomeRate", rownames(estimatedParams))
propEstimates <- estimatedParams[propRows, c("mean")]
propOutcomeFit <- propEstimates

pairs(fit, pars=c("intercept", "ageSlope", "interceptOutcome", "ageSlopeOutcome"))
pairs(fit, pars=c("outcomeRate"))

plot(log(propOutcome), log(propOutcomeFit))
abline(coef=c(0,1))

plot(propOutcome, propOutcomeFit)
abline(coef=c(0,1))

(propOutcome-propOutcomeFit)/propOutcome

