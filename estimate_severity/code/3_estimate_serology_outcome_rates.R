library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(rstan)
library(bayesplot)
library(tidybayes)
source("./functions_auxiliary.R")
source("./stan_utility.R")

countryData <- read.csv("../data/collected_data/locations_serology_data.csv",
                        stringsAsFactors=FALSE) %>%
  as_tibble(.)


outcome_reg <- rstan::stan_model("./3_estimate_serology_outcome_rates.stan")

outcome <- c("Hospitalized", "ICU", "Deaths")

outcomeData <- list()
meanAge <- list()
sdAge <- list()
priorReg <- list()
outcomeDataList <- list()
model <- list()
outcomeFitDf <- list()
outcomePlot <- list()

for (no in c(1:length(outcome))) {
  oStr <- outcome[no]
  # Select countries with outcome data
  selectInd <-  !is.na(countryData[[oStr]])
  outcomeData[[oStr]] <- countryData[which(selectInd),] %>%
    dplyr::mutate(., locationNum=as.integer(factor(Location)))
  meanAge[[oStr]] <- mean(outcomeData[[oStr]]$meanAge)
  sdAge[[oStr]] <- sd(outcomeData[[oStr]]$meanAge)
  outcomeData[[oStr]]$stdAge <- (outcomeData[[oStr]]$meanAge-meanAge[[oStr]])/sdAge[[oStr]]

  # Do simple regression to use reasonable priors
  outcomeVec <- outcomeData[[oStr]][[oStr]]
  casesVec <- outcomeData[[oStr]][["Cases"]]
  stdAge <- outcomeData[[oStr]][["stdAge"]]
  priorReg[[oStr]] <- glm(cbind(outcomeVec, casesVec-outcomeVec) ~ stdAge,
        family="binomial")
  # Put data input in list
  outcomeDataList[[oStr]] <- list(N=nrow(outcomeData[[oStr]]),
                    K=length(unique(outcomeData[[oStr]]$Location)),
                    location=outcomeData[[oStr]]$locationNum,
                    ageVec=outcomeData[[oStr]]$stdAge,
                    cases=outcomeData[[oStr]]$Cases,
                    outcomes=outcomeData[[oStr]][[oStr]],
                    slopePrior=priorReg[[oStr]]$coefficients[2],
                    interceptPrior=priorReg[[oStr]]$coefficients[1])
  # Fit model
  model[[oStr]] <- rstan::sampling(outcome_reg, data=outcomeDataList[[oStr]],
                             chains=4, iter=5000, refresh=0)

  posterior <- extract(model[[oStr]])
  meanSlope <- mean(posterior$ageSlope)
  meanIntercept <- mean(posterior$intercept)
  ageVec <- (seq(2.5, 90, 5) - meanAge[[oStr]])/sdAge[[oStr]]
  lin <- meanIntercept + meanSlope * ageVec
  meanFit <- exp(lin)/(1+exp(lin))
  outcomeFitDf[[oStr]] <- data.frame(meanAge=seq(2.5, 90, 5),
                                     outcomeProp=meanFit)

}

for (no in c(1:length(outcome))) {
  local({
  oStr <- outcome[no]
  outcomePlot[[oStr]] <- outcomeData[[oStr]] %>%
    ggplot(., aes(x=meanAge, y=get(oStr)/Cases*100, color=Location, linetype=Type)) +
         geom_line() +
         scale_y_continuous(trans = 'log10', labels=scaleFun) +
         geom_line(data=outcomeFitDf[[oStr]], aes(y=outcomeProp*100),
                   color="black", linetype="solid", size=2) +
         theme_bw() +
         xlab("Age") +
         ylab("% outcome")
  plotName <- paste("../data/plots/3_serology_fit_", oStr, ".png")
  ggsave(plotName, outcomePlot[[oStr]])
  fitName <- paste("../data/processed_data/3_serology_fit", oStr, ".RDS")
  modelList <- list(model=model[[oStr]], meanAge=meanAge[[oStr]],
                    sdAge=sdAge[[oStr]])
  saveRDS(modelList, fitName)
  })
}


