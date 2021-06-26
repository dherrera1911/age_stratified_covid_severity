library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(rstan)
library(bayesplot)
library(tidybayes)
source("./functions_auxiliary.R")
source("./stan_utility.R")
set.seed(2961)

#countryData <- read.csv("../data/collected_data/locations_serology_data.csv",
#                        stringsAsFactors=FALSE) %>%
#  as_tibble(.)
countryData <- read.csv("../data/collected_data/locations_serology_data_corrected.csv",
                        stringsAsFactors=FALSE) %>%
  as_tibble(.)

savingName <- "../data/processed_data/3_serology_fits_corrected.RDS"

outcome_reg <- rstan::stan_model("./3_estimate_serology_outcome_rates.stan")

outcome <- c("Hospitalized", "ICU", "Deaths")

outcomeData <- list()
meanAge <- list()
sdAge <- list()
priorReg <- list()
outcomeDataList <- list()
model <- list()
locationKey <- list()
for (no in c(1:length(outcome))) {
  oStr <- outcome[no]
  # Select countries with outcome data
  selectInd <-  !is.na(countryData[[oStr]])
  outcomeData[[oStr]] <- countryData[which(selectInd),] %>%
    dplyr::mutate(., locationNum=as.integer(factor(Location)))
  meanAge[[oStr]] <- mean(outcomeData[[oStr]]$meanAge)
  sdAge[[oStr]] <- sd(outcomeData[[oStr]]$meanAge)
  outcomeData[[oStr]]$stdAge <- (outcomeData[[oStr]]$meanAge-meanAge[[oStr]])/sdAge[[oStr]]

  # Put data input in list
  outcomeDataList[[oStr]] <- list(N=nrow(outcomeData[[oStr]]),
                    K=length(unique(outcomeData[[oStr]]$Location)),
                    location=outcomeData[[oStr]]$locationNum,
                    ageVec=outcomeData[[oStr]]$stdAge,
                    cases=outcomeData[[oStr]]$Cases,
                    outcomes=outcomeData[[oStr]][[oStr]])
  # Fit model
  model[[oStr]] <- rstan::sampling(outcome_reg, data=outcomeDataList[[oStr]],
                             chains=4, iter=5000, refresh=0)
  locationKey[[oStr]] <- unique(dplyr::select(outcomeData[[oStr]],
                                              Location, locationNum))
}

modelList <- list(model=model, locationKey=locationKey,
                  meanAge=meanAge, sdAge=sdAge)

saveRDS(modelList, savingName)

