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
  dplyr::mutate(., meanAge=mid_bin_age(Age)) %>%
  as_tibble(.)


# Death by age fit
hospCountryData <- dplyr::filter(countryData, !is.na(Hospitalized)) %>%
  dplyr::mutate(., locationNum=as.integer(factor(Location)))

plotHospData <- dplyr::mutate(hospCountryData,
                              hospProp=Hospitalized/Cases,
                              hospPropL=Hospitalized/CasesH,
                              hospPropH=Hospitalized/CasesL)

hospCountryPlot <- ggplot(plotHospData, aes(x=meanAge, y=hospProp*100,
                                            color=Location,
                                            linetype=Type)) +
       geom_line() +
       scale_y_continuous(trans = 'log10', labels=scaleFun)


lala <- hospCountryPlot %>%
  dplyr::mutate(., hospRate = Hospitalized/Cases,
                meanAge = meanAge-40) %>%
  with(., glm(hospRate ~ meanAge, 



modelData <- list(N=nrow(hospCountryData),
                  K=length(unique(hospCountryData$Location)),
                  location=hospCountryData$locationNum,
                  ageVec=hospCountryData$meanAge,
                  cases=hospCountryData$Cases,
                  outcomes=hospCountryData$Hospitalized)

outcome_reg <- rstan::stan_model("./4_fit_countries_data.stan")

hospFit <- rstan::sampling(outcome_reg, data=modelData,
                           chains=4, iter=5000, refresh=0)

write.csv(outcomeDistData, "../data/3_countries_outcome_dist.csv", row.names=FALSE)

