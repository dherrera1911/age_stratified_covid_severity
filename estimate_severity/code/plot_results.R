library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(rstan)
library(bayesplot)
library(tidybayes)
source("./functions_auxiliary.R")

############################
############################
###
### Plot serology data fit
###
############################
############################

countryData <- read.csv("../data/collected_data/locations_serology_data.csv",
                        stringsAsFactors=FALSE) %>%
  as_tibble(.)

serologyModels <- readRDS("../data/processed_data/3_serology_fits.RDS")

outcome <- c("Hospitalized", "ICU", "Deaths")
outcomeFitDf <- NULL

# get fitted line
for (no in c(1:length(outcome))) {
  oStr <- outcome[no]
  # extract model fit results
  posterior <- extract(serologyModels$model[[oStr]])
  meanSlope <- mean(posterior$ageSlope)
  meanIntercept <- mean(posterior$intercept)
  ageVec <- seq(2.5, 90, 5)
  stdAgeVec <- (ageVec - serologyModels$meanAge[[oStr]]) /
    serologyModels$sdAge[[oStr]]
  lin <- meanIntercept + meanSlope * stdAgeVec
  meanFit <- exp(lin)/(1+exp(lin))
  tempFitDf <- data.frame(meanAge=ageVec, outcomeProp=meanFit, Outcome_type=oStr)
  outcomeFitDf <- rbind(outcomeFitDf, tempFitDf)
}

longCountryData <- tidyr::pivot_longer(data=countryData, 
                                       cols=all_of(outcome),
                                       names_to="Outcome_type",
                                       values_to="Outcomes") %>%
  dplyr::filter(., !is.na(Outcomes))
longCountryData$Outcome_type <- factor(longCountryData$Outcome_type,
                                       levels=outcome)
outcomeFitDf$Outcome_type <- factor(outcomeFitDf$Outcome_type,
                                       levels=outcome)

serologyPlot <- longCountryData %>%
  ggplot(., aes(x=meanAge, y=Outcomes/Cases*100, color=Location,
                linetype=Type, facet=Outcome_type)) +
  geom_line() +
  facet_grid(.~Outcome_type) +
  scale_y_continuous(trans='log10', labels=scaleFun) +
  geom_line(data=outcomeFitDf, aes(y=outcomeProp*100),
            color="black", linetype="solid", size=2) +
  theme_bw() +
  xlab("Age") +
  ylab("% outcome")

ggsave("../data/plots/3_serology_regression.png", serologyPlot,
       width=25, height=10, units="cm")


############################
############################
###
### Plot hospital lethality data fit
###
############################
############################

lethalityData <- read.csv("../data/collected_data/hospitalized_patient_studies.csv",
                        stringsAsFactors=FALSE) %>%
  as_tibble(.)

lethalityModels <- readRDS("../data/processed_data/4_hospital_lethality_fit.RDS")
letType <- c("Hospital", "ICU")

# get fitted line
lethalityFitDf <- NULL
for (no in c(1:length(letType))) {
  oStr <- letType[no]
  # extract model fit results
  posterior <- extract(lethalityModels$model[[oStr]])
  meanSlope <- mean(posterior$ageSlope)
  meanIntercept <- mean(posterior$intercept)
  ageVec <- seq(2.5, 90, 5)
  stdAgeVec <- (ageVec - lethalityModels$meanAge[[oStr]]) /
    lethalityModels$sdAge[[oStr]]
  lin <- meanIntercept + meanSlope * stdAgeVec
  meanFit <- exp(lin)/(1+exp(lin))
  tempFitDf <- data.frame(meanAge=ageVec, outcomeProp=meanFit, Type=oStr)
  lethalityFitDf <- rbind(lethalityFitDf, tempFitDf)
}

lethalityData$Type <- factor(lethalityData$Type, levels=letType)
lethalityFitDf$Type <- factor(lethalityFitDf$Type, levels=letType)

lethalityPlot <- lethalityData %>%
  ggplot(., aes(x=meanAge, y=Deaths/Patients*100, color=Location,
                facet=Type)) +
  geom_line() +
  facet_grid(.~Type) +
  scale_y_continuous(trans='log10', labels=scaleFun) +
  geom_line(data=lethalityFitDf, aes(y=outcomeProp*100),
            color="black", linetype="solid", size=2) +
  theme_bw() +
  xlab("Age") +
  ylab("Mortality (%)")

ggsave("../data/plots/4_hospital_lethality_regression.png", lethalityPlot,
       width=17, height=10, units="cm")



