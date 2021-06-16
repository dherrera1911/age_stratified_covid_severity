library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(rstan)
library(bayesplot)
library(tidybayes)
library(matrixStats)
source("./functions_auxiliary.R")
source("./stan_utility.R")

# load literature IFR estimate
literatureIFR <- read.csv("../data/collected_data/literature_rates_estimations.csv",
                           stringsAsFactors=FALSE) %>%
  as_tibble(.) %>%
  dplyr::filter(., Type=="IFR")

# load fitted lethality data
lethalityFit <- readRDS("../data/processed_data/4_hospital_lethality_fit.RDS")

outcomes <- c("Hospital", "ICU")
ageVec <- literatureIFR$meanAge
stdAgeVec <- (ageVec - lethalityFit$meanAge[[oStr]]) / lethalityFit$sdAge[[oStr]]
propOutcomeMat <- list()
for (no in c(1:length(outcomes))) {
  oStr <- outcomes[no]
  outcomeFit <- lethalityFit$model[[oStr]]
  posterior <- extract(outcomeFit)
  propOutcomeMat[[oStr]] <- matrix(nrow=length(ageVec), ncol=0)
  for (n in c(1:length(posterior[[1]]))) {
    ageSlope <- posterior$ageSlope[n]
    intercept <- posterior$intercept[n]
    lin <- intercept + ageSlope * stdAgeVec
    sampleLethality <- exp(lin)/(1+exp(lin))
    samplePropOutcome <- literatureIFR$Proportion/sampleLethality
    propOutcomeMat[[oStr]] <- cbind(propOutcomeMat[[oStr]],
                                    as.matrix(samplePropOutcome))
  }
  meanProp <- rowMeans(propOutcomeMat[[oStr]]) 
  ciProp <- rowQuantiles(propOutcomeMat[[oStr]], probs=c(0.05, 0.95))
  meanCol <- paste(oStr, "_prop", sep="")
  lowerCICol <- paste(oStr, "_L", sep="")
  upperCICol <- paste(oStr, "_H", sep="")
  literatureIFR[[meanCol]] <- meanProp
  literatureIFR[[lowerCICol]] <- ciProp[,1]
  literatureIFR[[upperCICol]] <- ciProp[,2]
}
    
fileName <- "../data/processed_data/5_literature_outcome_estimates.csv"
write.csv(literatureIFR, fileName)

