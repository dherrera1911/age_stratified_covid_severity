library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(ggpubr)
library(tidybayes)
library(rstan)
library(matrixStats)

# Return a vector with the mean age in each bin
mid_bin_age <- function(binsVec) {
  ageBins <- strsplit(binsVec, "-") %>%
    lapply(., as.numeric) %>%
    lapply(., mean) %>%
    unlist(.)
  for (naInd in which(is.na(ageBins))) {
    naVal <- as.numeric(substr(binsVec[naInd], start=1, stop=2))
    ageBins[naInd] <- mean(c(naVal, 90))
  }
  # extract last value
  return(ageBins)
}

get_bins_limits <- function(ageBins) {
  ageList <- strsplit(ageBins, "-") %>%
    lapply(., as.numeric)
  ageLow <- NULL
  ageHigh <- NULL
  for (l in c(1:length(ageList))) {
    if (is.na(ageList[[l]])) {
      naVal <- as.numeric(substr(ageBins[l], start=1, stop=2))
      ageList[[l]] <- c(naVal, 300)
    }
    ageLow[l] <- ageList[[l]][1]
    ageHigh[l] <- ageList[[l]][2]
  }
  return(list(lower=ageLow, upper=ageHigh))
}

binomial_confint <- function(countTotal, occurrences, input="count"){
  lower <- NULL
  upper <- NULL
  if (input=="count") {
    countOccurrences <- occurrences
  } else {
    countOccurrences <- round(occurrences * countTotal)
  }
  for (i in c(1:length(countTotal))) {
    confint <- binom.test(countOccurrences[i], countTotal[i])$conf.int
    lower[i] <- confint[1]
    upper[i] <- confint[2]
  }
  confintList <- list(lower=lower, upper=upper)
}

# function used for plotting log axis
scaleFun <- function(x) sprintf("%1g", x)


# Get posterior samples of bayesian fit
proportion_samples <- function(model, ageVec,
                               slopeName="ageSlope",
                               interceptName="intercept") {
  posterior <- rstan::extract(model)
  fitSampleMat <- matrix(nrow=length(ageVec), ncol=0)
  for (n in c(1:length(posterior[[1]]))) {
    intSample <- posterior[[interceptName]][n]
    slopeSample <- posterior[[slopeName]][n]
    lin <- intSample + slopeSample * stdAgeVec
    fitProp <- exp(lin)/(1+exp(lin))
    fitSampleMat <- cbind(fitSampleMat, as.matrix(fitProp))
  }
  meanProp <- rowMeans(fitSampleMat) 
  ciProp <- matrixStats::rowQuantiles(fitSampleMat, probs=c(0.025, 0.975))
  sampleVec <- sort(rep(c(1:ncol(fitSampleMat)), nrow(fitSampleMat)))

  samplesDf <- data.frame(sample=sampleVec,
                          proportion=as.vector(fitSampleMat))
  sampleList <- list(samples=samplesDf, prop_mean=meanProp,
                     prop_L=ciProp[,1], prop_H=ciProp[,2])
  return(sampleList)
}
  
