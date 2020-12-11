library(dplyr)
library(tidyr)
library(lubridate)
library(rriskDistributions)
library(stringr)
library(ggplot2)
library(ggpubr)

# Take a demography dataframe and a vector indicating a
# new subdivission, and return demography for the new division
change_demography_bins <- function(demographyDf, newBins) {
  midAges <- mid_bin_age(as.character(demographyDf$age))
  newDemRow <- bin_ages(midAges, newBins)
  newDemography <- dplyr::mutate(demographyDf, newBin = newDemRow) %>%
    group_by(newBin) %>%
    summarize(., proportion = sum(proportion)) %>%
    ungroup(.) %>%
    dplyr::mutate(age = newBins) %>%
    dplyr::select(-newBin)
  return(newDemography)
}

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

# extrapolate bins
fit_to_lit_proportions <- function(proportionDf, newBins, mixed=FALSE) {
  # data frame for prediction
  newMidAge <- mid_bin_age(newBins) 
  ageData <- data.frame(meanAge=newMidAge, age=newBins, study=NA)
  # fit model
  proportionDf <- dplyr::filter(proportionDf, outcome > 0)
  if (mixed) {
    outcomeModel <- lmer(log(outcome) ~ meanAge + (meanAge+0|study) +
                         (1|study), data=proportionDf)
    predictions <- predict(outcomeModel, ageData, re.form=NA)
  } else {
    outcomeModel <- lm(log(outcome) ~ meanAge, data=proportionDf)
  }
  lmout <- as.data.frame(predict(outcomeModel, ageData, interval="confidence"))
  names(lmout) <- c("outcome", "outcomeL", "outcomeH")
  predictDf <- cbind(ageData, lmout)
  return(list(prediction=predictDf, meanParams=coef(outcomeModel),
              ciParams=confint(outcomeModel),
              model=outcomeModel))
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

recalculate_demography <- function(demographics, newBins) {
  demographics$meanAge <- mid_bin_age(as.character(demographics$age))
  newBinsLims <- get_bins_limits(as.character(newBins))
  newBinProp <- NULL
  for (l in c(1:length(newBins))) {
    oldBinInds <- with(demographics,
         which((meanAge >= newBinsLims[[1]][l]) &
                (meanAge <= newBinsLims[[2]][l])))
    newBinProp[l] <- sum(demographics$proportion[oldBinInds])
  }
  return(newBinProp)
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


scaleFun <- function(x) sprintf("%1g", x)

plot_literature_outcome <- function(literatureDf, title=NA, escalaLog=TRUE,
                                    colorVec=NA) {
  outcomePlot <- ggplot(literatureDf, aes(x=meanAge, y=outcome,
                                     color=study, fill=study)) +
    geom_line(size=0.6, linetype = "dashed") +
    geom_ribbon(aes(ymin = outcomeL, ymax = outcomeH), alpha = 0.2, colour=NA,
                show.legend=FALSE) +
    xlab("Edad") +
    ylab("% infectados evento") +
    labs(color = "Estudio") +
    theme_bw()
  if (!is.na(colorVec)) {
    outcomePlot <- outcomePlot +
      scale_colour_manual(values=colorVec) +
      scale_fill_manual(values=colorVec)
  }
  if (escalaLog) {
    outcomePlot <- outcomePlot +
    scale_y_continuous(trans = 'log10', labels=scaleFun)
  }
  if (!is.na(title)) {
    outcomePlot <- outcomePlot +
      ggtitle(title) +
      theme(plot.title = element_text(hjust = 0.5, size=10))
  }
  return(outcomePlot)
}

