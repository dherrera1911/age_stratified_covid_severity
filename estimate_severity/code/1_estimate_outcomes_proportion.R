############################################
############################################
# This script estimates the proportion of
# infected individuals that get severe and
# critical cases by age
#
# Written by Daniel Herrera, November 2020
# Contact at dherrera@fcien.edu.uy
############################################
############################################

library(dplyr)
library(tidyr)
library(ggplot2)
library(rstan)
library(bayesplot)
source("./functions_auxiliary.R")

#############################
# Load the literature values
#############################

icuLetality <- read.csv("../data/0_icu_letality_literature.csv",
                        stringsAsFactors=FALSE) %>%
  dplyr::mutate(., meanAge=mid_bin_age(age))
hospLetality <- read.csv("../data/0_hospitalized_letality_literature.csv",
                        stringsAsFactors=FALSE) %>%
  dplyr::mutate(., meanAge=mid_bin_age(age))

severeLiterature <- read.csv("../data/0_percentage_severe_literature.csv",
                        stringsAsFactors=FALSE)

ifrLiterature <- read.csv("../data/0_ifr_literature.csv", stringsAsFactors=FALSE) %>%
  dplyr::mutate(., meanAge=mid_bin_age(age), sd=(IFR_H-IFR_L)/4,
                studyNum=as.integer(factor(study)))


# Age strata to return in the fit
standardAges <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34",
            "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69",
            "70-74", "75-79", "80-84", "85-89", "90+") 
#uruEpiAges <- c("0-14", "15-24", "25-34", "35-44", "45-54", "55-64",
#            "65-74", "75+")

####################
# Fit a linear model to the hospital letality literature
####################

#letalityHospFit <- dplyr::filter(hospLetality, letality > 0) %>%
#  #lm(log(letality) ~ meanAge, weights = weight, data = .)
#  lm(log(letality) ~ meanAge, data = .)
#
## Put the fitted letality in a data.frame with the literature letalities
#tempLetality <- data.frame(age = standardAges, meanAge = mid_bin_age(standardAges))
#
#hospitalLetalityFit <- as.data.frame(exp(predict(letalityHospFit, tempLetality,
#                                     interval="confidence")))
#names(hospitalLetalityFit) <- c("letality", "letalityL", "letalityH")
#hospitalLetalityFit <- cbind(tempLetality, hospitalLetalityFit) %>%
#  dplyr::mutate(., study = "Fitted", Hosp=NA, Deaths=NA)
#
#severeAgeDf <- dplyr::select(hospLetality, -SD, -weight) %>%
#  rbind(., hospitalLetalityFit)
#
#write.csv(severeAgeDf, "../data/1_fitted_hospital_letality.csv", row.names=FALSE)
#
##################################
## Estimate percentage of severe cases by age from
## literature IFRs and from hospital letality
#################################
#
#estimatedProps <- list()
#severeLetalityMean <- dplyr::filter(severeAgeDf, study =="Fitted")
## mid age of each bin in the IFR studies
#ifrAges <- mid_bin_age(as.character(ifrLiterature$age))
## get the letality in icu and in hospital for each of the IFR studies bins
#icuLetalityStrat <- with(icuLetality, assign_bin_prop(ifrAges, age, letality))
#hospitalLetalityStrat <- with(hospitalLetalityFit,
#                              assign_bin_prop(ifrAges, age, letality))
#
## calculate the proportion for the outcomes in each age bin in each study
#estimatedProps <- ifrLiterature %>%
#  dplyr::mutate(., icuLetality=icuLetalityStrat,
#                hospitalLetality=hospitalLetalityStrat,
#                critical = IFR * (100/icuLetality),
#                criticalL = IFR_L * (100/icuLetality),
#                criticalH = IFR_H * (100/icuLetality),
#                severe = IFR * (100/hospitalLetality),
#                severeL = IFR_L * (100/hospitalLetality),
#                severeH = IFR_H * (100/hospitalLetality)) %>%
#  as_tibble(.)
#
##################################
## Fit a linear model to the log of estimated
## proportions of severe and critical cases
#################################
#
## Note: the function fit_to_lit_proportions fits to the column
## named outcome
#
#ifrFit <- dplyr::mutate(estimatedProps, outcome=IFR,
#                        meanAge=mid_bin_age(age)) %>%
#  fit_to_lit_proportions(., standardAges)
#criticalFit <- dplyr::mutate(estimatedProps, outcome=critical,
#                             meanAge=mid_bin_age(age)) %>%
#  fit_to_lit_proportions(., standardAges)
#severeFit <- dplyr::mutate(estimatedProps, outcome=severe,
#                             meanAge=mid_bin_age(age)) %>%
#  fit_to_lit_proportions(., standardAges)
#
## Export the fitted models
#modelList <- list(ifr=ifrFit$model, critical=criticalFit$model,
#                  severe=severeFit$model)
#saveRDS(modelList, "../data/1_fitted_models.Rds")
#
##ifrFitUru <- dplyr::mutate(estimatedProps, outcome=IFR,
##                        meanAge=mid_bin_age(age)) %>%
##  fit_to_lit_proportions(., uruEpiAges)
##ifrFitUru <- ifrFitUru$prediction %>%
##  dplyr::mutate(., desenlace="Death")
##criticalFitUru <- dplyr::mutate(estimatedProps, outcome=critical,
##                             meanAge=mid_bin_age(age)) %>%
##  fit_to_lit_proportions(., uruEpiAges)
##criticalFitUru <- criticalFitUru$prediction %>%
##  dplyr::mutate(., desenlace="ICU")
##severeFitUru <- dplyr::mutate(estimatedProps, outcome=severe,
##                             meanAge=mid_bin_age(age)) %>%
##  fit_to_lit_proportions(., uruEpiAges)
##severeFitUru <- severeFitUru$prediction %>%
##  dplyr::mutate(., desenlace="Hospitalized")
#
#
##uruCasesDemOutcomes <- rbind(ifrFitUru, criticalFitUru, severeFitUru) %>%
##  dplyr::mutate(., outcome=exp(outcome), outcomeL=exp(outcomeL),
##                outcomeH=exp(outcomeH))
##write.csv(uruCasesDemOutcomes, "../data/1_fitted_outcomes_epiUru_dem.csv",
##          row.names=FALSE)
#
## Put our estimates and fitted values together with
## previous literature estimates
#
## put calculations of severity together
#ifrFittedValues <- dplyr::mutate(ifrFit$prediction,
#                                 IFR=exp(outcome),
#                                 IFR_L=exp(outcomeL),
#                                 IFR_H=exp(outcomeH),
#                                 study="Fitted") %>%
#  dplyr::select(., -meanAge, -outcome, -outcomeL, -outcomeH)
#ifrDf <- rbind(ifrLiterature, ifrFittedValues)
#
#criticalFittedValues <- dplyr::mutate(criticalFit$prediction,
#                                 critical=exp(outcome),
#                                 criticalL=exp(outcomeL),
#                                 criticalH=exp(outcomeH),
#                                 study="Fitted") %>%
#  dplyr::select(., -meanAge, -outcome, -outcomeL, -outcomeH)
#criticalDf <- dplyr::select(estimatedProps, age, critical,
#                           criticalL, criticalH, study) %>%
#  rbind(., criticalFittedValues)
#
#severeFittedValues <- dplyr::mutate(severeFit$prediction,
#                                 severe=exp(outcome),
#                                 severeL=exp(outcomeL),
#                                 severeH=exp(outcomeH),
#                                 study="Fitted") %>%
#  dplyr::select(., -meanAge, -outcome, -outcomeL, -outcomeH)
#severeDf <- dplyr::select(estimatedProps, age, severe,
#                           severeL, severeH, study) %>%
#  rbind(., severeLiterature, severeFittedValues)
#
## put together the characteristics of the fits
#fitList <- list(ifrFit$model, criticalFit$model, severeFit$model)
#intercepts <- NULL
#interceptsL <- NULL
#interceptsH <- NULL
#slopes <- NULL
#slopesL <- NULL
#slopesH <- NULL
#R_squared <- NULL
#for (f in c(1:length(fitList))) {
#  intercepts[f] <- coef(fitList[[f]])[1]
#  interceptsL[f] <- confint(fitList[[f]])[1,1]
#  interceptsH[f] <- confint(fitList[[f]])[1,2]
#  slopes[f] <- coef(fitList[[f]])[2]
#  slopesL[f] <- confint(fitList[[f]])[2,1]
#  slopesH[f] <- confint(fitList[[f]])[2,2]
#  R_squared[f] <- summary(fitList[[f]])$r.squared
#}
#fittedParameter <- c("Fatality", "Critical", "Severe")
#
#fitsDf <- data.frame(Fit=fittedParameter,
#                     Intercept=intercepts,
#                     InterceptL=interceptsL,
#                     InterceptH=interceptsH,
#                     Slope=slopes,
#                     SlopeL=slopesL,
#                     SlopeH=slopesH,
#                     R_squared=R_squared)
#
###### Save relevant values
#write.csv(ifrDf, "../data/1_fitted_ifr.csv", row.names=FALSE)
#write.csv(criticalDf, "../data/1_fitted_critical.csv", row.names=FALSE)
#write.csv(severeDf, "../data/1_fitted_severe.csv", row.names=FALSE)
#write.csv(fitsDf, "../data/1_model_summaries.csv", row.names=FALSE)


################
################
# Bayesian fits
################
################

####################
# Fit a glm to the hospital letality literature
####################
hospLetality <- dplyr::mutate(hospLetality,
                              studyNum=as.integer(factor(study))) %>%
  dplyr::filter(., Deaths>0) %>%
  dplyr::filter(., meanAge < 75)

outputAges <- dplyr::filter(ifrLiterature, study=="Levin")[["meanAge"]]

letalityData <- with(hospLetality,
                     list(N=length(studyNum), K=length(unique(studyNum)),
                          group=studyNum, ageVec=meanAge/100, successes=Deaths,
                          totalCount=Hosp, Nages=length(outputAges),
                          outputAges=outputAges/100))

binom_reg <- rstan::stan_model("./slopes_intercepts_linear.stan")

initFun <- function(){list(ageSlope=rnorm(1, 5, 2),
                           #ageSlopeSigma=min(rnorm(1, 1, 1), 0),
                           intercept=rnorm(1, -4, 2))}

hospLetalityFit <- rstan::sampling(binom_reg, data=letalityData,
                           chains=4, iter=5000, refresh=0,
                           init=initFun)

letalityPars <- summary(hospLetalityFit)[["summary"]]
estimateRows <- grep("outcomeProb", rownames(letalityPars))
letalityEstimates <- letalityPars[estimateRows, c("mean")]

letalityDf <- data.frame(meanAge=outputAges,
                         letality=as.numeric(letalityEstimates)*100)

plotHospFit <- ggplot(hospLetality, aes(x=meanAge, y=letality, color=study)) +
  geom_point(size=3) +
#  geom_point(aes(y=Fitted), shape=2) +
  scale_y_continuous(trans = 'log10') +
  geom_line(data=letalityDf, color="black")


####################
# Fit a full model to estimate severity from an IFR study
####################

full_model <- rstan::stan_model("./full_model.stan")

cutAge <- 15
ifrLiteratureFilt <- dplyr::mutate(ifrLiterature,
                              studyNum=as.integer(factor(study)))
hospLetalityFilt <- dplyr::mutate(hospLetality,
                              studyNum=as.integer(factor(study))) %>%
  dplyr::filter(., meanAge>cutAge)

fitList <- list()
for (s in unique(ifrLiterature$study)) {
  ifrStudy <- dplyr::filter(ifrLiterature, study==s & meanAge>cutAge)

  fullData <- list(N=length(hospLetalityFilt$studyNum),
                   K=length(unique(hospLetalityFilt$studyNum)),
                   group=hospLetalityFilt$studyNum,
                   ageVec=hospLetalityFilt$meanAge/100,
                   outcomes=hospLetalityFilt$Deaths,
                   totalCount=hospLetalityFilt$Hosp,
                   ifrN=nrow(ifrStudy),
                   ifrK=length(unique(ifrStudy$study)),
                   ifr=ifrStudy$IFR/100,
                   ifr_sd=ifrStudy$sd/100,
                   ifr_age=ifrStudy$meanAge/100)

  initFun <- function(){list(ageSlope=rnorm(1, 5, 2),
                             ageSlopeSigma=max(0, rnorm(1, 2, 2)),
                             intercept=rnorm(1, -4, 2),
                             ageSlopeOutcome=rnorm(1, 5, 2),
                             interceptOutcome=rnorm(1, -4, 2))}

  fitList[[s]] <- rstan::sampling(full_model, data=fullData,
                         chains=3, iter=6000, refresh=0,
                         control=list(max_treedepth=15),
                         init=initFun)
}

pairs(fitList[[2]], pars=c("intercept", "ageSlope",
                  "interceptOutcome", "ageSlopeOutcome"))
pairs(fitList[1], pars=c("outcomeRate"))


estimationsDf <- NULL
for (s in unique(ifrLiterature$study)) {
  estimatedParams <- summary(fitList[[s]])[["summary"]]
  propRows <- grep("outcomeRate", rownames(estimatedParams))
  propEstimates <- estimatedParams[propRows, c("mean")]
  propEstimatesL <- estimatedParams[propRows, c("2.5%")]
  propEstimatesH <- estimatedParams[propRows, c("97.5%")]
  ifrStudy <- dplyr::filter(ifrLiterature, study==s & meanAge>cutAge)
  tempDf <- data.frame(meanAge=ifrStudy$meanAge,
                       hospRate=propEstimates*100,
                       hospRateL=propEstimatesL*100,
                       hospRateH=propEstimatesH*100,
                       study=s)
  estimationsDf <- rbind(estimationsDf, tempDf)
}


estimatedParams <- summary(fitList[[s]])[["summary"]]
propRows <- grep("outcomeRate", rownames(estimatedParams))
propEstimates <- estimatedParams[propRows, c("mean")]

propEstimatesL <- estimatedParams[propRows, c("2.5%")]
propEstimatesH <- estimatedParams[propRows, c("97.5%")]
ifrStudy <- dplyr::filter(ifrLiterature, study==s & meanAge>cutAge)


plotHospFit <- ggplot(hospLetality, aes(x=meanAge, y=letality, color=study)) +
  geom_point(size=3) +
#  geom_point(aes(y=Fitted), shape=2) +
  scale_y_continuous(trans = 'log10') +

plotHospFit2 <- ggplot(hospLetalityFilt, aes(x=log10(letality), y=log10(Fitted), color=study)) +
  geom_point(size=3) +
  geom_abline()


newLetRows <- grep("newLetality", rownames(estimatedParams))
newLetality <- as_tibble(estimatedParams[newLetRows,
                                 c("mean", "2.5%", "97.5%")]) %>%
  dplyr::rename(., letality=mean, letalityL="2.5%", letalityH="97.5%") %>%
  dplyr::mutate(., age=ifrStudy$age, meanAge=mid_bin_age(age),
                study=ifrStudy$study, IFR=ifrStudy$IFR/100,
                hospRate=IFR/letality)


letalityPlot <- ggplot(newLetality, aes(x=meanAge, y=letality)) +
  geom_point() +
  geom_point(aes(x=meanAge, y=IFR), color="red")

hospRatePlot <- ggplot(newLetality, aes(x=meanAge, y=hospRate*100, color=study)) +
  geom_point() +
  scale_y_continuous(trans = 'log10')





rateRows <- grep("outcomeRate", rownames(estimatedParams))
hospitalizationRate <- as_tibble(estimatedParams[rateRows,
                                 c("mean", "2.5%", "97.5%")]) %>%
  dplyr::rename(., severe=mean, severeL="2.5%", severeH="97.5%") %>%
  dplyr::mutate(., age=ifrStudy$age, meanAge=mid_bin_age(age),
                study=ifrStudy$study)

plotSevere <- ggplot(hospitalizationRate, aes(x=meanAge, y=severe,
                                              color=study)) +
  geom_line() +
  scale_y_continuous(trans = 'log10')

plotIFR <- ggplot(ifrLiterature, aes(x=meanAge, y=IFR,
                                              color=study)) +
  geom_line() +
  scale_y_continuous(trans = 'log10')









# Put the fitted letality in a data.frame with the literature letalities
tempLetality <- data.frame(age = standardAges, meanAge = mid_bin_age(standardAges))

hospitalLetalityFit <- as.data.frame(exp(predict(letalityHospFit, tempLetality,
                                     interval="confidence")))
names(hospitalLetalityFit) <- c("letality", "letalityL", "letalityH")
hospitalLetalityFit <- cbind(tempLetality, hospitalLetalityFit) %>%
  dplyr::mutate(., study = "Fitted", Hosp=NA, Deaths=NA)

severeAgeDf <- dplyr::select(hospLetality, -SD, -weight) %>%
  rbind(., hospitalLetalityFit)

write.csv(severeAgeDf, "../data/1_fitted_hospital_letality.csv", row.names=FALSE)




meanAge <- c(4.5, 14.5, 24.5, 34.5, 44.5, 54.5, 64.5, 74.5, 84.5, 90.0,
             38.5, 64.5, 74.5, 85.0, 10.0, 24.5, 34.5, 44.5, 54.5, 64.5,
             74.5, 85.0 , 4.5, 14.5, 24.5, 34.5, 44.5, 54.5, 64.5, 74.5,
             84.5, 90.0 , 4.5, 14.5, 24.5, 34.5, 44.5, 54.5, 64.5, 74.5,
             85.0, 2.0, 9.5, 19.5, 29.5, 39.5, 49.5, 59.5, 69.5, 79.5)

studyNum <- c(4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 2, 2, 2, 2, 5, 5, 5, 5, 5, 5, 5,
              5, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 6, 6, 6, 6, 6, 6, 6, 6, 6,
              1, 1, 1, 1, 1, 1, 1, 1, 1)

deaths <- c(0, 0, 4, 8, 22, 53, 84, 145, 170, 67, 134, 241, 572,
            1280, 5, 24, 89, 208, 632, 1542, 2844, 4857, 0, 0,
            0, 0, 0, 149, 504, 1656, 2698, 1162, 2, 6, 33, 73,
            277, 922, 2515, 6436, 17474, 0, 0, 1, 5, 12, 24, 67, 225, 514)

hosp <- c(26, 8, 97, 211, 352, 515 , 533, 451, 313, 128, 2896, 1621,
          2158, 3346, 789, 2149, 4660, 6303, 9724, 12236, 13543, 15370, 54, 39,
          180, 351, 862, 2072, 2803, 4037, 3816, 1314, 313, 339, 1911, 4722,
          10353, 16553, 19712, 23243, 29517, 24, 18, 77, 198, 274, 451, 499,
          587, 746)





full_model2 <- rstan::stan_model("./full_model_mixed.stan")

cutAge <- 20
ifrLiteratureFilt <- dplyr::mutate(ifrLiterature,
                              studyNum=as.integer(factor(study))) %>%
                     dplyr::filter(., meanAge>cutAge)
hospLetalityFilt <- dplyr::filter(hospLetality, meanAge>cutAge)

fitList <- list()

#for (s in unique(ifrLiterature$study)) {


fullData <- list(N=length(hospLetalityFilt$studyNum),
                 K=length(unique(hospLetalityFilt$studyNum)),
                 group=hospLetalityFilt$studyNum,
                 ageVec=hospLetalityFilt$meanAge/100,
                 outcomes=hospLetalityFilt$Deaths,
                 totalCount=hospLetalityFilt$Hosp,
                 ifrN=nrow(ifrLiterature),
                 ifrK=length(unique(ifrLiterature$study)),
                 ifr=ifrLiterature$IFR/100,
                 ifr_sd=ifrLiterature$sd/100,
                 ifr_age=ifrLiterature$meanAge/100,
                 ifr_study=ifrLiterature$studyNum)

initFun <- function(){list(ageSlope=rnorm(1, 5, 2),
                           ageSlopeSigma=max(0, rnorm(1, 2, 2)),
                           intercept=rnorm(1, -4, 2),
                           ageSlopeOutcome=rnorm(1, 5, 2),
                           interceptOutcome=rnorm(1, -4, 2))}

fit_mixed <- rstan::sampling(full_model2, data=fullData,
                       chains=3, iter=6000, refresh=0,
                       control=list(max_treedepth=15),
                       init=initFun)






