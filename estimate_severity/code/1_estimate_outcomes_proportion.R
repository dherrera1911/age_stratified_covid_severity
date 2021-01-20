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
source("./functions_auxiliary.R")

#############################
# Load the literature values
#############################

icuLetality <- read.csv("../data/0_icu_letality_literature.csv",
                        stringsAsFactors=FALSE) %>%
  dplyr::mutate(., meanAge=mid_bin_age(age))
hospLetality <- read.csv("../data/0_hospitalized_letality_literature.csv",
                        stringsAsFactors=FALSE) %>%
  dplyr::mutate(., meanAge=mid_bin_age(age),
                SD = (abs(letalityL-letality) + abs(letalityH-letality))/(2*1.96),
                weight = 1/SD)
severeLiterature <- read.csv("../data/0_percentage_severe_literature.csv",
                        stringsAsFactors=FALSE)
ifrLiterature <- read.csv("../data/0_ifr_literature.csv", stringsAsFactors=FALSE)

# Age strata to return in the fit
standardAges <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34",
            "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69",
            "70-74", "75-79", "80-84", "85-89", "90+") 
#uruEpiAges <- c("0-14", "15-24", "25-34", "35-44", "45-54", "55-64",
#            "65-74", "75+")

####################
# Fit a linear model to the hospital letality literature
####################

letalityHospFit <- dplyr::filter(hospLetality, letality > 0) %>%
  #lm(log(letality) ~ meanAge, weights = weight, data = .)
  lm(log(letality) ~ meanAge, data = .)

# Put the fitted letality in a data.frame with the literature letalities
tempLetality <- data.frame(age = standardAges, meanAge = mid_bin_age(standardAges))

hospitalLetalityFit <- as.data.frame(exp(predict(letalityHospFit, tempLetality,
                                     interval="confidence")))
names(hospitalLetalityFit) <- c("letality", "letalityL", "letalityH")
hospitalLetalityFit <- cbind(tempLetality, hospitalLetalityFit) %>%
  dplyr::mutate(., study = "Fitted")

severeAgeDf <- dplyr::select(hospLetality, -SD, -weight) %>%
  rbind(., hospitalLetalityFit)

write.csv(severeAgeDf, "../data/1_fitted_hospital_letality.csv", row.names=FALSE)

#################################
# Estimate percentage of severe cases by age from
# literature IFRs and from hospital letality
################################

estimatedProps <- list()
severeLetalityMean <- dplyr::filter(severeAgeDf, study =="Fitted")
# mid age of each bin in the IFR studies
ifrAges <- mid_bin_age(as.character(ifrLiterature$age))
# get the letality in icu and in hospital for each of the IFR studies bins
icuLetalityStrat <- with(icuLetality, assign_bin_prop(ifrAges, age, letality))
hospitalLetalityStrat <- with(hospitalLetalityFit,
                              assign_bin_prop(ifrAges, age, letality))

# calculate the proportion for the outcomes in each age bin in each study
estimatedProps <- ifrLiterature %>%
  dplyr::mutate(., icuLetality=icuLetalityStrat,
                hospitalLetality=hospitalLetalityStrat,
                critical = IFR * (100/icuLetality),
                criticalL = IFR_L * (100/icuLetality),
                criticalH = IFR_H * (100/icuLetality),
                severe = IFR * (100/hospitalLetality),
                severeL = IFR_L * (100/hospitalLetality),
                severeH = IFR_H * (100/hospitalLetality)) %>%
  as_tibble(.)

#################################
# Fit a linear model to the log of estimated
# proportions of severe and critical cases
################################

# Note: the function fit_to_lit_proportions fits to the column
# named outcome

ifrFit <- dplyr::mutate(estimatedProps, outcome=IFR,
                        meanAge=mid_bin_age(age)) %>%
  fit_to_lit_proportions(., standardAges)
criticalFit <- dplyr::mutate(estimatedProps, outcome=critical,
                             meanAge=mid_bin_age(age)) %>%
  fit_to_lit_proportions(., standardAges)
severeFit <- dplyr::mutate(estimatedProps, outcome=severe,
                             meanAge=mid_bin_age(age)) %>%
  fit_to_lit_proportions(., standardAges)

# Export the fitted models
modelList <- list(ifr=ifrFit$model, critical=criticalFit$model,
                  severe=severeFit$model)
saveRDS(modelList, "../data/1_fitted_models.Rds")

#ifrFitUru <- dplyr::mutate(estimatedProps, outcome=IFR,
#                        meanAge=mid_bin_age(age)) %>%
#  fit_to_lit_proportions(., uruEpiAges)
#ifrFitUru <- ifrFitUru$prediction %>%
#  dplyr::mutate(., desenlace="Death")
#criticalFitUru <- dplyr::mutate(estimatedProps, outcome=critical,
#                             meanAge=mid_bin_age(age)) %>%
#  fit_to_lit_proportions(., uruEpiAges)
#criticalFitUru <- criticalFitUru$prediction %>%
#  dplyr::mutate(., desenlace="ICU")
#severeFitUru <- dplyr::mutate(estimatedProps, outcome=severe,
#                             meanAge=mid_bin_age(age)) %>%
#  fit_to_lit_proportions(., uruEpiAges)
#severeFitUru <- severeFitUru$prediction %>%
#  dplyr::mutate(., desenlace="Hospitalized")


#uruCasesDemOutcomes <- rbind(ifrFitUru, criticalFitUru, severeFitUru) %>%
#  dplyr::mutate(., outcome=exp(outcome), outcomeL=exp(outcomeL),
#                outcomeH=exp(outcomeH))
#write.csv(uruCasesDemOutcomes, "../data/1_fitted_outcomes_epiUru_dem.csv",
#          row.names=FALSE)

# Put our estimates and fitted values together with
# previous literature estimates

# put calculations of severity together
ifrFittedValues <- dplyr::mutate(ifrFit$prediction,
                                 IFR=exp(outcome),
                                 IFR_L=exp(outcomeL),
                                 IFR_H=exp(outcomeH),
                                 study="Fitted") %>%
  dplyr::select(., -meanAge, -outcome, -outcomeL, -outcomeH)
ifrDf <- rbind(ifrLiterature, ifrFittedValues)

criticalFittedValues <- dplyr::mutate(criticalFit$prediction,
                                 critical=exp(outcome),
                                 criticalL=exp(outcomeL),
                                 criticalH=exp(outcomeH),
                                 study="Fitted") %>%
  dplyr::select(., -meanAge, -outcome, -outcomeL, -outcomeH)
criticalDf <- dplyr::select(estimatedProps, age, critical,
                           criticalL, criticalH, study) %>%
  rbind(., criticalFittedValues)

severeFittedValues <- dplyr::mutate(severeFit$prediction,
                                 severe=exp(outcome),
                                 severeL=exp(outcomeL),
                                 severeH=exp(outcomeH),
                                 study="Fitted") %>%
  dplyr::select(., -meanAge, -outcome, -outcomeL, -outcomeH)
severeDf <- dplyr::select(estimatedProps, age, severe,
                           severeL, severeH, study) %>%
  rbind(., severeLiterature, severeFittedValues)

# put together the characteristics of the fits
fitList <- list(ifrFit$model, criticalFit$model, severeFit$model)
intercepts <- NULL
interceptsL <- NULL
interceptsH <- NULL
slopes <- NULL
slopesL <- NULL
slopesH <- NULL
R_squared <- NULL
for (f in c(1:length(fitList))) {
  intercepts[f] <- coef(fitList[[f]])[1]
  interceptsL[f] <- confint(fitList[[f]])[1,1]
  interceptsH[f] <- confint(fitList[[f]])[1,2]
  slopes[f] <- coef(fitList[[f]])[2]
  slopesL[f] <- confint(fitList[[f]])[2,1]
  slopesH[f] <- confint(fitList[[f]])[2,2]
  R_squared[f] <- summary(fitList[[f]])$r.squared
}
fittedParameter <- c("Fatality", "Critical", "Severe")

fitsDf <- data.frame(Fit=fittedParameter,
                     Intercept=intercepts,
                     InterceptL=interceptsL,
                     InterceptH=interceptsH,
                     Slope=slopes,
                     SlopeL=slopesL,
                     SlopeH=slopesH,
                     R_squared=R_squared)

##### Save relevant values
write.csv(ifrDf, "../data/1_fitted_ifr.csv", row.names=FALSE)
write.csv(criticalDf, "../data/1_fitted_critical.csv", row.names=FALSE)
write.csv(severeDf, "../data/1_fitted_severe.csv", row.names=FALSE)
write.csv(fitsDf, "../data/1_model_summaries.csv", row.names=FALSE)

