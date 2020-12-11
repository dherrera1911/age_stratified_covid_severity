library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(RColorBrewer)
source("./functions_auxiliary.R")

##############################
# Put together the demographics of Uruguay
# source(https://en.wikipedia.org/wiki/Demographics_of_Uruguay)
##############################

# demographics
ageVec <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34",
            "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69",
            "70-74", "75-79", "80-84", "85-89", "90+") 
N <- c(220345, 238068, 256552, 261691, 241006, 228385, 233365,
       222521, 203098, 198773, 194565, 173007, 150775, 131563,
       112395, 93659, 70505, 37426, 18178)
uruPop <- tibble(age=ageVec, N=N, proportion=100*N/sum(N))

####################################
# Calculate glocal outcome proportion for
# Uruguay demographics
####################################
ifrEstimates <- read.csv("../data/1_fitted_ifr.csv")
criticalEstimates <- read.csv("../data/1_fitted_critical.csv")
severeEstimates <- read.csv("../data/1_fitted_severe.csv")

ifrEstimates$demProportion <- recalculate_demography(uruPop, ifrEstimates$age)
criticalEstimates$demProportion <- recalculate_demography(uruPop,
                                                          criticalEstimates$age)
severeEstimates$demProportion <- recalculate_demography(uruPop,
                                                          severeEstimates$age)

uruIFR <- group_by(ifrEstimates, study) %>%
  summarize(., percentage=sum(IFR*demProportion/100),
            percentageL=sum(IFR_L*demProportion/100),
            percentageH=sum(IFR_H*demProportion/100)) %>%
  ungroup(.) %>%
  dplyr::mutate(., outcome = "Death")

uruCritical <- group_by(criticalEstimates, study) %>%
  summarize(., percentage=sum(critical*demProportion/100),
            percentageL=sum(criticalL*demProportion/100),
            percentageH=sum(criticalH*demProportion/100)) %>%
  ungroup(.) %>%
  dplyr::mutate(., outcome = "Critical")

uruSevere <- group_by(severeEstimates, study) %>%
  summarize(., percentage=sum(severe*demProportion/100),
            percentageL=sum(severeL*demProportion/100),
            percentageH=sum(severeH*demProportion/100)) %>%
  ungroup(.) %>%
  dplyr::mutate(., outcome = "Severe")

uruOutcomeDf <- rbind(uruIFR, uruCritical, uruSevere)

write.csv(uruOutcomeDf, "../data/2_uru_demographic_outcomes.csv")

####################################
# calculate proportion of outcomes below specific ages
####################################

cutAges <- c(10, 20, 30, 40, 50, 60, 70, 80)
fitIFR <- dplyr::filter(ifrEstimates, study == "Fitted") %>%
  dplyr::mutate(., meanAge=mid_bin_age(as.character(age)),
                expectedRelativeOutcomes=IFR*demProportion)
fitCritical <- dplyr::filter(criticalEstimates, study == "Fitted") %>%
  dplyr::mutate(., meanAge=mid_bin_age(as.character(age)),
                expectedRelativeOutcomes=critical*demProportion)
fitSevere <- dplyr::filter(severeEstimates, study == "Fitted") %>%
  dplyr::mutate(., meanAge=mid_bin_age(as.character(age)),
                expectedRelativeOutcomes=severe*demProportion)

fitList <- list(fitIFR, fitCritical, fitSevere)
percentageUnder <- list(numeric(), numeric(), numeric())
for (ageInd in c(1:length(cutAges))) {
  for (f in c(1:length(fitList))) {
    ageMax <- cutAges[ageInd]
    sumRelativeOutcome <- sum(fitList[[f]]$expectedRelativeOutcomes)
    sumUnderCut <- dplyr::filter(fitList[[f]], meanAge < ageMax) %>%
      with(., sum(expectedRelativeOutcomes))
    percentageUnder[[f]][ageInd] <- sumUnderCut*100/sumRelativeOutcome
  }
}

ageCharacterVec <- paste("<", cutAges, sep="")
ageOutcomeDf <- data.frame(ageRange=ageCharacterVec,
                           Death=percentageUnder[[1]],
                           Critical=percentageUnder[[2]],
                           Severe=percentageUnder[[3]])

write.csv(ageOutcomeDf, "../data/2_outcome_age_distribution.csv", row.names=FALSE)


#####################################
# ICU demand estimate
#####################################
medianICUstay <- 11
ICUbeds <- 200
ICUprop <- dplyr::filter(uruOutcomeDf, outcome=="Critical" &
                         study=="Fitted")[["percentage"]]
saturationFlux <- ICUbeds/(medianICUstay*ICUprop/100)

medianHospStay <- 9.5
hospProp <- dplyr::filter(uruOutcomeDf, outcome=="Severe" &
                         study=="Fitted")[["percentage"]]
hospSaturation <- saturationFlux*medianHospStay*hospProp/100

ICUpropH <- dplyr::filter(uruOutcomeDf, outcome=="Critical" &
                         study=="Fitted")[["percentageH"]]
saturationFluxH <- ICUbeds/(medianICUstay*ICUpropH/100)

ICUpropL <- dplyr::filter(uruOutcomeDf, outcome=="Critical" &
                         study=="Fitted")[["percentageL"]]
saturationFluxL <- ICUbeds/(medianICUstay*ICUpropL/100)

##################################
# Write down and export Uruguay reported
# figures
# https://www.gub.uy/ministerio-salud-publica/comunicacion/noticias/informe-epidemiologico-covid-19-del-30-noviembre-2020
##################################

uruDeathsOver65Percentage <- 81

# covid incidence
ageVec <- c("0-14", "15-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75+")
uruCovidN <- c(565, 846, 1195, 916, 889, 715, 341, 362)
uruCovid <- tibble(age=ageVec, uruCovidN=uruCovidN,
                   proportionInfected=100*uruCovidN/sum(uruCovidN))

# covid ICU patients cumulative
uruCovidDeaths <- c(0, 0, 0, 0, 3, 12, 23, 39)
ifrCI <- binomial_confint(uruCovidN, uruCovidDeaths)
uruIFR <- tibble(age=ageVec, N=uruCovidDeaths,
                 proportion=100*uruCovidDeaths/uruCovidN,
                 proportionL=ifrCI$lower*100,
                 proportionH=ifrCI$upper*100)

# covid ICU patients cumulative
uruCovidICU <- c(0, 0, 0, 5, 9, 20, 26, 18)
icuCI <- binomial_confint(uruCovidN, uruCovidICU)
uruICU <- tibble(age=ageVec, N=uruCovidICU,
                 proportion=100*uruCovidICU/uruCovidN,
                 proportionL=icuCI$lower*100,
                 proportionH=icuCI$upper*100)

proportionHosp <- c(0, 0.4, 0.9, 3.5, 5.1, 8.5, 15.8, 24.6)
hospCI <- binomial_confint(uruCovidN, proportionHosp/100, input="proportion")
uruHospital <- tibble(age=ageVec, N = proportionHosp*uruCovidN/100,
                      proportion=proportionHosp, proportionL=hospCI$lower*100,
                      proportionH=hospCI$upper*100)

write.csv(uruCovid, "../data/2_uru_covid_incidence.csv", row.names=FALSE)
write.csv(uruIFR, "../data/2_uru_ifr_data.csv", row.names=FALSE)
write.csv(uruICU, "../data/2_uru_icu_data.csv", row.names=FALSE)
write.csv(uruHospital, "../data/2_uru_hospital_data.csv", row.names=FALSE)


