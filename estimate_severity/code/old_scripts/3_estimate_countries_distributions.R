library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(RColorBrewer)
library(wpp2019)
source("./functions_auxiliary.R")
data(popM)
data(popF)

fittedModels <- readRDS("../data/1_fitted_models.Rds")
countryData <- read.csv("../data/2_countries_outcomes.csv",
                        stringsAsFactors=FALSE) %>%
  dplyr::mutate(., meanAge=mid_bin_age(age))

####################################
# calculate expected distribution of outcomes by age
####################################
countryData$fittedIFR <- exp(predict(fittedModels$ifr, countryData))
countryData$fittedICU <- exp(predict(fittedModels$critical, countryData))
countryData$fittedSevere <- exp(predict(fittedModels$severe, countryData))

countryData$expectedRelativeDeath <- with(countryData, fittedIFR*CaseProp)
countryData$expectedRelativeICU <- with(countryData, fittedICU*CaseProp)
countryData$expectedRelativeHosp <- with(countryData, fittedSevere*CaseProp)

outcomeDistData <- group_by(countryData, country) %>%
  dplyr::mutate(.,
                cumDeathsFitted=cumsum(expectedRelativeDeath)/sum(expectedRelativeDeath)*100,
                cumICUFitted=cumsum(expectedRelativeICU)/sum(expectedRelativeICU)*100,
                cumHospFitted=cumsum(expectedRelativeHosp)/sum(expectedRelativeHosp)*100,
                cumDeathObs=cumsum(Deaths)/sum(Deaths)*100,
                cumICUObs=cumsum(ICU)/sum(ICU)*100,
                cumHospObs=cumsum(Hosp)/sum(Hosp)*100) %>%
  ungroup(.) %>%
  dplyr::select(., age, country, cumDeathsFitted, cumICUFitted, cumHospFitted,
                cumDeathObs, cumICUObs, cumHospObs) %>%
  dplyr::filter(., cumDeathsFitted!=100)


agesCut <- strsplit(outcomeDistData$age, "-")
ageVec <- NULL
for (agePair in agesCut) {
  ageVec <- c(ageVec, paste("<", agePair[2], sep=""))
}
outcomeDistData$ageLim <- ageVec

#write.csv(ageOutcomeDf, "../data/2_outcome_age_distribution.csv", row.names=FALSE)
#write.csv(ageOutcomeCaseDf, "../data/2_outcome_age_distribution_case.csv", row.names=FALSE)
write.csv(outcomeDistData, "../data/3_countries_outcome_dist.csv", row.names=FALSE)

