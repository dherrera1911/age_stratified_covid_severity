library(dplyr)
library(tidyr)
source("./functions_auxiliary.R")

ageVec <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34",
            "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69",
            "70-74", "75-79", "80-84", "85-89", "90+") 
N <- c(220345, 238068, 256552, 261691, 241006, 228385, 233365,
       222521, 203098, 198773, 194565, 173007, 150775, 131563,
       112395, 93659, 70505, 37426, 18178)
uruPop <- tibble(age=ageVec, N=N, proportion=100*N/sum(N))

ifrEstimates <- read.csv("../data/1_fitted_ifr.csv")
criticalEstimates <- read.csv("../data/1_fitted_critical.csv")
severeEstimates <- read.csv("../data/1_fitted_severe.csv")

ifrEstimates$demProportion <- recalculate_demography(uruPop, ifrEstimates$age)
criticalEstimates$demProportion <- recalculate_demography(uruPop, criticalEstimates$age)
severeEstimates$demProportion <- recalculate_demography(uruPop, severeEstimates$age)

uruIFR <- group_by(ifrEstimates, study) %>%
  summarize(., percentage=sum(IFR*demProportion/100),
            percentageL=sum(IFR_L*demProportion/100),
            percentageH=sum(IFR_H*demProportion/100)) %>%
  ungroup(.) %>%
  dplyr::mutate(., outcome="Death")

uruCritical <- group_by(criticalEstimates, study) %>%
  summarize(., percentage=sum(critical*demProportion/100),
            percentageL=sum(criticalL*demProportion/100),
            percentageH=sum(criticalH*demProportion/100)) %>%
  ungroup(.) %>%
  dplyr::mutate(., outcome="Critical")

uruSevere <- group_by(severeEstimates, study) %>%
  summarize(., percentage=sum(severe*demProportion/100),
            percentageL=sum(severeL*demProportion/100),
            percentageH=sum(severeH*demProportion/100)) %>%
  ungroup(.) %>%
  dplyr::mutate(., outcome="Severe")



ageVecCase <- c("0-15", "15-24", "25-34", "35-44", "45-54", "55-64",
                "65-74", "75+")
casesNov30 <- c(565, 846, 1195, 916, 889, 715, 341, 362)
casesDic18 <- c(1183, 1837, 2558, 1945, 1746, 1321, 647, 646)
caseDiff <- casesDic18-casesNov30
caseDiffProp <- caseDiff/sum(caseDiff)*100
nAges <- length(caseDiffProp)
youngest <- caseDiffProp[1]
oldest <- caseDiffProp[nAges]
middle <- caseDiffProp[-c(1,nAges)]

repYoung <- rep(youngest/3, each=3)
repMiddle <- rep(middle/2, each=2)
repOldest <- rep(oldest/3, each=3)
expandedProps <- c(repYoung, repMiddle, repOldest)
modifiedAgeVec <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34",
            "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69",
            "70-74", "75-79", "80-84", "85+") 

uruCase <- tibble(age=modifiedAgeVec, proportion=expandedProps)
ifrEstimates$demProportion <- recalculate_demography(uruCase, ifrEstimates$age)
criticalEstimates$demProportion <- recalculate_demography(uruCase,
                                                          criticalEstimates$age)

uruIFRCase <- group_by(ifrEstimates, study) %>%
  summarize(., percentage=sum(IFR*demProportion/100),
            percentageL=sum(IFR_L*demProportion/100),
            percentageH=sum(IFR_H*demProportion/100)) %>%
  ungroup(.) %>%
  dplyr::mutate(., outcome = "Death")

uruCriticalCase <- group_by(criticalEstimates, study) %>%
  summarize(., percentage=sum(critical*demProportion/100),
            percentageL=sum(criticalL*demProportion/100),
            percentageH=sum(criticalH*demProportion/100)) %>%
  ungroup(.) %>%
  dplyr::mutate(., outcome = "Critical")

