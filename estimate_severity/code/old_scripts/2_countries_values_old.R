library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(RColorBrewer)
library(wpp2019)
source("./functions_auxiliary.R")
data(popM)
data(popF)

###########################################
# Input the age stratified covid severity and critical data
# for different countries into a tidy dataframe
###########################################

################
# Uruguay
################
# Official data from:
# https://www.gub.uy/ministerio-salud-publica/tematica/boletines-epidemiologicos
# Report form December 18

# repoted data
ageVecUru <- c("0-14", "15-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75+")
uruN18Dec <- c(1183, 1837, 2558, 1945, 1746, 1321, 647, 646)
uruProp18Dec <- 100*uruN18Dec/sum(uruN18Dec)
uruN11Dec <- c(905, 1345, 1888, 1405, 1279, 1013, 479, 480)
uruProp11Dec <- 100*uruN11Dec/sum(uruN11Dec)
uruN4Dec <- c(648, 981, 1391, 1058, 1015, 821, 383, 395)
uruProp4Dec <- 100*uruN4Dec/sum(uruN4Dec)

uruDeaths <- c(0, 0, 0, 0, 3, 18, 29, 59)
uruICU <- c(0, 0, 2, 5, 16, 33, 45, 36)
uruProportionHosp <- c(0, 0.3, 1.1, 2.2, 4.1, 7.4, 15.0, 21.4)
uruHosp <- round(uruProportionHosp*uruN18Dec/100)
uruCount11DecDf <- data.frame(country="Uruguay", age=ageVecUru, Cases=uruN11Dec,
                         CaseProp=uruProp11Dec, Hosp=uruHosp,
                         ICU=uruICU, Deaths=uruDeaths)
uruCount4DecDf <- data.frame(country="Uruguay", age=ageVecUru, Cases=uruN4Dec,
                         CaseProp=uruProp4Dec, Hosp=uruHosp,
                         ICU=uruICU, Deaths=uruDeaths)
# calculated confints
uruDf <- count_df_confints(uruCount11DecDf)
uruDfDeaths <- count_df_confints(uruCount4DecDf)
uruDf$propDeaths <- uruDfDeaths$propDeaths
uruDf$propDeaths_L <- uruDfDeaths$propDeaths_L
uruDf$propDeaths_H <- uruDfDeaths$propDeaths_H
uruDf$Cases <- uruN18Dec
uruDf$CaseProp <- uruProp18Dec

################
# New Zealand
################
# case demographics New Zealand from, January 3
# https://www.health.govt.nz/our-work/diseases-and-conditions/covid-19-novel-coronavirus/covid-19-data-and-statistics/covid-19-case-demographics

ageVecNZ <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59",
                "60-69", "70-79", "80-89", "90+")
nzN <- c(92, 196, 517, 395, 308, 319, 222, 91, 32, 9)
#nzProp <- 100*nzN/sum(nzN)
nzDeaths <- c(0, 0, 0, 0, 0, 2,  3,  7,  8, 5)
nzICU <-    c(0, 1, 1, 1, 3, 4,  6,  10, 8, 5)
nzHosp <-   c(1, 3, 7, 16, 16, 29, 27, 27, 16, 9)
nzCountDf <- data.frame(country="New Zealand", age=ageVecNZ, Cases=nzN,
                         Hosp=nzHosp, ICU=nzICU, Deaths=nzDeaths)
# calculated confints
nzDf <- count_df_confints(nzCountDf)

################
# Iceland
################
# Data from https://www.covid.is/data
ageVecIceland <- c("0-29", "30-39", "40-49", "50-59", "60-69", "70-79",
                "80-89", "90+")
icelandN <- c(2058, 821, 847, 714, 503, 200, 90, 34)
icelandProp <- 100*icelandN/sum(icelandN)
icelandDeaths <- c(0, 1, 0, 0, 3, 5, 14, 6)
icelandICU <- 53
icelandHosp <- 316
icelandCountDf <- data.frame(country="Iceland", age=ageVecIceland,
                       Cases=icelandN, CaseProp=icelandProp,
                       Deaths=icelandDeaths)
icelandDf <- count_df_confints(icelandCountDf)

######################
# South korea
######################
# analysis on disease severity extracted from:
# https://www.thelancet.com/journals/lanwpc/article/PIIS2666-6065(20)30061-4/fulltext
ageVecKorea <- c("0-10", "10-19", "20-29", "30-39", "40-49", "50-59",
            "60-69", "70-79", "80+")
koreaN <- c(89, 397, 2174, 780, 1037, 1490, 1007, 517, 312)
koreaProp <- koreaN/sum(koreaN)*100
koreaICU <- c(0, 0, 2, 4, 2, 26, 50, 82, 117)
koreaHosp <- c(0, 1, 19, 14, 32, 128, 174, 199, 189)
koreaCountDf <- data.frame(country="South Korea", age=ageVecKorea,
                       Cases=koreaN, CaseProp=koreaProp,
                       ICU=koreaICU, Hosp=koreaHosp)
koreaDf <- count_df_confints(koreaCountDf)

################
# Spain
################
# Spain cases https://cnecovid.isciii.es/covid19/#documentaci%C3%B3n-y-datos
ageVecSpain <- c("0-19", "20-34", "35-49", "50-64", "65+")
spainDailyOutcome <- read.csv("../downloaded_data/spain/casos_hosp_uci_def_sexo_edad_provres.csv",
                    stringsAsFactors=FALSE) %>%
  as_tibble(.) %>%
  dplyr::mutate(., fecha=lubridate::date(fecha))

spainDeaths <- dplyr::filter(spainDailyOutcome, fecha <= "2020-05-11" &
                                  grupo_edad != "NC") %>%
  group_by(., grupo_edad) %>%
  summarize(., Deaths=sum(num_def)) %>%
  dplyr::mutate(., proportion=Deaths, age=grupo_edad) %>%
  change_demography_bins(., ageVecSpain) %>%
  dplyr::mutate(., Deaths=proportion)

spainICU <- dplyr::filter(spainDailyOutcome, fecha <= "2020-05-11" &
                                  grupo_edad != "NC") %>%
  group_by(., grupo_edad) %>%
  summarize(., ICU=sum(num_uci)) %>%
  dplyr::mutate(., proportion=ICU, age=grupo_edad) %>%
  change_demography_bins(., ageVecSpain) %>%
  dplyr::mutate(., ICU=proportion)

# change ICU for oldest age for deaths
spainICU$ICU[nrow(spainICU)] <- spainDeaths$Deaths[nrow(spainDeaths)]

spainHosp <- dplyr::filter(spainDailyOutcome, fecha <= "2020-05-11" &
                                  grupo_edad != "NC") %>%
  group_by(., grupo_edad) %>%
  summarize(., Hospitalized=sum(num_hosp)) %>%
  dplyr::mutate(., proportion=Hospitalized, age=grupo_edad) %>%
  change_demography_bins(., ageVecSpain) %>%
  dplyr::mutate(., Hospitalized=proportion)

# Spain seroprevalence from:
# Prevalence of SARS-CoV-2 in Spain (ENE-COVID): a nationwide,
# population-based seroepidemiological study
# date seroprev = April 27 - May 11
spainSeroprev <- c(3.8, 5.0, 4.9, 4.7, 4.5)
spainSeroprevL <- c(4.3, 4.3, 4.1, 3.8)
spainSeroprevH <- c(4.6, 5.8, 5.5, 5.3, 5.3)

spainPopM <- dplyr::filter(popM, name=="Spain") %>%
  dplyr::select(., age, "2020")
spainPopF <- dplyr::filter(popF, name=="Spain") %>%
  dplyr::select(age, "2020")
spainPop <- data.frame(age=spainPopM$age,
                       pop=(spainPopM[["2020"]]+spainPopF[["2020"]])*1000)
spainPop$proportion <- spainPop$pop # rename pop to proportion so function works 
spainPop <- change_demography_bins(spainPop, ageVecSpain) %>%
  rename(., pop=proportion)

spainN <- round(spainPop$pop*spainSeroprev/100)
spainProp <- spainN/sum(spainN)*100

spainCountDf <- data.frame(country="Spain", age=ageVecSpain,
                       Cases=spainN, CaseProp=spainProp,
                       Deaths=spainDeaths$Deaths, ICU=spainICU$ICU,
                       Hosp=spainHosp$Hospitalized)
spainDf <- count_df_confints(spainCountDf)

######################
# Ireland
######################
# Outcome data from: July 6th
# https://www.hpsc.ie/a-z/respiratory/coronavirus/novelcoronavirus/casesinireland/epidemiologyofcovid-19inireland/july2020/
# Seroprevalence data from:
# https://www.hpsc.ie/a-z/respiratory/coronavirus/novelcoronavirus/scopi/SCOPI%20report%20preliminary%20results%20final%20version.pdf

outcomeAgesIreland <- c("15-24", "25-34", "35-44", "45-54", "55-64")
irelandDeaths <- c(1, 5, 11, 23, 64)
irelandICU <- c(5, 15, 35, 92, 126)
irelandHosp <- c(72, 197, 267, 447, 492)

ageVecIreland <- c("20-29", "30-39", "40-49", "50-59", "60-69")
irelandSeroprev <- c(2.3, 1.4, 1.8, 1.5, 1.7)
irelandSeroprevL <- c(0.3, 0.8, 0.4, 0.7, 0.5, 0.6)
irelandSeroprevH <- c(2.7, 4.3, 5.1, 3.5, 3.7, 3.6, 3.8)

irelandPopM <- dplyr::filter(popM, name=="Ireland") %>%
  dplyr::select(., age, "2020")
irelandPopF <- dplyr::filter(popF, name=="Ireland") %>%
  dplyr::select(age, "2020")
irelandPop <- data.frame(age=irelandPopM$age,
                       pop=(irelandPopM[["2020"]]+irelandPopF[["2020"]])*1000)
irelandPop$proportion <- irelandPop$pop # rename pop to proportion so function works 
irelandPop <- change_demography_bins(irelandPop, c("0-14", outcomeAgesIreland, "65+")) %>%
  rename(., pop=proportion) %>%
  dplyr::filter(., !(age %in% c("0-14", "65+")))

irelandN <- round(irelandPop$pop*irelandSeroprev/100)
irelandProp <- irelandN/sum(irelandN)*100

irelandCountDf <- data.frame(country="Ireland", age=ageVecIreland,
                       Cases=irelandN, CaseProp=irelandProp,
                       Deaths=irelandDeaths, ICU=irelandICU,
                       Hosp=irelandHosp)
irelandDf <- count_df_confints(irelandCountDf)

######################
# Portugal
######################
# Outcome data from: July 6th
# https://covid19.min-saude.pt/relatorio-de-situacao/
# Seroprevalence data from:
# http://www.insa.min-saude.pt/resultados-preliminares-do-primeiro-inquerito-serologico-nacional-covid-19-relatorio/

#ageVecPortugal <- c("1-9", "10-19", "20-39", "40-59", "60+")
#portugalSeroprev <- c(2.9, 2.2, 2.9, 3.2, 2.9)
#portugalSeroprevL <- c(1.3, 0.8, 1.2, 1.5, 1.5)
#portugalSeroprevH <- c(6.2, 5.5, 7.0, 6.7, 5.3)
#
#portugalPopM <- dplyr::filter(popM, name=="Portugal") %>%
#  dplyr::select(., age, "2020")
#portugalPopF <- dplyr::filter(popF, name=="Portugal") %>%
#  dplyr::select(age, "2020")
#portugalPop <- data.frame(age=portugalPopM$age,
#                       pop=(portugalPopM[["2020"]]+portugalPopF[["2020"]])*1000)
#portugalPop$proportion <- portugalPop$pop # rename pop to proportion so function works 
#portugalPop <- change_demography_bins(portugalPop, c("0-19", ageVecPortugal, "70+")) %>%
#  rename(., pop=proportion) %>%
#  dplyr::filter(., !(age %in% c("0-19", "70+")))
#
#portugalN <- round(portugalPop$pop*portugalSeroprev/100)
#portugalProp <- portugalN/sum(portugalN)*100
#
#portugalCountDf <- data.frame(country="Portugal", age=ageVecPortugal,
#                       Cases=portugalN, CaseProp=portugalProp,
#                       Deaths=portugalDeaths, ICU=portugalICU,
#                       Hosp=portugalHosp)
#portugalDf <- count_df_confints(portugalCountDf)


######################
# Sweden
######################
# https://portal.icuregswe.org/siri/report/corona.alderkon
# https://www.icuregswe.org/en/data--results/covid-19-in-swedish-intensive-care/
# Seroprevalence from Levin et al, citation:
# 154. Sweden Public Health Authority. COVID-19 Report for Week 24 - COVID-19 veckorapport vecka 24. 2020.
# Dates of seroprev April 27-May24
# Deaths taken from Levin for June 18, ICU taken until 8 June
ageVecICUSwe <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59",
                "60-69", "70-79", "80-89", "90+")
swedenICUOrig <- c(6, 10, 82, 100, 256, 589, 689, 441, 89, 2)

outcomeAgesSweden <- c("0-19", "20-49", "50-69", "70+")
swedenDeaths <- c(1, 63, 504, 4485)
swedenICU <- c(sum(swedenICUOrig[1:2]), sum(swedenICUOrig[3:5]),
                   sum(swedenICUOrig[6:7]), sum(swedenICUOrig[8:10]))

ageVecSweden <- c("0-19", "20-49", "50-69", "70+")
swedenSeroprev <- c(5.7, 6.5, 4.8, 3.1)
swedenSeroprevL <- c(4.5, 5.2, 3.6, 2.1)
swedenSeroprevH <- c(7.0, 7.8, 6.0, 4.1)

swedenPopM <- dplyr::filter(popM, name=="Sweden") %>%
  dplyr::select(., age, "2020")
swedenPopF <- dplyr::filter(popF, name=="Sweden") %>%
  dplyr::select(age, "2020")
swedenPop <- data.frame(age=swedenPopM$age,
                       pop=(swedenPopM[["2020"]]+swedenPopF[["2020"]])*1000)
swedenPop$proportion <- swedenPop$pop # rename pop to proportion so function works 
swedenPop <- change_demography_bins(swedenPop, ageVecSweden) %>%
  rename(., pop=proportion)

swedenN <- round(swedenPop$pop*swedenSeroprev/100)
swedenProp <- swedenN/sum(swedenN)*100

swedenCountDf <- data.frame(country="Sweden", age=ageVecSweden,
                       Cases=swedenN, CaseProp=swedenProp,
                       Deaths=swedenDeaths, ICU=swedenICU)
swedenDf <- count_df_confints(swedenCountDf)

######################
# France
######################
# Seroprevalence from Carrat et al. Seroprevalence of SARS-CoV-2 among
# adults in three regions of France following the lockdown and
# associated risk factors: a multicohort study Figure 2
# Hospital data from https://www.santepubliquefrance.fr/regions/ile-de-france/documents/bulletin-regional/2020/covid-19-point-epidemiologique-en-ile-de-france-du-28-mai-2020
# Demography https://www.citypopulation.de/en/france/reg/admin/R11__%C3%AEle_de_france/

ageVecFranceICU <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59",
                "60-69", "70-79", "80-89", "90+")
ilDeFrance_inHosp <- c(26, 18, 96, 182, 360, 775, 1157, 1448, 1710, 1075)
ilDeFrance_inICU <- c(6, 6, 16, 24, 50, 167, 194, 161, 25, 6)
ilDeFrance_hospRecov <- c(263, 160, 738, 1719, 2734, 4186, 4577, 4027, 3613, 1669)
ilDeFrance_hospDead <- c(2, 3, 12, 47, 128, 490, 1019, 1633, 2221, 1375)
ilDeFrance_hospTot <- ilDeFrance_inHosp + ilDeFrance_hospDead + ilDeFrance_hospRecov

franceHospitalVec <- c(ilDeFrance_hospTot[3:8], sum(ilDeFrance_hospTot[9:10]))

# proportions
ageVecFranceSeroprev <- c("20-24", "25-29", "30-34", "35-39", "40-44", "45-49",
                          "50-54", "55-59", "60-64", "65-69", "70-74", "75-79",
                          "80-84", "85+")
ilDeFranceSeroprev <- c(8.0, 8.5, 16.0, 13.8, 17.1, 14.2, 8.4, 6.1, 8.0, 8.3, 
                        5.3, 5.7, 4.5, 5.7)
ilDeFranceSeroprevL <- c(0.9, 4.2, 12.1, 10.3, 13.5, 11.0, 5.7, 3.2, 4.9, 4.7,
                         3.6, 3.2, 0.9, 0.8)
ilDeFranceSeroprevH <- c(16.1, 12.2, 19.6, 16.8, 20.2, 10.5, 8.3, 10.5, 11.0,
                         6.86, 8.3, 8.0, 12.5)

# quick and dirty calculation before data is shared
ilDeFranceSeroprev <- colMeans(matrix(ilDeFranceSeroprev, nrow=2, ncol=7))


ageVecFrancePop <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59",
                "60-69", "70-79", "80-89", "90+")
ilDeFrancePop <- c(1589856, 1568051, 1667319, 1762912, 1667546, 1537661,
                   1178725, 815274, 418062, 121023)

ilDeFrancePop <- c(ilDeFrancePop[3:8], sum(ilDeFrancePop[9:10]))

# match age stratifications across the data, and put together
franceN <- round(ilDeFrancePop*ilDeFranceSeroprev/100)
franceProp <- franceN/sum(franceN)*100

ageVecFrance <- c("20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")
franceCountDf <- data.frame(country="Il de France", age=ageVecFrance,
                       Cases=franceN, CaseProp=franceProp,
                       Hosp=franceHospitalVec)
franceDf <- count_df_confints(franceCountDf)


######################
# England
######################
# ICU data ICNARC report on COVID-19 in critical care 03 July 2020
# Population from Levin et al
# Hospital data https://coronavirus.data.gov.uk/details/download
icuAgeVec <- c("16-30", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")
totICUEng <- 13025
propICUEng <- c(2.2, 5.8, 13.7, 27.7, 29.7, 18.0, 2.9)
nICUEng <- round(propICUEng/100*totICUEng)

seroprevAgeVecEng <- c("18-24", "25-34", "35-44", "45-54", "55-64",
                       "65-74", "75+")
englandSeroprev <- c(7.9, 7.8, 6.1, 6.4, 5.9, 3.2, 3.3)
englandSeroprevL <- c(7.3, 7.4, 5.7, 6.0, 5.5, 2.8, 2.9)
englandSeroprevH <- c(8.5, 8.3, 6.6, 6.9, 6.4, 3.6, 3.8)

englandPop <- c(4746616, 7609363, 7147939, 7623273, 6782486, 5576066,
                4777650)

englandN <- round(englandPop*englandSeroprev/100)
englandProp <- englandN/sum(englandN)*100

englandCountDf <- data.frame(country="England", age=seroprevAgeVecEng,
                       Cases=englandN, CaseProp=englandProp,
                       ICU=nICUEng)
englandDf <- count_df_confints(englandCountDf)


######################
# Netherlands
######################
# Hospital data https://data.rivm.nl/geonetwork/srv/dut/catalog.search#/metadata/2c4357c8-76e4-4662-9574-1deb8a73f724?tab=general
# More death data: https://www.rivm.nl/coronavirus-covid-19/grafieken
# Seroprevalence estimatesfirst week april: Nationwide seroprevalence of SARS-CoV-2 andidentification of risk factors in the general populationof the Netherlands during thefirst epidemic wave
# Seroprevalence 2 https://www.rivm.nl/en/pienter-corona-study/results

hospDataNL <- read.csv("../downloaded_data/netherlands/COVID-19_casus_landelijk.csv",
                       stringsAsFactors=FALSE, sep=";") %>%
  as_tibble(.) %>%
  dplyr::filter(., (Hospital_admission=="Yes" | Deceased=="Yes") &
                (lubridate::date(Date_statistics)<="2020-07-01")) %>%
  group_by(., Agegroup) %>%
  summarize(., nHosp=sum((Hospital_admission=="Yes" | Deceased=="Yes")),
            nDead=sum(Deceased=="Yes"),
            nonHospDead=sum(Deceased=="Yes" & Hospital_admission=="No")) %>%
  dplyr::filter(., !(Agegroup %in% c("<50", "Unknown")))

seroprevAgeVecNL <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59",
                "60-69", "70-79", "80-89", "90+")
netherlandsSeroprev <- c(0.5, 6.0, 7.8, 3.6, 3.6, 5.0, 4.0, 3.6, 6.0, 8.6)

netherlandsPopM <- dplyr::filter(popM, name=="Netherlands") %>%
  dplyr::select(., age, "2020")
netherlandsPopF <- dplyr::filter(popF, name=="Netherlands") %>%
  dplyr::select(age, "2020")
netherlandsPop <- data.frame(age=netherlandsPopM$age,
                       pop=(netherlandsPopM[["2020"]]+netherlandsPopF[["2020"]])*1000)
netherlandsPop$proportion <- netherlandsPop$pop # rename pop to proportion so function works 
netherlandsPop <- change_demography_bins(netherlandsPop, c(seroprevAgeVecNL)) %>%
  rename(., pop=proportion)

netherlandsN <- round(netherlandsPop$pop*netherlandsSeroprev/100)
netherlandsProp <- netherlandsN/sum(netherlandsN)*100

netherlandsCountDf <- data.frame(country="Netherlands", age=seroprevAgeVecNL,
                       Cases=netherlandsN, CaseProp=netherlandsProp,
                       Hosp=hospDataNL$nHosp, Deaths=hospDataNL$nDead)
netherlandsDf <- count_df_confints(netherlandsCountDf)


######################
# Georgia, USA
######################
# Hospital data https://www.fultoncountyga.gov/covid-19/epidemiology-reports
# Hospital data2 "Characteristics and Risk Factors for Hospitalization
# and Mortality among Persons with COVID-19 in Atlanta Metropolitan Area"
# Paper data are until 31 May. 
# Georgia pop from Levin et al
icuAgeVec <- c("0-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75+")
georgiaHosp <- c(18, 62, 79, 115, 172, 180, 260)
georgiaICU <- c(4, 13, 14, 33, 44, 57, 74)
georgiaDeaths <- c(2, 3, 2, 16, 33, 69, 170)
nHosp31May <- 888

georgiaHosp <- c(sum(georgiaHosp[2:3]), sum(georgiaHosp[4:5]))
georgiaICU <- c(sum(georgiaICU[2:3]), sum(georgiaICU[4:5]))
# remove ranges 0-17 and 65+ which include 0 in the interval,
# following Levin et al
seroprevAgeVecGeorgia <- c("18-49", "50-64")
georgiaSeroprev <- c(3.3, 4.9)
georgiaSeroprevL <- c(1.6, 6.4)
georgiaSeroprevH <- c(1.8, 12.9)

georgiaPop <- c(867468, 327713)

georgiaN <- round(georgiaPop*georgiaSeroprev/100)
georgiaProp <- georgiaN/sum(georgiaN)*100

georgiaCountDf <- data.frame(country="Georgia", age=seroprevAgeVecGeorgia,
                       Cases=georgiaN, CaseProp=georgiaProp,
                       Hosp=georgiaHosp, ICU=georgiaICU)
georgiaDf <- count_df_confints(georgiaCountDf)


######################
# New York City, USA
######################
# New York hospital data at 28 April
# https://www1.nyc.gov/site/doh/covid/covid-19-data-archive.page
# https://www1.nyc.gov/site/doh/covid/covid-19-data-totals.page#rates
# Seroprev data from Rosenberg et al Cumulative incidence and diagnosis
# of SARS-CoV-2 infection in New York
# Demography https://www.baruch.cuny.edu/nycdata/population-geography/pop-demography.htm

hospAgeVecNY <- c("0-17", "18-44", "45-64", "65-74", "75+")
newyorkHosp <- c(273, 5695, 14156, 9240, 11212)
newyorkDeaths <- c(5, 482, 2649, 2910, 5772)

newYorkDemAgeVec <- c("0-5", "5-9", "10-14", "15-19",
                      "20-24", "25-29", "30-34", "35-39", "40-44",
                      "45-49", "50-54", "55-59", "60-64", "65-69",
                      "70-74", "75-79", "80-84", "85+")
newYorkPop <- c(553277, 496622, 467016, 466963, 588268, 804436,
                728985, 625351, 550081, 553115, 553489, 530749,
                464246, 388657, 265894, 199912, 139369, 161243)

newYorkPop <- c(sum(newYorkPop[1:4]), sum(newYorkPop[5:9]),
                sum(newYorkPop[10:13]), sum(newYorkPop[14:15]),
                sum(newYorkPop[16:18]))

# seroprev in paper
ageVecSeroprevNY <- c("18-34", "35-44", "45-54", "55+")
newyorkSeroprev <- c(21.8, 23.4, 26.5, 21.5)
newyorkSeroprevL <- c(19.2, 20.6, 23.8, 19.6)
newyorkSeroprevH <- c(24.4, 26.2, 29.2, 23.5)

# extrapolated seroprev
newyorkSeroprev <- c(21.8, 22.6, 26.5, 21.5, 21.5)

newyorkN <- round(newYorkPop*newyorkSeroprev/100)
newyorkProp <- newyorkN/sum(newyorkN)*100

newyorkCountDf <- data.frame(country="New York, USA", age=hospAgeVecNY,
                       Cases=newyorkN, CaseProp=newyorkProp,
                       Hosp=newyorkHosp, Deaths=newyorkDeaths)
newyorkDf <- count_df_confints(newyorkCountDf)


####################################
# Put countries together and export
####################################
countriesDf <- dplyr::bind_rows(nzDf, koreaDf, icelandDf,
                                spainDf, irelandDf, swedenDf, franceDf,
                                englandDf, netherlandsDf, georgiaDf,
                                newyorkDf)

write.csv(countriesDf, "../data/2_countries_outcomes.csv", row.names=FALSE)


#write.csv(uruCovid, "../data/2_uru_covid_incidence.csv", row.names=FALSE)
#write.csv(uruIFR, "../data/2_uru_ifr_data.csv", row.names=FALSE)
#write.csv(uruICU, "../data/2_uru_icu_data.csv", row.names=FALSE)
#write.csv(uruHospital, "../data/2_uru_hospital_data.csv", row.names=FALSE)

