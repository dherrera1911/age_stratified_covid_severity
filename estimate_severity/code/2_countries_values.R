library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(RColorBrewer)
library(wpp2019)
library(rriskDistributions)
source("./functions_auxiliary.R")
data(popM)
data(popF)


###########################################
# Lay out the age stratified outcome data, and the
# estimates of cases for different countries
###########################################


################
# Iceland
################
# Test and outcome data obtained by mail

# Data from https://www.covid.is/data
# Test data https://www.covid.is/data-old till June 14 2020

icelandTestAges <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59",
                         "60-69", "70-79", "80+")
Tested_Iceland <- c(53, 145, 330, 274, 343, 325, 225, 80, 27)
Hospitalized_Iceland <- c(1, 0, 4, 7, 9, 19, 30, 24, 17)
ICU_Iceland <- c(0, 0, 0, 1, 2, 6, 12, 6, 1)
deaths_Iceland <- c(0, 0, 0, 0, 0, 0, 2, 3, 4)

oohDeaths_Iceland <- c(0, 0, 0, 0, 0, 0, 0, 0, 2)
ooiDeaths_Iceland <- c(0, 0, 0, 0, 0, 0, 0, 1, 4)

severeCases_Iceland <- Hospitalized_Iceland + oohDeaths_Iceland
criticalCases_Iceland <- ICU_Iceland + ooiDeaths_Iceland

icelandSeroprevAges <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59",
                         "60-69", "70-79", "80+")
icelandSeroprev <- c(0, 0.32, 0.90, 1.01, 1.44, 0.82, 0.49, 0.28, 0)
icelandSeroprevL <- c(0, 0.1, 0.52, 0.63, 0.98, 0.47, 0.19, 0, 0)
icelandSeroprevH <- c(0.27, 0.74, 1.42, 1.53, 2.04, 1.34, 0.99, 1.24, 2.42)

population_Iceland <- extract_country_population(popM=popM, popF=popF,
                                                 countryName="Iceland",
                                                 ageBins=icelandSeroprevAges)

seroprevCasesIceland <- round(population_Iceland*icelandSeroprev/100)
seroprevCasesIceland_L <- round(population_Iceland*icelandSeroprevL/100)
seroprevCasesIceland_H <- round(population_Iceland*icelandSeroprevH/100)

seroprevRatioIceland <- pmax(1, seroprevCasesIceland/Tested_Iceland)
seroprevRatioIceland_L <- pmax(1, seroprevCasesIceland_L/Tested_Iceland)
seroprevRatioIceland_H <- pmax(1, seroprevCasesIceland_H/Tested_Iceland)

caseRatioIcelandDf <- data.frame(age=icelandSeroprevAges,
   testedCases=Tested_Iceland,
   ratioSeroprev=pmax(1, seroprevRatioIceland),
   ratioSeroprevL=pmax(1, seroprevRatioIceland_L),
   ratioSeroprevH=pmax(1, seroprevRatioIceland_H))


testedPrevalenceIceland <- Tested_Iceland/population_Iceland*100
indSeroprevL <- which(icelandSeroprevL < testedPrevalenceIceland)
icelandSeroprevL_2 <- icelandSeroprevL
icelandSeroprevL_2[indSeroprevL] <- testedPrevalenceIceland[indSeroprevL]
indSeroprev <- which(icelandSeroprev < testedPrevalenceIceland)
icelandSeroprev_2 <- icelandSeroprev
icelandSeroprev_2[indSeroprev] <- testedPrevalenceIceland[indSeroprev]

# absolute numbers are reported
prevalenceL_Iceland2 <- Tested_Iceland/population_Iceland*100

Iceland <- data.frame(Age=icelandSeroprevAges,
                      Population=population_Iceland,
                      Prevalence=icelandSeroprev_2,
                      PrevalenceL=icelandSeroprevL_2,
                      PrevalenceH=icelandSeroprevH,
                      Hospitalized=severeCases_Iceland,
                      ICU=criticalCases_Iceland,
                      Deaths=deaths_Iceland,
                      Type="Seroprevalence",
                      Location="Iceland",
                      EndPointOutcome="2020-06-15",
                      EndPointCases="2020-06-15")

################
# New Zealand
################
# case demographics New Zealand from, January 3
# https://www.health.govt.nz/our-work/diseases-and-conditions/covid-19-novel-coronavirus/covid-19-data-and-statistics/covid-19-case-demographics

# Data obtained by mail
#  https://www.health.govt.nz/our-work/diseases-and-conditions/covid-19-novel-coronavirus/covid-19-data-and-statistics/covid-19-case-demographics#age-gender
age_NZ <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59",
                "60-69", "70-79", "80-89", "90+")
# absolute numbers are reported
Tested_NZ <- c(92, 196, 517, 395, 308, 319, 222, 91, 32, 9)
Hospitalized_NZ <- c(1, 3, 7, 16, 16, 29, 27, 27, 16, 9)
ICU_NZ <-    c(0, 1, 1, 1, 3, 4, 6, 10, 8, 5)
deaths_NZ <- c(0, 0, 0, 0, 0, 2, 3, 7,  8, 5)

population_NZ <- extract_country_population(popM=popM, popF=popF,
                                            countryName="New Zealand",
                                            ageBins=age_NZ)

prevalenceRaw_NZ <- Tested_NZ/population_NZ*100
prevalence_NZ <- prevalenceRaw_NZ*c(seroprevRatioIceland, seroprevRatioIceland[9])
prevalenceL_NZ <- prevalenceRaw_NZ*c(seroprevRatioIceland_L, seroprevRatioIceland_L[9])
prevalenceH_NZ <- prevalenceRaw_NZ*c(seroprevRatioIceland_H, seroprevRatioIceland_H[9])

NewZealand <- data.frame(Age=age_NZ,
                         Population=population_NZ,
                         Prevalence=prevalence_NZ,
                         PrevalenceL=prevalenceL_NZ,
                         PrevalenceH=prevalenceH_NZ,
                         Hospitalized=Hospitalized_NZ,
                         ICU=ICU_NZ,
                         Deaths=deaths_NZ,
                         Type="Testing",
                         Location="New_Zealand",
                         EndPointOutcome="2021-01-13",
                         EndPointCases="2021-01-13")

######################
# South korea
######################
# analysis on disease severity extracted from:
# https://www.thelancet.com/journals/lanwpc/article/PIIS2666-6065(20)30061-4/fulltext
# Has clearly nested statistics, can be useful
# Deaths from Severe COVID-19 Illness: Risk Factors and Its Burden on Critical Care Resources
age_Korea <- c("0-10", "10-19", "20-29", "30-39", "40-49", "50-59",
            "60-69", "70-79", "80+")
Tested_Korea <- c(89, 397, 2174, 780, 1037, 1490, 1007, 517, 312)
ICU_Korea <- c(0, 0, 2, 4, 2, 26, 50, 82, 117)
Hospitalized_Korea <- c(0, 1, 19, 14, 32, 128, 174, 199, 189)
#deaths_Korea <- c(0, 0, 0, 2, 3, 15, 35, 76, 120)
deaths_Korea2 <- c(0, 0, 0, 2, 1, 14, 36, 67, 107)

population_Korea <- extract_country_population(popM=popM, popF=popF,
                                            countryName="Republic of Korea",
                                            ageBins=age_Korea)

prevalenceRaw_Korea <- Tested_Korea/population_Korea*100
prevalence_Korea <- prevalenceRaw_Korea*c(seroprevRatioIceland)
prevalenceL_Korea <- prevalenceRaw_Korea*c(seroprevRatioIceland_L)
prevalenceH_Korea <- prevalenceRaw_Korea*c(seroprevRatioIceland_H)
 
Korea <- data.frame(Age=age_Korea,
                    Population=population_Korea,
                    Prevalence=prevalence_Korea,
                    PrevalenceL=prevalenceL_Korea,
                    PrevalenceH=prevalenceH_Korea,
                    Hospitalized=Hospitalized_Korea,
                    ICU=ICU_Korea,
                    Deaths=deaths_Korea2,
                    Type="Testing",
                    Location="South_Korea",
                    EndPointOutcome="2020-04-30",
                    EndPointCases="2020-04-30")

################
# Spain
################
# Spain cases https://cnecovid.isciii.es/covid19/#documentaci%C3%B3n-y-datos
# Spain seroprevalence Pastor-Barriuso et al https://www.medrxiv.org/content/medrxiv/early/2020/09/25/2020.08.06.20169722.full.pdf
# I take adjusted seroprevalence from Levin et al

age_Spain <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70+")
spainDailyOutcome <- read.csv("../downloaded_data/spain/casos_hosp_uci_def_sexo_edad_provres.csv",
                    stringsAsFactors=FALSE) %>%
  as_tibble(.) %>%
  dplyr::mutate(., fecha=lubridate::date(fecha))

spainDeaths <- dplyr::filter(spainDailyOutcome, fecha <= "2020-05-11" &
                                  grupo_edad != "NC") %>%
  group_by(., grupo_edad) %>%
  summarize(., Deaths=sum(num_def)) %>%
  dplyr::mutate(., proportion=Deaths, age=grupo_edad) %>%
  change_demography_bins(., age_Spain) %>%
  dplyr::mutate(., Deaths=proportion)

spainICU <- dplyr::filter(spainDailyOutcome, fecha <= "2020-05-11" &
                                  grupo_edad != "NC") %>%
  group_by(., grupo_edad) %>%
  summarize(., ICU=sum(num_uci)) %>%
  dplyr::mutate(., proportion=ICU, age=grupo_edad) %>%
  change_demography_bins(., age_Spain) %>%
  dplyr::mutate(., ICU=proportion)

spainHosp <- dplyr::filter(spainDailyOutcome, fecha <= "2020-05-11" &
                                  grupo_edad != "NC") %>%
  group_by(., grupo_edad) %>%
  summarize(., Hospitalized=sum(num_hosp)) %>%
  dplyr::mutate(., proportion=Hospitalized, age=grupo_edad) %>%
  change_demography_bins(., age_Spain) %>%
  dplyr::mutate(., Hospitalized=proportion)

Hospitalized_Spain <- spainHosp$Hospitalized
ICU_Spain <- spainICU$ICU
deaths_Spain <- spainDeaths$Deaths

# Spain seroprevalence from:
# Prevalence of SARS-CoV-2 in Spain (ENE-COVID): a nationwide,
# population-based seroepidemiological study
# date seroprev = April 27 - May 11
seroprev_Spain <- c(2.4, 2.9, 3.4, 3.0, 3.6, 3.7, 3.4, 3.1)
seroprevL_Spain <- c(1.5, 2.3, 2.7, 2.4, 3.0, 3.1, 2.8, 2.5)
seroprevH_Spain <- c(3.4, 3.5, 4.1, 3.6, 4.5, 4.7, 4.5, 5.0)

population_Spain <- extract_country_population(popM=popM, popF=popF,
                                               countryName="Spain",
                                               ageBins=age_Spain)

Spain <- data.frame(Age=age_Spain,
                    Population=population_Spain,
                    Prevalence=seroprev_Spain,
                    PrevalenceL=seroprevL_Spain,
                    PrevalenceH=seroprevH_Spain,
                    Hospitalized=Hospitalized_Spain,
                    ICU=ICU_Spain,
                    Deaths=deaths_Spain,
                    Type="Seroprevalence",
                    Location="Spain",
                    EndPointOutcome="2020-05-11",
                    EndPointCases="2020-05-11")


######################
# Ireland
######################
# Outcome data from: July 16th
# https://www.hpsc.ie/a-z/respiratory/coronavirus/novelcoronavirus/casesinireland/epidemiologyofcovid-19inireland/july2020/
# Seroprevalence data from:
# https://www.hpsc.ie/a-z/respiratory/coronavirus/novelcoronavirus/scopi/SCOPI%20report%20preliminary%20results%20final%20version.pdf

age_Ireland_outcome <- c("15-24", "25-34", "35-44", "45-54", "55-64")
deaths_Ireland <- c(1, 5, 12, 23, 65)
ICU_Ireland <- c(5, 15, 36, 91, 127)
Hospitalized_Ireland <- c(75, 198, 274, 448, 497)

age_Ireland_sero <- c("20-29", "30-39", "40-49", "50-59", "60-69")
seroprev_Ireland <- c(2.3, 1.4, 1.8, 1.5, 1.7)
seroprevL_Ireland <- c(0.8, 0.4, 0.7, 0.5, 0.6)
seroprevH_Ireland <- c(5.1, 3.5, 3.7, 3.6, 3.8)

population_Ireland <- extract_country_population(popM=popM, popF=popF,
                                                 countryName="Ireland",
                                                 ageBins=c("0-14", age_Ireland_outcome, "65+"))
population_Ireland <- population_Ireland[-c(1, length(population_Ireland))]

Ireland <- data.frame(Age=age_Ireland_outcome,
                      Population=population_Ireland,
                      Prevalence=seroprev_Ireland,
                      PrevalenceL=seroprevL_Ireland,
                      PrevalenceH=seroprevH_Ireland,
                      Hospitalized=Hospitalized_Ireland,
                      ICU=ICU_Ireland,
                      Deaths=deaths_Ireland,
                      Type="Seroprevalence",
                      Location="Ireland",
                      EndPointOutcome="2020-07-16",
                      EndPointCases="2020-05-16")


######################
# Sweden
######################
# https://portal.icuregswe.org/siri/report/corona.alderkon
# https://www.icuregswe.org/en/data--results/covid-19-in-swedish-intensive-care/
# On the information panel, look for the "Period" menu on the left
# Seroprevalence from Levin et al, citation:
# 154. Sweden Public Health Authority. COVID-19 Report for Week 24 - COVID-19 veckorapport vecka 24. 2020.
# Dates of seroprev April 27-May24
# Deaths taken from Levin for June 18, ICU taken until May 24
# Deaths taken from O'Driscoll for September
age_Sweden_ICU <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59",
                "60-69", "70-79", "80-89", "90+")
ICU_Sweden <- c(5, 7, 73, 91, 231, 527, 599, 386, 78, 1)

age_Sweden_deaths <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59",
                "60-69", "70-79", "80-89", "90+")
deathsSweden_september <- c(1, 0, 10, 18, 45, 164, 406, 1269, 2436, 1527)

age_Sweden_outcome_Levin <- c("0-19", "20-49", "50-69", "70+")
deaths_Sweden_Levin <- c(1, 63, 504, 4485)
#ICU_Sweden <- c(sum(ICU_Sweden[1:2]), sum(ICU_Sweden[3:5]),
#                   sum(ICU_Sweden[6:7]), sum(ICU_Sweden[8:10]))
deathsSweden <- round(deathsSweden_september*sum(deaths_Sweden_Levin)/sum(deathsSweden_september))

age_Sweden_seroprev <- c("0-19", "20-49", "50-69", "70+")
seroprev_Sweden <- c(5.7, 6.5, 4.8, 3.1)
seroprevL_Sweden <- c(4.5, 5.2, 3.6, 2.1)
seroprevH_Sweden <- c(7.0, 7.8, 6.0, 4.1)

# adjust to ICU ages
seroprev_Sweden_ICU <- c(rep(seroprev_Sweden[1], 2),
                     rep(seroprev_Sweden[2], 3),
                     rep(seroprev_Sweden[3], 2),
                     rep(seroprev_Sweden[4], 3))
seroprevL_Sweden_ICU <- c(rep(seroprevL_Sweden[1], 2),
                     rep(seroprevL_Sweden[2], 3),
                     rep(seroprevL_Sweden[3], 2),
                     rep(seroprevL_Sweden[4], 3))
seroprevH_Sweden_ICU <- c(rep(seroprevH_Sweden[1], 2),
                     rep(seroprevH_Sweden[2], 3),
                     rep(seroprevH_Sweden[3], 2),
                     rep(seroprevH_Sweden[4], 3))


population_Sweden <- extract_country_population(popM=popM, popF=popF,
                                                countryName="Sweden",
                                                ageBins=age_Sweden_ICU)

Sweden <- data.frame(Age=age_Sweden_ICU,
                     Population=population_Sweden,
                     Prevalence=seroprev_Sweden_ICU,
                     PrevalenceL=seroprevL_Sweden_ICU,
                     PrevalenceH=seroprevH_Sweden_ICU,
                     Hospitalized=NA,
                     ICU=ICU_Sweden,
                     Deaths=deathsSweden,
                     Type="Seroprevalence_convenience",
                     Location="Sweden",
                     EndPointOutcome="2020-05-24",
                     EndPointCases="2020-05-24")

######################
# France
######################
# Seroprevalence from Carrat et al. Seroprevalence of SARS-CoV-2 among
# adults in three regions of France following the lockdown and
# associated risk factors: a multicohort study Figure 2
# Seroprevalence endpoint = June 23, beginPoint=May 4
# Hospital data from https://www.santepubliquefrance.fr/regions/ile-de-france/documents/bulletin-regional/2020/covid-19-point-epidemiologique-en-ile-de-france-du-28-mai-2020
# Demography https://www.citypopulation.de/en/france/reg/admin/R11__%C3%AEle_de_france/

age_France_outcome <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59",
                "60-69", "70-79", "80-89", "90+")

ilDeFrance_inHosp <- c(24, 16, 82, 151, 313, 669, 1025, 1296, 1520, 944)
ilDeFrance_inICU <- c(5, 4, 16, 18, 45, 128, 161, 142, 23, 5)
ilDeFrance_hospRecov <- c(274, 166, 758, 1754, 2800, 4302, 4748, 4188, 3861, 1835)
ilDeFrance_hospDead <- c(2, 3, 12, 49, 132, 503, 1039, 1665, 2272, 1412)
ilDeFrance_hospTot <- ilDeFrance_inHosp + ilDeFrance_hospDead + ilDeFrance_hospRecov

Hospitalized_France <- c(ilDeFrance_hospTot[3:8], sum(ilDeFrance_hospTot[9:10]))
age_France_outcome <- c(age_France_outcome[3:8], "80+")

# proportions
age_France_Seroprev <- c("20-24", "25-29", "30-34", "35-39", "40-44", "45-49",
                          "50-54", "55-59", "60-64", "65-69", "70-74", "75-79",
                          "80-84", "85+")
seroprev_France <- c(8.0, 8.5, 16.0, 13.8, 17.1, 14.2, 8.4, 6.1, 8.0, 8.3, 
                        5.3, 5.7, 4.5, 5.7)
seroprevL_France <- c(0.9, 4.2, 12.1, 10.3, 13.5, 11.0, 5.7, 3.2, 4.9, 4.7,
                         3.6, 3.2, 0.9, 0.8)
seroprevH_France <- c(16.1, 12.2, 19.6, 16.8, 20.2, 17.3, 10.5, 8.3, 10.5, 11.0,
                         6.86, 8.3, 8.0, 12.5)

# Average adjacent seroprevalences, to fit demography data
seroprev_France <- colMeans(matrix(seroprev_France, nrow=2, ncol=7))
seroprevL_France <- colMeans(matrix(seroprevL_France, nrow=2, ncol=7))
seroprevH_France <- colMeans(matrix(seroprevH_France, nrow=2, ncol=7))

age_France_pop <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59",
                "60-69", "70-79", "80-89", "90+")
ilDeFrancePop <- c(1589856, 1568051, 1667319, 1762912, 1667546, 1537661,
                   1178725, 815274, 418062, 121023)
ilDeFrancePop <- c(ilDeFrancePop[3:8], sum(ilDeFrancePop[9:10]))

Ile_de_France <- data.frame(Age=age_France_outcome,
                         Population=ilDeFrancePop,
                         Prevalence=seroprev_France,
                         PrevalenceL=seroprevL_France,
                         PrevalenceH=seroprevH_France,
                         Hospitalized=Hospitalized_France,
                         ICU=NA,
                         Deaths=NA,
                         Type="Seroprevalence",
                         Location="Ile_de_France",
                         EndPointOutcome="2020-05-26",
                         EndPointCases="2020-06-23")


######################
# England
######################
# Age and outcome ages are not matched

# ICU data ICNARC report on COVID-19 in critical care 03 July 2020
# Seroprevalence and population from Levin et al
# Death data from Levin et al. and https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-daily-deaths/
# https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-daily-deaths/weekly-total-archive/
# Hospital data https://coronavirus.data.gov.uk/details/download
# Hospital 2 https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-hospital-activity/

# England 1
age_England_ICU <- c("16-30", "30-39", "40-49", "50-59", "60-69",
                     "70-79", "80+")
#totICUEng <- 13025
totICUEng <- 10287
propICUEng <- c(2.2, 5.8, 13.7, 27.7, 29.7, 18.0, 2.9)
ICU_England <- round(propICUEng/100*totICUEng)

age_England_seroprev <- c("0-17", "18-24", "25-34", "35-44", "45-54", "55-64",
                       "65-74", "75+")
seroprev_England <- c(9.2, 7.9, 7.8, 6.1, 6.4, 5.9, 3.2, 3.3)
seroprevL_England <- c(6.2, 7.3, 7.4, 5.7, 6.0, 5.5, 2.8, 2.9)
seroprevH_England <- c(12.2, 8.5, 8.3, 6.6, 6.9, 6.4, 3.6, 3.8)

population_England <- c(12023568, 4746616, 7609363, 7147939, 7623273, 6782486,
                        5576066, 4777650)
# 5 of July deaths
age_deaths_England1 <- c("20-39", "40-59", "60-79", "80+")
deaths_England1 <- c(211, 2253, 11053, 15540)

England1 <- data.frame(Age=age_England_ICU,
                       Population=population_England[-1],
                       Prevalence=seroprev_England[-1],
                       PrevalenceL=seroprevL_England[-1],
                       PrevalenceH=seroprevH_England[-1],
                       Hospitalized=NA,
                       ICU=ICU_England,
                       Deaths=NA,
                       Type="Seroprevalence",
                       Location="England",
                       EndPointOutcome="2020-07-03",
                       EndPointCases="2020-07-13")

age_England_Hosp <- c("0-17", "18-64", "65-84", "85+")
Hospitalized_England <- c(1188, 34584, 45724, 24204)
Hospitalized_England2 <- c(Hospitalized_England[1:2], sum(Hospitalized_England[3:4]))

age_England_death <- c("0-17", "18-24", "25-34", "35-44", "45-54", "55-64",
                       "65-74", "75+")
dethsTot_03July <- 36199
dethsTot_31July <- 36686
deaths_England <- c(11, 30, 131, 394, 1348, 3605, 7631, 38629)
deaths_England2 <- c(deaths_England[1], sum(deaths_England[2:6]),
                      sum(deaths_England[7:8]))

cases_England <- round(population_England*seroprev_England)
casesL_England <- round(population_England*seroprevL_England)
casesH_England <- round(population_England*seroprevH_England)

population_England2 <- c(population_England[1], sum(population_England[2:6]),
                         sum(population_England[7:8]))

seroprev_England2 <- c(cases_England[1], sum(cases_England[2:6]),
                    sum(cases_England[7:8]))/population_England2
seroprevL_England2 <- c(casesL_England[1], sum(casesL_England[2:6]),
                    sum(casesL_England[7:8]))/population_England2
seroprevH_England2 <- c(casesH_England[1], sum(casesH_England[2:6]),
                    sum(casesH_England[7:8]))/population_England2
age_England2 <- c("0-17", "18-64", "65+")

England2 <- data.frame(Age=age_England2,
                       Population=population_England2,
                       Prevalence=seroprev_England2,
                       PrevalenceL=seroprevL_England2,
                       PrevalenceH=seroprevH_England2,
                       Hospitalized=Hospitalized_England2,
                       ICU=NA,
                       Deaths=deaths_England2,
                       Type="Seroprevalence",
                       Location="England",
                       EndPointOutcome="2020-05-26",
                       EndPointCases="2020-06-23")

England <- rbind(England1, England2)

######################
# Netherlands
######################
# Hospital data https://data.rivm.nl/geonetwork/srv/dut/catalog.search#/metadata/2c4357c8-76e4-4662-9574-1deb8a73f724?tab=general
# More death data: https://www.rivm.nl/coronavirus-covid-19/grafieken
# Seroprevalence estimatesfirst week april: Nationwide seroprevalence of SARS-CoV-2 andidentification of risk factors in the general populationof the Netherlands during thefirst epidemic wave
# Seroprevalence 2 https://www.rivm.nl/en/pienter-corona-study/results
# Seroprevalence 2 Associations between measures of social distancing and SARS-CoV-2 seropositivity: a nationwide population-based study in the Netherlands

hospDataNL <- read.csv("../downloaded_data/netherlands/COVID-19_casus_landelijk.csv",
                       stringsAsFactors=FALSE, sep=";") %>%
  as_tibble(.) %>%
  dplyr::filter(., (Hospital_admission=="Yes" | Deceased=="Yes") &
                (lubridate::date(Date_statistics)<="2020-04-11")) %>%
  group_by(., Agegroup) %>%
  summarize(., nHosp=sum((Hospital_admission=="Yes")),
            nDead=sum(Deceased=="Yes"),
            nSevere=sum((Hospital_admission=="Yes" | Deceased=="Yes"))) %>%
  dplyr::mutate(., meanAge=mid_bin_age(Agegroup))

# Deaths for people under 50 are aggregated in this dataset,
# but can be disagregated using other publicly available data,
# as done below
u50Deaths <- dplyr::filter(hospDataNL, Agegroup=="<50")[["nDead"]]
deathsTot_U50 <- c(1, 2, 7, 22, 67)
deathsNL_U50 <- round(deathsTot_U50/sum(deathsTot_U50)*u50Deaths)
hospDataNL <- dplyr::filter(hospDataNL, !(Agegroup %in% c("<50", "Unknown")))
hospDataNL$nDead[which(hospDataNL$meanAge<50)] <- deathsNL_U50

age_Netherlands_outcome <- hospDataNL$Agegroup
deaths_Netherlands <- hospDataNL$nDead
Hospitalized_Netherlands <- hospDataNL$nHosp
Severe_Netherlands <- hospDataNL$nSevere

# Newer study
#age_Netherlands_seroprev <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59",
#                "60-69", "70-79", "80-89", "90+")
#seroprev_Netherlands <- c(0.5, 6.0, 7.8, 3.6, 3.6, 5.0, 4.0, 3.6, 6.0, 8.6)
#seroprevL_Netherlands <- c(0.2, 5.0, 6.8, 2.8, 2.6, 4.0, 3.5, 2.8, 3.2, 3.2)
#seroprevH_Netherlands <- c(1.0, 6.7, 9.9, 5.0, 5.3, 6.2, 4.8, 4.7, 11.5, 14.0)

# Older study
specificity <- 1
sensitivity <- 0.844
age_Netherlands_seroprev <- c("2-12", "13-17", "18-24", "25-39", "40-49",
  "50-59", "60-69", "70+")
nTests <- c(457, 129, 226, 696, 429, 485, 377, 301)
nPositive <- c(4, 1, 12, 24, 11, 8, 7, 7)
rawPrevalence <- nPositive/nTests
rawPrevalenceCI <- binomial_confint(nTests, nPositive)
adjustedPrevalence <- (rawPrevalence+specificity-1)/(sensitivity+specificity-1)
adjustedPrevalenceL <- rawPrevalenceCI$lower/(sensitivity+specificity-1)
adjustedPrevalenceH <- rawPrevalenceCI$upper/(sensitivity+specificity-1)

# repeat last age to match age vecs
adjustedPrevalence <- c(adjustedPrevalence, rep(adjustedPrevalence[8],2))
adjustedPrevalenceL <- c(adjustedPrevalenceL, rep(adjustedPrevalenceL[8],2))
adjustedPrevalenceH <- c(adjustedPrevalenceH, rep(adjustedPrevalenceH[8],2))

population_Netherlands <- extract_country_population(popM=popM, popF=popF,
                                                countryName="Netherlands",
                                                ageBins=age_Netherlands_outcome)

Netherlands <- data.frame(Age=age_Netherlands_outcome,
                          Population=population_Netherlands,
                          Prevalence=adjustedPrevalence*100,
                          PrevalenceL=adjustedPrevalenceL*100,
                          PrevalenceH=adjustedPrevalenceH*100,
                          Hospitalized=Severe_Netherlands,
                          ICU=NA,
                          Deaths=deaths_Netherlands,
                          Type="Seroprevalence",
                          Location="Netherlands",
                          EndPointOutcome="2020-04-11",
                          EndPointCases="2020-05-11")
                          #EndPointOutcome="2020-07-01",
                          #EndPointCases="2020-07-01")


######################
# Georgia, USA
######################
# Hospital data https://www.fultoncountyga.gov/covid-19/epidemiology-reports
# Hospital data2 "Characteristics and Risk Factors for Hospitalization
# and Mortality among Persons with COVID-19 in Atlanta Metropolitan Area"
# Paper data are until 31 May. 
# Georgia pop from Levin et al
# Seroprev: Estimated Community Seroprevalence of SARS-CoV-2 Antibodies — Two Georgia Counties
# webpage with old Georgia files: https://github.com/CEIDatUGA/COVID-19-DATA/tree/master/georgia/ga_GDPH_daily_status_report/from-dashboard/raw-data
# Note: Seroprevalence estimate is from Fulton + Kalb counties, but since
# outcomes are only Fulton, we use the populaiton of Fulton alone to
# estimate number of infections
age_Atlanta_outcome <- c("0-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75+")
Hospitalized_Atlanta <- c(18, 62, 79, 115, 172, 180, 260)
ICU_Atlanta <- c(4, 13, 14, 33, 44, 57, 74)
deaths_Atlanta <- c(2, 3, 2, 16, 33, 69, 170)

# Official hospital count at 31 may coincides with count from paper
nHosp31May <- 888

Hospitalized_Atlanta <- c(sum(Hospitalized_Atlanta[2:3]),
                          sum(Hospitalized_Atlanta[4:5]),
                          sum(Hospitalized_Atlanta[6:7]))
ICU_Atlanta <- c(sum(ICU_Atlanta[2:3]),
                 sum(ICU_Atlanta[4:5]),
                 sum(ICU_Atlanta[6:7]))
deaths_Atlanta <- c(sum(deaths_Atlanta[2:3]),
                    sum(deaths_Atlanta[4:5]),
                    sum(deaths_Atlanta[6:7]))
# remove ranges 0-17 and 65+ which include 0 in the interval,
# following Levin et al
age_Atlanta_seroprev <- c("18-49", "50-64", "65+")
seroprev_Atlanta <- c(3.3, 4.9, 0.7)
seroprevL_Atlanta <- c(1.6, 1.8, 0.1)
seroprevH_Atlanta <- c(6.4, 12.9, 4.5)

# Pupulation of Fulton county, excluding Kalb
# https://censusreporter.org/profiles/05000US13121-fulton-county-ga/
popAges <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59",
             "60-69", "70-79", "80+")
fultonPop <- c(121264, 139338, 170663, 167043, 147087, 136780,
                   100174, 52860, 28728)
fultonPop <- c(sum(fultonPop[3:5]),
               fultonPop[6]+fultonPop[7]/2,
               fultonPop[7]/2+sum(fultonPop[8:9]))

# Atlanta metropolitan area
# https://censusreporter.org/profiles/16000US1304000-atlanta-ga/
#age_Atlanta_pops <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59",
#             "60-69", "70-79", "80+")
#prop <- c(11, 10, 22, 17, 13, 11, 9, 5, 3)
#atlantaPop <- round(prop*506804/100)
#atlantaPop <- c(sum(atlantaPop[3:5]),
#                atlantaPop[6]+atlantaPop[7]/2,
#                atlantaPop[7]/2+sum(atlantaPop[8:9]))

Atlanta <- data.frame(Age=age_Atlanta_seroprev,
                      Population=fultonPop,
                      Prevalence=seroprev_Atlanta,
                      PrevalenceL=seroprevL_Atlanta,
                      PrevalenceH=seroprevH_Atlanta,
                      Hospitalized=Hospitalized_Atlanta,
                      ICU=ICU_Atlanta,
                      Deaths=deaths_Atlanta,
                      Type="Seroprevalence",
                      Location="Atlanta",
                      EndPointOutcome="2020-05-31",
                      EndPointCases="2020-05-03")


######################
# New York City, USA
######################
# New York hospital data at 28 April
# https://www1.nyc.gov/site/doh/covid/covid-19-data-archive.page
# https://www1.nyc.gov/site/doh/covid/covid-19-data-totals.page#rates
# Seroprev data from Rosenberg et al Cumulative incidence and diagnosis
# of SARS-CoV-2 infection in New York
# Demography https://www.baruch.cuny.edu/nycdata/population-geography/pop-demography.htm
age_NYC_outcome <- c("0-17", "18-44", "45-64", "65-74", "75+")
Hospitalized_NYC <- c(273, 5695, 14156, 9240, 11212)
deaths_NYC <- c(5, 482, 2649, 2910, 5772)

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
age_NYC_seroprev <- c("18-34", "35-44", "45-54", "55+")
seroprev_NYC_source <- c(21.8, 23.4, 26.5, 21.5)
seroprevL_NYC_source <- c(19.2, 20.6, 23.8, 19.6)
seroprevH_NYC_source <- c(24.4, 26.2, 29.2, 23.5)

# interpolated seroprev (second value is mean of 1 and 2 in
# original data. Last value is extrapolated
seroprev_NYC <- c(21.8, 22.6, 26.5, 21.5, 21.5)
seroprevL_NYC <- c(19.2, 19.9, 23.8, 19.6, 19.6)
seroprevH_NYC <- c(24.4, 23.5, 29.2, 23.5, 23.5)

NYC <- data.frame(Age=age_NYC_outcome,
                  Population=newYorkPop,
                  Prevalence=seroprev_NYC,
                  PrevalenceL=seroprevL_NYC,
                  PrevalenceH=seroprevH_NYC,
                  Hospitalized=Hospitalized_NYC,
                  ICU=NA,
                  Deaths=deaths_NYC,
                  Type="Seroprevalence",
                  Location="NYC",
                  EndPointOutcome="2020-04-28",
                  EndPointCases="2020-04-28")

######## Canada
# https://health-infobase.canada.ca/covid-19/epidemiological-summary-covid-19-cases.html
# https://covid-19.ontario.ca/data
# https://covid-19.ontario.ca/covid-19-epidemiologic-summaries-public-health-ontario
# https://data.ontario.ca/en/dataset/covid-19-cases-in-hospital-and-icu-by-ontario-health-region
# https://health-infobase.canada.ca/covid-19/epidemiological-summary-covid-19-cases.html?stat=num&measure=deaths#a2
# https://public.tableau.com/app/profile/tphseu/viz/EpidemiologicalSummaryofCOVID-19Cases/EpiSummary
# Seroprevalence from July seroprevalence report

age_Toronto_outcome <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59",
                "60-69", "70-79", "80-89", "90+")

ICU_Toronto_sinceAug1 <- c(11, 13, 41, 88, 163, 308, 493, 393, 190, 35)
ICU_Toronto_allTime <- c(12, 15, 54, 100, 202, 405, 605, 483, 233, 50)
ICU_Toronto <- ICU_Toronto_allTime - ICU_Toronto_sinceAug1

Hospitalized_Toronto_sinceAug1 <- c(58, 62, 325, 476, 646, 1062, 1238, 1351, 1448, 695)
Hospitalized_Toronto_allTime <- c(65, 67, 362, 548, 758, 1269, 1482, 1627, 1824, 877)
Hospitalized_Toronto <- Hospitalized_Toronto_allTime - Hospitalized_Toronto_sinceAug1 + ICU_Toronto

deaths_Toronto_sinceAug1 <- c(0, 0, 9, 23, 42, 133, 291, 441, 738, 523)
deaths_Toronto_allTime <- c(1, 0, 10, 24, 51, 172, 410, 675, 1207, 950)
deaths_Toronto <- deaths_Toronto_allTime - deaths_Toronto_sinceAug1

totalHospOntario <- 4672
totalICUOntario <- 1000
totalDeathsOntario <- 2777
ICU_Ontario <- round(ICU_Toronto*totalICUOntario/sum(ICU_Toronto))
ICU_Ontario <- c(ICU_Ontario[-c(9,10)], sum(ICU_Ontario[c(9,10)]))
Hospitalized_Ontario <- round(Hospitalized_Toronto*totalHospOntario/sum(Hospitalized_Toronto))
Hospitalized_Ontario <- c(Hospitalized_Ontario[-c(9,10)], sum(Hospitalized_Ontario[c(9,10)]))

deaths_Ontario_coarse_ages <- c("0-19", "20-39", "40-59", "60-79", "80+")
deaths_Ontario_coarse <- c(1, 11, 117, 744, 1904)

# Redistribute Ontarios death with ontario proportions within bins
withinBinList <- list(c(1,2), c(3,4), c(5,6), c(7,8))
ratiosToronto <- list()
for (i in c(1:length(withinBinList))) {
  binDeaths <- deaths_Toronto[withinBinList[[i]]]
  ratiosToronto[[i]] <- binDeaths/sum(binDeaths)
}

deaths_Ontario <- round(c(deaths_Ontario_coarse[1]*ratiosToronto[[1]],
                    deaths_Ontario_coarse[2]*(ratiosToronto[[2]]+c(-0.1, 0.1)),
                    deaths_Ontario_coarse[3]*ratiosToronto[[3]],
                    deaths_Ontario_coarse[4]*ratiosToronto[[4]],
                    deaths_Ontario_coarse[5]))

age_Ontario_seroprev <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59",
                "60-69", "70-79", "80+")
seroprev_Ontario_male_H <- c(7.1, 4.2, 3.0, 2.8, 2.1, 1.3, 1.7, 2.0, 2.7)
seroprev_Ontario_female_H <- c(4.2, 1.1, 2.8, 2.3, 2.1, 2.1, 0.9, 1.8, 1.9)
seroprev_Ontario_male_L <- c(0.20, 0.6, 0.5, 0.5, 0.2, 0, 0.1, 0.1, 0)
seroprev_Ontario_female_L <- c(0, 0, 0.5, 0.4, 0.1, 0, 0, 0, 0)
seroprev_Ontario_male <- c(2.1, 2.4, 1.8, 1.7, 1.1, 0.2, 0.9, 0.7, 0.8)
seroprev_Ontario_female <- c(0.0, 0.2, 1.7, 1.4, 0.9, 0.7, 0.2, 0.5, 0.4)

Population_Ontario_male <- c(760313, 840140, 1099167, 1020536, 901607,
                             1011444, 853563, 528877, 263801)
Population_Ontario_female <- c(725357, 805212, 1017927, 1014260, 951829,
                               1030621, 911642, 605684, 392034)

Population_Ontario <- Population_Ontario_male + Population_Ontario_female
propMale_Ontario <- Population_Ontario_male/(Population_Ontario)
propFeale_Ontario <- Population_Ontario_female/(Population_Ontario)

seroprev_Ontario <- seroprev_Ontario_male*propMale_Ontario +
  seroprev_Ontario_female*propFeale_Ontario
seroprevL_Ontario <- seroprev_Ontario_male_L*propMale_Ontario +
  seroprev_Ontario_female_L*propFeale_Ontario
seroprevH_Ontario <- seroprev_Ontario_male_H*propMale_Ontario +
  seroprev_Ontario_female_H*propFeale_Ontario

Ontario <- data.frame(Age=age_Ontario_seroprev,
                  Population=Population_Ontario,
                  Prevalence=seroprev_Ontario,
                  PrevalenceL=seroprevL_Ontario,
                  PrevalenceH=seroprevH_Ontario,
                  Hospitalized=Hospitalized_Ontario,
                  ICU=ICU_Ontario,
                  Deaths=deaths_Ontario,
                  Type="Seroprevalence_convenience",
                  Location="Ontario",
                  EndPointOutcome="2020-07-30",
                  EndPointCases="2020-07-30")

# Utah?
# https://coronavirus.utah.gov/case-counts/
# https://coronavirus.utah.gov/case-counts/


####################################
# Put countries together and export
####################################
countriesDf <- dplyr::bind_rows(Iceland, NewZealand, Korea, Spain, Ireland,
  Sweden, Ile_de_France, England, Netherlands, Atlanta, NYC, Ontario) %>%
  dplyr::mutate(., meanAge=mid_bin_age(Age))

write.csv(countriesDf, "../data/collected_data/locations_serology_data.csv",
          row.names=FALSE)


