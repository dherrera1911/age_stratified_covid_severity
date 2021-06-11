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
# Lay out the age stratified outcome data, and the
# estimates of cases for different countries
###########################################


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
ICU_NZ <-    c(0, 1, 1, 1, 3, 4, 6,  10, 8, 5)
deaths_NZ <- c(0, 0, 0, 0, 0, 2, 3,  7,  8, 5)

NewZealand <- data.frame(Age=age_NZ,
                         Cases=Tested_NZ,
                         CasesL=NA,
                         CasesH=NA,
                         Hospitalized=Hospitalized_NZ,
                         ICU=ICU_NZ,
                         Deaths=deaths_NZ,
                         Type="Testing",
                         Location="New_Zealand",
                         EndPointOutcome="2021-01-13",
                         EndPointCases="2021-01-13")

################
# Iceland
################
# Data from https://www.covid.is/data
#ageVecIceland <- c("0-29", "30-39", "40-49", "50-59", "60-69", "70-79",
#                "80-89", "90+")
#icelandN <- c(2058, 821, 847, 714, 503, 200, 90, 34)
#icelandProp <- 100*icelandN/sum(icelandN)
#icelandDeaths <- c(0, 1, 0, 0, 3, 5, 14, 6)
#icelandICU <- 53
#icelandHosp <- 316
#icelandCountDf <- data.frame(country="Iceland", age=ageVecIceland,
#                       Cases=icelandN, CaseProp=icelandProp,
#                       Deaths=icelandDeaths)
#icelandDf <- count_df_confints(icelandCountDf)

######################
# South korea
######################
# analysis on disease severity extracted from:
# https://www.thelancet.com/journals/lanwpc/article/PIIS2666-6065(20)30061-4/fulltext
# Has clearly nested statistics, can be useful
age_Korea <- c("0-10", "10-19", "20-29", "30-39", "40-49", "50-59",
            "60-69", "70-79", "80+")
Tested_Korea <- c(89, 397, 2174, 780, 1037, 1490, 1007, 517, 312)
ICU_Korea <- c(0, 0, 2, 4, 2, 26, 50, 82, 117)
Hospitalized_Korea <- c(0, 1, 19, 14, 32, 128, 174, 199, 189)

Korea <- data.frame(Age=age_Korea,
                         Cases=Tested_Korea,
                         CasesL=NA,
                         CasesH=NA,
                         Hospitalized=Hospitalized_Korea,
                         ICU=ICU_Korea,
                         Deaths=NA,
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

spainPopM <- dplyr::filter(popM, name=="Spain") %>%
  dplyr::select(., age, "2020")
spainPopF <- dplyr::filter(popF, name=="Spain") %>%
  dplyr::select(age, "2020")
spainPop <- data.frame(age=spainPopM$age,
                       pop=(spainPopM[["2020"]]+spainPopF[["2020"]])*1000)
spainPop$proportion <- spainPop$pop # rename pop to proportion so function works 
spainPop <- change_demography_bins(spainPop, age_Spain) %>%
  rename(., pop=proportion)

cases_Spain <- round(spainPop$pop*seroprev_Spain/100)
casesL_Spain <- round(spainPop$pop*seroprevL_Spain/100)
casesH_Spain <- round(spainPop$pop*seroprevH_Spain/100)

Spain <- data.frame(Age=age_Spain,
                         Cases=cases_Spain,
                         CasesL=casesL_Spain,
                         CasesH=casesH_Spain,
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

irelandPopM <- dplyr::filter(popM, name=="Ireland") %>%
  dplyr::select(., age, "2020")
irelandPopF <- dplyr::filter(popF, name=="Ireland") %>%
  dplyr::select(age, "2020")
irelandPop <- data.frame(age=irelandPopM$age,
                       pop=(irelandPopM[["2020"]]+irelandPopF[["2020"]])*1000)
irelandPop$proportion <- irelandPop$pop # rename pop to proportion so function works 
irelandPop <- change_demography_bins(irelandPop, c("0-14", age_Ireland_outcome, "65+")) %>%
  rename(., pop=proportion) %>%
  dplyr::filter(., !(age %in% c("0-14", "65+")))

cases_Ireland <- round(irelandPop$pop*seroprev_Ireland/100)
casesL_Ireland <- round(irelandPop$pop*seroprevL_Ireland/100)
casesH_Ireland <- round(irelandPop$pop*seroprevH_Ireland/100)

Ireland <- data.frame(Age=age_Ireland_outcome,
                         Cases=cases_Ireland,
                         CasesL=casesL_Ireland,
                         CasesH=casesH_Ireland,
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
age_Sweden_ICU <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59",
                "60-69", "70-79", "80-89", "90+")
swedenICUOrig <- c(5, 7, 73, 91, 231, 527, 599, 386, 78, 1)

age_Sweden_outcome <- c("0-19", "20-49", "50-69", "70+")
deaths_Sweden <- c(1, 63, 504, 4485)
ICU_Sweden <- c(sum(swedenICUOrig[1:2]), sum(swedenICUOrig[3:5]),
                   sum(swedenICUOrig[6:7]), sum(swedenICUOrig[8:10]))

age_Sweden_seroprev <- c("0-19", "20-49", "50-69", "70+")
seroprev_Sweden <- c(5.7, 6.5, 4.8, 3.1)
seroprevL_Sweden <- c(4.5, 5.2, 3.6, 2.1)
seroprevH_Sweden <- c(7.0, 7.8, 6.0, 4.1)

swedenPopM <- dplyr::filter(popM, name=="Sweden") %>%
  dplyr::select(., age, "2020")
swedenPopF <- dplyr::filter(popF, name=="Sweden") %>%
  dplyr::select(age, "2020")
swedenPop <- data.frame(age=swedenPopM$age,
                       pop=(swedenPopM[["2020"]]+swedenPopF[["2020"]])*1000)
swedenPop$proportion <- swedenPop$pop # rename pop to proportion so function works 
swedenPop <- change_demography_bins(swedenPop, age_Sweden_outcome) %>%
  rename(., pop=proportion)

cases_Sweden <- round(swedenPop$pop*seroprev_Sweden/100)
casesL_Sweden <- round(swedenPop$pop*seroprevL_Sweden/100)
casesH_Sweden <- round(swedenPop$pop*seroprevH_Sweden/100)

Sweden <- data.frame(Age=age_Sweden_outcome,
                         Cases=cases_Sweden,
                         CasesL=casesL_Sweden,
                         CasesH=casesH_Sweden,
                         Hospitalized=NA,
                         ICU=ICU_Sweden,
                         Deaths=deaths_Sweden,
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

ilDeFrance_inHosp <- c(26, 18, 96, 182, 360, 775, 1157, 1448, 1710, 1075)
ilDeFrance_inICU <- c(6, 6, 16, 24, 50, 167, 194, 161, 25, 6)
ilDeFrance_hospRecov <- c(263, 160, 738, 1719, 2734, 4186, 4577, 4027, 3613, 1669)
ilDeFrance_hospDead <- c(2, 3, 12, 47, 128, 490, 1019, 1633, 2221, 1375)
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

# match age stratifications across the data, and put together
cases_France <- round(ilDeFrancePop*seroprev_France/100)
casesL_France <- round(ilDeFrancePop*seroprevL_France/100)
casesH_France <- round(ilDeFrancePop*seroprevH_France/100)

Ile_de_France <- data.frame(Age=age_France_outcome,
                         Cases=cases_France,
                         CasesL=casesL_France,
                         CasesH=casesH_France,
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
# Death data from Levin et al.
# Hospital data https://coronavirus.data.gov.uk/details/download
age_England_ICU <- c("16-30", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")
totICUEng <- 13025
propICUEng <- c(2.2, 5.8, 13.7, 27.7, 29.7, 18.0, 2.9)
ICU_England <- round(propICUEng/100*totICUEng)

age_England_death <- c("18-24", "25-34", "35-44", "45-54", "55-64",
                       "65-74", "75+")
deaths_England <- c(30, 131, 394, 1348, 3605, 7631, 38629)

age_England_seroprev <- c("18-24", "25-34", "35-44", "45-54", "55-64",
                       "65-74", "75+")
seroprev_England <- c(7.9, 7.8, 6.1, 6.4, 5.9, 3.2, 3.3)
seroprevL_England <- c(7.3, 7.4, 5.7, 6.0, 5.5, 2.8, 2.9)
seroprevH_England <- c(8.5, 8.3, 6.6, 6.9, 6.4, 3.6, 3.8)

population_England <- c(4746616, 7609363, 7147939, 7623273, 6782486, 5576066,
                4777650)

cases_England <- round(population_England*seroprev_England/100)
casesL_England <- round(population_England*seroprevL_England/100)
casesH_England <- round(population_England*seroprevH_England/100)

England <- data.frame(Age=age_England_ICU,
                         Cases=cases_England,
                         CasesL=casesL_England,
                         CasesH=casesH_England,
                         Hospitalized=NA,
                         ICU=ICU_England,
                         Deaths=NA,
                         Type="Seroprevalence",
                         Location="England",
                         EndPointOutcome="2020-05-26",
                         EndPointCases="2020-06-23")


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

age_Netherlands_seroprev <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59",
                "60-69", "70-79", "80-89", "90+")
seroprev_Netherlands <- c(0.5, 6.0, 7.8, 3.6, 3.6, 5.0, 4.0, 3.6, 6.0, 8.6)
seroprevL_Netherlands <- c(0.2, 5.0, 6.8, 2.8, 2.6, 4.0, 3.5, 2.8, 3.2, 3.2)
seroprevH_Netherlands <- c(1.0, 6.7, 9.9, 5.0, 5.3, 6.2, 4.8, 4.7, 11.5, 14.0)

netherlandsPopM <- dplyr::filter(popM, name=="Netherlands") %>%
  dplyr::select(., age, "2020")
netherlandsPopF <- dplyr::filter(popF, name=="Netherlands") %>%
  dplyr::select(age, "2020")
netherlandsPop <- data.frame(age=netherlandsPopM$age,
                       pop=(netherlandsPopM[["2020"]]+netherlandsPopF[["2020"]])*1000)
netherlandsPop$proportion <- netherlandsPop$pop # rename pop to proportion so function works 
netherlandsPop <- change_demography_bins(netherlandsPop, c(age_Netherlands_seroprev)) %>%
  rename(., pop=proportion)

cases_Netherlands <- round(netherlandsPop$pop*seroprev_Netherlands/100)
casesL_Netherlands <- round(netherlandsPop$pop*seroprevL_Netherlands/100)
casesH_Netherlands <- round(netherlandsPop$pop*seroprevH_Netherlands/100)

Netherlands <- data.frame(Age=age_Netherlands_outcome,
                         Cases=cases_Netherlands,
                         CasesL=casesL_Netherlands,
                         CasesH=casesH_Netherlands,
                         Hospitalized=Severe_Netherlands,
                         ICU=NA,
                         Deaths=deaths_Netherlands,
                         Type="Seroprevalence",
                         Location="Netherlands",
                         EndPointOutcome="2020-07-01",
                         EndPointCases="2020-07-01")


######################
# Georgia, USA
######################
# Hospital data https://www.fultoncountyga.gov/covid-19/epidemiology-reports
# Hospital data2 "Characteristics and Risk Factors for Hospitalization
# and Mortality among Persons with COVID-19 in Atlanta Metropolitan Area"
# Paper data are until 31 May. 
# Georgia pop from Levin et al
# Seroprev: Estimated Community Seroprevalence of SARS-CoV-2 Antibodies — Two Georgia Counties
age_Atlanta_outcome <- c("0-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75+")
Hospitalized_Atlanta <- c(18, 62, 79, 115, 172, 180, 260)
ICU_Atlanta <- c(4, 13, 14, 33, 44, 57, 74)
deaths_Atlanta <- c(2, 3, 2, 16, 33, 69, 170)
nHosp31May <- 888

Hospitalized_Atlanta <- c(sum(Hospitalized_Atlanta[2:3]), sum(Hospitalized_Atlanta[4:5]))
ICU_Atlanta <- c(sum(ICU_Atlanta[2:3]), sum(ICU_Atlanta[4:5]))
deaths_Atlanta <- c(sum(deaths_Atlanta[2:3]), sum(deaths_Atlanta[4:5]))
# remove ranges 0-17 and 65+ which include 0 in the interval,
# following Levin et al
age_Atlanta_seroprev <- c("18-49", "50-64")
seroprev_Atlanta <- c(3.3, 4.9)
seroprevL_Atlanta <- c(1.6, 6.4)
seroprevH_Atlanta <- c(1.8, 12.9)

atlantaPop <- c(867468, 327713)

cases_Atlanta <- round(atlantaPop*seroprev_Atlanta/100)
casesL_Atlanta <- round(atlantaPop*seroprevL_Atlanta/100)
casesH_Atlanta <- round(atlantaPop*seroprevH_Atlanta/100)

Atlanta <- data.frame(Age=age_Atlanta_seroprev,
                         Cases=cases_Atlanta,
                         CasesL=casesL_Atlanta,
                         CasesH=casesH_Atlanta,
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

cases_NYC <- round(newYorkPop*seroprev_NYC/100)
casesL_NYC <- round(newYorkPop*seroprevL_NYC/100)
casesH_NYC <- round(newYorkPop*seroprevH_NYC/100)

NYC <- data.frame(Age=age_NYC_outcome,
                  Cases=cases_NYC,
                  CasesL=casesL_NYC,
                  CasesH=casesH_NYC,
                  Hospitalized=Hospitalized_NYC,
                  ICU=NA,
                  Deaths=deaths_NYC,
                  Type="Seroprevalence",
                  Location="NYC",
                  EndPointOutcome="2020-04-28",
                  EndPointCases="2020-04-28")

####################################
# Put countries together and export
####################################
countriesDf <- dplyr::bind_rows(NewZealand, Korea, Spain, Ireland,
  Sweden, Ile_de_France, England, Netherlands, Atlanta, NYC)

write.csv(countriesDf, "../data/collected_data/locations_serology_data.csv", row.names=FALSE)


