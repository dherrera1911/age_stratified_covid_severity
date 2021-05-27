############################################
############################################
# This script puts several literature values for
# different covid variables into a tidy format
# and exports them into files.
#
# Written by Daniel Herrera, November 2020
# Contact at dherrera@fcien.edu.uy
############################################
############################################

library(dplyr)
library(tidyr)
source("./functions_auxiliary.R")

##########################################
##########################################
#### Writing down of literature values
##########################################
##########################################

##############################
##############################
# Age segregated IFR's
##############################
##############################

# O'Driscoll et. al. Meta-analysis
ageODriscoll <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34",
            "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69",
            "70-74", "75-79", "80+") 
ifrDriscollVec <- c(0.003, 0.001, 0.001, 0.003, 0.006, 0.013, 0.024, 0.040,
         0.075, 0.121, 0.207, 0.323, 0.456, 1.075, 1.674,
         3.203, 8.292)
ifrDriscollLow <- c(0.002, 0.000, 0.001, 0.002, 0.005, 0.011, 0.021,
                    0.034, 0.064, 0.104, 0.177, 0.277, 0.392, 0.921,
                    1.435, 2.744, 7.105)
ifrDriscollHigh <- c(0.004, 0.001, 0.001, 0.003, 0.008, 0.015, 0.028,
                    0.047, 0.087, 0.140, 0.239, 0.373, 0.527, 1.244,
                    1.937, 3.705, 9.593)
ifrDriscoll <- data.frame(Age=ageODriscoll, IFR=ifrDriscollVec,
                          IFR_L=ifrDriscollLow, IFR_H=ifrDriscollHigh,
                          study="Driscoll")

# Brazeau et al. Meta-analysis
ageBrazeau <- c("0-9", "10-14", "15-19", "20-24", "25-29", "30-34",
            "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69",
            "70-74", "75-79", "80-84", "85-89", "90+") 
ifrBrazeauVec <- c(0.01, 0.01, 0.02, 0.03, 0.04, 0.06, 0.10, 0.16, 0.24,
            0.38, 0.60, 0.94, 1.47, 2.31, 3.61, 5.66, 8.86, 17.37)
ifrBrazeauLow <- c(0.00, 0.00, 0.00, 0.00, 0.00, 0.01, 0.01, 0.02,
            0.03, 0.05, 0.10, 0.18, 0.35, 0.65, 1.21, 2.23, 4.06, 9.7)
ifrBrazeauHigh <- c(0.06, 0.11, 0.18, 0.3 , 0.46, 0.71, 1.03, 1.47, 2.03,
             2.74, 3.64, 4.79, 6.27, 8.21, 10.8, 14.3, 19.3, 31.12)
ifrBrazeau <- data.frame(Age=ageBrazeau, IFR=ifrBrazeauVec, IFR_L=ifrBrazeauLow,
                    IFR_H=ifrBrazeauHigh, study="Brazeau")

# Levin et al meta-analysis. These values are not the main
# presented, but the ones in the supplementary section with
# a more fine-grained age stratification
ageLevin <- c("0-9", "10-19", "20-29", "30-39", "40-49",
            "50-59", "60-69", "70-79", "80+")
ifrLevinVec <- c(0.001, 0.003, 0.011, 0.035, 0.116, 0.384, 1.27, 4.19, 15.61)
ifrLevinLow <- c(0.00007, 0.002, 0.009, 0.030, 0.101, 0.335, 1.09, 3.45, 12.2)
ifrLevinHigh <- c(0.0013, 0.004, 0.013, 0.042, 0.134, 0.441, 1.49, 5.10, 20.0)
ifrLevin <- data.frame(Age=ageLevin, IFR=ifrLevinVec, IFR_L=ifrLevinLow,
                       IFR_H=ifrLevinHigh, study="Levin")

ifrDf <- rbind(ifrDriscoll, ifrBrazeau, ifrLevin)

write.csv(ifrDf, "../data/collected_data/0_literature_ifr.csv", row.names=FALSE)



###############################
###############################
# Letality among critical patients
###############################
###############################

# overall letality of some studies
criticalFatalityDf <- data.frame(fatalityICU = c(62, 46.4, 44.3),
                                 study = c("Xu", "Veneces", "ICNARC"))

# Letality by age in UK, ICNARC report for 9 September
age_ICNARC <- c("16-39", "40-49", "50-59", "60-69", "70-79", "80+")
# absolute numbers are reported
discharged_ICNARC <- c(734, 1131, 1982, 1689, 786, 137)
deaths_ICNARC <- c(131, 311, 991, 1467, 1145, 191)
ICU_ICNARC <- discharged_ICNARC + deaths_ICNARC
letality_ICNARC <- NULL
letalityL_ICNARC <- NULL
letalityH_ICNARC <- NULL
# calculate binomial confidence intervals
for (n in c(1:length(ICU_ICNARC))) {
  letalityTest <- binom.test(deaths_ICNARC[n], ICU_ICNARC[n])
  letality_ICNARC[n] <- letalityTest$estimate*100
  letalityL_ICNARC[n] <- letalityTest$conf.int[1]*100
  letalityH_ICNARC[n] <- letalityTest$conf.int[2]*100
}

criticalFatalityICNARC <- data.frame(Age=age_ICNARC,
                                     Letality=letality_ICNARC,
                                     LetalityL=letalityL_ICNARC,
                                     LetalityH=letalityH_ICNARC,
                                     ICU=ICU_ICNARC,
                                     Deaths=deaths_ICNARC,
                                     Study="ICNARC",
                                     Type="Hospital_study",
                                     Location="UK",
                                     EndPoint="2020-09-07")

# Letality in critical patients NY, Cummings et al
age_NYC <- c("20-29", "30-39", "40-49", "50-59", "60-69", "70-79",
             "80-89", "90+")
# absolute numbers are reported
ICU_NYC <- c(8, 19, 28, 52, 69, 52, 23, 6)
deaths_NYC <- c(0, 5, 6, 18, 22, 28, 17, 5)
letality_NYC <- NULL
letalityL_NYC <- NULL
letalityH_NYC <- NULL
# calculate binomial confidence intervals
for (n in c(1:length(ICU_NYC))) {
  letalityTest <- binom.test(deaths_NYC[n], ICU_NYC[n])
  letality_NYC[n] <- letalityTest$estimate*100
  letalityL_NYC[n] <- letalityTest$conf.int[1]*100
  letalityH_NYC[n] <- letalityTest$conf.int[2]*100
}

criticalFatalityCummings <- data.frame(Age=age_NYC,
                                     Letality=letality_NYC,
                                     LetalityL=letalityL_NYC,
                                     LetalityH=letalityH_NYC,
                                     ICU=ICU_NYC,
                                     Deaths=deaths_NYC,
                                     Study="Cummings",
                                     Type="Hospital_study",
                                     Location="NYC, USA",
                                     EndPoint="2020-04-28")

# Letality in critical patients Georgia, Chishinga et al
age_Atln <- c("0-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75+")
# absolute numbers are reported
ICU_Atln <- c(4, 13, 14, 33, 44, 57, 74)
deaths_Atln <- c(2, 3, 2, 16, 33, 69, 170)
# calculate binomial confidence intervals
letality_Atln <- NULL
letalityL_Atln <- NULL
letalityH_Atln <- NULL
for (n in c(1:length(ICU_Atln))) {
  if (ICU_Atln[n] >= deaths_Atln[n]) {
    letalityTest <- binom.test(deaths_Atln[n], ICU_Atln[n])
    letality_Atln[n] <- letalityTest$estimate*100
    letalityL_Atln[n] <- letalityTest$conf.int[1]*100
    letalityH_Atln[n] <- letalityTest$conf.int[2]*100
  } else {
    letality_Atln[n] <- NA
    letalityL_Atln[n] <- NA
    letalityH_Atln[n] <- NA
  }
}

criticalFatalityChishinga <- data.frame(Age=age_Atln,
                                     Letality=letality_Atln,
                                     LetalityL=letalityL_Atln,
                                     LetalityH=letalityH_Atln,
                                     ICU=ICU_Atln,
                                     Deaths=deaths_Atln,
                                     Study="Chishinga",
                                     Type="Population_data",
                                     Location="Atlanta, USA",
                                     EndPoint="2020-05-31")

### Spain
spainDailyOutcome <- read.csv("../downloaded_data/spain/casos_hosp_uci_def_sexo_edad_provres.csv",
                                                  stringsAsFactors=FALSE) %>%
  as_tibble(.) %>%
    dplyr::mutate(., fecha=lubridate::date(fecha))

spainDeaths <- dplyr::filter(spainDailyOutcome, fecha <= "2020-05-11" &
                                  grupo_edad != "NC") %>%
  group_by(., grupo_edad) %>%
  summarize(., Deaths=sum(num_def)) %>%
  dplyr::mutate(., proportion=Deaths, age=grupo_edad) %>%
  dplyr::mutate(., Deaths=proportion)

spainICU <- dplyr::filter(spainDailyOutcome, fecha <= "2020-05-11" &
                                  grupo_edad != "NC") %>%
  group_by(., grupo_edad) %>%
  summarize(., ICU=sum(num_uci)) %>%
  dplyr::mutate(., proportion=ICU, age=grupo_edad) %>%
  dplyr::mutate(., ICU=proportion)

age_Spain <- spainDeaths$age
# absolute numbers are reported
ICU_Spain <- spainICU$ICU
deaths_Spain <- spainDeaths$Deaths
# calculate binomial confidence intervals
letality_Spain <- NULL
letalityL_Spain <- NULL
letalityH_Spain <- NULL
for (n in c(1:length(ICU_Spain))) {
  if (ICU_Spain[n] >= deaths_Spain[n]) {
    letalityTest <- binom.test(deaths_Spain[n], ICU_Spain[n])
    letality_Spain[n] <- letalityTest$estimate*100
    letalityL_Spain[n] <- letalityTest$conf.int[1]*100
    letalityH_Spain[n] <- letalityTest$conf.int[2]*100
  } else {
    letality_Spain[n] <- NA
    letalityL_Spain[n] <- NA
    letalityH_Spain[n] <- NA
  }
}

criticalFatalitySpain <- data.frame(age=age_Spain,
                                     letality=letality_Spain,
                                     letalityL=letalityL_Spain,
                                     letalityH=letalityH_Spain,
                                     ICU=ICU_Spain,
                                     Deaths=deaths_Spain,
                                     Study="Spain",
                                     Type="Population_data",
                                     Location="Spain",
                                     EndPoint="2020-05-11")


# Data from Ireland
# https://www.hpsc.ie/a-z/respiratory/coronavirus/novelcoronavirus/casesinireland/epidemiologyofcovid-19inireland/july2020/
age_Ireland <- c("0-4", "5-14", "15-24", "25-34", "35-44", "45-54", "55-64",
             "65-74", "75-84", "85+")
# absolute numbers are reported
ICU_Ireland <- c(0, 2, 5, 15, 36, 91, 127, 109, 46, 6)
deaths_Ireland <- c(0, 0, 1, 5, 12, 24, 67, 225, 514, 658)
letality_Ireland <- NULL
letalityL_Ireland <- NULL
letalityH_Ireland <- NULL
for (n in c(1:length(ICU_Ireland))) {
  if ((ICU_Ireland[n] >= deaths_Ireland[n]) & (ICU_Ireland[n] > 0)) {
    letalityTest <- binom.test(deaths_Ireland[n], ICU_Ireland[n])
    letality_Ireland[n] <- letalityTest$estimate*100
    letalityL_Ireland[n] <- letalityTest$conf.int[1]*100
    letalityH_Ireland[n] <- letalityTest$conf.int[2]*100
  } else {
    letality_Ireland[n] <- NA
    letalityL_Ireland[n] <- NA
    letalityH_Ireland[n] <- NA
  }
}

criticalFatalityIreland <- data.frame(Age=age_Ireland,
                                     Letality=letality_Ireland,
                                     LetalityL=letalityL_Ireland,
                                     LetalityH=letalityH_Ireland,
                                     ICU=ICU_Ireland,
                                     Deaths=deaths_Ireland,
                                     Study="Ireland",
                                     Type="Population_data",
                                     Location="Ireland",
                                     EndPoint="2020-07-31")

# Data from New Zealand, obtained by mail
#  https://www.health.govt.nz/our-work/diseases-and-conditions/covid-19-novel-coronavirus/covid-19-data-and-statistics/covid-19-case-demographics#age-gender
age_NZ <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59",
                "60-69", "70-79", "80-89", "90+")
# absolute numbers are reported
ICU_NZ <-    c(0, 1, 1, 1, 3, 4, 6,  10, 8, 5)
deaths_NZ <- c(0, 0, 0, 0, 0, 2, 3,  7,  8, 5)
letality_NZ <- NULL
letalityL_NZ <- NULL
letalityH_NZ <- NULL
for (n in c(1:length(ICU_NZ))) {
  if ((ICU_NZ[n] >= deaths_NZ[n]) & (ICU_NZ[n] > 0)) {
    letalityTest <- binom.test(deaths_NZ[n], ICU_NZ[n])
    letality_NZ[n] <- letalityTest$estimate*100
    letalityL_NZ[n] <- letalityTest$conf.int[1]*100
    letalityH_NZ[n] <- letalityTest$conf.int[2]*100
  } else {
    letality_NZ[n] <- NA
    letalityL_NZ[n] <- NA
    letalityH_NZ[n] <- NA
  }
}

criticalFatalityNZ <- data.frame(age=age_NZ,
                                 Letality=letality_NZ,
                                 LetalityL=letalityL_NZ,
                                 LetalityH=letalityH_NZ,
                                 ICU=ICU_NZ,
                                 Deaths=deaths_NZ,
                                 Study="New_Zealand")
                                 Type="Population_data",
                                 Location="New_Zealand",
                                 EndPoint="2021-01-13")

# Put data together
criticalLetalityData <- rbind(criticalFatalityICNARC,
                              criticalFatalityCummings,
                              criticalFatalityChishinga,
                              criticalFatalitySpain,
                              criticalFatalityIreland,
                              criticalFatalityNZ)

write.csv(criticalLetalityData, "../data/0_icu_letality_literature.csv",
          row.names=FALSE)


###############################
###############################
# Letality among hospitalised patients
###############################
###############################

# Globlal letality reported in different studies
severeFatalityDf <- data.frame(letalityHosp = c(24.9, 21, 25.7, 22, 18.1, 4.4),
                                 study = c("Westblade", "Richardson", "RECOVERY",
                                           "Karagiannidis", "Salje", "Xia"))

# Age stratified letality

# Richardson et al.
agesVec <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69",
             "70-79", "80-89", "90+")
deathsMaleVec <- c(0, 0, 3, 6, 19, 40, 56, 91, 94, 28)
deathsFemaleVec <- c(0, 0, 1, 2, 3, 13, 28, 54, 76, 39)
hospitalizedMaleVec <- c(13, 1, 42, 130, 233, 327, 300, 254, 155, 44)
hospitalizedFemaleVec <- c(13, 7, 55, 81, 119, 188, 233, 197, 158, 84)
deathsVec <- deathsFemaleVec + deathsMaleVec
hospitalizedVec <- hospitalizedFemaleVec + hospitalizedMaleVec

letalityVec <- NULL
letalityVecL <- NULL
letalityVecH <- NULL
for (l in c(1:length(deathsVec))) {
  result <- binom.test(deathsVec[l], hospitalizedVec[l])
  letalityVec[l] <- result$estimate * 100
  letalityVecL[l] <- result$conf.int[1] * 100
  letalityVecH[l] <- result$conf.int[2] * 100
}

severeFatalityRichardson <- data.frame(age = agesVec,
                                       letality = letalityVec,
                                       letalityL = letalityVecL,
                                       letalityH = letalityVecH,
                                       Hosp = hospitalizedVec,
                                       Deaths= deathsVec,
                                       study = "Richardson")

# Karagiannidis
agesVec <- c("18-59", "60-69", "70-79", "80+")
patientsNoVentilator <- c(2474, 1239, 1623, 2958)
patientsVentilator <- c(422, 382, 535, 388)
mortalityNoVentilator <- c(0.7, 5.4, 14.6, 33.8)
mortalityVentilator <- c(27.7, 45.5, 62.6, 72.2)
hospitalizedVec <- patientsVentilator + patientsNoVentilator
deathsVec <- round((patientsVentilator*mortalityVentilator +
                    patientsNoVentilator*mortalityNoVentilator)/100)
letalityVec <- NULL
letalityVecL <- NULL
letalityVecH <- NULL
for (l in c(1:length(deathsVec))) {
  result <- binom.test(deathsVec[l], hospitalizedVec[l])
  letalityVec[l] <- result$estimate * 100
  letalityVecL[l] <- result$conf.int[1] * 100
  letalityVecH[l] <- result$conf.int[2] * 100
}

severeFatalityKaragiannidis <- data.frame(age = agesVec,
                                         letality = letalityVec,
                                         letalityL = letalityVecL,
                                         letalityH = letalityVecH,
                                         Hosp = hospitalizedVec,
                                         Deaths= deathsVec,
                                         study = "Karagiannidis")

# Salje
agesVec <- c("0-20", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")
letalityVec <- c(0.6, 1.1, 1.9, 3.3, 6.5, 12.6, 21.0, 31.6)
letalityVecL <- c(0.2, 0.7, 1.5, 2.9, 6.0, 12.0, 20.3, 30.9)
letalityVecH <- c(1.3, 1.6, 2.3, 3.8, 7.0, 13.2, 21.7, 32.4)
# Estimate counts from the Salje intervals
saljeVar <- ((letalityVecL-letalityVecH)/100/4)^2
saljeNum <- (letalityVec/100)*(1-letalityVec/100)
hospitalizedVec <- round(saljeNum/saljeVar)
deathsVec <- round(hospitalizedVec*letalityVec/100)
severeFatalitySalje <- data.frame(age = agesVec,
                                 letality = letalityVec,
                                 letalityL = letalityVecL,
                                 letalityH = letalityVecH,
                                 Hosp = hospitalizedVec,
                                 Deaths = deathsVec,
                                 study = "Salje")

# Netherlands data
hospDataNL <- read.csv("../downloaded_data/netherlands/COVID-19_casus_landelijk.csv",
                       stringsAsFactors=FALSE, sep=";") %>%
  as_tibble(.) %>%
  dplyr::filter(., (Hospital_admission=="Yes" | Deceased=="Yes") &
                (lubridate::date(Date_statistics)<="2020-07-01")) %>%
  group_by(., Agegroup) %>%
  summarize(., nHosp=sum((Hospital_admission=="Yes" | Deceased=="Yes")),
            nDead=sum(Deceased=="Yes")) %>%
  dplyr::mutate(., meanAge=mid_bin_age(Agegroup))

u50Deaths <- dplyr::filter(hospDataNL, Agegroup=="<50")[["nDead"]]
deathsTot_U50 <- c(1, 2, 7, 22, 67)
deathsNL_U50 <- round(deathsTot_U50/sum(deathsTot_U50)*u50Deaths)

hospDataNL <- dplyr::filter(hospDataNL, !(Agegroup %in% c("<50", "Unknown")))
hospDataNL$nDead[which(hospDataNL$meanAge<50)] <- deathsNL_U50

hospDataNL <- dplyr::mutate(hospDataNL, hospDead=nDead/nHosp*100)

deathsVec_NL <- hospDataNL$nDead
hospitalizedVec_NL <- hospDataNL$nHosp
letalityVec_NL <- NULL
letalityVecL_NL <- NULL
letalityVecH_NL <- NULL
for (l in c(1:length(deathsVec_NL))) {
  result <- binom.test(deathsVec_NL[l], hospitalizedVec_NL[l])
  letalityVec_NL[l] <- result$estimate * 100
  letalityVecL_NL[l] <- result$conf.int[1] * 100
  letalityVecH_NL[l] <- result$conf.int[2] * 100
}
severeNetherlands <- data.frame(age=hospDataNL$Agegroup,
                                letality=letalityVec_NL,
                                letalityL=letalityVecL_NL,
                                letalityH=letalityVecH_NL,
                                Hosp=hospitalizedVec_NL,
                                Deaths=deathsVec_NL,
                                study="Netherlands")

# Spain data
spainHosp <- dplyr::filter(spainDailyOutcome, fecha <= "2020-05-11" &
                                  grupo_edad != "NC") %>%
  group_by(., grupo_edad) %>%
  summarize(., Hospitalized=sum(num_hosp)) %>%
  dplyr::mutate(., proportion=Hospitalized, age=grupo_edad) %>%
  dplyr::mutate(., Hospitalized=proportion)

agesVec <- spainDeaths$age
# absolute numbers are reported
hospitalizedVec <- spainHosp$Hospitalized
deathsVec <- spainDeaths$Deaths
invalidData <- which(hospitalizedVec<=deathsVec)
if (length(invalidData)>0) {
  hospitalizedVec <- hospitalizedVec[-invalidData]
  deathsVec <- deathsVec[-invalidData]
  agesVec <- agesVec[-invalidData]
}
letality <- NULL
letalityL <- NULL
letalityH <- NULL
# calculate binomial confidence intervals
for (n in c(1:length(hospitalizedVec))) {
  letalityTest <- binom.test(deathsVec[n], hospitalizedVec[n])
  letality[n] <- letalityTest$estimate*100
  letalityL[n] <- letalityTest$conf.int[1]*100
  letalityH[n] <- letalityTest$conf.int[2]*100
}

severeSpain <- data.frame(age=ageVec, letality=letality,
                          letalityL=letalityL,
                          letalityH=letalityH,
                          Hosp=hospitalizedVec,
                          Deaths=deathsVec,
                          study="Spain")

# Ireland data
agesVec <- c("0-4", "5-14", "15-24", "25-34", "35-44", "45-54", "55-64",
             "65-74", "75-84", "85+")
# absolute numbers are reported
hospitalizedVec <- c(24, 18, 77, 198, 274, 451, 499, 587, 746, 477)
deathsVec <- c(0, 0, 1, 5, 12, 24, 67, 225, 514, 658)
invalidData <- which(hospitalizedVec<=deathsVec)
if (length(invalidData)>0) {
  hospitalizedVec <- hospitalizedVec[-invalidData]
  deathsVec <- deathsVec[-invalidData]
  agesVec <- agesVec[-invalidData]
}
letality <- NULL
letalityL <- NULL
letalityH <- NULL
# calculate binomial confidence intervals
for (n in c(1:length(hospitalizedVec))) {
  letalityTest <- binom.test(deathsVec[n], hospitalizedVec[n])
  letality[n] <- letalityTest$estimate*100
  letalityL[n] <- letalityTest$conf.int[1]*100
  letalityH[n] <- letalityTest$conf.int[2]*100
}

severeIreland <- data.frame(age=agesVec,
                                     letality=letality,
                                     letalityL=letalityL,
                                     letalityH=letalityH,
                                     Hosp=hospitalizedVec,
                                     Deaths=deathsVec,
                                     study="Ireland")

# Data from New Zealand
ageVecNZ <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59",
                "60-69", "70-79", "80-89", "90+")
# absolute numbers are reported
nzHosp <-   c(1, 3, 7, 16, 16, 29, 27, 27, 16, 9)
nzDeaths <- c(0, 0, 0, 0, 0, 2,  3,  7,  8, 5)
invalids <- which(nzHosp == 0)
if (length(invalids) > 0) {
  nzHosp <- nzHosp[-invalids]
  nzDeaths <- nzDeaths[-invalids]
  ageVecNZ <- ageVecNZ[-invalids]
}
letality <- NULL
letalityL <- NULL
letalityH <- NULL
# calculate binomial confidence intervals
for (n in c(1:length(nzHosp))) {
  letalityTest <- binom.test(nzDeaths[n], nzHosp[n])
  letality[n] <- letalityTest$estimate*100
  letalityL[n] <- letalityTest$conf.int[1]*100
  letalityH[n] <- letalityTest$conf.int[2]*100
}
severeNZ <- data.frame(age=ageVecNZ,
                                     letality=letality,
                                     letalityL=letalityL,
                                     letalityH=letalityH,
                                     Hosp=nzHosp,
                                     Deaths=nzDeaths,
                                     study="New Zealand")

# Data from Georgia, USA
ageVecGeorgia <- c("0-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75+")
georgiaHosp <- c(18, 62, 79, 115, 172, 180, 260)
georgiaDeaths <- c(2, 3, 2, 16, 33, 69, 170)
letality <- NULL
letalityL <- NULL
letalityH <- NULL
# calculate binomial confidence intervals
for (n in c(1:length(georgiaHosp))) {
  letalityTest <- binom.test(georgiaDeaths[n], georgiaHosp[n])
  letality[n] <- letalityTest$estimate*100
  letalityL[n] <- letalityTest$conf.int[1]*100
  letalityH[n] <- letalityTest$conf.int[2]*100
}
severeGeorgia <- data.frame(age=ageVecGeorgia,
                                     letality=letality,
                                     letalityL=letalityL,
                                     letalityH=letalityH,
                                     Hosp=georgiaHosp,
                                     Deaths=georgiaDeaths,
                                     study="Atlanta, USA")


# Data from NYC, USA
ageVecNY <- c("0-17", "18-44", "45-64", "65-74", "75+")
newyorkHosp <- c(273, 5695, 14156, 9240, 11212)
newyorkDeaths <- c(5, 482, 2649, 2910, 5772)
letality <- NULL
letalityL <- NULL
letalityH <- NULL
# calculate binomial confidence intervals
for (n in c(1:length(newyorkHosp))) {
  letalityTest <- binom.test(newyorkDeaths[n], newyorkHosp[n])
  letality[n] <- letalityTest$estimate*100
  letalityL[n] <- letalityTest$conf.int[1]*100
  letalityH[n] <- letalityTest$conf.int[2]*100
}
severeNY <- data.frame(age=ageVecNY,
                                     letality=letality,
                                     letalityL=letalityL,
                                     letalityH=letalityH,
                                     Hosp=newyorkHosp,
                                     Deaths=newyorkDeaths,
                                     study="New York City, USA")


letalitySevereDf <- rbind(severeFatalityRichardson, severeFatalityKaragiannidis,
                     severeFatalitySalje, severeNetherlands,
                     severeSpain, severeIreland, severeNZ, severeGeorgia,
                     severeNY)

write.csv(letalitySevereDf, "../data/0_hospitalized_letality_literature.csv",
          row.names=FALSE)

#################################
# Literature estimates of % critical and % hospitalized
################################

# Age stratified Verity et. al.
ageVec <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69",
            "70-79", "80+")
cfrVerityVec <- c(0.0026, 0.0148, 0.06, 0.146, 0.295, 1.25, 3.99,
                  8.61, 13.4)
ifrVerityVec <- c(0.00161, 0.00659, 0.0309, 0.0844, 0.161, 0.595,
                  1.93, 4.29, 7.80)
severeVerityVec <- c(0, 0.0408, 1.04, 3.43, 4.25, 8.16, 11.8, 16.6, 18.4)
severeVerityL <- c(0, 0.0243, 0.622, 2.04, 2.53, 4.86, 7.01, 9.87, 11.0)
severeVerityH <- c(0, 0.0832, 2.13, 7.0, 8.68, 16.7, 24.0, 33.8, 37.6)
dfVerity <- data.frame(age=ageVec, CFR=cfrVerityVec,
                       IFR=ifrVerityVec, severe=severeVerityVec,
                       severeL=severeVerityL, severeH=severeVerityH)

# Salje et al.
agesVec <- c("0-20", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")
severeSaljeVec <- c(0.1, 0.5, 1.1, 1.4, 2.9, 5.8, 9.3, 26.2)
severeSaljeL <- c(0.08, 0.3, 0.6, 0.8, 1.6, 3.3, 5.2, 14.8)
severeSaljeH <- c(0.2, 0.8, 1.7, 2.3, 4.7, 9.5, 15.1, 42.7)
severe2ICU <- c(22.2, 11.6, 15.9, 22.2, 27.6, 30.8, 24.9, 5.6)
severe2Death <- c(0.6, 1.1, 1.9, 3.3, 6.5, 12.6, 21.0, 31.6)
dfSalje <- data.frame(age = agesVec, severe = severeSaljeVec,
                      severeL = severeSaljeL,
                      severeH = severeSaljeH,
                      severe2ICU = severe2ICU) %>%
dplyr::mutate(., critical = severe * severe2ICU/100,
              criticalL = severeL * severe2ICU/100,
              criticalH = severeH * severe2ICU/100)
# For 80+ substitute for death prop, since they don't seem to go into ICU much
dfSalje$critical[nrow(dfSalje)] <- tail(severeSaljeVec,1) * tail(severe2Death,1)/100
dfSalje$criticalL[nrow(dfSalje)] <- tail(severeSaljeL,1) * tail(severe2Death,1)/100
dfSalje$criticalH[nrow(dfSalje)] <- tail(severeSaljeH,1) * tail(severe2Death,1)/100

veritySevereDf <- dplyr::mutate(dfVerity, study="Verity") %>%
  dplyr::select(., age, severe, severeL, severeH, study)
saljeSevereDf <- dplyr::mutate(dfSalje, study = "Salje") %>%
  dplyr::select(., age, severe, severeL, severeH, study)

literatureSevereDf <- rbind(veritySevereDf, saljeSevereDf)
write.csv(literatureSevereDf, "../data/0_percentage_severe_literature.csv",
          row.names=FALSE)

