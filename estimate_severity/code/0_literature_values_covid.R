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
# Age segregated IFR's
##############################

# O'Driscoll et. al. Meta-analysis
ageVec <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34",
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
ifrDriscoll <- data.frame(age = ageVec, IFR = ifrDriscollVec,
                          IFR_L = ifrDriscollLow, IFR_H = ifrDriscollHigh,
                          study = "Driscoll")

# Brazeau et al. Meta-analysis
ageVec <- c("0-9", "10-14", "15-19", "20-24", "25-29", "30-34",
            "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69",
            "70-74", "75-79", "80-84", "85-89", "90+") 
ifrBrazeauVec <- c(0.01, 0.01, 0.02, 0.03, 0.04, 0.06, 0.10, 0.16, 0.24,
            0.38, 0.60, 0.94, 1.47, 2.31, 3.61, 5.66, 8.86, 17.37)
ifrBrazeauLow <- c(0.00, 0.00, 0.00, 0.00, 0.00, 0.01, 0.01, 0.02,
            0.03, 0.05, 0.10, 0.18, 0.35, 0.65, 1.21, 2.23, 4.06, 9.7)
ifrBrazeauHigh <- c(0.06, 0.11, 0.18, 0.3 , 0.46, 0.71, 1.03, 1.47, 2.03,
             2.74, 3.64, 4.79, 6.27, 8.21, 10.8, 14.3, 19.3, 31.12)
ifrBrazeau <- data.frame(age=ageVec, IFR = ifrBrazeauVec, IFR_L = ifrBrazeauLow,
                    IFR_H = ifrBrazeauHigh, study = "Brazeau")

# Levin et al meta-analysis. These values are not the main
# presented, but the ones in the supplementary section with
# a more fine-grained age stratification
ageVec <- c("0-9", "10-19", "20-29", "30-39", "40-49",
            "50-59", "60-69", "70-79", "80+")
ifrLevinVec <- c(0.001, 0.003, 0.011, 0.035, 0.116, 0.384, 1.27, 4.19, 15.61)
ifrLevinLow <- c(0.00007, 0.002, 0.009, 0.030, 0.101, 0.335, 1.09, 3.45, 12.2)
ifrLevinHigh <- c(0.0013, 0.004, 0.013, 0.042, 0.134, 0.441, 1.49, 5.10, 20.0)
ifrLevin <- data.frame(age = ageVec, IFR = ifrLevinVec, IFR_L = ifrLevinLow,
                       IFR_H = ifrLevinHigh, study = "Levin")

ifrDf <- rbind(ifrDriscoll, ifrBrazeau, ifrLevin)

write.csv(ifrDf, "../data/0_ifr_literature.csv", row.names=FALSE)

###############################
# Letality among critical patients
###############################

# overall letality of some studies
criticalFatalityDf <- data.frame(fatalityICU = c(62, 46.4, 44.3),
                                 study = c("Xu", "Veneces", "ICNARC"))

# Letality by age in UK, ICNARC report
agesVec <- c("16-39", "40-49", "50-59", "60-69", "70-79", "80+")
# absolute numbers are reported
dischargedICU <- c(482, 750, 1268, 1054, 504, 85)
diedICU <- c(103, 239, 782, 1146, 900, 132)
totalICU <- dischargedICU + diedICU
letality <- NULL
letalityL <- NULL
letalityH <- NULL
# calculate binomial confidence intervals
for (n in c(1:length(totalICU))) {
  letalityTest <- binom.test(diedICU[n], totalICU[n])
  letality[n] <- letalityTest$estimate*100
  letalityL[n] <- letalityTest$conf.int[1]*100
  letalityH[n] <- letalityTest$conf.int[2]*100
}

criticalFatalityICNARC <- data.frame(age=agesVec,
                                     letality=letality,
                                     letalityL=letalityL,
                                     letalityH=letalityH)

write.csv(criticalFatalityICNARC, "../data/0_icu_letality_literature.csv",
          row.names=FALSE)

###############################
# Letality among hospitalised patients
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
                                         study = "Karagiannidis")

# Salje
agesVec <- c("0-20", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")
letalityVec <- c(0.6, 1.1, 1.9, 3.3, 6.5, 12.6, 21.0, 31.6)
letalityVecL <- c(0.2, 0.7, 1.5, 2.9, 6.0, 12.0, 20.3, 30.9)
letalityVecH <- c(1.3, 1.6, 2.3, 3.8, 7.0, 13.2, 21.7, 32.4)
severeFatalitySalje <- data.frame(age = agesVec,
                                 letality = letalityVec,
                                 letalityL = letalityVecL,
                                 letalityH = letalityVecH,
                                 study = "Salje")

letalitySevereDf <- rbind(severeFatalityRichardson, severeFatalityKaragiannidis,
                     severeFatalitySalje)

# Palaiodimos (minority population, overrepresented in obesity)
agesVec <- c("30-50", "51-64", "65-73", "74+")
hospitalizedVec <- c(51, 53, 46, 50)
deathsVec <- c(6, 12, 10, 20)
letalityVec <- NULL
letalityVecL <- NULL
letalityVecH <- NULL
for (l in c(1:length(deathsVec))) {
  result <- binom.test(deathsVec[l], hospitalizedVec[l])
  letalityVec[l] <- result$estimate * 100
  letalityVecL[l] <- result$conf.int[1] * 100
  letalityVecH[l] <- result$conf.int[2] * 100
}
severeFatalityPalaiodimos <- data.frame(age = agesVec,
                                 letality = letalityVec,
                                 letalityL = letalityVecL,
                                 letalityH = letalityVecH,
                                 study = "Palaiodimos")



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

