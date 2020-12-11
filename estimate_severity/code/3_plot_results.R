library(lubridate)
library(gridExtra)
library(ggpubr)
source("./functions_auxiliary.R")

###############################################
# Load data
###############################################

ifrLit <- read.csv("../data/1_fitted_ifr.csv", stringsAsFactors=FALSE)
criticalLit <- read.csv("../data/1_fitted_critical.csv", stringsAsFactors=FALSE)
severeLit <- read.csv("../data/1_fitted_severe.csv", stringsAsFactors=FALSE)

hospLetality <- read.csv("../data/1_fitted_hospital_letality.csv",
                      stringsAsFactors=FALSE)
uruSevere <- read.csv("../data/2_uru_hospital_data.csv", stringsAsFactors=FALSE) %>%
  dplyr::mutate(., meanAge=mid_bin_age(age))
uruICU <- read.csv("../data/2_uru_icu_data.csv", stringsAsFactors=FALSE) %>%
  dplyr::mutate(., meanAge=mid_bin_age(age))
uruDeaths <- read.csv("../data/2_uru_ifr_data.csv", stringsAsFactors=FALSE) %>%
  dplyr::mutate(., meanAge=mid_bin_age(age))

outcomeAgeStrat <- read.csv("../data/2_outcome_age_distribution.csv",
                            stringsAsFactors=FALSE)

#######################################################
# 0 Plot literature estimates in log scale to show regression
#######################################################

ifrLit <- ifrLit %>%
  dplyr::rename(., outcome=IFR, outcomeL=IFR_L, outcomeH=IFR_H) %>%
  dplyr::mutate(., Desenlace="Muerte", meanAge=mid_bin_age(age))
criticalLit <- criticalLit %>%
  dplyr::rename(., outcome=critical, outcomeL=criticalL, outcomeH=criticalH) %>%
  dplyr::mutate(., Desenlace="CTI", meanAge=mid_bin_age(age))
severeLit <- severeLit %>% 
  dplyr::rename(., outcome=severe, outcomeL=severeL, outcomeH=severeH) %>%
  dplyr::mutate(., Desenlace="Hospitalizado", meanAge=mid_bin_age(age))

escalaLog <- TRUE
colorVector <- setNames(c("#983275", "#406E8E", "#ED5B46", "#8BD0B4", "#493319",
                          "black"), unique(severeLit$study))
# plot IFR log
litIFRPlot <- dplyr::filter(ifrLit, study != "Fitted") %>%
  plot_literature_outcome(., title="",
                                      escalaLog=escalaLog,
                                      colorVec=colorVector) +
  ylab("% infectados fallecidos") +
  geom_line(data = dplyr::filter(ifrLit, study=="Fitted"),
            size=1) +
  geom_ribbon(data = dplyr::filter(ifrLit, study=="Fitted"),
              aes(ymin = outcomeL, ymax = outcomeH), alpha = 0.2, colour=NA,
              show.legend=FALSE)

# plot critical log
litCriticalPlot <- dplyr::filter(criticalLit, study != "Fitted") %>%
  plot_literature_outcome(., title="",
                                      escalaLog=escalaLog,
                                      colorVec=colorVector) +
  ylab("% infectados criticos") +
  geom_line(data = dplyr::filter(criticalLit, study=="Fitted"),
            size=1) +
  geom_ribbon(data = dplyr::filter(criticalLit, study=="Fitted"),
              aes(ymin = outcomeL, ymax = outcomeH), alpha = 0.2, colour=NA,
              show.legend=FALSE)

# plot severe log
litSeverePlot <- plot_literature_outcome(severeLit,
                                         title="",
                                         escalaLog=escalaLog,
                                         colorVec=colorVector) +
  ylab("% infectados graves") +
  geom_line(data = dplyr::filter(severeLit, study=="Fitted"),
            size=1) +
  geom_ribbon(data = dplyr::filter(severeLit, study=="Fitted"),
              aes(ymin = outcomeL, ymax = outcomeH), alpha = 0.2, colour=NA,
              show.legend=FALSE)

ggsave("../plots/0_gravedad_literatura.png", litSeverePlot,
       width = 13, height = 10, units = "cm")
ggsave("../plots/0_critico_literatura.png", litCriticalPlot,
       width = 13, height = 10, units = "cm")
ggsave("../plots/0_muertes_literatura.png", litIFRPlot,
       width = 13, height = 10, units = "cm")

# plot hospital letality
colorVector <- setNames(c("black", "#A8DADC", "#456B9D", "#1D3557"),
                        unique(hospLetalityPlot$study))
hospLetality <- dplyr::rename(hospLetality, outcome=letality,
                              outcomeL=letalityL, outcomeH=letalityH)
hospLetalityPlot <- plot_literature_outcome(hospLetality,
                                         title="",
                                         escalaLog=escalaLog,
                                         colorVec=colorVector) +
  ylab("% hospitalizados fallecidos") +
  geom_line(data = dplyr::filter(hospLetality, study=="Fitted"),
            size=1) +
  geom_ribbon(data = dplyr::filter(hospLetality, study=="Fitted"),
              aes(ymin = outcomeL, ymax = outcomeH), alpha = 0.2, colour=NA,
              show.legend=FALSE)
ggsave("../plots/0_letalidad_hospitalaria_literatura.png", hospLetalityPlot,
       width = 13, height = 10, units = "cm")

#######################################################
# 1 Compare ratio between ages for our estimate and
#   those reported by the CDC
#######################################################

# compare severe ratio with CDC
agesCDC <- c("0-4", "5-17", "18-29", "30-39", "40-49", "50-64", "65-74", "75-85", "85+")
meanAgesCDC <- mid_bin_age(agesCDC)
CDC_ratios <- c(1/4, 1/9, 1, 2, 3, 4, 5, 8, 13)
cdcDf <- data.frame(meanAge = meanAgesCDC, outcome = CDC_ratios, study="CDC",
                    outcomeH = NA, outcomeL = NA)

fittedSevere <- dplyr::filter(severeLit, study=="Fitted")
denominatorRatio <- mean(fittedSevere$outcome[5:6])
severeRatioDf <- dplyr::mutate(fittedSevere, study="Estimado",
                                outcome = outcome/denominatorRatio) %>%
  dplyr::select(., meanAge, outcome, outcomeL, outcomeH, study) %>%
  rbind(., cdcDf)

ratioPlot <- ggplot(severeRatioDf, aes(x = meanAge, y = outcome, fill=study,
                                       color=study)) +
  geom_line() +
  geom_ribbon(aes(ymin = outcomeL, ymax = outcomeH), alpha = 0.2, colour=NA,
              show.legend=FALSE) +
  xlab("Edad") +
  scale_y_continuous(trans = 'log10', labels=scales::comma) +
  theme_bw() +
  ylab("Cambio relativo en % grave (referencia 25 años)")

ggsave("../plots/0_ratio_severidad.png", ratioPlot,
       width = 13, height = 10, units = "cm")

#######################################################
# 2 Plot our fits for all outcomes together
#######################################################

ifrFit <- dplyr::filter(ifrLit, study=="Fitted")
criticalFit <- dplyr::filter(criticalLit, study=="Fitted")
severeFit <- dplyr::filter(severeLit, study=="Fitted")

allTogether <- rbind(ifrFit, criticalFit, severeFit) %>%
  dplyr::mutate(., study=Desenlace) %>%
  as_tibble(.)

allTogether$Desenlace <- factor(allTogether$Desenlace,
                                levels = c("Muerte",
                                           "CTI",
                                           "Hospitalizado"))

outcomePlots <- dplyr::filter(allTogether) %>%
  ggplot(., aes(x=meanAge, y = outcome, color=Desenlace, fill=Desenlace)) +
  geom_line(size=0.7) +
  geom_ribbon(aes(ymin = outcomeL, ymax = outcomeH), alpha = 0.2,
              show.legend=FALSE, color=NA) +
  ylab("% infecciones con desenlace") +
  xlab("Edad") +
  theme_bw()

ggsave("../plots/2_desenlaces_estimados.png", outcomePlots,
       width = 15, height = 10, units = "cm")


#######################################################
# 3 Agregarle los datos de Uruguay
#######################################################

uruSevere$Desenlace <- "Hospitalizado"
uruSevere <- dplyr::mutate(uruSevere, outcome=proportion, outcomeL=proportionL,
                           outcomeH=proportionH)
uruICU$Desenlace <- "CTI"
uruICU <- dplyr::mutate(uruICU, outcome=proportion, outcomeL=proportionL,
                           outcomeH=proportionH)
uruDeaths$Desenlace <- "Muerte"
uruDeaths <- dplyr::mutate(uruDeaths, outcome=proportion, outcomeL=proportionL,
                           outcomeH=proportionH)

uruguayDataPlot <- outcomePlots +
  geom_point(data = uruSevere, aes(x = meanAge, y=proportion), size=2) +
  geom_errorbar(data = uruSevere, aes(x = meanAge, ymin=proportionL,
                                      ymax=proportionH), size=0.5, width=0.3) +
  geom_point(data = uruICU, aes(x = meanAge, y=proportion), size=2) +
  geom_errorbar(data = uruICU, aes(x = meanAge, ymin=proportionL,
                                      ymax=proportionH), size=0.5, width=0.3) +
  geom_point(data = uruDeaths, aes(x = meanAge, y=proportion), size=2) +
  geom_errorbar(data = uruDeaths, aes(x = meanAge, ymin=proportionL,
                                      ymax=proportionH), size=0.5, width=0.3)

ggsave("../plots/3_comparacion_con_uruguay.png", uruguayDataPlot, width=13,
       height=10, units="cm")

#######################################################
# 4 Graficar distribución de desenlaces entre edades
#######################################################

proportionOutcomesStrat <- read.csv("../data/2_outcome_age_distribution.csv",
                                    stringsAsFactors=FALSE) %>%
  tidyr::pivot_longer(., c("Death", "Critical", "Severe"), names_to="Desenlace",
                      values_to="proportion")

ageVec <- as.numeric(substr(as.character(proportionOutcomesStrat$ageRange),
                            start=2, stop=3))
proportionOutcomesStrat$Edad <- ageVec

proportionOutcomesStrat$Desenlace <- factor(proportionOutcomesStrat$Desenlace,
                                            levels=c("Death", "Critical", "Severe"))
levels(proportionOutcomesStrat$Desenlace) <- c("Muerte", "CTI", "Hospitalizado")

outcomeDistributionPlot <- proportionOutcomesStrat %>%
  ggplot(., aes(x=Edad, y = proportion, color=Desenlace, fill=Desenlace)) +
  geom_line(size=0.7) +
  ylab("% edades en desenlaces") +
  xlab("Edad") +
  scale_x_continuous(breaks=unique(ageVec),
                     labels=unique(proportionOutcomesStrat$ageRange)) +
  theme_bw()

# calculate the distribution of outcomes in Uruguay
deathCumUru <- cumsum(uruDeaths$N)/ sum(uruDeaths$N)*100
icuCumUru <- cumsum(uruICU$N)/ sum(uruICU$N)*100
hospCumUru <- cumsum(uruSevere$N)/ sum(uruSevere$N)*100
binLims <- get_bins_limits(uruDeaths$age)
ageVec <- binLims$upper
ageVec <- ageVec
uruCumulativeDf <- data.frame(Edad=ageVec, Muerte=deathCumUru,
                              CTI=icuCumUru, Hospitalizado=hospCumUru)
uruCumulativeDf <- uruCumulativeDf[-nrow(uruCumulativeDf),] %>%
  tidyr::pivot_longer(., c("Muerte", "CTI", "Hospitalizado"), names_to="Desenlace",
                      values_to="proportion") %>%
  dplyr::mutate(., Desenlace=factor(Desenlace,
                                    levels=c("Muerte", "CTI", "Hospitalizado")),
                Datos="Uruguay")

outcomePropData <- dplyr::select(proportionOutcomesStrat, -ageRange) %>%
  dplyr::mutate(., Datos="Estimado")%>%
  rbind(., uruCumulativeDf)

uruguayOutcomeDistributionPlot <- outcomePropData %>%
  ggplot(., aes(x=Edad, y = proportion, color=Desenlace, fill=Desenlace,
                linetype=Datos)) +
  geom_line(size=0.7) +
  ylab("% edades en desenlaces") +
  xlab("Edad") +
  scale_x_continuous(breaks=unique(ageVec),
                     labels=unique(proportionOutcomesStrat$ageRange)) +
  theme_bw()

ggsave("../plots/4_distribucion_edades_desenlace.png", outcomeDistributionPlot,
       width=13, height=10, units="cm")
ggsave("../plots/4_distribucion_edades_desenlace_uru.png",
       uruguayOutcomeDistributionPlot, width=13, height=10, units="cm")

