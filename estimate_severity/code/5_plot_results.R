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

countriesOutcomes <- read.csv("../data/2_countries_outcomes.csv",
                              stringsAsFactors=FALSE) %>%
  dplyr::mutate(., meanAge=mid_bin_age(age))
countriesOutcomeDist <- read.csv("../data/3_countries_outcome_dist.csv",
                              stringsAsFactors=FALSE)

#######################################################
# 0 Plot literature estimates in log scale to show regression
#######################################################

ifrLit <- ifrLit %>%
  dplyr::rename(., outcome=IFR, outcomeL=IFR_L, outcomeH=IFR_H) %>%
  dplyr::mutate(., Outcome="Death", meanAge=mid_bin_age(age))
criticalLit <- criticalLit %>%
  dplyr::rename(., outcome=critical, outcomeL=criticalL, outcomeH=criticalH) %>%
  dplyr::mutate(., Outcome="ICU", meanAge=mid_bin_age(age))
severeLit <- severeLit %>% 
  dplyr::rename(., outcome=severe, outcomeL=severeL, outcomeH=severeH) %>%
  dplyr::mutate(., Outcome="Hospitalized", meanAge=mid_bin_age(age))

escalaLog <- TRUE
studies <- unique(severeLit$study)
colorVector <- setNames(c("#983275", "#406E8E", "#ED5B46", "#8BD0B4", "#493319",
                          "black"), studies[1:(length(studies))])
# plot IFR log
litIFRPlot <- dplyr::filter(ifrLit, study != "Fitted") %>%
  plot_literature_outcome_line(., title="", escalaLog=escalaLog,
                                      colorVec=colorVector) +
  ylab("% infected dead") +
  geom_line(data = dplyr::filter(ifrLit, study=="Fitted"),
            aes(x=meanAge, y=outcome), size=1, color="black") +
  geom_ribbon(data = dplyr::filter(ifrLit, study=="Fitted"),
              aes(ymin = outcomeL, ymax = outcomeH), alpha = 0.2, 
              colour=NA, fill="black", show.legend=FALSE)

# plot critical log
litCriticalPlot <- dplyr::filter(criticalLit, study != "Fitted") %>%
  plot_literature_outcome_line(., title="", escalaLog=escalaLog,
                                      colorVec=colorVector) +
  ylab("% infected critical") +
  geom_line(data = dplyr::filter(criticalLit, study=="Fitted"),
            aes(x=meanAge, y=outcome), size=1, color="black") +
  geom_ribbon(data = dplyr::filter(criticalLit, study=="Fitted"),
              aes(ymin = outcomeL, ymax = outcomeH), alpha = 0.2, colour=NA,
              fill="black", show.legend=FALSE)

# plot severe log
litSeverePlot <- dplyr::filter(severeLit, study != "Fitted") %>%
  plot_literature_outcome_line(., title="", escalaLog=escalaLog,
                          colorVec=colorVector) +
  ylab("% infected severe") +
  geom_line(data = dplyr::filter(severeLit, study=="Fitted"),
            aes(x=meanAge, y=outcome), size=1, color="black") +
  geom_ribbon(data = dplyr::filter(severeLit, study=="Fitted"),
              aes(ymin = outcomeL, ymax = outcomeH), alpha = 0.2, colour=NA,
              fill="black", show.legend=FALSE)

ggsave("../plots/0_gravedad_literatura.png", litSeverePlot,
       width = 13, height = 10, units = "cm")
ggsave("../plots/0_critico_literatura.png", litCriticalPlot,
       width = 13, height = 10, units = "cm")
ggsave("../plots/0_muertes_literatura.png", litIFRPlot,
       width = 13, height = 10, units = "cm")

# plot hospital letality
colorVector <- setNames(c("black", "magenta", "#456B9D", "#1D3557"),
                        unique(hospLetality$study))
hospLetality <- dplyr::rename(hospLetality, outcome=letality,
                              outcomeL=letalityL, outcomeH=letalityH)

hospLetalityPlot <- dplyr::filter(hospLetality, study!="Fitted") %>%
  plot_literature_outcome_line(., title="", escalaLog=escalaLog,
                          colorVec=NA) +
  ylab("% in hospital mortality") +
  geom_line(data = dplyr::filter(hospLetality, study=="Fitted"),
            aes(x=meanAge, y=outcome), color="black", size=1) +
  geom_ribbon(data = dplyr::filter(hospLetality, study=="Fitted"),
              aes(ymin = outcomeL, ymax = outcomeH), alpha = 0.2, colour=NA,
              fill="black", show.legend=FALSE)

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
severeRatioDf <- dplyr::mutate(fittedSevere, study="Estimated",
                                outcome = outcome/denominatorRatio) %>%
  dplyr::select(., meanAge, outcome, outcomeL, outcomeH, study) %>%
  rbind(., cdcDf) %>%
  dplyr::rename(., Source=study)

ratioPlot <- ggplot(severeRatioDf, aes(x = meanAge, y = outcome, fill=Source,
                                       color=Source)) +
  geom_line() +
  geom_ribbon(aes(ymin = outcomeL, ymax = outcomeH), alpha = 0.2, colour=NA,
              show.legend=FALSE) +
  xlab("Age") +
  scale_y_continuous(trans = 'log10', labels=scales::comma) +
  theme_bw() +
  ylab("Relative proportion of severe (reference 25 years)")

ggsave("../plots/0_ratio_severidad.png", ratioPlot,
       width = 13, height = 10, units = "cm")

#######################################################
# 2 Plot our fits for all outcomes together
#######################################################

ifrFit <- dplyr::filter(ifrLit, study=="Fitted")
criticalFit <- dplyr::filter(criticalLit, study=="Fitted")
severeFit <- dplyr::filter(severeLit, study=="Fitted")

allTogether <- rbind(ifrFit, criticalFit, severeFit) %>%
  dplyr::mutate(., study=Outcome) %>%
  as_tibble(.)

allTogether$Outcome <- factor(allTogether$Outcome,
                                levels = c("Death", "ICU", "Hospitalized"))

outcomePlots <- dplyr::filter(allTogether) %>%
  ggplot(., aes(x=meanAge, y = outcome, color=Outcome, fill=Outcome)) +
  geom_line(size=0.7) +
  geom_ribbon(aes(ymin = outcomeL, ymax = outcomeH), alpha = 0.2,
              show.legend=FALSE, color=NA) +
  ylab("% infections with outcome") +
  xlab("Age") +
  theme_bw()

ggsave("../plots/2_desenlaces_estimados.png", outcomePlots,
       width = 15, height = 10, units = "cm")

#######################################################
# 3 Add real world data
#######################################################
countrySevereDf <- dplyr::select(countriesOutcomes, country, age, meanAge,
                          propHosp, propHosp_L, propHosp_H) %>%
  dplyr::mutate(., Outcome="Hospitalized", Country=country)
countryCriticalDf <- dplyr::select(countriesOutcomes, country, age, meanAge,
                          propICU, propICU_L, propICU_H) %>%
  dplyr::mutate(., Outcome="ICU", Country=country)
countryDeathDf <- dplyr::select(countriesOutcomes, country, age, meanAge,
                          propDeaths, propDeaths_L, propDeaths_H) %>%
  dplyr::mutate(., Outcome="Death", Country=country)

pd <- position_dodge(3)
plotLims <- c(0.001, 100)

countrySeverePlot <- ggplot(countrySevereDf, aes(x=meanAge, y=propHosp,
                                                 shape=Country, linetype=Country)) +
  geom_point(position=pd, size=3) +
  geom_linerange(aes(ymin=propHosp_L, ymax=propHosp_H), size=0.5, position=pd,
                 linetype=1) +
  geom_line(size=0.5, position=pd) +
  scale_y_continuous(trans = 'log10', labels=scaleFun, limits=plotLims) +
  geom_line(data=severeFit, aes(x=meanAge, y=outcome), size=0.7, color="blue",
            inherit.aes=FALSE) +
  geom_ribbon(data=severeFit, aes(x=meanAge, ymin=outcomeL, ymax=outcomeH),
              alpha=0.2, show.legend=FALSE, color=NA,
              fill="blue", inherit.aes=FALSE) +
  ylab("% severe") +
  xlab("Age") +
  theme_bw() +
  NULL

countryCriticalPlot <- ggplot(countryCriticalDf, aes(x=meanAge, y=propICU,
                                                 shape=Country, linetype=Country)) +
  geom_point(position=pd, size=3) +
  geom_linerange(aes(ymin=propICU_L, ymax=propICU_H), size=0.5, position=pd,
                 linetype=1) +
  geom_line(size=0.5, position=pd) +
  scale_y_continuous(trans = 'log10', labels=scaleFun, limits=plotLims) +
  geom_line(data=criticalFit, aes(x=meanAge, y=outcome), size=0.7, color="green",
            inherit.aes=FALSE) +
  geom_ribbon(data=criticalFit, aes(x=meanAge, ymin=outcomeL, ymax=outcomeH),
              alpha=0.2, show.legend=FALSE, color=NA,
              fill="green", inherit.aes=FALSE) +
  ylab("% critical") +
  xlab("Age") +
  theme_bw() +
  NULL

countryDeathPlot <- ggplot(countryDeathDf, aes(x=meanAge, y=propDeaths,
                                                 shape=Country, linetype=Country)) +
  geom_point(position=pd, size=3) +
  geom_linerange(aes(ymin=propDeaths_L, ymax=propDeaths_H), size=0.5, position=pd,
                 linetype=1) +
  geom_line(size=0.5, position=pd) +
  scale_y_continuous(trans = 'log10', labels=scaleFun, limits=plotLims) +
  geom_line(data=ifrFit, aes(x=meanAge, y=outcome), size=0.7, color="red",
            inherit.aes=FALSE) +
  geom_ribbon(data=ifrFit, aes(x=meanAge, ymin=outcomeL, ymax=outcomeH),
              alpha=0.2, show.legend=FALSE, color=NA,
              fill="red", inherit.aes=FALSE) +
  ylab("% dead") +
  xlab("Age") +
  theme_bw() +
  NULL

countriesPlot <- ggpubr::ggarrange(countrySeverePlot, countryCriticalPlot,
                                   countryDeathPlot, nrow=1, common.legend=TRUE)

ggsave("../plots/3_estimates_and_data.png", countriesPlot, width=25,
       height=10, units="cm")

#######################################################
# 4 Plot distribution of ages within outcomes
#######################################################
propOutcomes <- read.csv("../data/3_countries_outcome_dist.csv",
                         stringsAsFactors=FALSE)
propOutcomes$ageLimNum <- as.numeric(substr(as.character(propOutcomes$ageLim),
                            start=2, stop=3))
fittedOutcomes <- with(propOutcomes, c(cumDeathsFitted, cumICUFitted, cumHospFitted))
observedOutcomes <- with(propOutcomes, c(cumDeathObs, cumICUObs, cumHospObs))
nObs <- nrow(propOutcomes)
outcomeType <- factor(rep(c("Death", "ICU", "Hospitalized"), each=nObs),
                                levels = c("Death", "ICU", "Hospitalized"))
country <- rep(propOutcomes$country, rep=3)
ageNum <- rep(propOutcomes$ageLimNum, rep=3)
ageChar <- rep(propOutcomes$ageLim, rep=3)

propOutcomesLong <- data.frame(ageMax=ageChar, ageMaxNum=ageNum,
                               Country=country, Outcome=outcomeType,
                               Fitted=fittedOutcomes, Observed=observedOutcomes) %>%
  tidyr::pivot_longer(., cols=c("Fitted", "Observed"), names_to="Source",
                      values_to="Proportion")

propOutcomesPlot <- ggplot(data=propOutcomesLong,
                           aes(x=ageMaxNum, y=Proportion, color=Outcome,
                               linetype=Source)) +
  geom_line() +
  facet_wrap(vars(Country), nrow=2) +
  theme_bw() +
  xlab("Age") +
  ylab("Percentage of outcomes with indicated age")

ggsave("../plots/4_age_distribution_outcomes.png",
       propOutcomesPlot, width=17, height=15, units="cm")

#######################################################
# 5 Compare to flu
#######################################################
severe18_49 <- dplyr::filter(severeFit, meanAge>20 & meanAge <50)
severe50_64 <- dplyr::filter(severeFit, meanAge>49 & meanAge <65)
severeProp18_49 <- mean(severe18_49$outcome)
severeProp50_64 <- mean(severe50_64$outcome)

fluSympt18_49 <- 14428065
fluSympt50_64 <- 13237932
fluHosp18_49 <- 80985
fluHosp50_64 <- 140385
fluProp18_49 <- fluHosp18_49/fluSympt18_49*100
fluProp50_64 <- fluHosp50_64/fluSympt50_64*100

