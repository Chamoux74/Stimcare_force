library(lme4)
library(lmerTest)
library(ggeffects)
library(stargazer)

#density and histogram plot for normality

#normality

ggplot(DFIMVCmax1, aes(x=IMVC)) +
  geom_histogram(aes(y=after_stat(density)),      # Histogram with density instead of count on y-ax,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +
  facet_grid(cols = vars(condition), rows = vars(instant_mesure))

#analyse sur les pourcentage avec post48

dfpourcentage$condition <- factor(dfpourcentage$condition, levels = c("placebo", "patch"))
dfpourcentage$instant <- factor(dfpourcentage$instant, levels = c("pre","mid", "post", "post48"))

modele_mixte <-
  lmer(IMVCdif ~ condition * instant + (1|sujet), data = dfpourcentage)

summary(modele_mixte)

modele_mixte1 <-
  lmer(IMVCdif ~ condition + instant + (1|sujet), data = dfpourcentage)

summary(modele_mixte1)

anova(modele_mixte)

#analyse sur les pourcentage sans post48

dfpourcentage1 <- filter(dfpourcentage, instant != "post48")

dfpourcentage1$condition <- as.factor(dfpourcentage1$condition)
dfpourcentage1$instant <- as.factor(dfpourcentage1$instant)

modele_mixte2 <-
  lmer(IMVCdif ~ condition * instant + (1|sujet), data = dfpourcentage1)

summary(modele_mixte2)

modele_mixte3 <-
  lmer(IMVCdif ~ condition + instant + (1|sujet), data = dfpourcentage1)

summary(modele_mixte3)

anova(modele_mixte)

#analyse sur les valeurs absolue

DFIMVCmax$condition <- factor(DFIMVCmax$condition, levels = c("placebo", "patch"))
DFIMVCmax$instant_mesure <- factor(DFIMVCmax$instant_mesure, levels = c("pre", "mid", "post","post48"))

modele_mixte4 <-
  lmer(IMVC ~ condition * instant_mesure + (1|sujet), data = DFIMVCmax)

summary(modele_mixte4)

modele_mixte5 <-
  lmer(IMVC ~ condition + instant_mesure + (1|sujet), data = DFIMVCmax)

summary(modele_mixte5)

anova(modele_mixte5)

# analyse sans le post48

#modèle

DFIMVCmax1 <- filter(DFIMVCmax, instant_mesure != "post48")

DFIMVCmax1$condition <- factor(DFIMVCmax1$condition, levels = c("placebo", "patch"))
DFIMVCmax1$instant_mesure <- factor(DFIMVCmax1$instant_mesure, levels = c("pre", "mid", "post"))

#add VO2 parameters

VO2max <- rep(c(70,62, 62, 69,85,61,65,61,56,56,65,60,73,69,71,60), each =6)
DFIMVCmax1 <- cbind(DFIMVCmax1,VO2max)

DFIMVCmax1$VO2max <- as.factor(DFIMVCmax1$VO2max)
DFIMVCmax1$journee <- as.factor(DFIMVCmax1$journee)
DFIMVCmax1$Machine <- as.factor(DFIMVCmax1$Machine)

modele_mixte6 <-
  lmer(
    IMVC ~ condition * instant_mesure + (1 |
                                           sujet) + (1 |
                                                                       Machine),
    data = DFIMVCmax1,
    REML = FALSE
  )

modele_mixte6_1 <-
  lmer(
    IMVC ~ condition * instant_mesure + (1 |
                                           sujet/journee/Machine),
    data = DFIMVCmax1,
    REML = FALSE
  )

summary(modele_mixte6)
summary(modele_mixte6_1)

plot(modele_mixte6)
qqnorm(resid(modele_mixte6))
qqline(resid(modele_mixte6))

class(modele_mixte6) <- "lmerMod"

stargazer(modele_mixte6, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")


modele_mixte7 <-
  lmer(IMVC ~ condition + instant_mesure + (1|sujet), data = DFIMVCmax1)

summary(modele_mixte7)

#confident interval

confint.merMod(modele_mixte7)

anova(modele_mixte7, modele_mixte6)

#plot fitted values/ residual permet de voir si le modèle choisi est bon

plot(fitted(modele_mixte7), residuals(modele_mixte7), xlab = "Fitted Values", ylab = "Residuals") +
  abline(h = 0, lty = 2) +
  lines(smooth.spline(fitted(modele_mixte7), residuals(modele_mixte7)))

#plot with mean line and individual data

DFIMVCmax1 %>% ggplot(aes(x = instant_mesure, y = IMVC)) +
  geom_path(
    aes(group = condition, color = condition),
    alpha = 0.3,
    linejoin = "round"
  ) +
  stat_summary(
    aes(group = condition,linetype = condition),
    fun.y = mean,
    geom = "line",
    size = 1
  ) +
  stat_summary(
    aes(group = condition, shape = condition),
    fun.y = mean,
    geom = "point",
    size = 4,
    fill = "white"
  ) +
  scale_color_manual(values = c("#91393d", "#97a325"))

write.csv(DFIMVCmax1, "C:/Users/maxch/Git/DFIMVCmax_machine.csv")
DFIMVCmax1 <- read.csv("C:/Users/maxch/Git/DFIMVCmax_machine.csv")
