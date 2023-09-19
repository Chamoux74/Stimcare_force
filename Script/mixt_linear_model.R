library(lme4)
library(lmerTest)
require(nlme)
library(ggplot2)
library(outliers)
library(kableExtra)
library(pander)
library(tigerstats)
library(performance)
library(MuMIn)
library(phia)
library(emmeans)
library(sjPlot)

#factor DFIMVC_ssPOST48

DFIMVCmax1 <- read.csv("Data/DFIMVCmax_machine.csv")
DFIMVCmax_sanspost48 <- DFIMVCmax1

DFIMVCmax_sanspost48$instant_mesure <-
  factor(DFIMVCmax_sanspost48$instant_mesure ,
         levels = c("pre", "mid", "post"))

#description data

data_summary <- function(x) {
  m <- mean(x)
  ymin <- m-sd(x)
  ymax <- m+sd(x)
  return(c(y=m,ymin=ymin,ymax=ymax))
}

# on crée le graphique en indiquant le jeu de données et les variables d'intérêt
p<- ggplot(data=DFIMVCmax_sanspost48, aes(x= instant_mesure, y= IMVC, fill=condition) ) +
  geom_violin()+
  scale_fill_brewer(palette="PRGn")+
  theme(legend.position="right")+
  geom_dotplot(binaxis = "y", stackdir = "center", position = "dodge",dotsize=1/4)+
  stat_summary(fun.data=data_summary,geom="pointrange", color="red", size=0.50,position=position_dodge(0.9))
p

#Create first model

DFIMVCmax_sanspost48$VO2max <- as.factor(DFIMVCmax_sanspost48$VO2max)
DFIMVCmax_sanspost48$journee <- as.factor(DFIMVCmax_sanspost48$journee)
DFIMVCmax_sanspost48$Machine <- as.factor(DFIMVCmax_sanspost48$Machine)
DFIMVCmax_sanspost48$condition <- factor(DFIMVCmax_sanspost48$condition, levels = c("placebo", "patch"))

modele_mixte1 <-
  lme4::lmer(
    IMVC ~ condition * instant_mesure + (1 |
                                           sujet/journee/Machine/condition),
    data = DFIMVCmax_sanspost48,
    REML = FALSE
  )


#outlier and application condition

##lmer model

residus <- residuals(modele_mixte1, type="pearson",scaled=TRUE)
DFIMVCmax_sanspost48$residus<-residus
outliers::grubbs.test(DFIMVCmax_sanspost48$residus, type = 10, opposite = FALSE, two.sided = FALSE)

##clean
DFclean <- DFIMVCmax_sanspost48

data.frame()->valeur.influentes
while(outliers::grubbs.test(DFclean$residus, type = 10, opposite = FALSE, two.sided = FALSE)$p.value <0.05)  {
  max<-which.max(abs(DFclean$residus)) #cherche la valeur maximale qu'on stocke dans l'objet max                            # récupère les observations considérées comme influentes et les stocke
  valeur.influentes<-rbind(valeur.influentes,DFclean[max, ])
  DFclean<-DFclean[ -max, ] # supprime la valeur maximale de rat.clean
}

## normality test for residuals

n1<-shapiro.test(DFIMVCmax_sanspost48$residus)
n2<-shapiro.test(DFclean$residus)
r<-data.frame(W=c(n1$statistic, n2$statistic),
              p=c(n1$p.value, n2$p.value))
dimnames(r)[[1]]<-c("jeu de données complet", "jeu de données nettoyées lmer")
kable(pandoc.table(r, style='simple',split.tables=150))

#plot normality

ggplot(DFclean, aes(x=residus)) +
  geom_histogram(aes(y=after_stat(density)),      # Histogram with density instead of count on y-ax,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")

#ggqqplot

ggplot(DFclean, aes(sample=residus))+stat_qq()

#normality random parameters model lmer

aleatoires <- lmer(FMD~1+(1|sujet), data=test_clean3)

pr01 <- profile(aleatoires)
xyplot(pr01, aspect = 1.3, layout=c(3,1))
xyplot(pr01, aspect = 1.3, layout=c(3,1), absVal=T)

r_int<- ranef(modelemixte3)$sujet$"(Intercept)"
qqnorm(r_int)
shapiro.test(r_int)

#independance between random var and residual

splom(pr01)

#Testting interest of random effect lmer

ranova(modelemixte3)

#plot interest of random effect

dotplot(ranef(modelemixte3, condVar=T))

#ICC for random factor Sujet

icc(modelemixte3)

#test effet fixe

modelemixte4 <- lmer(FMD ~ condition:instant + (1|sujet), data = test_clean3, REML = F)

anova(modelemixte3, modelemixte4)

#test fixed and random effects

st <- step(modelemixte3) # this function test each fixed and random effect of model and determine
#if there is some effects who are not necessary to the mdoel
st

#plot(st)

#determination of variance in model trough F test value

anova(modelemixte3, type = 1)

# effect size

r.squaredGLMM(modelemixte3) # only for normal distributed variable for each condition
r.squaredLR(modelemixte3) # ok for all, with adjusted r squared which is more precise

#testing contrast

contrasts(test_clean3$instant)

#contr.instant(3, base = 1, contrasts = TRUE, sparse = FALSE)

#interaction test

testInteractions(modelemixte3)
means <- interactionMeans(modelemixte3)

plot(means)

testInteractions(modelemixte3, adjustment = "holm") #with holm correction to avoid type 1 error
testInteractions(modelemixte3, pairwise = "condition", fixed = "instant")

#second method to calculate interraction contrast

emm_options(lmer.df = "satterthwaite")
emmeans_out <- emmeans(modelemixte3, ~instant*condition, weights = "show.levels")
emmeans_out
plot(emmeans_out)
pair1 <- pairs(emmeans_out, adjust ="holm")
summary(pair1)
pair2 <- pairs(emmeans_out, by = c("instant"), adjust = "holm")
summary(pair2)
pair3 <- pairs(emmeans_out, by = c("condition"), adjust = "holm")
summary(pair3)

#plot contrast

emmeans_out1 <- summary(emmeans_out)
pd <- position_dodge(0.1)
ggplot(emmeans_out1, aes(x=instant , y=emmean, colour=condition, group= condition)) +
  geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE), colour="black", width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3)

#variance homogeneity

modelemixte3_vara <-
  lme(
    FMD ~ condition * instant,
    data = test_clean3,
    random =  ~ 1 | sujet,
    method = "ML",
    na.action = na.omit, weights = varIdent(form = ~1|instant))

VarCorr(modelemixte3_vara)
summary(modelemixte3_vara)$modelStruct$varStruct

#if not homogenous anova between two model

modelemixte1_1 <-  lme(
  FMD ~ condition * instant,
  data = test_clean3,
  random =  ~ 1 | sujet,
  method = "ML",
  na.action = na.omit)

anova(modelemixte1_1, modelemixte3_vara)

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



