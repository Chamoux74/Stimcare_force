library(lme4)
library(lmerTest)
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
library(nlme)
library(xtable)
library(robustlmm)

#factor DFIMVC_ssPOST48

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

DFIMVCmax_sanspost48$condition <- factor(DFIMVCmax_sanspost48$condition, levels = c("placebo", "patch"))

modele_mixte1 <-
  lme4::lmer(
    IMVC ~ condition * instant_mesure + (1 |
                                           sujet/condition),
    data = DFIMVCmax_sanspost48,
    REML = FALSE
  )

modele_mixte1_1 <-
  rlmer(
    IMVC ~ condition * instant_mesure + (1 |sujet/condition),
    data = DFIMVCmax_sanspost48,
    REML = FALSE
  )



summary(modele_mixte1_1)
summary(modele_mixte1)

tab_model(modele_mixte1_1)

anova(modele_mixte1, modele_mixte1_1)

#outlier and application condition

##lmer model

residus <- residuals(modele_mixte1_1, type="pearson",scaled=TRUE)
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
aleatoires <- lmer(IMVC~1+(1|sujet/condition), data= DFIMVCmax_sanspost48)

pr01 <- profile(aleatoires)
xyplot(pr01, aspect = 1.3, layout=c(3,1))
xyplot(pr01, aspect = 1.3, layout=c(3,1), absVal=T)

r_int<- ranef(modele_mixte1)$sujet$"(Intercept)"
qqnorm(r_int)
shapiro.test(r_int)

#independance between random var and residual

splom(pr01)

#Testting interest of random effect lmer

ranova(modele_mixte1_1)

#plot interest of random effect
ranef(modele_mixte1)
dotplot(ranef(modele_mixte1, condVar=T))

#ICC for random factor Sujet


icc(modele_mixte1_1)

#test effet fixe

anova(modele_mixte1_1)

#determination of variance in model trough F test value

anova(modele_mixte1, type= 3)

# effect size

modele_mixte1_2 <-
  nlme::lme(
    IMVC ~ condition * instant_mesure,
    data = DFIMVCmax_sanspost48,
    random = ~ 1 | sujet / condition,
    method = "ML"
  )

r.squaredGLMM(modele_mixte1_1) # only for normal distributed variable for each condition
r.squaredLR(modele_mixte1_2) # ok for all, with adjusted r squared which is more precise


#contr.instant(3, base = 1, contrasts = TRUE, sparse = FALSE)

#interaction test

phia::testInteractions(modele_mixte1)
means <- phia::interactionMeans(modele_mixte1)

plot(means)

testInteractions(modele_mixte1_1, adjustment = "none")
testInteractions(modele_mixte1_1, adjustment = "holm") #with holm correction to avoid type 1 error

int_instant <- phia::testInteractions(
  modele_mixte1,
  pairwise = "condition",
  fixed = "instant_mesure",
  adjustment = "holm"
)# test with simple effect

int_cond <- phia::testInteractions(
  modele_mixte1,
  pairwise = "instant_mesure",
  fixed = "condition",
  adjustment = "holm"
)

#second method to calculate interraction contrast

emm_options(lmer.df = "satterthwaite")
emmeans_out <- emmeans(modele_mixte1_1, ~instant_mesure*condition, weights = "show.levels")
emmeans_out
plot(emmeans_out)
pair1 <- pairs(emmeans_out, adjust ="holm")
summary(pair1)
pair2 <- pairs(emmeans_out, by = c("instant_mesure"), adjust = "holm", )
summary(pair2)
pair3 <- pairs(emmeans_out, by = c("condition"), adjust = "holm")
summary(pair3)

#variance homogeneity

modele_mixte1_2_vara <- nlme::lme(
  IMVC ~ condition * instant_mesure,
  data = DFIMVCmax_sanspost48,
  random = ~ 1 | sujet / condition,
  method = "ML",
  weights = varIdent(form = ~1|instant_mesure))

VarCorr(modele_mixte1_2_vara)
summary(modele_mixte1_2_vara)$modelStruct$varStruct

#if not homogenous anova between two model

anova(modele_mixte1_2, modele_mixte1_2_vara)

# plot to investigated crossed fixed and random effect in repeated measures
interaction.plot(DFIMVCmax_sanspost48$condition, DFIMVCmax_sanspost48$sujet, DFIMVCmax_sanspost48$IMVC, las=1,
                 trace.label="identifiant du sujet", xlab="Traitement", ylab="IMVCmax")

interaction.plot(DFIMVCmax_sanspost48$instant_mesure, DFIMVCmax_sanspost48$sujet, DFIMVCmax_sanspost48$IMVC, las=1,
                 trace.label="identifiant du sujet", xlab="instant", ylab="IMVC")

# plot individual variation

p<-ggplot(DFIMVCmax_sanspost48, aes(x=instant_mesure, y=IMVC, colour=sujet, group=sujet))+
  geom_line() +
  facet_wrap(~condition)
p


#covariance matrix structure

ACF(modele_mixte1_2)
plot(ACF(modele_mixte1_2, maxLag = 10),alpha = 0.01)

modelemixte1_2_1 <- update(modele_mixte1_2, correlation=corAR1(form=~1|sujet/condition))
#covariance structure = autoregressiv, know if we upgrade matrix witrh the new one

anova(modele_mixte1_2, modelemixte1_2_1)
#no differences, with spécifics covariance model

#plot linear model

plot_model(modele_mixte1, show.values = T, width = 0.1, show.p = T)
t <- tab_model(modele_mixte1, show.reflvl = T, show.intercept = F, p.style = "numeric", show.se = T)

