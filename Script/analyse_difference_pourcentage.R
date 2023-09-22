library(magrittr)
library(dplyr)
library(stats)
library(rstatix)

# comparaison de pourcentage PRE POST

preplacebo <-
  DFIMVCmax %>%  filter(condition == "placebo") %>% filter(instant_mesure == "pre") %>% select(IMVC)

prepatch <-
  DFIMVCmax %>%  filter(condition == "patch") %>% filter(instant_mesure == "pre") %>% select(IMVC)

postplacebo <-
  DFIMVCmax %>%  filter(condition == "placebo") %>% filter(instant_mesure == "post") %>% select(IMVC)

postpatch <-
  DFIMVCmax %>%  filter(condition == "patch") %>% filter(instant_mesure == "post") %>% select(IMVC)

midplacebo <-
  DFIMVCmax %>%  filter(condition == "placebo") %>% filter(instant_mesure == "mid") %>% select(IMVC)

midpatch <-
  DFIMVCmax %>%  filter(condition == "patch") %>% filter(instant_mesure == "mid") %>% select(IMVC)

post48placebo <-
  DFIMVCmax %>%  filter(condition == "placebo") %>% filter(instant_mesure == "post48") %>% select(IMVC)

post48patch <-
  DFIMVCmax %>%  filter(condition == "patch") %>% filter(instant_mesure == "post48") %>% select(IMVC)

df1 <-
  cbind(
    preplacebo,
    prepatch,
    postplacebo,
    postpatch,
    midplacebo,
    midpatch,
    post48placebo,
    post48patch
  )

colnames(df1) <-
  c(
    "IMVC_PRE_PB",
    "IMVC_PRE_P" ,
    "IMVC_POST_PB",
    "IMVC_POST_P",
    "IMVC_MID_PB",
    "IMVC_MID_P",
    "IMVC_POST48_PB",
    "IMVC_POST48_P"
  )

dfprepostpb <- mutate(df1 , PRE_POST_PB = (IMVC_POST_PB - IMVC_PRE_PB)/IMVC_PRE_PB*100) %>% select(PRE_POST_PB)
dfprepostp <- mutate(df1 , PRE_POST_P = (IMVC_POST_P - IMVC_PRE_P)/IMVC_PRE_P*100) %>% select(PRE_POST_P)
dfpremidpb <- mutate(df1 , PRE_MID_PB = (IMVC_MID_PB - IMVC_PRE_PB)/IMVC_PRE_PB*100) %>% select(PRE_MID_PB)
dfpremidp <- mutate(df1 , PRE_MID_P = (IMVC_MID_P - IMVC_PRE_P)/IMVC_PRE_P*100)%>% select(PRE_MID_P)
dfprepost48pb <- mutate(df1 , PRE_POST48_PB = (IMVC_POST48_PB - IMVC_PRE_PB)/IMVC_PRE_PB*100) %>% select(PRE_POST48_PB)
dfprepost48p <- mutate(df1 , PRE_POST48_P = (IMVC_POST48_P - IMVC_PRE_P)/IMVC_PRE_P*100) %>% select(PRE_POST48_P)
dfmidpostpb <- mutate(df1 , MID_POST_PB = (IMVC_POST_PB - IMVC_MID_PB)/IMVC_MID_PB*100) %>% select(MID_POST_PB)
dfmidpostp <- mutate(df1 , MID_POST_P = (IMVC_POST_P-IMVC_MID_P)/IMVC_MID_P*100) %>% select (MID_POST_P)
dfmidpost48pb <- mutate(df1 , MID_POST48_PB = (IMVC_POST48_PB - IMVC_MID_PB)/IMVC_MID_PB*100) %>% select(MID_POST48_PB)
dfmidpost48p <- mutate(df1 , MID_POST48_P = (IMVC_POST48_P - IMVC_MID_P)/IMVC_MID_P*100) %>% select (MID_POST48_P)
dfpostpost48pb <- mutate(df1 , POST_POST48_PB = (IMVC_POST48_PB - IMVC_POST_PB)/IMVC_POST_PB*100) %>% select(POST_POST48_PB)
dfpostpost48p <- mutate(df1 , POST_POST48_P = (IMVC_POST48_P - IMVC_POST_P)/IMVC_POST_P*100) %>% select (POST_POST48_P)

sujet <- c("AB", "BA", "BM", "BR", "GA", "GM", "JS","MA","MF", "PN", "RF", "RO", "SL", "SP", "TM", "VP")
post <- c("post")
mid <- c("mid")
pre <- c("pre")
midpost <-c("midpost")
post48h <- c("post48")

predif <- rep(0, times = 16)

dfprepostpb <- cbind(dfprepostpb, placebo, sujet , post)
dfprepostp <- cbind(dfprepostp, patch, sujet, post)
dfpremidpb <- cbind(dfpremidpb, placebo, sujet, mid)
dfpremidp <- cbind(dfpremidp, patch, sujet, mid)
dfprepost48pb <- cbind(dfprepost48pb, placebo, sujet, post48h)
dfprepost48p <- cbind(dfprepost48p,patch, sujet, post48h)
dfmidpostpb <- cbind(dfmidpostpb, placebo, sujet, midpost)
dfmidpostp <- cbind(dfmidpostp, patch, sujet, midpost)
dfprepb<- cbind(predif, placebo, sujet, pre)
dfprep <- cbind(predif, patch, sujet, pre)

colnames(dfprepostp) <- c("IMVCdif" , "condition", "sujet", "instant")
colnames(dfprepostpb) <- c("IMVCdif" , "condition","sujet", "instant")
colnames(dfpremidp) <- c("IMVCdif" , "condition", "sujet", "instant")
colnames(dfpremidpb) <- c("IMVCdif" , "condition","sujet", "instant")
colnames(dfprepost48p) <- c("IMVCdif" , "condition", "sujet", "instant")
colnames(dfprepost48pb) <- c("IMVCdif" , "condition","sujet", "instant")
colnames(dfprep) <- c("IMVCdif" , "condition", "sujet", "instant")
colnames(dfprepb) <- c("IMVCdif" , "condition","sujet", "instant")
colnames(dfmidpostp) <- c("IMVCdif" , "condition", "sujet", "instant")
colnames(dfmidpostpb) <- c("IMVCdif" , "condition", "sujet", "instant")
colnames(dfprep) <- c("IMVCdif" , "condition", "sujet", "instant")
colnames(dfprepb) <- c("IMVCdif" , "condition", "sujet", "instant")

dfpourcentage <-
  rbind(dfprepostp ,
        dfprepostpb,
        dfpremidp,
        dfpremidpb)

dfpourcentage$IMVCdif <- as.numeric(dfpourcentage$IMVCdif)

#shapiro

shapiro.test(dfprepostpb$IMVCdif)

#anova

res_aov <- rstatix::anova_test(
  data = dfpourcentage, dv = IMVCdif, wid = sujet ,
  within = c(condition, instant) , effect.size = "ges",
  detailed = TRUE,
)

res_aov

table2 <- get_anova_table(res_aov , correction = "GG")
kable(table2,format = "latex")

one.way <- dfpourcentage %>%
  group_by(instant) %>%
  anova_test(dv = IMVCdif, wid = sujet, within = condition) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way

table3 <- get_anova_table(one.way , correction = "GG")
kable(table3,format = "latex")

one.way1 <- dfpourcentage %>%
  group_by(condition) %>%
  anova_test(dv = IMVCdif, wid = sujet, within = instant) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way1

table4 <- get_anova_table(one.way1 , correction = "GG")
kable(table4,format = "latex")

ttestpourc1 <-
  dfpourcentage %>% group_by(condition) %>% t_test(IMVCdif ~ instant ,
                                                 paired = T,
                                                 p.adjust.method = "bonferroni")
ttestpourc1

table5 <- get_anova_table(ttestpourc1 , correction = "GG")
kable(table5,format = "latex")

# plot différence PRE POST pourcentage + pvalue

statTtest <- dfpourcentage %>%
  group_by(instant) %>%
  t_test(IMVCdif ~ condition) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance()

dfpourcentage %>% ggplot(aes(x = instant , y = IMVCdif, fill = condition)) +
  stat_summary(fun.data = mean_se, position = position_dodge(0.65), geom = "errorbar", width = 0.3) +
  stat_summary(fun.y = mean ,position = position_dodge(width = 0.65), geom = "bar", width = 0.6, color = "black") +
  scale_fill_manual(values = c("black", "grey")) +
  ylab("Percentage change in KE IMVC from PRE to MID / POST / POST48 (%)") +
  # stat_summary(aes(label = round(..y.., 2)),
  #   geom = "text",
  #   fun.y = mean,
  #   size = 4) +
    theme_bw() +
    theme(
    plot.title = element_text(hjust = 0.5 , size = 12 , face = "bold") ,
    axis.text = element_text(size = 7) ,
    axis.title = element_text(size = 8 , face = "bold") ,
    legend.position = "right" ,
    legend.title = element_text(size = 8 , face = "bold") ,
    legend.text = element_text(size = 6) ,
    legend.background = element_rect(color = "black" , size = 0.1)
  ) +
  stat_compare_means(label = "p.format" ,
                     method = "t.test", paired = T , label.y = c(-16, -16, 10.5), bracket.size = 10)

bob <- test3 %>% aggregate(
  IMVC ~ condition + instant_mesure,
  FUN = function(x) {
    mean(x)
  }
)

dfprepost %>% ggbarplot("condition" ,
                  "IMVC_PRE_POST_%" ,
                  fill = "condition",
                  label = TRUE)
