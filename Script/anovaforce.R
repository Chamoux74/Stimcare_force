library(data.table)
library(ggpubr)
library(rstatix)
library(ggplot2)
library(agricolae)
library(kableExtra)
library(stargazer)

#selectpatch

selectPREpatchforce <- dfmax[rownames(dfmax) %like% "PRE",]
selectpostpatchforce <- dfmax[rownames(dfmax) %like% "POST",]
selectmidpatchforce <- dfmax[rownames(dfmax) %like% "MID",]
selectpost48patchforce <- dfmax[rownames(dfmax) %like% "POST48",]


selectpostpatchforce <-
  selectpostpatchforce[-grep("POST48", rownames(selectpostpatchforce)),]
selectpostpatchforce <-
  selectpostpatchforce[-grep("POSTARFD", rownames(selectpostpatchforce)),]
selectpostpatchforce <-
  selectpostpatchforce[-grep("POSTBRFD", rownames(selectpostpatchforce)),]

selectprepatchforce <-
  selectPREpatchforce[-grep("PREARFD", rownames(selectPREpatchforce)),]
selectprepatchforce <-
  selectprepatchforce[-grep("PREBRFD", rownames(selectprepatchforce)),]

selectmidpatchforce <-
  selectmidpatchforce[-grep("MIDBRFD", rownames(selectmidpatchforce)),]
selectmidpatchforce <-
  selectmidpatchforce[-grep("MIDARFD", rownames(selectmidpatchforce)),]


selectpost48patchforce <-
  selectpost48patchforce[-grep("POST48ARFD", rownames(selectpost48patchforce)),]
selectpost48patchforce <-
  selectpost48patchforce[-grep("POST48BRFD", rownames(selectpost48patchforce)),]

#selectplacebo

selectpreplaceboforce <- dfmaxplacebo[rownames(dfmaxplacebo) %like% "PRE",]
selectpostplaceboforce <- dfmaxplacebo[rownames(dfmaxplacebo) %like% "POST",]
selectmidplaceboforce <- dfmaxplacebo[rownames(dfmaxplacebo) %like% "MID",]
selectpost48placeboforce <- dfmaxplacebo[rownames(dfmaxplacebo) %like% "POST48",]

selectpostplaceboforce <- selectpostplaceboforce[-grep("POST48", rownames(selectpostplaceboforce)), ]
selectpostplaceboforce <- selectpostplaceboforce[-grep("POSTARFD", rownames(selectpostplaceboforce)), ]
selectpostplaceboforce <- selectpostplaceboforce[-grep("POSTBRFD", rownames(selectpostplaceboforce)), ]

selectpreplaceboforce <- selectpreplaceboforce[-grep("PREARFD", rownames(selectpreplaceboforce)), ]
selectpreplaceboforce <- selectpreplaceboforce[-grep("PREBRFD", rownames(selectpreplaceboforce)), ]

selectmidplaceboforce <- selectmidplaceboforce[-grep("MIDBRFD", rownames(selectmidplaceboforce)), ]
selectmidplaceboforce <- selectmidplaceboforce[-grep("MIDARFD", rownames(selectmidplaceboforce)), ]

selectpost48placeboforce<- selectpost48placeboforce[-grep("POST48ARFD", rownames(selectpost48placeboforce)), ]
selectpost48placeboforce<- selectpost48placeboforce[-grep("POST48BRFD", rownames(selectpost48placeboforce)), ]

pre <- c("pre")
mid <- c("mid")
post <- c("post")
post48 <- c("post48")

placebo <- c("placebo")
patch <- c("patch")

selectprepatchforce <- cbind(selectprepatchforce , pre , patch )
colnames(selectprepatchforce) <- c("force" , "IMVC" , "instant_mesure" , "condition")

selectpreplaceboforce <- cbind(selectpreplaceboforce  , pre , placebo)
colnames(selectpreplaceboforce) <- c("force" , "IMVC" , "instant_mesure" , "condition")

selectmidpatchforce <- cbind(selectmidpatchforce  , mid , patch)
colnames(selectmidpatchforce) <- c("force" , "IMVC" , "instant_mesure" , "condition")

selectmidplaceboforce <- cbind(selectmidplaceboforce , mid , placebo)
colnames(selectmidplaceboforce) <- c("force" , "IMVC" , "instant_mesure" , "condition")

selectpostpatchforce <- cbind(selectpostpatchforce , post , patch)
colnames(selectpostpatchforce) <- c("force" , "IMVC" , "instant_mesure" , "condition")

selectpostplaceboforce <- cbind(selectpostplaceboforce , post , placebo)
colnames(selectpostplaceboforce) <- c("force" , "IMVC" , "instant_mesure" , "condition")

selectpost48patchforce <- cbind(selectpost48patchforce , post48 , patch)
colnames(selectpost48patchforce) <- c("force" , "IMVC" , "instant_mesure" , "condition")

selectpost48placeboforce <- cbind(selectpost48placeboforce , post48 , placebo)
colnames(selectpost48placeboforce) <- c("force" , "IMVC" , "instant_mesure" , "condition")

shapiro.test(selectprepatchforce$IMVC)
shapiro.test(selectpreplaceboforce$IMVC)
shapiro.test(selectmidpatchforce$IMVC)
shapiro.test(selectmidplaceboforce$IMVC)
shapiro.test(selectpostpatchforce$IMVC)
shapiro.test(selectpostplaceboforce$IMVC)
shapiro.test(selectpost48patchforce$IMVC)
shapiro.test(selectpost48placeboforce$IMVC)


identify_outliers(selectprepatchforce , IMVC , variable = NULL)
identify_outliers(selectpreplaceboforce , IMVC , variable = NULL)
identify_outliers(selectmidpatchforce , IMVC , variable = NULL)
identify_outliers(selectmidplaceboforce , IMVC , variable = NULL)
identify_outliers(selectpostpatchforce , IMVC , variable = NULL)
identify_outliers(selectpostplaceboforce , IMVC , variable = NULL)
identify_outliers(selectpost48patchforce , IMVC , variable = NULL)
identify_outliers(selectpost48placeboforce , IMVC , variable = NULL)


sujetprepatch <- gsub("^[^_]+_+[^A-Z]|_[^_]+$" , "" , row.names(selectprepatchforce))

sujetpreplacebo <- gsub("^[^_]+_+[^A-Z]|_[^_]+$" , "" , row.names(selectpreplaceboforce))

sujetmidpatch <- gsub("^[^_]+_+[^A-Z]|_[^_]+$" , "" , row.names(selectmidpatchforce))

sujetmidplacebo <- gsub("^[^_]+_+[^A-Z]|_[^_]+$" , "" , row.names(selectmidplaceboforce))

sujetpostpatch <- gsub("^[^_]+_+[^A-Z]|_[^_]+$" , "" , row.names(selectpostpatchforce))

sujetpostplacebo <- gsub("^[^_]+_+[^A-Z]|_[^_]+$" , "" , row.names(selectpostplaceboforce))

sujetpost48patch <- gsub("^[^_]+_+[^A-Z]|_[^_]+$" , "" , row.names(selectpost48patchforce))

sujetpost48placebo <- gsub("^[^_]+_+[^A-Z]|_[^_]+$" , "" , row.names(selectpost48placeboforce))


selectprepatchforce <- cbind(selectprepatchforce , sujetprepatch)
colnames(selectprepatchforce) <- c("force" , "IMVC" , "instant_mesure" , "condition" , "sujet")

selectpreplaceboforce <- cbind(selectpreplaceboforce  , sujetpreplacebo)
colnames(selectpreplaceboforce) <- c("force" , "IMVC" , "instant_mesure" , "condition" , "sujet")

selectmidpatchforce <- cbind(selectmidpatchforce  , sujetmidpatch)
colnames(selectmidpatchforce) <- c("force" , "IMVC" , "instant_mesure" , "condition" , "sujet")

selectmidplaceboforce <- cbind(selectmidplaceboforce , sujetmidplacebo)
colnames(selectmidplaceboforce) <- c("force" , "IMVC" , "instant_mesure" , "condition" , "sujet")

selectpostpatchforce <- cbind(selectpostpatchforce , sujetpostpatch)
colnames(selectpostpatchforce) <- c("force" , "IMVC" , "instant_mesure" , "condition" , "sujet")

selectpostplaceboforce <- cbind(selectpostplaceboforce , sujetpostplacebo)
colnames(selectpostplaceboforce) <- c("force" , "IMVC" , "instant_mesure" , "condition" , "sujet")

selectpost48patchforce <- cbind(selectpost48patchforce , sujetpost48patch)
colnames(selectpost48patchforce) <- c("force" , "IMVC" , "instant_mesure" , "condition" , "sujet")

selectpost48placeboforce <- cbind(selectpost48placeboforce , sujetpost48placebo)
colnames(selectpost48placeboforce) <- c("force" , "IMVC" , "instant_mesure" , "condition" , "sujet")

dftot <-
  rbind(
    selectprepatchforce ,
    selectpreplaceboforce ,
    selectmidpatchforce ,
    selectmidplaceboforce ,
    selectpostpatchforce ,
    selectpostplaceboforce ,
    selectpost48patchforce ,
    selectpost48placeboforce
  )

DFIMVCmax <- group_by(.data = dftot, sujet, instant_mesure , condition) %>%
  summarise_at(vars(IMVC), list(IMVC = max)) %>%
  as.data.frame()

DFIMVCmean <- group_by(.data = dftot, sujet, instant_mesure , condition) %>%
  summarise_at(vars(IMVC), list(IMVCmean = mean)) %>%
  as.data.frame()

# #Sans la mesure en post48
#
DFIMVCmax_sanspost48 <- filter(DFIMVCmax, instant_mesure  != "post48")
DFIMVCmean_sanspost48 <- filter(DFIMVCmean, instant_mesure != "post48")

#analyse

model <- lm(IMVC~condition*instant_mesure, data = DFIMVCmax_sanspost48)
plot(model, 1)
levene_test(data = DFIMVCmax_sanspost48, IMVC~condition*instant_mesure)

res.aov1 <- rstatix::anova_test(
  data = DFIMVCmax, dv = IMVC, wid = sujet ,
  within = c(condition, instant_mesure) , effect.size = "ges",
  detailed = TRUE,
)

res.aov1

table1 <- get_anova_table(res.aov1 , correction = "GG")
kable(ttesttime1,format = "latex")

res.aov <- rstatix::anova_test(
  data = DFIMVCmean_sanspost48, dv = IMVCmean, wid = sujet ,
  within = c(condition, instant_mesure) , effect.size = "ges",
  detailed = TRUE,
)

res.aov

#anova 1 way IMVCmax

one.way <- DFIMVCmax_sanspost48 %>%
  group_by(instant_mesure) %>%
  anova_test(dv = IMVC, wid = sujet, within = condition) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way

one.way1 <- DFIMVCmax %>%
  group_by(condition) %>%
  anova_test(dv = IMVC, wid = sujet, within = instant_mesure) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way1



#anova 1 way IMVCmean

one.way2 <- DFIMVCmean_sanspost48 %>%
  group_by(instant_mesure) %>%
  anova_test(dv = IMVCmean, wid = sujet, within = condition) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way2

one.way3 <- DFIMVCmean_sanspost48 %>%
  group_by(condition) %>%
  anova_test(dv = IMVCmean, wid = sujet, within = instant_mesure) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way3

#ttest IMVCmax

DFIMVCmax$instant_mesure <- factor(DFIMVCmax$instant_mesure, levels = c("pre", "mid", "post", "post48"))
DFIMVCmax_sanspost48$instant_mesure <-
  factor(DFIMVCmax_sanspost48$instant_mesure,
         levels = c("pre", "mid", "post"))

ttesttime1 <- DFIMVCmax_sanspost48 %>%
  group_by(condition) %>%
  pairwise_t_test(
    IMVC ~ instant_mesure, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
ttesttime1

ttestcond1 <- DFIMVCmax_sanspost48 %>%
  group_by(instant_mesure) %>%
  pairwise_t_test(
    IMVC ~ condition, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
ttestcond1

ESIMVCmax <-
  DFIMVCmax_sanspost48 %>% group_by(condition) %>% cohens_d(IMVC ~ instant_mesure, paired = T)

ESmax <- select(ESIMVCmax, effsize, magnitude)

ttesttime1 <- cbind(ttesttime1, ESmax)

ESIMVCmax1 <-
  DFIMVCmax %>% group_by(condition) %>% cohens_d(IMVC ~ instant_mesure, paired = T)

ESmax1 <- select(ESIMVCmax1, effsize, magnitude)

ttesttime1 <- cbind(ttesttime1, ESmax1)


#ttesT IMVCmean

ttesttime <- DFIMVCmean %>%
  group_by(condition) %>%
  pairwise_t_test(
    IMVCmean ~ instant_mesure, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
ttesttime

ttestcond <- DFIMVCmax1 %>%
  group_by(instant_mesure) %>%
  pairwise_t_test(
    IMVC ~ condition, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
ttestcond

ESIMVCmax <-
  DFIMVCmax %>% group_by(condition) %>% cohens_d(IMVC ~ instant_mesure, paired = T)

ESIMVCmax1 <-
  DFIMVCmax %>% group_by(instant_mesure) %>% cohens_d(IMVC ~ condition, paired = T)

plotfmax <- ggboxplot(
  test3 ,
  x = "instant_mesure",
  y = "IMVC",
  color = "condition",
  palette = c("#00AFBB" , "#FC4E07"),
  order = c(
    "pre" ,
    "mid" ,
    "post" #,
    #"post48"
  ),
  add = "jitter" ,size = 0.6 , shape = "condition" ,
  ylab = "IMVC",
  xlab = "instant_mesure" ,
  title = "IMVC_patch_placebo_PRE/MID/POST"
) +
  stat_summary(
    geom = "point",
    fun.y = mean , aes(group = condition) ,
    shape = 20 ,
    size = 4 ,
    position = position_dodge2(width = 0.75,
                               preserve = "single")
  ) +
  theme_bw() + theme(
    plot.title = element_text(hjust = 0.5 , size = 12 , face = "bold") ,
    axis.text = element_text(size = 7) ,
    axis.title = element_text(size = 8 , face = "bold") ,
    legend.position = "right" ,
    legend.title = element_text(size = 8 , face = "bold") ,
    legend.text = element_text(size = 6) ,
    legend.background = element_rect(color = "black" , size = 0.1)
  )

plotfmax

ttesttime1 <- ttesttime[ttesttime$condition == "patch",] %>% add_xy_position(x = "instant_mesure")
ttesttime2 <- ttesttime[ttesttime$condition == "placebo",] %>% add_xy_position(x = "instant_mesure")

ttesttime1$xmin <- c(1.8 , 2 , 2.8)
ttesttime1$xmax <- c(2.8, 1 , 0.8 )

ttesttime2$xmin <- c(2.2 , 2.2 , 3.2)
ttesttime2$xmax <- c(3.2 , 1.2 , 1.2 )

testplot <- plotfmax +
  stat_pvalue_manual(
    ttesttime1 ,
    tip.length = 0 ,
    hide.ns = TRUE ,
    label = "p = {p.adj}" , color = "#00AFBB" , y.position = c(530 , 440)
  ) +
  stat_pvalue_manual(
    ttesttime2 ,
    tip.length = 0 ,
    hide.ns = TRUE ,
    label = "p = {p.adj}" , color = "#FC4E07" , y.position = c(1540 , 1690, 1770)
  )

testplot

#analyse indiv pre mid post

test3$instant_mesure <-
  factor(test3$instant_mesure , levels = c("pre" , "mid", "post"))

plotindiv3 <- ggplot(test3, aes( x = instant_mesure , y = IMVC )) +
  theme_bw() +  theme(
    plot.title = element_text(hjust = 0.5 , size = 12 , face = "bold") ,
    axis.text = element_text(size = 7) ,
    axis.title = element_text(size = 8 , face = "bold") ,
    strip.background = element_rect(color = "black" , fill = "#373737")
    ,
    strip.text = element_text(
      color = "white" ,
      face = "bold" ,
      size = 8
    ) ,
    legend.position = "right" ,
    legend.title = element_text(size = 8 , face = "bold") ,
    legend.text = element_text(size = 6) ,
    legend.background = element_rect(color = "black" , size = 0.1)
  ) +
  geom_line(
    aes(
      x = instant_mesure ,
      group = sujet ,
      color = as.factor(sujet)
    ) ,
    size = 0.7 ,
    position = "identity" ,
    linetype = "dashed"
  ) +
  geom_point(
    aes(x = instant_mesure , group = sujet),
    shape = 21,
    colour = "black",
    size = 2,
    position = "identity"
  ) +
  geom_boxplot( outlier.shape = NA, coef = 0 ,
                aes(x = instant_mesure , y = IMVC) ,
                width = .35,
                fill = "white" , alpha = 0.3
  )  +
  scale_color_manual(
    values = c(
      #"purple" ,
      "#0416f5" ,
                "#b00000" ,
                "#19a3e8" ,
                "#fd4c4c" ,
                "#E7B800" ,
                "#5ef11a" ,
                "#c58ede" ,
                "#3e020b" ,
                "#febd02" ,
                "#16161e" ,
                "#24844b" ,
                "#f604fd" ,
                "#439bab" ,
                "#c5c896" ,
                "#6e711d" ,
                "#109c02" #,
                #"#b71385"
    )) +
  labs(color = "sujet") +
  stat_summary(
    geom = "errorbar" ,
    fun.data = mean_sd ,
    colour = "black" ,
    size = 1 ,
    width = 0.2
  ) +
  stat_summary(
    fun = mean,
    shape = 17 ,
    size = 1 ,
    position = "identity",
    color = "#ff0000"
  ) +
  facet_wrap(vars(condition) , scales = "free_y") +
  labs(title = "IMVC_individual_variation_PRE/MID/POST")

plotindiv3


#analyse PRE/POST

test5 <- filter(DFIMVCmax1, instant_mesure != "mid")
test6 <- test4[!test4$instant_mesure == "mid", ]

ttest2 <- test5 %>%
  group_by(instant_mesure) %>%
  pairwise_t_test(
    IMVC ~ condition , paired = TRUE,
    p.adjust.method = "bonferroni"
  )

ttest2

ttest3 <- test6 %>%
  group_by(condition) %>%
  pairwise_t_test(
    forcemean ~ instant_mesure , paired = TRUE,
    p.adjust.method = "bonferroni"
  )

ttest3

ttest4 <- test5 %>%
  group_by(instant_mesure) %>%
  pairwise_t_test(
    IMVC ~ condition , paired = TRUE,
    p.adjust.method = "bonferroni"
  )

ttest4

#Analyse POST /POST48

test7 <- test[!test$instant_mesure %in% c("pre" ,"mid") ,]
test8 <- test2[!test2$instant_mesure %in% c("pre", "mid") ,  ]

ttest5 <- test7 %>%
  group_by(condition) %>%
  pairwise_t_test(
    IMVC ~ instant_mesure , paired = TRUE,
    p.adjust.method = "bonferroni"
  )

ttest5

ttest6 <- test7 %>%
  group_by(instant_mesure) %>%
  pairwise_t_test(
    IMVC ~ condition , paired = TRUE,
    p.adjust.method = "bonferroni"
  )

ttest6

#plot POST POST48

plotpostpost48 <- ggboxplot(
  test7 ,
  x = "instant_mesure",
  y = "IMVC",
  color = "condition",
  palette = c("#00AFBB" , "#FC4E07"),
  order = c(
    #"pre" ,
    #"mid" ,
    "post" ,
    "post48"
  ),
  add = "jitter" , size = 0.6 , shape = "condition" ,
  ylab = "IMVC",
  xlab = "instant_mesure" ,
  title = "IMVC_patch_placebo_POST/POST48"
) +
  stat_summary(
    geom = "point",
    fun.y = mean , aes(group = condition) ,
    shape = 20 ,
    size = 4 ,
    position = position_dodge2(width = 0.75,
                               preserve = "single")
  ) +
  theme_bw() +theme(
    plot.title = element_text(hjust = 0.5 , size = 12 , face = "bold") ,
    axis.text = element_text(size = 7) ,
    axis.title = element_text(size = 8 , face = "bold") ,
    legend.position = "right" ,
    legend.title = element_text(size = 8 , face = "bold") ,
    legend.text = element_text(size = 6) ,
    legend.background = element_rect(color = "black" , size = 0.1)
  ) +
  ylim(500, 1750)

plotpostpost48

ttesttime5 <-
  ttest5[ttest5$condition == "patch", ] %>% add_xy_position(x = "instant_mesure")
ttesttime6 <-
  ttest5[ttest5$condition == "placebo", ] %>% add_xy_position(x = "instant_mesure")

ttesttime5$xmin <- 0.8
ttesttime5$xmax <- 1.8
ttesttime6$xmin <- 1.2
ttesttime6$xmax <- 2.2


plotpostpost48 +
  stat_pvalue_manual(
    ttesttime5,
    tip.length = 0 ,
    hide.ns = FALSE ,
    label = "p = {p.adj}"  , y.position =1650 , color = "#00AFBB"
  ) +
  stat_pvalue_manual(
    ttesttime6,
    tip.length = 0 ,
    hide.ns = FALSE ,
    label = "p = {p.adj}"  , y.position = 625 , color = "#FC4E07"
  )

#Analyse PRE POST48

test9 <- test[!test$instant_mesure %in% c("post" ,"mid") ,]

ttest7 <- test9 %>%
  group_by(condition) %>%
  pairwise_t_test(
    IMVC ~ instant_mesure , paired = TRUE,
    p.adjust.method = "bonferroni"
  )

ttest7

ttest8 <- test9 %>%
  group_by(instant_mesure) %>%
  pairwise_t_test(
    IMVC ~ condition , paired = TRUE,
    p.adjust.method = "bonferroni"
  )

ttest8

#plot pre post48

plotprepost48 <- ggboxplot(
  test9 ,
  x = "instant_mesure",
  y = "IMVC",
  color = "condition",
  palette = c("#00AFBB" , "#FC4E07"),
  order = c(
    "pre" ,
    #"mid" ,
    #"post" ,
    "post48"
  ),
  add = "jitter" , size = 0.6 , shape = "condition" ,
  ylab = "IMVC",
  xlab = "instant_mesure" ,
  title = "IMVC_patch_placebo_PRE/POST48"
) +
  stat_summary(
    geom = "point",
    fun.y = mean , aes(group = condition) ,
    shape = 20 ,
    size = 4 ,
    position = position_dodge2(width = 0.75,
                               preserve = "single")
  ) +
  theme_bw() + theme(
    plot.title = element_text(hjust = 0.5 , size = 12 , face = "bold") ,
    axis.text = element_text(size = 7) ,
    axis.title = element_text(size = 8 , face = "bold") ,
    legend.position = "right" ,
    legend.title = element_text(size = 8 , face = "bold") ,
    legend.text = element_text(size = 6) ,
    legend.background = element_rect(color = "black" , size = 0.1)
  )

plotprepost48

ttesttime7 <- ttest7[ttest7$condition == "patch",] %>% add_xy_position(x = "instant_mesure")
ttesttime8 <- ttest7[ttest7$condition == "placebo",] %>% add_xy_position(x = "instant_mesure")

ttesttime7$xmin <- c(1.8)
ttesttime7$xmax <- c(0.8)
ttesttime8$xmin <- c(2.2)
ttesttime8$xmax <- c(1.2)

ttest8 <- ttest8 %>% add_xy_position(x= "instant_mesure")

ttestcondi <- ttest8[ttest8$instant_mesure == "post48" , ]
ttestcondi1 <- ttest8[ttest8$instant_mesure == "pre" , ]

ttestcondi$xmin <- c(2)
ttestcondi$xmax <- c(2)
ttestcondi1$xmin <- c(1)
ttestcondi1$xmax <- c(1)

ttest8 <- rbind(ttestcondi , ttestcondi1)

plotprepost48 +
  stat_pvalue_manual(
    ttesttime7,
    tip.length = 0 ,
    hide.ns = FALSE ,
    label = "p = {p.adj}"  , y.position = 1650 , color = "#00AFBB"
  ) +
  stat_pvalue_manual(
    ttesttime8,
    tip.length = 0 ,
    hide.ns = FALSE ,
    label = "p = {p.adj}"  , y.position = 670 , color = "#FC4E07" , linetype = "dashed"
  ) +
  stat_pvalue_manual(
    ttest8,
    tip.length = 0 ,
    hide.ns = FALSE ,
    label = "p = {p.adj}" , y.position = c(825 , 825)
  )

#analyse indiv pre post48

test9$instant_mesure <- factor(test9$instant_mesure , levels = c("pre" , "post48"))


plotindiv2 <- ggplot(test9, aes( x = instant_mesure , y = IMVC )) +
  theme_bw() + theme(
    plot.title = element_text(hjust = 0.5 , size = 12 , face = "bold") ,
    axis.text = element_text(size = 7) ,
    axis.title = element_text(size = 8 , face = "bold") ,
    strip.background = element_rect(color = "black" , fill = "#373737")
    ,
    strip.text = element_text(
      color = "white" ,
      face = "bold" ,
      size = 8
    ) ,
    legend.position = "right" ,
    legend.title = element_text(size = 8 , face = "bold") ,
    legend.text = element_text(size = 6) ,
    legend.background = element_rect(color = "black" , size = 0.1)
  ) +
  geom_line(
    aes(
      x = instant_mesure ,
      group = sujet ,
      color = as.factor(sujet)
    ) ,
    size = 0.7 ,
    position = "identity" ,
    linetype = "dashed"
  ) +
  geom_point(
    aes(x = instant_mesure , group = sujet),
    shape = 21,
    colour = "black",
    size = 2,
    position = "identity"
  ) +
  geom_boxplot( outlier.shape = NA, coef = 0 ,
                aes(x = instant_mesure , y = IMVC) ,
                width = .35,
                fill = "white" , alpha = 0.3
  )  +
  scale_color_manual(
    values = c(
      #"purple" ,
      "#0416f5" ,
                "#b00000" ,
                "#19a3e8" ,
                "#fd4c4c" ,
                "#E7B800" ,
                "#5ef11a" ,
                "#c58ede" ,
                "#3e020b" ,
                "#febd02" ,
                "#16161e" ,
                "#24844b" ,
                "#f604fd" ,
                "#439bab" ,
                "#c5c896" ,
                "#6e711d" ,
                "#109c02" #,
                #"#b71385"
    )) +
  labs(color = "sujet") +
  stat_summary(
    geom = "errorbar" ,
    fun.data = mean_sd ,
    colour = "black" ,
    size = 1 ,
    width = 0.2
  ) +
  stat_summary(
    fun = mean,
    shape = 17 ,
    size = 1 ,
    position = "identity",
    color = "#ff0000"
  ) +
  facet_wrap(vars(condition) , scales = "free_y") +
  labs(title = "IMVC_individual_variation_PRE/POST48")

plotindiv2

#analyse plus petite différence significative

model <- aov(IMVC ~ condition + instant_mesure, data = test)
summary(model)
out <- LSD.test(model,c("condition" , "instant_mesure"), p.adj="bonferroni")
out
#stargraph
# Variation range: max and min
plot(out)
