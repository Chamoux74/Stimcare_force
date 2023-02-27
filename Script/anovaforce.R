library(data.table)
library(ggpubr)
library(rstatix)
library(ggplot2)
library(agricolae)

#selectpatch

selectPREpatchforce <- dfmax[rownames(dfmax) %like% "PRE",]
selectpostpatchforce <- dfmax[rownames(dfmax) %like% "POST",]
selectmidpatchforce <- dfmax[rownames(dfmax) %like% "MID",]
selectpost48patchforce <- dfmax[rownames(dfmax) %like% "POST48",]

selectpostpatchforce <- selectpostpatchforce[-grep("POST48", rownames(selectpostpatchforce)), ]
selectpostpatchforce <- selectpostpatchforce[-grep("POSTARFD", rownames(selectpostpatchforce)), ]
selectpostpatchforce <- selectpostpatchforce[-grep("POSTBRFD", rownames(selectpostpatchforce)), ]

selectprepatchforce <- selectPREpatchforce[-grep("PREARFD", rownames(selectPREpatchforce)), ]
selectprepatchforce <- selectprepatchforce[-grep("PREBRFD", rownames(selectprepatchforce)), ]

selectmidpatchforce <- selectmidpatchforce[-grep("MIDBRFD", rownames(selectmidpatchforce)), ]
selectmidpatchforce <- selectmidpatchforce[-grep("MIDARFD", rownames(selectmidpatchforce)), ]


selectpost48patchforce <- selectpost48patchforce[-grep("POST48ARFD", rownames(selectpost48patchforce)), ]
selectpost48patchforce <- selectpost48patchforce[-grep("POST48BRFD", rownames(selectpost48patchforce)), ]

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



plotforce <- ggboxplot(
  dftot ,
  x = "instant_mesure",
  y = "IMVC",
  color = "condition",
  palette = c("#00AFBB" , "#FC4E07"),
  order = c(
    "pre" ,
    "mid" ,
    "post" ,
    "post48"
  ),
  add = "jitter" ,
  ylab = "IMVC",
  xlab = "instant_mesure" ,
  title = "IMVC_patch_placebo"
) +
  stat_summary(
    geom = "point",
    fun.y = mean , aes(group = condition) ,
    shape = 20 ,
    size = 4 ,
    position = position_dodge2(width = 0.75,
                               preserve = "single")
  ) +
  stat_summary(
    geom = "errorbar" ,
    fun.data = mean_sd , aes(group = condition) ,
    colour = "grey" ,
    linetype = "dotted" ,
    size = 1 , position = position_dodge2(width = 0.75,
                                          preserve = "single")
  )

plotforce

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


ggqqplot(dftot, "IMVC", ggtheme = theme_bw()) +
  facet_grid(instant_mesure ~ condition , labeller = "label_both")

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

test <- group_by(.data = dftot, sujet, instant_mesure , condition) %>%
  summarise_at(vars(IMVC), list(IMVC = max)) %>%
  as.data.frame()

test2 <- group_by(.data = dftot, sujet, instant_mesure , condition) %>%
  summarise_at(vars(IMVC), list(forcemean = mean)) %>%
  as.data.frame()

#dftot <- subset(dftot, !duplicated(dftot))

res.aov1 <- rstatix::anova_test(
  data = test2, dv = forcemean, wid = sujet ,
  within = c(condition, instant_mesure) , effect.size = "ges",
  detailed = TRUE,
)

get_anova_table(res.aov1 , correction = "auto")

res.aov <- rstatix::anova_test(
  data = test, dv = IMVC, wid = sujet ,
  within = c(condition, instant_mesure) , effect.size = "ges",
  detailed = TRUE,
)

get_anova_table(res.aov , correction = "auto")

ttest2 <- test2 %>%
  pairwise_t_test(
    forcemean ~ instant_mesure , paired = TRUE,
    p.adjust.method = "bonferroni"
  )

ttest1 <- test %>%
  pairwise_t_test(
    IMVC ~ instant_mesure , paired = TRUE,
    p.adjust.method = "bonferroni"
  )

ttest1

one.way <- test %>%
  group_by(instant_mesure) %>%
  anova_test(dv = IMVC, wid = sujet, within = condition) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way

one.way1 <- test %>%
  group_by(condition) %>%
  anova_test(dv = IMVC, wid = sujet, within = instant_mesure) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way1

ttesttime <- test %>%
  group_by(condition) %>%
  pairwise_t_test(
    IMVC ~ instant_mesure, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
ttesttime

comparaison <-
  list(
    c("pre", "mid") ,
    c("pre", "post") ,
    c("pre" , "post48") ,
    c("mid" , "post") ,
    c("mid" , "post48") ,
    c("post" , "post48")
  )

plotfmax <- ggboxplot(
  test ,
  x = "instant_mesure",
  y = "IMVC",
  color = "condition",
  palette = c("#00AFBB" , "#FC4E07"),
  order = c(
    "pre" ,
    "mid" ,
    "post" ,
    "post48"
  ),
  add = "jitter" ,
  ylab = "IMVC",
  xlab = "instant_mesure" ,
  title = "IMVC_patch_placebo"
) +
  stat_summary(
    geom = "point",
    fun.y = mean , aes(group = condition) ,
    shape = 20 ,
    size = 4 ,
    position = position_dodge2(width = 0.75,
                               preserve = "single")
  ) +
  stat_summary(
    geom = "errorbar" ,
    fun.data = mean_sd , aes(group = condition) ,
    colour = "grey" ,
    linetype = "dotted" ,
    size = 1 , position = position_dodge2(width = 0.75,
                                          preserve = "single")
  )

plotfmax

ttest1 <- ttest1 %>% add_xy_position(x = "instant_mesure")
ttest1$xmin <- c(2 , 2 , 2 , 3 , 3 , 4)
ttest1$xmax <- c(3 , 4 , 1 , 4 , 1 , 1)

plotfmax +
  stat_pvalue_manual(
    ttest1,
    tip.length = 0 ,
    hide.ns = FALSE ,
    label = "p = {p.adj}"  , y.position = c(1590, 1790, 500 , 530 , 400 , 1700)
  )

ttesttime1 <- ttesttime[ttesttime$condition == "patch",] %>% add_xy_position(x = "instant_mesure")
ttesttime2 <- ttesttime[ttesttime$condition == "placebo",] %>% add_xy_position(x = "instant_mesure")

ttesttime1$xmin <- c(2 , 2 , 2 , 3 , 3 , 4)
ttesttime1$xmax <- c(3, 4 , 1 , 4 , 1 , 1)

ttesttime2$xmin <- c(2 , 2 , 2 , 3 , 3 , 4)
ttesttime2$xmax <- c(3, 4 , 1 , 4 , 1 , 1)

testplot <- plotfmax +
  stat_pvalue_manual(
    ttesttime1 ,
    tip.length = 0 ,
    hide.ns = TRUE ,
    label = "p = {p.adj}" , color = "#00AFBB" , y.position = c(440 , 560 , 510)
  )
testplot

testplot +
  stat_pvalue_manual(
    ttesttime2 ,
    tip.length = 0 ,
    hide.ns = TRUE ,
    label = "p = {p.adj}" , color = "#FC4E07" , y.position = c(1740 , 1690, 1640 , 1830)
  )

model <- aov(IMVC ~ condition + instant_mesure, data = test)
summary(model)
out <- LSD.test(model,c("condition" , "instant_mesure"), p.adj="bonferroni")
out
#stargraph
# Variation range: max and min
plot(out)
