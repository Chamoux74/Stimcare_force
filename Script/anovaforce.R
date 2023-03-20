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

#Sans la mesure en post48

test3 <- test[!test$instant_mesure == "post48" ,]
test4 <-test2[!test2$instant_mesure == "post48", ]

#analyse

res.aov1 <- rstatix::anova_test(
  data = test4, dv = forcemean, wid = sujet ,
  within = c(condition, instant_mesure) , effect.size = "ges",
  detailed = TRUE,
)

get_anova_table(res.aov1 , correction = "auto")

res.aov <- rstatix::anova_test(
  data = test3, dv = IMVC, wid = sujet ,
  within = c(condition, instant_mesure) , effect.size = "ges",
  detailed = TRUE,
)

get_anova_table(res.aov , correction = "auto")

one.way <- test3 %>%
  group_by(instant_mesure) %>%
  anova_test(dv = IMVC, wid = sujet, within = condition) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way

one.way1 <- test3 %>%
  group_by(condition) %>%
  anova_test(dv = IMVC, wid = sujet, within = instant_mesure) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way1

ttesttime <- test3 %>%
  group_by(condition) %>%
  pairwise_t_test(
    IMVC ~ instant_mesure, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
ttesttime

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
  theme_bw()

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

testindiv <- test3[test3$condition == "patch" , ]
testindiv1 <- test3[test3$condition == "placebo" , ]

testindiv$instant_mesure <- factor(testindiv$instant_mesure , levels = c("pre" , "mid", "post"))
testindiv1$instant_mesure <- factor(testindiv1$instant_mesure , levels = c("pre" , "mid" , "post"))

plotindiv <- ggplot(testindiv1, aes( x = instant_mesure , y = IMVC )) +
  theme_bw() +  #theme(
  #panel.grid.major.x = element_blank(),
  #panel.grid.minor.x = element_blank()
  #) +
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
  labs(title = "IMVC_placebo")

plotindiv

#analyse PRE/POST

test5 <- test3[!test3$instant_mesure == "mid", ]
test6 <- test4[!test4$instant_mesure == "mid", ]

ttest2 <- test5 %>%
  group_by(condition) %>%
  pairwise_t_test(
    IMVC ~ instant_mesure , paired = TRUE,
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
  theme_bw()

plotpostpost48

ttesttime5 <- ttest5[ttest5$condition == "patch",] %>% add_xy_position(x = "instant_mesure")
ttesttime6 <- ttest5[ttest5$condition == "placebo",] %>% add_xy_position(x = "instant_mesure")

plotpostpost48 +
  stat_pvalue_manual(
    ttesttime5,
    tip.length = 0 ,
    hide.ns = FALSE ,
    label = "p = {p.adj}"  , y.position = , color = "#00AFBB"
  ) +
  stat_pvalue_manual(
    ttesttime6,
    tip.length = 0 ,
    hide.ns = FALSE ,
    label = "p = {p.adj}"  , y.position = 625 , color = "#FC4E07"
  )

#Analyse PRE POST48



#analyse plus petite différence significative

model <- aov(IMVC ~ condition + instant_mesure, data = test)
summary(model)
out <- LSD.test(model,c("condition" , "instant_mesure"), p.adj="bonferroni")
out
#stargraph
# Variation range: max and min
plot(out)
