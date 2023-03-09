library(data.table)
library(ggpubr)
library(rstatix)
library(ggplot2)
library(agricolae)

#selectpatch

selectprepatchrfd <-
  as.data.frame(dfrfdpatch[rownames(dfrfdpatch) %like% "PRE",])
selectpostpatchrfd <-
  as.data.frame (dfrfdpatch[rownames(dfrfdpatch) %like% "POST",])
selectmidpatchrfd <-
  as.data.frame(dfrfdpatch[rownames(dfrfdpatch) %like% "MID",])
selectpost48patchrfd <-
  as.data.frame(dfrfdpatch[rownames(dfrfdpatch) %like% "POST48",])

colnames(selectprepatchrfd)  <- "rfd"
colnames(selectpostpatchrfd)  <- "rfd"
colnames(selectmidpatchrfd)  <- "rfd"
colnames(selectpost48patchrfd)  <- "rfd"

#enlever les mesure qui nous intérressent pas et remettre le nom des lignes pour chaque instant de mesure

patternpost <- "POST48|POSTAIMVC|POSTBIMVC|GM|JS|MA|MF"
row_names_listpost <-
  -grep(patternpost , rownames(selectpostpatchrfd)) %>% as.numeric()
row_names_listpost <- rownames(selectpostpatchrfd)[row_names_listpost]

selectpostpatchrfd <-
  as.data.frame(selectpostpatchrfd[rownames(selectpostpatchrfd) %in% row_names_listpost,]) %>%
  `rownames<-`(row_names_listpost)

patternpre <- "PREAIMVC|PREBIMVC|GM|JS|MA|MF"
row_names_listpre <-
  -grep(patternpre , rownames(selectprepatchrfd)) %>% as.numeric()
row_names_listpre <- rownames(selectprepatchrfd)[row_names_listpre]

selectprepatchrfd <-
  as.data.frame(selectprepatchrfd[rownames(selectprepatchrfd) %in% row_names_listpre,]) %>%
  `rownames<-`(row_names_listpre)

patternmid <- "MIDAIMVC|MIDBIMVC|GM|JS|MA|MF"
row_names_listmid <-
  -grep(patternmid , rownames(selectmidpatchrfd)) %>% as.numeric()
row_names_listmid <- rownames(selectmidpatchrfd)[row_names_listmid]

selectmidpatchrfd <-
  as.data.frame(selectmidpatchrfd[rownames(selectmidpatchrfd) %in% row_names_listmid,]) %>%
  `rownames<-`(row_names_listmid)

patternpost48 <- "POST48AIMVC|POST48BIMVC|GM|JS|MA|MF"
row_names_listpost48 <-
  -grep(patternpost48 , rownames(selectpost48patchrfd)) %>% as.numeric()
row_names_listpost48 <- rownames(selectpost48patchrfd)[row_names_listpost48]

selectpost48patchrfd <-
  as.data.frame(selectpost48patchrfd[rownames(selectpost48patchrfd) %in% row_names_listpost48,]) %>%
  `rownames<-`(row_names_listpost48)

#selectplacebo

selectpreplaceborfd <- as.data.frame(dfrfdpb[rownames(dfrfdpb) %like% "PRE",])
selectpostplaceborfd <- as.data.frame(dfrfdpb[rownames(dfrfdpb) %like% "POST",])
selectmidplaceborfd <- as.data.frame(dfrfdpb[rownames(dfrfdpb) %like% "MID",])
selectpost48placeborfd <- as.data.frame(dfrfdpb[rownames(dfrfdpb) %like% "POST48",])

colnames(selectpreplaceborfd)  <- "rfd"
colnames(selectpostplaceborfd)  <- "rfd"
colnames(selectmidplaceborfd)  <- "rfd"
colnames(selectpost48placeborfd)  <- "rfd"

patternpostpb <- "POST48|POSTAIMVC|POSTBIMVC|PN|BA"
row_names_listpostpb <-
  -grep(patternpostpb , rownames(selectpostplaceborfd)) %>% as.numeric()
row_names_listpostpb <- rownames(selectpostplaceborfd)[row_names_listpostpb]

selectpostplaceborfd <-
  as.data.frame(selectpostplaceborfd[rownames(selectpostplaceborfd) %in% row_names_listpostpb,]) %>%
  `rownames<-`(row_names_listpostpb)

patternprepb <- "PREAIMVC|PREBIMVC|PN|BA"
row_names_listprepb <-
  -grep(patternprepb , rownames(selectpreplaceborfd)) %>% as.numeric()
row_names_listprepb <- rownames(selectpreplaceborfd)[row_names_listprepb]

selectpreplaceborfd <-
  as.data.frame(selectpreplaceborfd[rownames(selectpreplaceborfd) %in% row_names_listprepb,]) %>%
  `rownames<-`(row_names_listprepb)

patternmidpb <- "MIDAIMVC|MIDBIMVC|PN|BA"
row_names_listmidpb <-
  -grep(patternmidpb , rownames(selectmidplaceborfd)) %>% as.numeric()
row_names_listmidpb <- rownames(selectmidplaceborfd)[row_names_listmidpb]

selectmidplaceborfd <-
  as.data.frame(selectmidplaceborfd[rownames(selectmidplaceborfd) %in% row_names_listmidpb,]) %>%
  `rownames<-`(row_names_listmidpb)

patternpost48pb <- "POST48AIMVC|POST48BIMVC|PN|BA"
row_names_listpost48pb <-
  -grep(patternpost48pb , rownames(selectpost48placeborfd)) %>% as.numeric()
row_names_listpost48pb <- rownames(selectpost48placeborfd)[row_names_listpost48pb]

selectpost48placeborfd <-
  as.data.frame(selectpost48placeborfd[rownames(selectpost48placeborfd) %in% row_names_listpost48pb,]) %>%
  `rownames<-`(row_names_listpost48pb)

#selection de des caractères représentant le sujet dans chaque data frame

sujetprepatch <- gsub("^[^_]+_+[^A-Z]|_[^_]+$" , "" , row.names(selectprepatchrfd))

sujetpreplacebo <- gsub("^[^_]+_+[^A-Z]|_[^_]+$" , "" , row.names(selectpreplaceborfd))

sujetmidpatch <- gsub("^[^_]+_+[^A-Z]|_[^_]+$" , "" , row.names(selectmidpatchrfd))

sujetmidplacebo <- gsub("^[^_]+_+[^A-Z]|_[^_]+$" , "" , row.names(selectmidplaceborfd))

sujetpostpatch <- gsub("^[^_]+_+[^A-Z]|_[^_]+$" , "" , row.names(selectpostpatchrfd))

sujetpostplacebo <- gsub("^[^_]+_+[^A-Z]|_[^_]+$" , "" , row.names(selectpostplaceborfd))

sujetpost48patch <- gsub("^[^_]+_+[^A-Z]|_[^_]+$" , "" , row.names(selectpost48patchrfd))

sujetpost48placebo <- gsub("^[^_]+_+[^A-Z]|_[^_]+$" , "" , row.names(selectpost48placeborfd))

pre <- c("pre")
mid <- c("mid")
post <- c("post")
post48 <- c("post48")

placebo <- c("placebo")
patch <- c("patch")

#ajout de la collones instant de mesure, condiiton et sujet dans chaque data frame

selectprepatchrfd <- cbind(selectprepatchrfd , pre , patch , sujetprepatch)
colnames(selectprepatchrfd) <- c("rfd" , "instant_mesure" , "condition" , "sujet")

selectpreplaceborfd <- cbind(selectpreplaceborfd  , pre , placebo , sujetpreplacebo)
colnames(selectpreplaceborfd) <- c("rfd" , "instant_mesure" , "condition" , "sujet")

selectmidpatchrfd <- cbind(selectmidpatchrfd  , mid , patch , sujetmidpatch)
colnames(selectmidpatchrfd) <- c("rfd" , "instant_mesure" , "condition" , "sujet")

selectmidplaceborfd <- cbind(selectmidplaceborfd , mid , placebo , sujetmidplacebo)
colnames(selectmidplaceborfd) <- c("rfd" , "instant_mesure" , "condition" , "sujet")

selectpostpatchrfd <- cbind(selectpostpatchrfd , post , patch , sujetpostpatch)
colnames(selectpostpatchrfd) <- c("rfd" , "instant_mesure" , "condition" , "sujet")

selectpostplaceborfd <- cbind(selectpostplaceborfd , post , placebo , sujetpostplacebo)
colnames(selectpostplaceborfd) <- c("rfd" , "instant_mesure" , "condition" , "sujet")

selectpost48patchrfd <- cbind(selectpost48patchrfd , post48 , patch , sujetpost48patch)
colnames(selectpost48patchrfd) <- c("rfd" , "instant_mesure" , "condition" , "sujet")

selectpost48placeborfd <- cbind(selectpost48placeborfd , post48 , placebo , sujetpost48patch)
colnames(selectpost48placeborfd) <- c("rfd" , "instant_mesure" , "condition" , "sujet")

#moyenne par sujet et ajout des collones condition et instant de mesure

selectprepatchrfd <- aggregate(selectprepatchrfd[, 1], list(selectprepatchrfd$sujet), mean)
selectprepatchrfd <- cbind(selectprepatchrfd , pre , patch)
colnames(selectprepatchrfd) <- c("sujet" , "rfd" , "instant_mesure" , "condition")

selectpreplaceborfd <- aggregate(selectpreplaceborfd[, 1], list(selectpreplaceborfd$sujet), mean)
selectpreplaceborfd <- cbind(selectpreplaceborfd , pre , placebo)
colnames(selectpreplaceborfd) <- c("sujet" , "rfd" , "instant_mesure" , "condition")

selectmidpatchrfd <- aggregate(selectmidpatchrfd[, 1], list(selectmidpatchrfd$sujet), mean)
selectmidpatchrfd <- cbind(selectmidpatchrfd , mid , patch)
colnames(selectmidpatchrfd) <- c("sujet" , "rfd" , "instant_mesure" , "condition")

selectmidplaceborfd <- aggregate(selectmidplaceborfd[, 1], list(selectmidplaceborfd$sujet), mean)
selectmidplaceborfd <- cbind(selectmidplaceborfd , mid , placebo)
colnames(selectmidplaceborfd) <- c("sujet" , "rfd" , "instant_mesure" , "condition")

selectpostpatchrfd <- aggregate(selectpostpatchrfd[, 1], list(selectpostpatchrfd$sujet), mean)
selectpostpatchrfd <- cbind(selectpostpatchrfd , post , patch)
colnames(selectpostpatchrfd) <- c("sujet" , "rfd" , "instant_mesure" , "condition")

selectpostplaceborfd <- aggregate(selectpostplaceborfd[, 1], list(selectpostplaceborfd$sujet), mean)
selectpostplaceborfd <- cbind(selectpostplaceborfd , post , placebo)
colnames(selectpostplaceborfd) <- c("sujet" , "rfd" , "instant_mesure" , "condition")

selectpost48patchrfd <- aggregate(selectpost48patchrfd[, 1], list(selectpost48patchrfd$sujet), mean)
selectpost48patchrfd <- cbind(selectpost48patchrfd , post48 , patch)
colnames(selectpost48patchrfd) <- c("sujet" , "rfd" , "instant_mesure" , "condition")

selectpost48placeborfd <- aggregate(selectpost48placeborfd[, 1], list(selectpost48placeborfd$sujet), mean)
selectpost48placeborfd <- cbind(selectpost48placeborfd , post48 , placebo)
colnames(selectpost48placeborfd) <- c("sujet" , "rfd" , "instant_mesure" , "condition")

#combiner tout les dataframe

dftotrfd <-
  rbind(
    selectprepatchrfd ,
    selectpreplaceborfd ,
    selectmidpatchrfd ,
    selectmidplaceborfd ,
    selectpostpatchrfd ,
    selectpostplaceborfd ,
    selectpost48patchrfd ,
    selectpost48placeborfd
  )

#plot

plotrfd <- ggboxplot(
  dftotrfd ,
  x = "instant_mesure",
  y = "rfd",
  color = "condition",
  palette = c("#00AFBB" , "#FC4E07"),
  order = c(
    "pre" ,
    "mid" ,
    "post" ,
    "post48"
  ),
  add = "jitter" ,
  ylab = "RFD (N/s)",
  xlab = "instant_mesure" ,
  title = "RFD_patch_placebo"
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

plotrfd

# anova

shapiro.test(selectprepatchrfd$rfd)
shapiro.test(selectpreplaceborfd$rfd)
shapiro.test(selectmidpatchrfd$rfd)
shapiro.test(selectmidplaceborfd$rfd)
shapiro.test(selectpostpatchrfd$rfd)
shapiro.test(selectpostplaceborfd$rfd)
shapiro.test(selectpost48patchrfd$rfd)
shapiro.test(selectpost48placeborfd$rfd)

res.aov1 <- rstatix::anova_test(
  data = dftotrfd, dv = rfd , wid = sujet ,
  within = c(condition, instant_mesure) , effect.size = "ges",
  detailed = TRUE,
)

get_anova_table(res.aov1 , correction = "auto")

ttestrfdinstantmesure <- dftotrfd %>%
  pairwise_t_test(
    rfd ~ instant_mesure , paired = TRUE,
    p.adjust.method = "bonferroni"
  )

ttestrfdinstantmesure

one.way <- dftotrfd %>%
  group_by(instant_mesure) %>%
  anova_test(dv = rfd , wid = sujet, within = condition) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way

one.way1 <- dftotrfd %>%
  group_by(condition) %>%
  anova_test(dv = rfd, wid = sujet, within = instant_mesure) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way1

ttesttime <- dftotrfd %>%
  group_by(condition) %>%
  pairwise_t_test(
    rfd ~ instant_mesure, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
ttesttime
