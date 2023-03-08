library(data.table)
library(ggpubr)
library(rstatix)
library(ggplot2)
library(agricolae)

#selectpatch

selectprepatchrfd <- as.data.frame(dfrfdpatch[rownames(dfrfdpatch) %like% "PRE", ])
selectpostpatchrfd <- as.data.frame (dfrfdpatch[rownames(dfrfdpatch) %like% "POST", ])
selectmidpatchrfd <- as.data.frame(dfrfdpatch[rownames(dfrfdpatch) %like% "MID", ])
selectpost48patchrfd <- as.data.frame(dfrfdpatch[rownames(dfrfdpatch) %like% "POST48", ])

colnames(selectprepatchrfd)  <- "rfd"
colnames(selectpostpatchrfd)  <- "rfd"
colnames(selectmidpatchrfd)  <- "rfd"
colnames(selectpost48patchrfd)  <- "rfd"

pattern <- "POST48|POSTAIMVC|POSTBIMVC|GM|JS|MA|MF"
row_names_list <- -grep(pattern , rownames(selectpostpatchrfd)) %>% as.numeric()
row_names_list <- rownames(selectpostpatchrfd)[row_names_list]

test <-
  as.data.frame(selectpostpatchrfd[rownames(selectpostpatchrfd) %in% row_names_list, ]) %>%
  `rownames<-`(row_names_list)

selectpostpatchrfd <-
  selectpostpatchrfd[-grep("POST48", rownames(selectpostpatchrfd)),]
selectpostpatchrfd <-
  as.data.frame(selectpostpatchrfd[-grep("POSTAIMVC", rownames(selectpostpatchrfd)),])
selectpostpatchrfd <-
  as.data.frame(selectpostpatchrfd[-grep("POSTBIMVC", rownames(selectpostpatchrfd)),])
selectpostpatchrfd <-
  as.data.frame(selectpostpatchrfd[-grep("GM", rownames(selectpostpatchrfd)), ])
selectpostpatchrfd <-
  as.data.frame(selectpostpatchrfd[-grep("JS", rownames(selectpostpatchrfd)), ])
selectpostpatchrfd <-
  as.data.frame(selectpostpatchrfd[-grep("MA", rownames(selectpostpatchrfd)), ])
selectpostpatchrfd <-
  as.data.frame(selectpostpatchrfd[-grep("MF", rownames(selectpostpatchrfd)), ])

selectprepatchrfd <-
  as.data.frame(selectprepatchrfd[-grep("PREAIMVC", rownames(selectprepatchrfd)),])
selectprepatchrfd <-
  as.data.frame(selectprepatchrfd[-grep("PREBIMVC", rownames(selectprepatchrfd)),])
selectprepatchrfd <-
  as.data.frame(selectprepatchrfd[-grep("GM", rownames(selectprepatchrfd)), ])
selectprepatchrfd <-
  as.data.frame(selectprepatchrfd[-grep("JS", rownames(selectprepatchrfd)), ])
selectprepatchrfd <-
  as.data.frame(selectprepatchrfd[-grep("MA", rownames(selectprepatchrfd)), ])
selectprepatchrfd <-
  as.data.frame(selectprepatchrfd[-grep("MF", rownames(selectprepatchrfd)), ])

selectmidpatchrfd <-
  as.data.frame(selectmidpatchrfd[-grep("MIDBIMVC", rownames(selectmidpatchrfd)),])
selectmidpatchrfd <-
  as.data.frame(selectmidpatchrfd[-grep("MIDAIMVC", rownames(selectmidpatchrfd)),])
selectmidpatchrfd <-
  as.data.frame(selectmidpatchrfd[-grep("GM", rownames(selectmidpatchrfd)), ])
selectmidpatchrfd <-
  as.data.frame(selectmidpatchrfd[-grep("JS", rownames(selectmidpatchrfd)), ])
selectmidpatchrfd <-
  as.data.frame(selectmidpatchrfd[-grep("MA", rownames(selectmidpatchrfd)), ])
selectmidpatchrfd <-
  as.data.frame(selectmidpatchrfd[-grep("MF", rownames(selectmidpatchrfd)), ])

selectpost48patchrfd <-
  as.data.frame(selectpost48patchrfd[-grep("POST48AIMVC", rownames(selectpost48patchrfd)),])
selectpost48patchrfd <-
  as.data.frame(selectpost48patchrfd[-grep("POST48BIMVC", rownames(selectpost48patchrfd)),])
selectpost48patchrfd <-
  as.data.frame(selectpost48patchrfd[-grep("GM", rownames(selectpost48patchrfd)), ])
selectpost48patchrfd <-
  as.data.frame(selectpost48patchrfd[-grep("JS", rownames(selectpost48patchrfd)), ])
selectpost48patchrfd <-
  as.data.frame(selectpost48patchrfd[-grep("MA", rownames(selectpost48patchrfd)), ])
selectpost48patchrfd <-
  as.data.frame(selectpost48patchrfd[-grep("MF", rownames(selectpost48patchrfd)), ])

#selectplacebo

selectpreplaceborfd <- as.data.frame(dfrfdpb[rownames(dfrfdpb) %like% "PRE",])
selectpostplaceborfd <- as.data.frame(dfrfdpb[rownames(dfrfdpb) %like% "POST",])
selectmidplaceborfd <- as.data.frame(dfrfdpb[rownames(dfrfdpb) %like% "MID",])
selectpost48placeborfd <- as.data.frame(dfrfdpb[rownames(dfrfdpb) %like% "POST48",])

colnames(selectpreplaceborfd)  <- "rfd"
colnames(selectpostplaceborfd)  <- "rfd"
colnames(selectmidplaceborfd)  <- "rfd"
colnames(selectpost48placeborfd)  <- "rfd"

selectpostplaceborfd <-
  as.data.frame(selectpostplaceborfd[-grep("POST48", rownames(selectpostplaceborfd)),])
selectpostplaceborfd <-
  as.data.frame(selectpostplaceborfd[-grep("POSTAIMVC", rownames(selectpostplaceborfd)),])
selectpostplaceborfd <-
  as.data.frame(selectpostplaceborfd[-grep("POSTBIMVC", rownames(selectpostplaceborfd)),])
selectpostplaceborfd <-
  as.data.frame(selectpostplaceborfd[-grep("BA", rownames(selectpostplaceborfd)),])
selectpostplaceborfd <-
  as.data.frame(selectpostplaceborfd[-grep("PN", rownames(selectpostplaceborfd)),])

selectpreplaceborfd <-
  as.data.frame(selectpreplaceborfd[-grep("PREAIMVC", rownames(selectpreplaceborfd)),])
selectpreplaceborfd <-
  as.data.frame(selectpreplaceborfd[-grep("PREBIMVC", rownames(selectpreplaceborfd)),])
selectpreplaceborfd <-
  as.data.frame(selectpreplaceborfd[-grep("BA", rownames(selectpreplaceborfd)), ])
selectpreplaceborfd <-
  as.data.frame(selectpreplaceborfd[-grep("PN", rownames(selectpreplaceborfd)), ])

selectmidplaceborfd <-
  as.data.frame(selectmidplaceborfd[-grep("MIDBIMVC", rownames(selectmidplaceborfd)),])
selectmidplaceborfd <-
  as.data.frame(selectmidplaceborfd[-grep("MIDAIMVC", rownames(selectmidplaceborfd)),])
selectmidplaceborfd <-
  as.data.frame(selectmidplaceborfd[-grep("BA", rownames(selectmidplaceborfd)), ])
selectmidplaceborfd <-
  as.data.frame(selectmidplaceborfd[-grep("PN", rownames(selectmidplaceborfd)), ])

selectpost48placeborfd <-
  as.data.frame(selectpost48placeborfd[-grep("POST48AIMVC", rownames(selectpost48placeborfd)),])
selectpost48placeborfd <-
  as.data.frame(selectpost48placeborfd[-grep("POST48BIMVC", rownames(selectpost48placeborfd)),])
selectpost48placeborfd <-
  as.data.frame(selectpost48placeborfd[-grep("BA", rownames(selectpost48placeborfd)), ])
selectpost48placeborfd <-
  as.data.frame(selectpost48placeborfd[-grep("PN", rownames(selectpost48placeborfd)), ])

