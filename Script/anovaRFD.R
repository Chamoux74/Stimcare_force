library(data.table)
library(ggpubr)
library(rstatix)
library(ggplot2)
library(agricolae)

#selectpatch

selectPREpatchrfd <- dfmax[rownames(dfmax) %like% "PRE", ]
selectpostpatchrfd <- dfmax[rownames(dfmax) %like% "POST", ]
selectmidpatchrfd <- dfmax[rownames(dfmax) %like% "MID", ]
selectpost48patchrfd <- dfmax[rownames(dfmax) %like% "POST48", ]

selectpostpatchrfd <-
  selectpostpatchrfd[-grep("POST48", rownames(selectpostpatchrfd)),]
selectpostpatchrfd <-
  selectpostpatchrfd[-grep("POSTAIMVC", rownames(selectpostpatchrfd)),]
selectpostpatchrfd <-
  selectpostpatchrfd[-grep("POSTBIMVC", rownames(selectpostpatchrfd)),]
selectpostpatchrfd <-
  selectpostpatchrfd[-grep("GM", rownames(selectpostpatchrfd)), ]
selectpostpatchrfd <-
  selectpostpatchrfd[-grep("JS", rownames(selectpostpatchrfd)), ]
selectpostpatchrfd <-
  selectpostpatchrfd[-grep("MA", rownames(selectpostpatchrfd)), ]
selectpostpatchrfd <-
  selectpostpatchrfd[-grep("MF", rownames(selectpostpatchrfd)), ]

selectprepatchrfd <-
  selectPREpatchrfd[-grep("PREAIMVC", rownames(selectPREpatchrfd)),]
selectprepatchrfd <-
  selectprepatchrfd[-grep("PREBIMVC", rownames(selectprepatchrfd)),]
selectprepatchrfd <-
  selectprepatchrfd[-grep("GM", rownames(selectprepatchrfd)), ]
selectprepatchrfd <-
  selectprepatchrfd[-grep("JS", rownames(selectprepatchrfd)), ]
selectprepatchrfd <-
  selectprepatchrfd[-grep("MA", rownames(selectprepatchrfd)), ]
selectprepatchrfd <-
  selectprepatchrfd[-grep("MF", rownames(selectprepatchrfd)), ]

selectmidpatchrfd <-
  selectmidpatchrfd[-grep("MIDBIMVC", rownames(selectmidpatchrfd)),]
selectmidpatchrfd <-
  selectmidpatchrfd[-grep("MIDAIMVC", rownames(selectmidpatchrfd)),]
selectmidpatchrfd <-
  selectmidpatchrfd[-grep("GM", rownames(selectmidpatchrfd)), ]
selectmidpatchrfd <-
  selectmidpatchrfd[-grep("JS", rownames(selectmidpatchrfd)), ]
selectmidpatchrfd <-
  selectmidpatchrfd[-grep("MA", rownames(selectmidpatchrfd)), ]
selectmidpatchrfd <-
  selectmidpatchrfd[-grep("MF", rownames(selectmidpatchrfd)), ]

selectpost48patchrfd <-
  selectpost48patchrfd[-grep("POST48AIMVC", rownames(selectpost48patchrfd)),]
selectpost48patchrfd <-
  selectpost48patchrfd[-grep("POST48BIMVC", rownames(selectpost48patchrfd)),]
selectpost48patchrfd <-
  selectpost48patchrfd[-grep("GM", rownames(selectpost48patchrfd)), ]
selectpost48patchrfd <-
  selectpost48patchrfd[-grep("JS", rownames(selectpost48patchrfd)), ]
selectpost48patchrfd <-
  selectpost48patchrfd[-grep("MA", rownames(selectpost48patchrfd)), ]
selectpost48patchrfd <-
  selectpost48patchrfd[-grep("MF", rownames(selectpost48patchrfd)), ]

#selectplacebo

selectpreplaceborfd <- dfmaxplacebo[rownames(dfmaxplacebo) %like% "PRE",]
selectpostplaceborfd <- dfmaxplacebo[rownames(dfmaxplacebo) %like% "POST",]
selectmidplaceborfd <- dfmaxplacebo[rownames(dfmaxplacebo) %like% "MID",]
selectpost48placeborfd <- dfmaxplacebo[rownames(dfmaxplacebo) %like% "POST48",]

selectpostplaceborfd <-
  selectpostplaceborfd[-grep("POST48", rownames(selectpostplaceborfd)),]
selectpostplaceborfd <-
  selectpostplaceborfd[-grep("POSTAIMVC", rownames(selectpostplaceborfd)),]
selectpostplaceborfd <-
  selectpostplaceborfd[-grep("POSTBIMVC", rownames(selectpostplaceborfd)),]
selectpostplaceborfd <-
  selectpostplaceborfd[-grep("BA", rownames(selectpostplaceborfd)),]
selectpostplaceborfd <-
  selectpostplaceborfd[-grep("PN", rownames(selectpostplaceborfd)),]

selectpreplaceborfd <-
  selectpreplaceborfd[-grep("PREAIMVC", rownames(selectpreplaceborfd)),]
selectpreplaceborfd <-
  selectpreplaceborfd[-grep("PREBIMVC", rownames(selectpreplaceborfd)),]
selectpreplaceborfd <-
  selectpreplaceborfd[-grep("BA", rownames(selectpreplaceborfd)), ]
selectpreplaceborfd <-
  selectpreplaceborfd[-grep("PN", rownames(selectpreplaceborfd)), ]

selectmidplaceborfd <-
  selectmidplaceborfd[-grep("MIDBIMVC", rownames(selectmidplaceborfd)),]
selectmidplaceborfd <-
  selectmidplaceborfd[-grep("MIDAIMVC", rownames(selectmidplaceborfd)),]
selectmidplaceborfd <-
  selectmidplaceborfd[-grep("BA", rownames(selectmidplaceborfd)), ]
selectmidplaceborfd <-
  selectmidplaceborfd[-grep("PN", rownames(selectmidplaceborfd)), ]

selectpost48placeborfd <-
  selectpost48placeborfd[-grep("POST48AIMVC", rownames(selectpost48placeborfd)),]
selectpost48placeborfd <-
  selectpost48placeborfd[-grep("POST48BIMVC", rownames(selectpost48placeborfd)),]
selectpost48placeborfd <-
  selectpost48placeborfd[-grep("BA", rownames(selectpost48placeborfd)), ]
selectpost48placeborfd <-
  selectpost48placeborfd[-grep("PN", rownames(selectpost48placeborfd)), ]

