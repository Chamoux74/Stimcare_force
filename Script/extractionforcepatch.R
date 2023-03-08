library(xml2)
library(XML)
library(tibble)
library(purrr)
library(dplyr)

forcepatch <-
  list.files(
    path = "C:/Users/maxch/Git/FORCE/Data/PATCH",
    pattern = "\\.xml",
    all.files = TRUE,
    full.names = TRUE
  )

readxml <- function(r){read_xml(r)}

xmlpatch <- lapply(forcepatch , readxml)
names(xmlpatch) <- tools::file_path_sans_ext(basename(forcepatch))

m <- function (ext) {xml_find_all(ext , xpath = "//mesure/valeur")}
extforcetime <- lapply(xmlpatch , m)

time1 <- function (t) {xml_attr(t, "time")}
valuestime <- lapply(extforcetime , time1)
num <- function (numero) {as.numeric(numero)}
valuestime1 <- lapply(valuestime , num)

force1 <- function(f) {xml_attr(f, "force")}
valuesforce <- lapply(extforcetime , force1)
valuesforce1 <- lapply(valuesforce, num)

force <- function(dataf) {as.data.frame(dataf)}
time <- function(input) {as.data.frame(input)}

dflistforce <- lapply(valuesforce1 , force)
dflisttime <- lapply(valuestime1 , time)

dftimeforcepatch <- mapply(cbind, dflistforce , dflisttime , SIMPLIFY = FALSE)

selectmax <- function(y) {max(y$dataf)}
dfmax <- as.data.frame(lapply(dflistforce , selectmax))
dfmax <- t(dfmax)
colnames(dfmax) <- "forcekj"
dfmax <- as.data.frame(dfmax)
newton <- as.data.frame(c(dfmax$forcekj*9.81))

colnames(newton) <- "IMVC"
dfmax <- cbind(dfmax , newton)

#calcule RFD

seuil <- 2

# Parcourir chaque data frame de la liste
for (i in 1:length(dftimeforcepatch)) {
  df1 <- dftimeforcepatch[[i]]

  # Trouver la première ligne où deux lignes consécutives sont supérieures au seuil
  start_row <- 2
  for (j in start_row:nrow(df1)) {
    if (((df1[j, 1] - df1[(j - 1), 1]) > seuil && (df1[(j + 1), 1] - df1[j, 1]) > seuil)) {
      start_row <- j
      break
    }
  }

  # Conserver toutes les lignes à partir de la première ligne où deux lignes consécutives sont supérieures au seuil
  dftimeforcepatch[[i]] <- as.data.frame(df1[start_row:nrow(df1),])
}

rem <- function (remove) {remove[c(1:21) , ]}
data200mspatch <- lapply(dftimeforcepatch , rem)


newton <- function(nt){nt$dataf*9.81}
data200mspatch <- lapply(data200mspatch , newton)
data200mspatch <- lapply(data200mspatch , as.data.frame)

rate <- "rfd"

data200mspatch <- lapply(data200mspatch , setNames , rate)

moy <- function (m) {mean(m$rfd)}

dfrfdpatch <- as.data.frame(lapply(data200mspatch , moy))
dfrfdpatch<- dfrfdpatch/0.2
dfrfdpatch <- t(dfrfdpatch)
