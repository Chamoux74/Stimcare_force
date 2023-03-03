library(xml2)
library(XML)
library(tibble)
library(purrr)
library(dplyr)

forceplacebo <-
  list.files(
    path = "C:/Users/maxch/Git/FORCE/Data/PLACEBO",
    pattern = "\\.xml",
    all.files = TRUE,
    full.names = TRUE
  )

readxml <- function(r){read_xml(r)}

xmlplacebo <- lapply(forceplacebo , readxml)
names(xmlplacebo) <- tools::file_path_sans_ext(basename(forceplacebo))

m <- function (ext) {xml_find_all(ext , xpath = "//mesure/valeur")}
extforcetimepb <- lapply(xmlplacebo , m)

time1 <- function (t) {xml_attr(t, "time")}
valuestimepb <- lapply(extforcetimepb , time1)
num <- function (numero) {as.numeric(numero)}
valuestimepb1 <- lapply(valuestimepb , num)

force1 <- function(f) {xml_attr(f, "force")}
valuesforcepb <- lapply(extforcetimepb , force1)
valuesforcepb1 <- lapply(valuesforcepb, num)

force <- function(dataf) {as.data.frame(dataf)}
time <- function(input) {as.data.frame(input)}

dflistforcepb <- lapply(valuesforcepb1 , force)
dflisttimepb <- lapply(valuestimepb1 , time)

dftimeforcepb <- mapply(cbind, dflistforcepb , dflisttimepb , SIMPLIFY = FALSE)

dfmaxplacebo <- as.data.frame(lapply(dftimeforcepb , selectmax))
dfmaxplacebo <- t(dfmaxplacebo)
colnames(dfmaxplacebo) <- "forcekj"
dfmaxplacebo <- as.data.frame(dfmaxplacebo)
newtonpb <- as.data.frame(c(dfmaxplacebo$forcekj*9.81))

colnames(newtonpb) <- "IMVC"
dfmaxplacebo <- cbind(dfmaxplacebo , newtonpb)

#calcule RFD

newton <- function(nt){nt$dataf*9.81}
dflistforcepb <- lapply(dflistforcepb , newton)
dftimeforcepb <- mapply(cbind, dflistforcepb , dflisttimepb , SIMPLIFY = FALSE)
rn <- c("force", "time")
dftimeforcepb <- lapply(dftimeforcepb , setNames , rn)

dif <- function(d) {diff(d$force)}
diference <- lapply(dftimeforcepb , dif)
add <- function(ad) {ad <- c(0,ad)}
diference <- lapply(diference , add)
diference <- mapply(cbind , diference , dftimeforcepb , SIMPLIFY = FALSE)
renames <- c("diff", "force" , "temps")
diference <- lapply(diference, setNames , renames)

seuil <- 2

# Parcourir chaque data frame de la liste
for (i in 1:length(dftimeforcepb)) {
  df <- dftimeforcepb[[i]]

# Trouver la première ligne où deux lignes consécutives sont supérieures au seuil
start_row <- 2
  for (j in start_row:nrow(df)) {
    if (((df[j, 1] - df[(j - 1), 1]) & (df[(j + 1), 1] - df[j, 1]) > seuil)) {
      start_row <- j
      break
    }
  }

# Conserver toutes les lignes à partir de la première ligne où deux lignes consécutives sont supérieures au seuil
  dftimeforcepb[[i]] <- df[start_row:nrow(df),]
}

rem <- function (remove) {remove[-c(1:which(remove$diff > 2)) , ]}
test <- lapply(diference , start_row)

rem <- function (remove) {remove[c(1:21) , ]}
diference <- lapply(diference , rem)

moy <- function (m) {mean(m$force)}

rfd <- as.data.frame(lapply(diference , moy))
rfd <- rfd/0.2
rfdplacebo <- t(rfd)

