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

dflistforce <- mapply(cbind, dflistforce , dflisttime , SIMPLIFY = FALSE)

selectmax <- function(y) {max(y$dataf)}
dfmax <- as.data.frame(lapply(dflistforce , selectmax))
dfmax <- t(dfmax)
colnames(dfmax) <- "forcekj"
dfmax <- as.data.frame(dfmax)
newton <- as.data.frame(c(dfmax$forcekj*9.81))

colnames(newton) <- "IMVC"
dfmax <- cbind(dfmax , newton)

#calcule RFD

newton <- function(nt){nt$dataf*9.81}
dflistforce <- lapply(dflistforce , newton)
dftimeforce <- mapply(cbind, dflistforce , dflisttime , SIMPLIFY = FALSE)

dftimeforce <- lapply(dftimeforce , setNames , rn)

diferencepatch <- lapply(dftimeforce , dif)

diferencepatch <- lapply(diferencepatch , add)
diferencepatch <- mapply(cbind , diferencepatch , dftimeforce , SIMPLIFY = FALSE)
renames <- c("diff", "force" , "temps")
diferencepatch <- lapply(diferencepatch , setNames , renames)


#rem <- function (remove) {remove[-c(1:which(remove$diff > 2)) , ]}
#diferencepatch <- lapply(diferencepatch , rem)

#rem <- function (remove) {remove[c(1:21) , ]}
#diferencepatch <- lapply(diferencepatch , rem)

#rfdpatch <- as.data.frame(lapply(diferencepatch , moy))
#rfdpatch <- rfdpatch/0.2
#rfdpatch <- t(rfdpatch)
