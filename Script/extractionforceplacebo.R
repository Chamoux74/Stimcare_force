library(xml2)
library(XML)
library(tibble)
library(purrr)
library(dplyr)

forceplacebo <-
  list.files(
    path = "C:/Users/maxch/Documents/Analyse/DATA/STIMCARE_FORCE/PLACEBO",
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
