# Informations ------------------------------------------------------------
# Title: import.R
# Author: Félix Boudry
# Contact: <felix.boudry@univ-perp.fr>
# License: GPLv3
# Description: Import, clean and structure data.

library(xml2)
library(magrittr)
library(tools)
library(tibble)

my_data <- lst()
conditions <- c("PATCH", "PLACEBO")

for (my_condition in conditions) {
  print(my_condition)
  file_names <-
    list.files(
      path = paste0("Data/", my_condition),
      pattern = "\\.xml",
      all.files = TRUE,
      full.names = TRUE
    )

  imported_files <-
    lapply(
      X = file_names,
      FUN = function(df_name) {
        read_xml(df_name) %>%
          xml_find_all("//mesure/valeur") %>%
          xml_attrs() %>%
          do.call(what = rbind) %>%
          as.data.frame()
      }
    ) %>%
    `names<-`(file_names %>% basename() %>% file_path_sans_ext())
  my_data <- append(x = my_data, values = lst(imported_files))
}

names(my_data) <- conditions
