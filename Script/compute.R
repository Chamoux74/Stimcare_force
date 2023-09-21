# Informations ------------------------------------------------------------
# Title: compute.R
# Author: Félix Boudry
# Contact: <felix.boudry@univ-perp.fr>
# License: GPLv3
# Description: Compute needed value and analyse them

my_max <- lapply(X = my_data, FUN = \(x) {
  lapply(X = x, FUN = \(x) {
    max(x$force %>% as.numeric())
  })
}) %>% lapply(\(x) {
  as.data.frame(x) %>% t() %>% as.data.frame()
})

my_max_kj <- lapply(X = my_max, FUN = \(x) {
  lapply(X = x, FUN = \(x) {
    x * 9.81
  })
}) %>% lapply(\(x) {
  as.data.frame(x) %>% t() %>% as.data.frame()
})
