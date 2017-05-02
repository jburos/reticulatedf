pandas <- NULL
numpy <- NULL
cohorts <- NULL

.onLoad <- function(libname, pkgname) {
  # delay load modules (will only be loaded when accessed via $)
  try({pandas <<- import("pandas", delay_load = TRUE)},
    silent = TRUE)
  try({numpy <<- import("numpy", delay_load = TRUE)},
      silent = TRUE)
  try({cohorts <<- import("cohorts", delay_load = TRUE)},
      silent = TRUE)

}
