pymagnitude <- NULL

.onLoad <- function(libname, pkgname) {
  pymagnitude <<- reticulate::import("pymagnitude", delay_load = TRUE)
}
