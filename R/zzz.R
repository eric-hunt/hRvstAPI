.onLoad <- function(libname, pkgname) {
  assign("keyring_service", "Harvest (hRvstAPI)", envir = topenv())
}
