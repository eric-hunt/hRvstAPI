.onLoad <- function(libname, pkgname) {
  assign("keyring_service", "Harvest (hRvstAPI)", envir = topenv())
  assign("base_url", "https://api.harvestapp.com/v2/", envir = topenv())
  assign("user_agent", "https://github.com/eric-hunt/hRvstAPI", envir = topenv())
}
