.onLoad <- function(libname, pkgname) {
  # TODO These are currently unused, but might provide more flexibility
  # for the user to alter how this wrapper works.
  op <- options()
  op.hRvstAPI <- list(
    hRvstAPI.service = "Harvest (hRvstAPI)",
    hRvstAPI.url = "https://api.harvestapp.com/v2/",
    hRvstAPI.agent = "https://github.com/eric-hunt/hRvstAPI"
  )
  toset <- !(names(op.hRvstAPI) %in% names(op))
  if (any(toset)) options(op.hRvstAPI[toset])
}
