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

  test_path <- paste0(getwd(), "/hRvstAPI_data")

  new_prefix <- NULL

  if (!fs::dir_exists(test_path)) {
    new_prefix <- tryCatch(
      fs::dir_create(test_path),
      error = function(e) NULL
    )
  } else {
    new_prefix <- test_path
  }

  update_params <- c(".rds_path", ".db_path")

  if (!is.null(new_prefix)) {
    purrr::walk(
      update_params,
      function(value_name) {
        suffix <- stringr::str_extract(get(value_name), "(?<=~).*")
        new_value <- fs::path_tidy(paste0(new_prefix, suffix))
        assign(
          value_name,
          new_value,
          envir = topenv()
        )
      }
    )
  }
}
