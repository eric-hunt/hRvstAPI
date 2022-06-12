#' Title
#'
#' @param ...
#' @param path
#'
#' @return
#' @export
hrvst_rds <- function(..., path = NULL) {
  if (missing(path) || is.null(path)) {
    path <- "~/hRvst_data.rds"
  }

  resources <- purrr::set_names(list(...))
  print(resources)
}



#' Title
#'
#' @param rds_file
#' @param path
#'
#' @return
#' @export
hrvst_db <- function(rds_file = NULL, path = NULL) {
  if (missing(rds_file) || is.null(rds_file)) {
    rds_file <- "~/hRvst_data.rds"
  }

}
