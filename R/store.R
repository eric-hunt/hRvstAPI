#' Download Harvest API v2 requests into a local file.
#'
#' @param ...
#' @param path
#' @param sql_colnames
#'
#' @return
#' @export
hrvst_rds <- function(..., path = NULL, sql_colnames = TRUE) {
  if (missing(path) || is.null(path)) {
    path <- "~/hRvst_data.rds"
  }

  assertthat::assert_that(
    rlang::is_bool(sql_colnames),
    msg = "Argument sql_colnames should be TRUE or FALSE."
  )

  if (missing(...) || is.null(...)) {
    resources <- list(
      `users` = "users",
      `clients` = "clients",
      `projects` = "projects",
      `tasks` = "tasks",
      `project_assignments` = "project assignments",
      `user_assignments` = "user assignments",
      `task_assignments` = "task assignments",
      `time_entries` = "time entries",
      `project_budget_report` = "budget report"
    )
  } else {
    resources <- purrr::set_names(list(...))
  }

  data <- purrr::map(
    resources,
    function(req_string) {
      hrvst_req(req_string)
    }
  )

  if (sql_colnames) {
    data <- purrr::map(
      data,
      function(df) {
        dplyr::rename_with(
          df,
          .fn = \(x) stringr::str_replace(x, "\\.", "_"),
          .cols = tidyselect::contains(".")
        )
      }
    )
  }

  if (rlang::is_interactive()) {
    View(data)
  }

  readr::write_rds(data, file = path)
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
