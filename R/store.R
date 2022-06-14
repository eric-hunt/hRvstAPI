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
