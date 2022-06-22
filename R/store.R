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
    path <- hRvstAPI::.rds_path
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
    rds_file <- hRvstAPI::.rds_path
  }

  import <- readr::read_rds(rds_file)

  tables <- names(import)

  columns <- purrr::map(import, colnames)

  has_key <- purrr::map(columns, \(x) {any(grepl("^id$", x, perl = TRUE))})

  statements <- purrr::pmap(
    list(tables, import, columns, has_key),
    function(nm, df, cols, keyed) {
      if (keyed) {
        cols <- glue::glue_collapse(
          stringr::str_replace(cols, "^id$", "id INTEGER PRIMARY KEY"),
          sep = ", "
        )
        glue::glue("CREATE TABLE {nm}({cols});")
      } else {
        cols <- glue::glue_collapse(cols, sep = ", ")
        glue::glue("CREATE TABLE {nm}({cols});")
      }
    }
  ) |> purrr::set_names(tables)

  dbconn <- DBI::dbConnect(RSQLite::SQLite(), dbname = hRvstAPI::.db_path)

  print(dbconn)

  print(DBI::dbGetInfo(dbconn))

  print(statements)

  purrr::walk(
    statements,
    \(stmnt) RSQLite::dbExecute(conn = dbconn, statement = stmnt)
  )

  print(RSQLite::dbListTables(dbconn))

  DBI::dbDisconnect(dbconn)
}
