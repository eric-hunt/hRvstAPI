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
      `task_assignments` = "task assignments"#,
      # `time_entries` = "time entries"
    )
  } else {
    resources <- purrr::set_names(list(...))
  }

  req_data <- purrr::map(
    resources,
    function(req_string) {
      hrvst_req(req_string)
    }
  )

  if (sql_colnames) {
    req_data <- purrr::map(
      req_data,
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
    View(req_data)
  }

  readr::write_rds(req_data, file = path)
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

  # import <- readr::read_rds(rds_file)
  # dfs <- purrr::map(import, \(df) dplyr::select_if(df, \(col) !is.list(col)))
  dfs <- readr::read_rds(rds_file)
  tables <- names(dfs)
  columns <- purrr::map(dfs, colnames)
  has_key <- purrr::map(columns, \(x) {any(grepl("^id$", x, perl = TRUE))})

  # Connect to db..
  dbconn <- DBI::dbConnect(RSQLite::SQLite(), dbname = hRvstAPI::.db_path)
  print(dbconn)
  print(DBI::dbGetInfo(dbconn))

  # Disconnect from db and confirm on exit..
  on.exit(DBI::dbDisconnect(dbconn))
  on.exit(cat("Connection closed? ", !DBI::dbIsValid(dbconn), "\n"), add = TRUE)

  statements <- purrr::pmap(
    list(tables, columns, has_key),
    function(nm, cols, has_key) {
      if (has_key) {
        cols <- glue::glue_sql_collapse(
          stringr::str_replace(cols, "^id$", "id INTEGER PRIMARY KEY"),
          sep = ", "
        )
        cat(nm, ":\n", cols, "\n", "\n")
        glue::glue_sql(
          "CREATE TABLE IF NOT EXISTS {nm}({cols});",
          .con = dbconn
        )
      } else {
        cols <- glue::glue_sql_collapse(cols, sep = ", ")
        glue::glue_sql(
          "CREATE TABLE IF NOT EXISTS {nm}({cols});",
          .con = dbconn
        )
        cat(nm, ":\n", cols, "\n", "\n")
      }
    }
  ) |> purrr::set_names(tables)

  print(statements)

  purrr::walk(
    statements,
    \(stmnt) RSQLite::dbExecute(conn = dbconn, statement = stmnt)
  )

  print(RSQLite::dbListTables(dbconn))
}
