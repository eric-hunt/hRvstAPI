#' Download Harvest API v2 requests into a local file.
#'
#' @param ...
#' @param rds_path A string -- file path where .rds file containing Harvest API data should be created.
#' @param sql_colnames
#'
#' @return
#' @export
create_rds <- function(..., rds_path = NULL, sql_colnames = TRUE) {
  if (missing(rds_path) || is.null(rds_path)) {
    rds_path <- hRvstAPI::.rds_path
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

  readr::write_rds(req_data, file = rds_path)
}



#' Create a SQLite database to hold Harvest API v2 request data.
#'
#' @param db_path
#' @param rds_path
#'
#' @return
#' @export
#'
#' @examples
create_db <- function(db_path = NULL, rds_path = NULL) {
  if (missing(rds_path) || is.null(rds_path)) {
    rds_path <- hRvstAPI::.rds_path
  }
  if (missing(db_path) || is.null(db_path)) {
    db_path <- hRvstAPI::.db_path
  }

  dbconn <- withr::local_db_connection(
    DBI::dbConnect(RSQLite::SQLite(), dbname = db_path)
  )
  print(dbconn)
  withr::defer(cat("\nConnection closed? ", !DBI::dbIsValid(dbconn), "\n"),
               priority = "last")

  dfs <- readr::read_rds(rds_path)
  tables <- names(dfs)
  columns <- purrr::map(dfs, colnames)
  has_id <- purrr::map(columns, \(x) {any(grepl("^id$", x, perl = TRUE))})

  cat("If not present, the following tables (and columns within each) will be created:\n")

  statements <- purrr::pmap(
    list(tables, columns, has_id),
    function(nm, cols, has_id) {
      if (has_id) {
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

  purrr::walk(
    statements,
    \(stmnt) RSQLite::dbExecute(conn = dbconn, statement = stmnt)
  )

  cat("The following tables now exist in the database:\n")
  print(RSQLite::dbListTables(dbconn))
}


  purrr::walk2(
    tables,
    dfs,
    function(tbl, df) {
      RSQLite::dbWriteTable(
        conn = dbconn,
        name = tbl,
        value = df,
        overwrite = FALSE,
        append = TRUE
      )
    }
  )

  purrr::map(
    tables,
    function(tblnm) {
      query <- glue::glue_sql("SELECT * FROM {tblnm} LIMIT 10;", .con = dbconn)
      print(query)
      RSQLite::dbGetQuery(dbconn, query)
    }
  )

  print(RSQLite::dbListTables(dbconn))
}
