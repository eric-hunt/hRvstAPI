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



#' Query a database and collect/return the result.
#'
#' @param db_connection
#' @param query_string
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
query_db <- function(db_connection, query_string, ...) {
  varargs <- rlang::list2(query_string, ..., .con = db_connection)
  query <- do.call(glue::glue_sql, varargs)
  DBI::dbGetQuery(
    db_connection,
    query
  )
}



#' Get the name of the primary key column for a table.
#'
#' @param db_connection
#' @param tbl
#'
#' @return
#' @export
#'
#' @examples
key_col <- function(db_connection, tbl) {
  tbl_info <- hRvstAPI::query_db(
    db_connection,
    "PRAGMA table_info({tbl_name})",
    tbl_name = tbl
  )
  tbl_info[tbl_info$pk == 1, "name"]
}



#' Get the unique set of key values that already exist in a table.
#'
#' @param db_connection
#' @param tbl
#' @param key
#'
#' @return
#' @export
#'
#' @examples
get_keys <- function(db_connection, tbl, key = NULL) {
  if (missing(key) || is.null(key)) {
    stop("Cannot get key when key is NULL.")
  }
  hRvstAPI::query_db(
    db_connection,
    "SELECT DISTINCT {`key_name`} FROM {`tbl_name`}",
    key_name = key, tbl_name = tbl
  )[[1]]
}



#' Update the local SQLite database with new Harvest API v2 request data.
#'
#' @param db_path
#' @param rds_path
#'
#' @return
#' @export
#'
#' @examples
update_db <- function(db_path = NULL, rds_path = NULL) {
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
  tables <- names(dfs) |> purrr::set_names() # name with itself
  columns <- purrr::map(dfs, colnames)
  has_id <- purrr::map(columns, \(x) {any(grepl("^id$", x, perl = TRUE))})
  key_cols <- purrr::map(tables, \(tbl) hRvstAPI::key_col(dbconn, tbl))
  has_key <- purrr::map(key_cols, \(tbl_col) !is.null(tbl_col))
  existing <- purrr::map2(
    tables,
    key_cols,
    function(tbl, key) {
      hRvstAPI::get_keys(dbconn, tbl, key)
    }
  )

  filtered_dfs <- purrr::pmap(
    list(has_key, dfs, key_cols, existing),
    function(bool, df, key_col, vals) {
      if (bool) {
        dplyr::filter(df, !(!!rlang::sym(key_col) %in% vals))
      } else {
        df
      }
    }
  )

  purrr::walk2(
    tables,
    filtered_dfs,
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
}
