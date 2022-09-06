#' Download Harvest API v2 requests into a local file.
#'
#' @param ... Resources as "named" key/value pairs to be requested. ([hRvstAPI::hrvst_req()])
#' @param is_active A string -- 'true' or 'false' passed to [hRvstAPI::hrvst_GET()].
#' @param updated_since A string -- passed to [hRvstAPI::hrvst_GET()].
#' @param weeks_ago An integer -- overrides *updated_since* argument; the number of weeks prior to the current week to convert to a date and pass to the 'updated_since' query parameter.
#' @param from TODO
#' @param to TODO
#' @param rds_path A string -- file path where a local .rds file containing Harvest API data should be created.
#' @param sql_colnames A Boolean -- automatically convert '.' to '_' in column names for easier SQL query construction.
#' @param .extra_params Key-value pairs -- optional additional query parameters passed to `...` of [hRvstAPI::hrvst_GET()].
#'
#' @return
#' @export
#'
#' @seealso [hRvstAPI::hrvst_req()]
#'
create_rds <- function(..., is_active = NULL, updated_since = NULL,
                       weeks_ago = NULL, from = NULL, to = NULL,
                       rds_path = NULL, sql_colnames = TRUE,
                       .extra_params = NULL) {
  if (is.null(rds_path)) {
    rds_path <- hRvstAPI::.rds_path
  }

  assertthat::assert_that(
    rlang::is_bool(sql_colnames),
    msg = "Argument sql_colnames should be TRUE or FALSE."
  )

  if (missing(...)) {
    resources <- list(
      `users` = "users",
      `clients` = "clients",
      `projects` = "projects",
      `tasks` = "tasks",
      # `project_assignments` = "project assignments",
      `user_assignments` = "user assignments",
      `task_assignments` = "task assignments",
      `time_entries` = "time entries"
    )
  } else {
    resources <- purrr::set_names(list(...))
  }

  req_data <- purrr::map(
    resources,
    function(req_string) {
      hrvst_req(
        req_string,
        is_active = is_active,
        updated_since = updated_since,
        weeks_ago = weeks_ago,
        from = from,
        to = to,
        ... = .extra_params
      )
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

  if (rlang::is_interactive() && !shiny::isRunning()) {
    View(req_data)
  }

  readr::write_rds(req_data, file = rds_path)
}



#' Create a SQLite database to hold Harvest API v2 request data.
#'
#' @param db_path A string -- file path where a local .sqlite file containing Harvest API data should be created.
#' @param rds_path A string -- file path where a local .rds file containing Harvest API data should exist.
#'
#' @return
#' @export
#'
create_db <- function(db_path = NULL, rds_path = NULL) {
  if (is.null(rds_path)) {
    rds_path <- hRvstAPI::.rds_path
  }
  if (is.null(db_path)) {
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



#' Query the local database and collect/return the result.
#'
#' @param query_string A string -- the query to be executed.
#' @param ... Key-value pairs -- additional arguments for interpolation passed to [glue::glue_sql()].
#' @param .db_loc A string -- file path where a local .sqlite file containing Harvest API data should exist.
#'
#' @return A [tibble::tibble()] of data collected from the query.
#' @export
#'
query_db <- function(query_string, ..., .db_loc = hRvstAPI::.db_path) {
  db <- withr::local_db_connection(
    RSQLite::dbConnect(RSQLite::SQLite(), dbname = .db_loc)
  )

  print(db)
  cat("\n")

  withr::defer(
    cat("\nConnection closed? ", !DBI::dbIsValid(db), "\n"),
    priority = "last"
  )

  statement <- glue::glue_sql(query_string, ..., .con = db)

  print(statement)

  cat("\nCollecting the query results..\n")

  DBI::dbGetQuery(db, statement) |>
    tibble::as_tibble()
}



#' Get the name of the primary key column for a table.
#'
#' @param db_connection A DBI compatible connection object -- the database containing Harvest API v2 request data.
#' @param tbl A string -- name of the table for which to determine the primary key column.
#'
#' @return A string -- the primary key column of *tbl*.
#' @export
#'
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
#' @param db_connection A DBI compatible connection object -- the database containing Harvest API v2 request data.
#' @param tbl A string -- name of the table from which to gather primary keys.
#' @param key A string -- name of the primary key column in *tbl*.
#'
#' @return A numeric vector -- a vector of integer primary keys which currently exist in *tbl*.
#' @export
#'
get_keys <- function(db_connection, tbl, key = NULL) {
  if (is.null(key)) {
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
#' @param db_path A string -- file path where a local .sqlite file containing Harvest API data should be updated.
#' @param rds_path A string -- file path where a local .rds file containing Harvest API data should exist.
#'
#' @return
#' @export
#'
update_db <- function(db_path = NULL, rds_path = NULL) {
  if (is.null(rds_path)) {
    rds_path <- hRvstAPI::.rds_path
  }
  if (is.null(db_path)) {
    db_path <- hRvstAPI::.db_path
  }

  dbconn <- withr::local_db_connection(
    DBI::dbConnect(RSQLite::SQLite(), dbname = db_path)
  )
  print(dbconn)
  withr::defer(cat("\nConnection closed? ", !DBI::dbIsValid(dbconn), "\n"),
               priority = "last")

  dfs <- readr::read_rds(rds_path) |> purrr::discard(\(df) nrow(df) == 0)
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
