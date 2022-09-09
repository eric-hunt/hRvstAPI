#' Query the local database and collect/return the result.
#'
#' @param query_string A string -- the query to be executed.
#' @param ... Key-value pairs -- additional arguments for interpolation passed to [glue::glue_sql()].
#' @param .is_active A Boolean -- restrict results to active entries; defaults to TRUE
#' @param .db_loc A string -- file path where a local .sqlite file containing Harvest API data should exist.
#'
#' @return A [tibble::tibble()] of data collected from the query.
#' @export
#'
query_db <- function(query_string, ...,
                     .is_active = TRUE, .db_loc = hRvstAPI::.db_path) {
  db <- withr::local_db_connection(
    RSQLite::dbConnect(RSQLite::SQLite(), dbname = .db_loc)
  )

  print(db)
  cat("\n")

  withr::defer(
    cat("\nConnection closed? ", !DBI::dbIsValid(db), "\n"),
    priority = "last"
  )

  varargs <- rlang::dots_list(
    ...,
    is_active = .is_active,
    .named = TRUE,
    .ignore_empty = "all",
    .homonyms = "first"
  )

  statement <- rlang::inject(
    glue::glue_sql(
      query_string,
      !!!varargs,
      .con = db
    )
  )

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
