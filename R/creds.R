#' See if and where user credentials exist.
#'
#' Useful for informing subsequent user action within an application.
#'
#' @return A list -- variables related to credential existence.
#' @export
check_creds <- function() {
  report <- list()
  if (rlang::is_interactive()) {
    report$session_interactive <- TRUE
  }
  if (!require(keyring)) {
    message("Keyring is not installed.")
    report$keyring_installed <- FALSE
  }
  if (require(keyring)) {
    message("Keyring is installed.")
    report$keyring_installed <- TRUE
  }
  if (!keyring::has_keyring_support()) {
    message("Keyring not supported. Use .Renviron to set credentials.")
    report$keyring_supported <- FALSE
  }
  if (keyring::has_keyring_support()) {
    message("Keyring is supported.")
    report$keyring_supported <- TRUE
  }
  if (nrow(keyring::key_list(keyring_service)) == 0) {
    message("No credentials found in system keyring.")
    report$creds_in_keyring <- FALSE
    report$too_many_creds <- FALSE
  }
  if (nrow(keyring::key_list(keyring_service)) == 1) {
    acct_id <- keyring::key_list(keyring_service)$username
    message("Credentials found in system keyring for account: ", acct_id)
    report$creds_in_keyring <- TRUE
    report$too_many_creds <- FALSE
    message("Import these credentials to the environment with `retrieve_creds()`")
  }
  if (nrow(keyring::key_list(keyring_service)) > 1) {
    message("Multiple credentials fount in system keyring. Please clear and add credentials again, or set with .Renviron.")
    report$creds_in_keyring <- TRUE
    report$too_many_creds <- TRUE
  }
  if (isTRUE(
    sapply(
      c("HRVST_ACCT_ID", "HRVST_TOKEN"),
      \(x) nzchar(Sys.getenv(x))
    )
  )) {
    report$creds_already_exist <- TRUE
  } else {
    report$creds_already_exist <- FALSE
  }
  return(invisible(report))
}



#' Return the user's Harvest API v2 account ID.
#'
#' @return A string -- the user account ID.
#' @export
#'
#' @seealso \href{https://help.getharvest.com/api-v2/authentication-api/authentication/authentication/}{Harvest API V2 Documentation | Authentication}
harvest_acct_id <- function() {
  acct_id <- Sys.getenv("HRVST_ACCT_ID")
  if (nzchar(acct_id)) {
    return(acct_id)
  } else {
    message("Account ID is missing. Running credential setup now..")
    # set_creds() #TODO
    # harvest_acct_id()
  }
}



#' Return the user's Harvest API v2 personal access token.
#'
#' @return A string -- the user personal access token.
#' @export
#'
#' @seealso \href{https://help.getharvest.com/api-v2/authentication-api/authentication/authentication/}{Harvest API V2 Documentation | Authentication}
harvest_token <- function() {
  token <- Sys.getenv("HRVST_TOKEN")
  if (nzchar(token)) {
    return(token)
  } else {
    message("Personal access token is missing. Running credential setup now..")
    # set_creds() #TODO
    # harvest_token()
  }
}



#' Retrieve user credentials from the keyring to environment.
#'
#' This is intended to be a helper function for setting credentials.
#'
#' @export
retrieve_creds <- function() {
  Sys.setenv("HRVST_ACCT_ID" = keyring::key_list(keyring_service)$username)
  Sys.setenv("HRVST_TOKEN" = keyring::key_get(keyring_service))
  message("Credentials have been retrieved from the keyring and set in the environment.")
}



#' Delete all hRvstAPI Harvest credentials from the keyring.
#'
#' @export
clear_keyring_creds <- function() {
  keyring::key_delete(service = keyring_service)
  message("Credentials have been cleared from the keyring.")
}



#' Set user credentials as environment variables.
#'
#' @export
set_creds <- function() {
  report <- check_creds()

  # If no user is present..
  if (!report$session_interactive) {
    # ..and credentials exist in .Renviron..
    if (report$creds_already_exist) {
      message("Credentials are already set in the environment.")
      # ..and credentials exist in the keyring..
    } else if (report$creds_in_keyring && !report$too_many_creds) {
      retrieve_creds()
      # ..and no credentials or ambiguous credentials exist in the keyring..
    } else if (!report$creds_in_keyring || report$too_many_creds) {
      assertthat::assert_that(
        report$creds_already_exist,
        msg = "No credentials or ambiguous credentials exist in the keyring,\n
          and account ID or PAT are not set as environment variables.\n
          Session is not interactive. Stopping."
      )
    } else {
      stop("Something is wrong with the user credentials. Consider setting in .Renviron.")
    }
  }
  # If a user is present..
  if (report$session_interactive) {
    # ..and credentials exist in .Renviron..
    if (report$creds_already_exist) {
      message("Credentials are already set in the environment.")
      # ..and credentials exist in the keyring..
    } else if (report$creds_in_keyring && !report$too_many_creds) {
      retrieve_creds()
      # ..and credentials do not exist in the keyring or environment..
    } else if (!report$creds_in_keyring) {
      # add_creds() #TODO
      retrieve_creds()
      # ..and ambiguous credentials exist in the keyring..
    } else if (report$too_many_creds) {
      # TODO get user approval to remove multiple keyring creds
      # clear_keyring_creds()
      # add_creds() #TODO
      retrieve_creds()
    } else {
      stop("Something is wrong with the user credentials. Consider setting in .Renviron.")
    }
  } else {
    stop("This is a strange session. Stopping.")
  }
}
