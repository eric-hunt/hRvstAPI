#' See if and where user credentials exist.
#'
#' Useful for informing subsequent user action within an application.
#'
#' @return
#' @export
#'
#' @examples
report_creds <- function() {
  report <- list()
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
  return(invisible(report))
}



#' Return the user's Harvest API v2 account ID.
#'
#' @return
#' @export
#'
#' @seealso \href{https://help.getharvest.com/api-v2/authentication-api/authentication/authentication/}{Harvest API V2 Documentation | Authentication}
#'
#' @examples
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
#' @return
#' @export
#'
#' @seealso \href{https://help.getharvest.com/api-v2/authentication-api/authentication/authentication/}{Harvest API V2 Documentation | Authentication}
#'
#' @examples
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



#' Retrieve user credentials from keyring to environment.
#'
#' @return
#' @export
#'
#' @examples
retrieve_creds <- function() {
  Sys.setenv("HRVST_ACCT_ID" = keyring::key_list(keyring_service)$username)
  Sys.setenv("HRVST_TOKEN" = keyring::key_get(keyring_service))
}



#' Delete all hRvstAPI Harvest credentials from the keyring.
#'
#' @return
#' @export
#'
#' @examples
clear_keyring_creds <- function() {
  keyring::key_delete(service = keyring_service)
}
