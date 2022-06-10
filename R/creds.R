#' @include params.R
NULL

#' See if and where user credentials exist.
#'
#' Useful for informing subsequent user action within an application.
#'
#' @return A list -- variables related to credential existence.
#' @export
check_creds <- function() {
  report <- list()
  # is session interactive?
  report$session_interactive <- rlang::is_interactive()
  if (!("keyring" %in% installed.packages()[, 1])) {
    message("Keyring is not installed.")
    report$keyring_installed <- FALSE
    report$keyring_supported <- FALSE
  }
  # is `keyring` installed?
  if ("keyring" %in% installed.packages()[, 1]) {
    message("Keyring is installed.")
    report$keyring_installed <- TRUE
    # is `keyring` supported?
    if (!keyring::has_keyring_support()) {
      message("Keyring not supported. Use .Renviron to set credentials.")
      report$keyring_supported <- FALSE
    }
    if (keyring::has_keyring_support()) {
      message("Keyring is supported.")
      report$keyring_supported <- TRUE
    }
    # are credentials already in the system keyring?
    # no..
    if (nrow(keyring::key_list(hRvstAPI::.service)) == 0) {
      message("No credentials found in system keyring.")
      report$creds_in_keyring <- FALSE
      report$too_many_creds <- FALSE
    }
    # yes..
    if (nrow(keyring::key_list(hRvstAPI::.service)) == 1) {
      acct_id <- keyring::key_list(hRvstAPI::.service)$username
      message("Credentials found in system keyring for account: ", acct_id)
      report$creds_in_keyring <- TRUE
      report$too_many_creds <- FALSE
      message("Import these credentials to the environment with `retrieve_creds()`")
    }
    # yes, but more than one set..
    if (nrow(keyring::key_list(hRvstAPI::.service)) > 1) {
      message("Multiple credentials fount in system keyring. Please clear and add credentials again, or set with .Renviron.")
      report$creds_in_keyring <- TRUE
      report$too_many_creds <- TRUE
    }
  }
  # are credentials already in the environment?
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
#' @seealso [Harvest API V2 Documentation | Authentication](https://help.getharvest.com/api-v2/authentication-api/authentication/authentication/)
harvest_acct_id <- function() {
  acct_id <- Sys.getenv("HRVST_ACCT_ID")
  if (nzchar(acct_id)) {
    return(acct_id)
  } else {
    message("Account ID is missing. Running credential setup now..")
    set_creds()
    harvest_acct_id()
  }
}



#' Return the user's Harvest API v2 personal access token.
#'
#' @return A string -- the user personal access token.
#' @export
#'
#' @seealso [Harvest API V2 Documentation | Authentication](https://help.getharvest.com/api-v2/authentication-api/authentication/authentication/)
harvest_token <- function() {
  token <- Sys.getenv("HRVST_TOKEN")
  if (nzchar(token)) {
    return(token)
  } else {
    message("Personal access token is missing. Running credential setup now..")
    set_creds()
    harvest_token()
  }
}



#' Retrieve user credentials from the keyring to environment.
#'
#' This is intended to be a helper function for setting credentials.
#'
#' @export
retrieve_creds <- function() {
  Sys.setenv("HRVST_ACCT_ID" = keyring::key_list(hRvstAPI::.service)$username)
  Sys.setenv("HRVST_TOKEN" = keyring::key_get(hRvstAPI::.service))
  message("Credentials have been retrieved from the keyring and set in the environment.")
}



#' Delete all hRvstAPI Harvest credentials from the keyring.
#'
#' @export
clear_keyring_creds <- function() {
  keyring::key_delete(service = hRvstAPI::.service)
  message("Credentials have been cleared from the keyring.")
}



#' Delete all hRvstAPI Harvest credentials from the environment.
#'
#' @export
clear_environment_creds <- function() {
  Sys.unsetenv("HRVST_ACCT_ID")
  Sys.unsetenv("HRVST_TOKEN")
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
      add_creds()
      retrieve_creds()
      # ..and ambiguous credentials exist in the keyring..
    } else if (report$too_many_creds) {
      # TODO get user approval to remove multiple keyring creds
      clear_keyring_creds()
      clear_environment_creds()
      add_creds()
      retrieve_creds()
    } else {
      stop("Something is wrong with the user credentials. Consider setting in .Renviron.")
    }
  } else {
    stop("This is a strange session. Stopping.")
  }
}



#' Add user credentials to the keyring.
#'
#' @export
add_creds <- function() {
  # Give three tries to add the account ID..
  add_acct_id <- function(message = "Please enter your Harvest account ID.") {
    stringr::str_remove(
      stringr::str_squish(
        getPass::getPass(msg = message)
      ),
      pattern = " "
    )
  }
  acct_id <- add_acct_id()
  acct_id_test <- isTRUE(!is.null(acct_id) && nzchar(acct_id))
  acct_id_tries <- 1
  while (!acct_id_test) {
    if (acct_id_tries >= 3) {
      stop("Too many tries to enter nothing.")
    }
    acct_id <- add_acct_id(
      message = "Account ID is NULL or missing. Please re-enter."
    )
    acct_id_tries <- acct_id_tries + 1
  }
  # Give three tries to add the PAT..
  add_token <- function(message = "Please enter your Harvest token.") {
    stringr::str_remove(
      stringr::str_squish(
        getPass::getPass(msg = message)
      ),
      pattern = " "
    )
  }
  token <- add_token()
  token_test <- isTRUE(!is.null(token) && nzchar(token))
  token_tries <- 1
  while (!token_test) {
    if (token_tries >= 3) {
      stop("Too many tries to enter nothing.")
    }
    token <- add_token(
      message = "Token is NULL or missing. Please re-enter."
    )
    token_tries <- token_tries + 1
  }
  # Set the values to the keyring..
  keyring::key_set_with_value(
    service = hRvstAPI::.service,
    username = acct_id,
    password = token
  )
}
