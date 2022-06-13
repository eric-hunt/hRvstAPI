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
  # is `keyring` installed/supported?
  if (!("keyring" %in% installed.packages()[, 1])) {
    message("Keyring is not installed.")
    report$keyring_installed <- FALSE
    report$keyring_supported <- FALSE
  }
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
    }
    # yes, but more than one set..
    if (nrow(keyring::key_list(hRvstAPI::.service)) > 1) {
      message("Multiple credentials fount in system keyring.\n
              Please clear and add credentials again, or set with .Renviron.")
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



#' Delete all hRvstAPI Harvest credentials from the keyring.
#'
#'This is intended to be a helper function for setting credentials.
#'
clear_keyring_creds <- function() {
  keyring::key_delete(service = hRvstAPI::.service)
  message("Credentials have been cleared from the keyring.")
}



#' Delete all hRvstAPI Harvest credentials from the environment.
#'
#'This is intended to be a helper function for setting credentials.
#'
clear_environment_creds <- function() {
  Sys.unsetenv("HRVST_ACCT_ID")
  Sys.unsetenv("HRVST_TOKEN")
  message("Credentials have been cleared from the environment.")
}



#' Add user credentials to the keyring.
#'
#' This is intended to be a helper function for setting credentials.
#'
#' @param .report A list -- report returned from hRvstAPI::check_creds()
#'
add_creds <- function(.report = NULL) {
  assertthat::assert_that(
    !is.null(.report),
    msg = "No `check_creds()` report provided to `add_creds()`."
  )
  report <- .report

  if (shiny::isRunning()) {
    # If a Shiny app is using this API wrapper add creds this way..
    # TODO retrieve with a `shiny` modal..
  } else if(report$session_interactive) {
    # ..otherwise add creds this way..
    add_acct_id <- function(message = "Please enter your Harvest account ID.") {
      stringr::str_remove(
        stringr::str_squish(
          getPass::getPass(msg = message)
        ),
        pattern = " "
      )
    }
    acct_id <- add_acct_id()
    # Give three tries to add the account ID..
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

    add_token <- function(message = "Please enter your Harvest token.") {
      stringr::str_remove(
        stringr::str_squish(
          getPass::getPass(msg = message)
        ),
        pattern = " "
      )
    }
    token <- add_token()
    # Give three tries to add the PAT..
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

    # Assign the values to the keyring or environment..
    if (report$keyring_supported) {
      # Set the values to the keyring..
      keyring::key_set_with_value(
        service = hRvstAPI::.service,
        username = acct_id,
        password = token
      )
      message("Credentials have been saved to the system keyring.")
    } else if (!report$keyring_supported) {
      # Set values to the environment..
      Sys.setenv("HRVST_ACCT_ID" = acct_id)
      Sys.setenv("HRVST_TOKEN" = token)
      message("Keyring not supported.\n
            Credentials have been set in the environment.")
    }
  } else {
    stop("Unable to add credentials to keyring or environment.\n
         Try setting credentials in .Renviron.")
  }
}



#' Retrieve user credentials from the keyring to environment.
#'
#' This is intended to be a helper function for setting credentials.
#'
#' @param .report A list -- report returned from hRvstAPI::check_creds()
#'
retrieve_creds <- function(.report = NULL) {
  message("Attempting to retrieve credentials from system keyring.")
  assertthat::assert_that(
    !is.null(.report),
    msg = "No `check_creds()` report provided to `retrieve_creds()`."
  )
  report <- .report

  clear_environment_creds()

  cred_setter <- function() {
    Sys.setenv("HRVST_ACCT_ID" = keyring::key_list(hRvstAPI::.service)$username)
    Sys.setenv("HRVST_TOKEN" = keyring::key_get(hRvstAPI::.service))
    message("Credentials have been retrieved from the keyring
            and set in the environment.")
  }

  if (report$keyring_supported) {
    if (!report$creds_in_keyring) {
      add_creds(report)
      cred_setter()
    } else if (report$creds_in_keyring && !report$too_many_creds) {
      cred_setter()
    } else if (report$too_many_creds) {
      # TODO add user agreement to clear creds from keyring
      clear_keyring_creds()
      add_creds(report)
      cred_setter()
    } else {
      stop("Unable to retrieve credentials from keyring.\n
         Try setting credentials in .Renviron.")
    }
  } else {
    add_creds(report)
  }
}



#' Set user credentials as environment variables.
#'
#' @export
set_creds <- function() {
  report <- check_creds()

  if (!report$session_interactive) {
    # If no user is present..
    if (report$creds_already_exist) {
      # ..and credentials exist in .Renviron..
      message("Credentials are already set in the environment.")
    } else if (report$creds_in_keyring && !report$too_many_creds) {
      # ..and credentials exist in the keyring..
      retrieve_creds(report)
    } else if (!report$creds_in_keyring || report$too_many_creds) {
      # ..and no credentials or ambiguous credentials exist in the keyring..
      assertthat::assert_that(
        report$creds_already_exist,
        msg = "No credentials or ambiguous credentials exist in the keyring,\n
          and account ID or PAT are not set as environment variables.\n
          Session is not interactive. Stopping."
      )
    } else {
      stop("Something is wrong with the user credentials.
           Consider setting in .Renviron.")
    }
  } else if (report$session_interactive) {
    # If a user is present..
    if (report$creds_already_exist) {
      # ..and credentials exist in .Renviron..
      message("Credentials are already set in the environment.")
    } else if (!report$creds_already_exist) {
      # ..and credentials don't exist in .Renviron..
      retrieve_creds(report)
    }
  } else {
    stop("Session is interactive, but cannot retrieve or add credentials.\n
         Consider setting credentials in .Renviron.")
  }
}



#' Return the user's Harvest API v2 account ID.
#'
#' @return A string -- the user account ID.
#' @export
#'
#' @seealso [Harvest API V2 Documentation | Authentication](https://help.getharvest.com/api-v2/authentication-api/authentication/authentication/)
hrvst_acct_id <- function() {
  acct_id <- Sys.getenv("HRVST_ACCT_ID")
  if (nzchar(acct_id)) {
    return(acct_id)
  } else {
    message("Account ID is missing. Running credential setup now..")
    set_creds()
    hrvst_acct_id()
  }
}



#' Return the user's Harvest API v2 personal access token.
#'
#' @return A string -- the user personal access token.
#' @export
#'
#' @seealso [Harvest API V2 Documentation | Authentication](https://help.getharvest.com/api-v2/authentication-api/authentication/authentication/)
hrvst_token <- function() {
  token <- Sys.getenv("HRVST_TOKEN")
  if (nzchar(token)) {
    return(token)
  } else {
    message("Personal access token is missing. Running credential setup now..")
    set_creds()
    hrvst_token()
  }
}
