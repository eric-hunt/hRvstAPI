#' Return the user's Harvest API v2 account ID.
#'
#' @return
#' @export
#'
#' @seealso \href{https://help.getharvest.com/api-v2/authentication-api/authentication/authentication/}{Harvest API V2 Documentation | Authentication}
#'
#' @examples
harvest_acct_id <- function() {
  acct_id <- Sys.getenv("HARVEST_ACCT_ID")
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
  token <- Sys.getenv("HARVEST_TOKEN")
  if (nzchar(token)) {
    return(token)
  } else {
    message("Personal access token is missing. Running credential setup now..")
    # set_creds() #TODO
    # harvest_token()
  }
}
