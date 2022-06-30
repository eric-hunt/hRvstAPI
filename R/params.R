#' hRvstAPI parameters
#'
#' These are common values used by hRvstAPI functions like the base URL for
#' the Harvest API v2, and the name of the hRvstAPI GitHub repo for
#' a user agent to be logged with each GET request made to the Harvest API.
#' In the future these may be set as options to give the user even more control
#' over this wrapper.
#'
#' @name params
NULL

#' @export
#' @rdname params
.service <- "Harvest (hRvstAPI)"

#' @export
#' @rdname params
.url <- "https://api.harvestapp.com/v2/"

#' @export
#' @rdname params
.agent <- "https://github.com/eric-hunt/hRvstAPI"

#' @export
#' @rdname params
.rds_path <- "~/hRvst.rds"

#' @export
#' @rdname params
.db_path <- "~/hRvst.sqlite"
