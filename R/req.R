#' Create an authenticated GET request with headers and queries.
#'
#' @param base_url
#' @param headers
#' @param ...
#'
#' @return
harvest_GET <- function(base_url = NULL, headers = NULL,
                        is_active = NULL, ...) {
  if (missing(base_url) || rlang::is_null(base_url)) {
    base_url <- hRvstAPI::base_url
  }

  common_headers <- c(
    headers,
    list(
      `Harvest-Account-Id` = hRvstAPI::harvest_acct_id(),
      # Authorization = glue::glue("Bearer {hRvstAPI::harvest_token()}"),
      `User-Agent` = hRvstAPI::user_agent
    )
  )

  if (missing(headers)) {
    all_headers <- common_headers
  } else {
    all_headers <- c(
      common_headers,
      headers
    )
  }

  if (!missing(is_active) && !rlang::is_null(is_active)) {
    assertthat::assert_that(
      rlang::is_bool(is_active),
      msg = "If provided, argument is_active must be TRUE or FALSE."
    )
  }

  if (missing(...) && (missing(is_active) || rlang::is_null(is_active))) {
    queries <- NULL
  } else {
    queries <- list(is_active = is_active, ...)
  }

  req_obj <- httr2::request(base_url = base_url) |>
    httr2::req_headers(!!!all_headers) |>
    httr2::req_auth_bearer_token(hRvstAPI::harvest_token())

  if (!rlang::is_null(queries)) {
    httr2::req_url_query(req_obj, !!!queries)
  } else {
    req_obj
  }
}



#' Perform a request for a Harvest API resource.
#'
#' @param resource
#' @param all_pages
#' @param base_url
#' @param headers
#' @param ...
#'
#' @return
#' @export
#'
#' @seealso \href{https://help.getharvest.com/api-v2/introduction/overview/general/#rate-limiting}{Harvest API V2 Documentation | Rate Limiting}
harvest_req <- function(resource = NULL, all_pages = TRUE,
                        base_url = NULL, headers = NULL,
                        is_active = NULL, ...) {
  assertthat::assert_that(
    rlang::is_bool(all_pages),
    msg = "Argument all_pages must be TRUE or FALSE."
  )

  resource <- match.arg(
    resource,
    c(
      NULL,
      "clients",
      "projects",
      "tasks",
      "users",
      "user assignments",
      "task assignments",
      "time entries",
      "budget report",
      "time report"
    )
  )

  resource <- switch(
    resource,
    NULL = NULL,
    "clients" = "clients",
    "projects" = "projects",
    "tasks" = "tasks",
    "users" = "users",
    "user assignments" = "user_assignments",
    "task assignments" = "task_assignments",
    "time" = "time_entries",
    "budget report" = "budget_report",
    "time report" = "time_report"
  )

  # Harvest API v2 Rate Limiting
  # https://help.getharvest.com/api-v2/introduction/overview/general/#rate-limiting
  if (grepl("report", resource)) {
    rate_limit <- 100/(15*60) # 100 requests per 15 seconds for general API
  } else {
    rate_limit <- 100/15100 # 100 requests per 15 minutes for reports API
  }

  if (any(grepl("page", names(list(...))))) {
    all_pages <- FALSE
  }

  first_req <- hRvstAPI::harvest_GET(
    base_url = base_url,
    headers = headers,
    is_active = is_active,
    ...
  ) |>
    httr2::req_url_path_append(resource) |>
    httr2::req_throttle(rate = rate_limit) |>
    httr2::req_retry(max_tries = 5)

  print(first_req)

  do_parse_resp <- function(.req = NULL) {
    assertthat::assert_that(
      !rlang::is_null(.req),
      msg = "Request is missing in `do_parse_resp()`."
    )
    httr2::req_perform(.req) |>
      httr2::resp_body_json(simplifyVector = TRUE, flatten = TRUE)
  }

  resp <- first_req |> do_parse_resp()

  total_pages <- resp$total_pages

  cat("The total number of pages available is:", total_pages, ".\n")

  all_resp <- list(resp)

  next_link <- resp$links[["next"]]

  if (all_pages) {
    while (!rlang::is_null(next_link)) {
      resp <- hRvstAPI::harvest_GET(base_url = next_link) |>
        do_parse_resp()
      all_resp <- c(all_resp, list(resp))
      next_link <- resp$links[["next"]]
    }
    cat("All", total_pages, "pages were requested.\n")
    cat(length(all_resp), "pages were successfully downloaded.")
  }

  if (!all_pages) {
    cat("Only page", all_resp[[1]][["page"]], "was downloaded.\n")
  }

  return(all_resp)
}
