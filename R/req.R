#' @include params.R
NULL

#' Create an authenticated GET request with headers and queries.
#'
#' The basic headers that are automatically set in this function are
#' `Harvest-Account-Id`, `Authorization`, and `User-Agent`. Other headers
#' can be added by the user and will be concatenated, but currently
#' these important headers are set by the `hRvstAPI::harvest_acct_id()`,
#' `hRvstAPI::harvest_token()`, and `hRvstAPI::.agent` objects.
#'
#' @param base_url A string -- the common URL component for Harvest API v2 requests. (default value NULL will refer to hRvstAPI::.url)
#' @param headers A list -- the headers required for authentication of each Harvest API v2 request.
#' @param ... A named list of (optional) additional query parameters.
#'
#' @return An HTTP response: an S3 list with class `httr2_request`.
#' @export
#' @seealso [httr2::request()]
harvest_GET <- function(base_url = NULL, headers = NULL,
                        is_active = NULL, ...) {
  if (missing(base_url) || rlang::is_null(base_url)) {
    base_url <- hRvstAPI::.url
  }

  common_headers <- c(
    headers,
    list(
      `Harvest-Account-Id` = hRvstAPI::harvest_acct_id(),
      `Authorization` = paste0("Bearer ", hRvstAPI::harvest_token()),
      `User-Agent` = hRvstAPI::.agent
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
    httr2::req_headers(!!!all_headers)# |>
    # httr2::req_auth_bearer_token(hRvstAPI::harvest_token())

  if (!rlang::is_null(queries)) {
    httr2::req_url_query(req_obj, !!!queries)
  } else {
    req_obj
  }
}



#' Perform a request for a Harvest API resource.
#'
#' @description
#' This is the primary function used to gather resources from the
#' Harvest API v2. It uses [hRvstAPI::harvest_GET()] to construct
#' an [httr2::request()] and then performs that request after
#' modifying the URL.
#'
#' *Requests are automatically throttled to comply with
#' Harvest API v2 constraints.*
#'
#' @details
#' If only a specific page is required, the `page = [n]` parameter
#' can be passed to `...` to be added to the query parameters.
#' Adding this query parameter will automatically set *all_pages*
#' to FALSE.
#'
#' @param resource A string -- modifies the URL path to point to a specific Harvest API v2 resource. Currently available options:
#' - **users**
#'     ([Users API](https://help.getharvest.com/api-v2/users-api/))
#' - **clients**
#'     ([Clients API](https://help.getharvest.com/api-v2/clients-api/))
#' - **projects**
#'     ([Projects API](https://help.getharvest.com/api-v2/projects-api/))
#' - **tasks**
#'     ([Tasks API](https://help.getharvest.com/api-v2/tasks-api/))
#' - **project assignments**
#'     (via [Users API](https://help.getharvest.com/api-v2/users-api/))
#'     *i.e Which projects belong to which clients.*
#' - **user assignments**
#'     (via [Projects API](https://help.getharvest.com/api-v2/projects-api/))
#'     *i.e. How users are assigned to each projects.*
#' - **task assignments**
#'     (via [Projects API](https://help.getharvest.com/api-v2/projects-api/))
#'     *i.e. What tasks are assigned to each project.*
#' - **time entries**
#'     (via [Timesheets API](https://help.getharvest.com/api-v2/timesheets-api/))
#' - **budget reports**
#'     (via [Reports API](https://help.getharvest.com/api-v2/reports-api/))
#' - **time reports**
#'     (via [Reports API](https://help.getharvest.com/api-v2/reports-api/))
#' @param all_pages A boolean -- should all pages be gathered for a requested resource? (default value TRUE)
#' @param base_url -- A string -- the common URL component for Harvest API v2 requests, passed to [hRvstAPI::harvest_GET()].
#' @param headers A list -- the headers required for authentication of each Harvest API v2 request, passed to [hRvstAPI::harvest_GET()].
#' @param ... A named list of (optional) additional query parameters, passed to [hRvstAPI::harvest_GET()].
#'
#' @return A [tibble::tibble()] of all response content.
#' @export
#'
#' @seealso [Harvest API V2 Documentation | Rate Limiting](https://help.getharvest.com/api-v2/introduction/overview/general/#rate-limiting)
harvest_req <- function(resource = NULL, all_pages = TRUE,
                        base_url = NULL, headers = NULL,
                        is_active = NULL, ...) {
  assertthat::assert_that(
    rlang::is_bool(all_pages),
    msg = "Argument all_pages must be TRUE or FALSE."
  )

  resource_arg <- match.arg(
    resource,
    c(
      NULL,
      "clients",
      "projects",
      "tasks",
      "users",
      "user assignments",
      "task assignments",
      "project assignments",
      "time entries",
      "budget report",
      "time report"
    )
  )

  resource_path <- switch(
    resource_arg,
    NULL = NULL,
    "clients" = "clients",
    "projects" = "projects",
    "tasks" = "tasks",
    "users" = "users",
    "user assignments" = "user_assignments",
    "task assignments" = "task_assignments",
    "project assignments" = "users",
    "time" = "time_entries",
    "budget report" = "budget_report",
    "time report" = "time_report"
  )

  # TODO expand path for user project assignments for all users

  # Harvest API v2 Rate Limiting
  # https://help.getharvest.com/api-v2/introduction/overview/general/#rate-limiting
  if (grepl("report", resource_arg)) {
    rate_limit <- 100/(15*60) # 100 requests per 15 minutes for reports API
  } else {
    rate_limit <- 100/15 # 100 requests per 15 seconds for general API
  }

  if (any(grepl("page", names(list(...))))) {
    all_pages <- FALSE
  }

  # function to perform the request and parse JSON to R list..
  perform_and_parse <- function(.req = NULL) {
    assertthat::assert_that(
      !rlang::is_null(.req),
      msg = "`perform_and_parse()`: request is missing."
    )
    httr2::req_perform(.req) |>
      httr2::resp_body_json(simplifyVector = TRUE, flatten = TRUE)
  }

  # function to extract data and format as a tibble..
  extract_data <- function(.parsed_resp, .mapped = TRUE) {
    assertthat::assert_that(
      rlang::is_bool(.mapped),
      msg = "`extract_data()`: .mapped must be TRUE or FALSE."
    )
    if (.mapped) {
      accessors <- c(1) # `map` or `apply` accesses first level of list
    } else {
      accessors <- c(1, 1) # need another accessor to enter first level of list
    }
    tibble::as_tibble(purrr::pluck(.parsed_resp, !!!accessors)) |>
      dplyr::mutate(resp_page = .parsed_resp$page)
  }

  first_req <- hRvstAPI::harvest_GET(
    base_url = base_url,
    headers = headers,
    is_active = is_active,
    ...
  ) |>
    httr2::req_url_path_append(resource_path) |>
    httr2::req_throttle(rate = rate_limit) |>
    httr2::req_retry(max_tries = 5)

  print(first_req)

  resp <- first_req |> perform_and_parse()

  total_pages <- resp$total_pages

  cat("The total number of pages available is:", total_pages, ".\n")

  all_resp <- list(resp)

  next_link <- resp$links[["next"]]

  if (all_pages) {
    while (!rlang::is_null(next_link)) {
      resp <- hRvstAPI::harvest_GET(base_url = next_link) |>
        perform_and_parse()
      all_resp <- c(all_resp, list(resp))
      next_link <- resp$links[["next"]]
    }
    cat("All", total_pages, "pages were requested.\n")
    cat(length(all_resp), "pages were successfully downloaded.")
  }

  if (!all_pages) {
    cat("Only page", all_resp[[1]][["page"]], "was downloaded.\n")
  }

  if (resource_arg == "project assignments") {
    user_ids <- purrr::flatten_chr(
      purrr::map(all_resp, purrr::pluck, "users", "id")
    )
    purrr::map(
      purrr::set_names(user_ids),
      function(id) {
        httr2::req_url_path_append(first_req, id, "project_assignments") |>
          perform_and_parse()
      }
    ) |>
      purrr::imap_dfr(
        function(parsed_resp, nm) {
          extract_data(parsed_resp) |>
            dplyr::mutate(user_id = readr::parse_integer(nm))
        }
      )
  } else {
    all_resp |>
      purrr::map_dfr(extract_data)
  }
}
