# Species list endpoint ----

#' Species list request
#'
#' Request a single page from the species list
#' @param ... URL filters accepted by the species list endpoint
#' @return Request object
req_species_list <- function(...) {
  params <- list(..., indoor = 0)
  single_params <- purrr::discard(params, ~length(.x) > 1)
  multi_params <- purrr::keep(params, ~length(.x) > 1)
  req <- glue::glue("https://perenual.com/api/species-list?",
                    "key={Sys.getenv('PERENUAL_KEY')}") |>
    httr2::request() |>
    req_error(is_error = \(resp) FALSE) |>
    httr2::req_url_query(!!!single_params)
  if (length(multi_params) > 0) {
    multi_string <- multi_params |>
      purrr::imap(~ paste0("&", .y, multi_fn(.x))) |>
      paste0(collapse = "")
    req <- httr2::req_url(req, paste0(req$url, multi_string))
  }
  req
}

#' Process page response
#'
#' Apply post hoc filters to a page response from the species list endpoint
#' @param page_resp Page response from the species list endpoint
#' @return List of
#' \item{data}{Any remaining data after filters are applied}
#' \item{page_count}{The page count. Only used for the first page.}
#' \item{status}{API status ("OK" or "api_error")}
process_page_resp <- function(page_resp, addl_filters) {

  if (page_resp$status_code != 200) {
    return(list(status = "api_error"))
  }

  page_list <- page_resp |>
    httr2::resp_body_string() |>
    jsonlite::parse_json()

  if (!addl_filters$include_no_img) {
    page_list$data <- page_list$data |>
      purrr::discard(~ is.null(.x$default_image$regular_url))
  }

  if (!addl_filters$include_trees) {
    page_list$data <- purrr::discard(page_list$data, ~ .x$id %in% tree_ids)
  }

  if (length(addl_filters$flower_color) != 0) {
    color_ids <- purrr::reduce(color_idx[addl_filters$flower_color], union)
    page_list$data <- purrr::keep(page_list$data, ~ .x$id %in% color_ids)
  }

  list(
    data = set_plant_names(page_list$data),
    page_count = page_list$last_page,
    status = "OK"
  )

}

#' Request batch of pages from species list
#'
#' Request a batch of pages from species list using url filters. Then apply
#' post hoc filters.
#' @param url_args Url arguments to include in request
#' @param addl_filters Additional filters to apply after the request
#' @param rem_pages Set of remaining pages that match the filters
#' @param pages_to_add Number of pages to request. Defaults to three.
#' @return List of
#' \item{data}{Combined species list data matching filters}
#' \item{ok_ids}{IDs of matching plants.}
#' \item{rem_pages}{Remaining pages that match the URL filters.}
#' \item{status}{API status ("OK" or "api_error")}
req_page_batch <- function(url_args, addl_filters, rem_pages,
                           pages_to_add = 3) {

  if (length(rem_pages) == 0) {
    return(list(status = "OK", rem_pages = NULL))
  }

  status <- "OK"

  if (length(rem_pages) > pages_to_add) {
    addl_pages <- sample(rem_pages, pages_to_add)
  } else {
    addl_pages <- rem_pages
  }

  return_list <- list(rem_pages = setdiff(rem_pages, addl_pages))

  processed_pages <- addl_pages |>
    purrr::map(~ do.call(req_species_list, c(url_args, page = .x))) |>
    httr2::req_perform_sequential(on_error = "return", progress = FALSE) |>
    purrr::map(~ process_page_resp(.x, addl_filters))

  if (processed_pages[[length(processed_pages)]]$status == "api_error") {
    status <- "api_error"
  }

  ok_pages <- processed_pages |>
    purrr::keep(~ .x$status == "OK" & length(.x$data) > 0)

  combined_data <- purrr::map(ok_pages, ~ .x$data) |>
    purrr::flatten()

  list(
    data = combined_data,
    ok_ids = to_api_ids(names(combined_data)),
    rem_pages = setdiff(rem_pages, addl_pages),
    status = status
  )

}

# Details endpoint ----

#' Details request
#'
#' Request a single plant from the details endpoint. Errors are suppressed
#' @param id ID to request
#' @return Response
req_details <- function(id) {
  glue::glue("https://perenual.com/api/species/details/{id}?",
             "key={Sys.getenv('PERENUAL_KEY')}") |>
    httr2::request() |>
    httr2::req_retry(max_tries = 3) |>
    req_error(is_error = \(resp) FALSE) |>
    httr2::req_perform()
}

#' Request batch of details
#'
#' Request a batch of details from the details endpoint.
#' @param ids IDs to request
#' @return List of
#' \item{data}{A list of details data for successful requests.}
#' \item{rem_ids}{Remaining IDs that were not used.}
#' \item{status}{API status ("OK" or "api_error")}
req_details_batch <- function(ids) {

  attempts <- 0
  details <- NULL

  # Try to get 12 valid records to display
  while (length(details) < 12 && attempts < 15) {

    if (attempts == length(ids)) {
      break
    }

    id <- ids[attempts + 1]
    resp <- req_details(id)

    if (resp$status != 200) {
      return_list <- list(
        data = details,
        rem_ids = setdiff(ids, ids[1:attempts]),
        status = "api_error"
      )
      return(return_list)
    }

    details_record <- resp_to_list(resp)
    if (valid_plant_entry(details_record)) {
      details[[paste0("plant_", id)]] <- details_record
    } else if (interactive()) {
      message(paste("Invalid record for id ", id))
    }
    attempts <- attempts + 1
  }

  list(
    data = details,
    rem_ids = setdiff(ids, ids[1:attempts]),
    status = "OK"
  )

}

#' Convert details response to list
#'
#' Convert a response from the details endpoint to a list
#' @param resp Response from the details endpoint
#' @return List of plant details for one plant
resp_to_list <- function(resp) {
  resp |>
    httr2::resp_body_string() |>
    jsonlite::parse_json()
}

# Care guide endpoint ----

req_care_guide <- function(id) {
  glue::glue("https://perenual.com/api/species-care-guide-list?species_id={id}",
             "&key={Sys.getenv('PERENUAL_KEY')}") |>
    httr2::request() |>
    httr2::req_perform()
}

req_care_guide_safe <- function(id) {
  id |>
    req_care_guide() |>
    httr2::resp_body_string() |>
    jsonlite::parse_json() |>
    _$data[[1]]
}

# Putting it all together ----

#' Request more plants
#'
#' Higher level function that uses req_page_batch and req_details batch to try
#' to get another set of details.
#' @inheritParams req_page_batch
#' @return List of
#' \item{details}{A list of details data for successful requests.}
#' \item{rem_pages}{Remaining species list pages that were not used.}
#' \item{rem_ids}{Remaining IDs that were not used.}
#' \item{status}{API status ("OK" or "api_error")}
req_more_plants <- function(url_args, addl_filters, rem_pages) {

  attempts <- 0
  details <- ok_ids <- rem_ids <- NULL

  while (all(attempts < 10, length(ok_ids) < 4, length(rem_pages) != 0)) {

    # Get another page of results and apply filters
    page_batch <- req_page_batch(
      url_args = url_args,
      addl_filters = addl_filters,
      rem_pages = rem_pages,
      pages_to_add = 1
    )

    rem_pages <- page_batch$rem_pages

    if (page_batch$status == "api_error") {
      return_list <- list(
        details = details,
        rem_pages = rem_pages,
        rem_ids = rem_ids,
        status = "api_error"
      )
      return(return_list)
    }

    if (length(page_batch$ok_ids) > 0) {

      details_batch <- req_details_batch(page_batch$ok_ids)
      rem_ids <- details_batch$rem_ids

      if (details_batch$status == "api_error") {
        return_list <- list(
          details = details,
          rem_pages = rem_pages,
          rem_ids = rem_ids,
          status = "api_error"
        )
        return(return_list)
      }

      if (length(details_batch$data) > 0) {
        details <- c(details, details_batch$data)
        ok_ids <- c(ok_ids, to_api_ids(names(details_batch$data)))
      }
    }
    attempts <- attempts + 1
  }

  list(
    details = details,
    rem_pages = rem_pages,
    rem_ids = rem_ids,
    status = "OK"
  )

}
