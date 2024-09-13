#' @import jsonlite httr2

make_url_args <- function(input, input_ids) {
  input_ids |>
    purrr::map(~ if (.x == "flower_color") { NULL } else { input[[.x]] }) |>
    purrr::set_names(input_ids) |>
    purrr::compact()
}

# Functions for querying the species list endpoint ----

query_species_list <- function(...) {
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

# Functions for querying the details endpoint ----

query_details <- function(id) {
  glue::glue("https://perenual.com/api/species/details/{id}?",
             "key={Sys.getenv('PERENUAL_KEY')}") |>
    httr2::request() |>
    httr2::req_retry(max_tries = 3) |>
    req_error(is_error = \(resp) FALSE) |>
    httr2::req_perform()
}

resp_to_list <- function(resp) {
  resp |>
    httr2::resp_body_string() |>
    jsonlite::parse_json()
}

# Functions for querying the care guide endpoint ----

query_care_guide <- function(id) {
  glue::glue("https://perenual.com/api/species-care-guide-list?species_id={id}",
             "&key={Sys.getenv('PERENUAL_KEY')}") |>
    httr2::request() |>
    httr2::req_perform()
}

#TODO: Add error handling; add different code for no care guides available
query_care_guide_safe <- function(id) {
  id |>
    query_care_guide() |>
    httr2::resp_body_string() |>
    jsonlite::parse_json() |>
    _$data[[1]]
}


process_page_resp <- function(page_resp, addl_filters) {

  if (page_resp$status_code != 200) {
    return(list(status = "api_error"))
  }

  page_list <- page_resp |>
    httr2::resp_body_string() |>
    jsonlite::parse_json()

  page_list$data <- page_list$data |>
    purrr::discard(~ is.null(.x$default_image$regular_url))

  if (!addl_filters$include_trees) {
    page_list$data <- purrr::discard(page_list$data, ~ .x$id %in% tree_ids)
  }

  if (not_empty(addl_filters$flower_color)) {
    color_ids <- purrr::reduce(color_idx[addl_filters$flower_color], union)
    page_list$data <- purrr::keep(page_list$data, ~ .x$id %in% color_ids)
  }

  list(
    data = set_plant_names(page_list$data),
    page_count = page_list$last_page,
    status = "OK"
  )

}

# The first response will only return page 1 of results. Get additional pages
#' @param url_args Url arguments to include in query
#' @param addl_filters Additional filters to apply after the query
#' @param rem_pages Set of remaining pages that match the filters
#' @param pages_to_add Number of pages to query. Default 3
#' @noRd
query_page_batch <- function(url_args, addl_filters, rem_pages,
                             pages_to_add = 3) {

  if (length(rem_pages) == 0) {
    return(list(status = "OK", rem_pages = NULL))
  }

  print("Querying three more pages...")

  status <- "OK"

  if (length(rem_pages) > pages_to_add) {
    addl_pages <- sample(rem_pages, pages_to_add)
  } else {
    addl_pages <- rem_pages
  }

  return_list <- list(rem_pages = setdiff(rem_pages, addl_pages))

  processed_pages <- addl_pages |>
    purrr::map(~ do.call(query_species_list, c(url_args, page = .x))) |>
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

query_details_batch <- function(ids) {

  # Try to get 12 valid records to display
  attempts <- 0
  details <- NULL

  while(length(details) < 12 && attempts < 15) {

    if (attempts == length(ids)) {
      break
    }

    id <- ids[attempts + 1]

    print(paste("Trying id", id))
    resp <- query_details(id)

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
      print("Successful!")
      details[[paste0("plant_", id)]] <- details_record
      print(length(details))
    } else {
      #TODO: Remove
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


# Only run when out of IDs / no details
query_more_plants <- function(url_args, addl_filters, rem_pages) {
  attempts <- 0
  details <- ok_ids <- rem_ids <- NULL
  while (all(attempts < 10, length(ok_ids) < 4, length(rem_pages) != 0)) {

    # Get another page of results and apply filters
    page_batch <- query_page_batch(
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

      details_batch <- query_details_batch(page_batch$ok_ids)
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
        if (any(purrr::map_vec(details_batch$data,
                               ~ is.null(.x$default_image$regular_url)))) {
          print("NULL IMAGE!")
        }
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
