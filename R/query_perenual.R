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
    httr2::req_url_query(!!!single_params)
  if (length(multi_params) > 0) {
    multi_string <- multi_params |>
      purrr::imap(~ paste0("&", .y, multi_fn(.x))) |>
      paste0(collapse = "")
    req <- httr2::req_url(req, paste0(req$url, multi_string))
  }
  httr2::req_perform(req)
}

query_list_safe <- function(url_args, addl_filters) {

  resp <- tryCatch(do.call(query_species_list, url_args), error = identity)

  if (inherits(resp, "error") || resp$status_code != 200) {
    return("api_error")
  }

  resp_list <- resp |>
    httr2::resp_body_string() |>
    jsonlite::parse_json()

  if (addl_filters$filter_no_img %||% FALSE) {
    resp_list$data <- resp_list$data |>
      purrr::discard(~ is.null(.x$default_image$regular_url))
  }

  if (addl_filters$filter_trees %||% FALSE) {
    resp_list$data <- purrr::discard(resp_list$data, ~ .x$id %in% tree_ids)
  }

  if (not_empty(addl_filters$flower_color)) {
    color_ids <- purrr::reduce(color_idx[addl_filters$flower_color], union)
    resp_list$data <- purrr::keep(resp_list$data, ~ .x$id %in% color_ids)
  }

  resp_list$data <- set_plant_names(resp_list$data)

  list(
    data = if (length(resp_list$data) == 0) NULL else resp_list$data,
    page_count = resp_list$last_page
  )

}

# The first response will only return page 1 of results. Get additional pages
query_addl_pages <- function(url_args, addl_filters, rem_pages,
                             pages_to_add = 4) {

  print("Queried another page...")

  if (length(rem_pages) == 0) {
    return(NULL)
  }

  if (length(rem_pages) > pages_to_add) {
    addl_pages <- sample(rem_pages, pages_to_add)
  } else {
    addl_pages <- rem_pages
  }

  return_list <- list(rem_pages = setdiff(rem_pages, addl_pages))
  for (page in addl_pages) {
    addl_response <-
      query_list_safe(url_args = c(url_args, page = page), addl_filters)
    if (identical(addl_response, "api_error")) {
      return("api_error")
    }
    addl_ids <- unname(purrr::map_vec(addl_response$data, ~ .x$id))
    addl_imgs <- purrr::map(addl_response$data, ~ .x$default_image)
    return_list$ids <- c(return_list$ids, addl_ids)
    return_list$imgs <- c(return_list$data, addl_imgs)
  }

  return_list

}

# Functions for querying the details endpoint ----

query_details <- function(id) {
  glue::glue("https://perenual.com/api/species/details/{id}?",
             "key={Sys.getenv('PERENUAL_KEY')}") |>
    httr2::request() |>
    httr2::req_perform()
}

query_details_safe <- function(id) {
  resp <- tryCatch(query_details(id), error = identity)
  if (inherits(resp, "error") || resp$status_code != 200) {
    return("api_error")
  }
  details_record <- jsonlite::parse_json(httr2::resp_body_string(resp))
  if (!valid_plant_entry(details_record)) {
    return("invalid_record")
  }
  details_record
}

#TODO: Can remove?
resp_to_list <- function(resp) {
  resp_list <- resp |>
    httr2::resp_body_string() |>
    jsonlite::parse_json()
  list(data = resp_list$data, page_count = resp_list$last_page)
}

query_details_batch <- function(ids) {

  # Try to get 12 valid records to display
  attempts <- 0
  details <- NULL
  while(all(attempts < length(ids), length(details) < 12, attempts < 20)) {
    id <- ids[attempts + 1]
    print(paste("Trying id", id))
    details_record <- query_details_safe(id)
    if (identical(details_record, "api_error")) {
      message(paste("API error occurred for id ", id))
      return("api_error")
    }
    if (identical(details_record, "invalid_record")) {
      message(paste("Invalid record for id ", id))
    } else {
      print("Successful!")
      details[[paste0("plant_", id)]] <- details_record
      print(paste0("Length from details batch: ", length(details)))
    }
    attempts <- attempts + 1
  }

  list(data = details, rem_ids = setdiff(ids, ids[1:attempts]))

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
