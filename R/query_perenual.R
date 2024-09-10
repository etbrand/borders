#' @import jsonlite httr2

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

  if (addl_filters$filter_no_trees %||% FALSE) {
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

query_care_guide <- function(id) {
  glue::glue("https://perenual.com/api/species-care-guide-list?species_id={id}",
             "&key={Sys.getenv('PERENUAL_KEY')}") |>
    httr2::request() |>
    httr2::req_perform()
}

#TODO: Add error handling; change name to query_care_guide_safe; add different code for no care guides available
get_care_guide <- function(id) {
  id |>
    query_care_guide() |>
    httr2::resp_body_string() |>
    jsonlite::parse_json() |>
    _$data[[1]]
}

#TODO: Can remove?
resp_to_list <- function(resp) {
  resp_list <- resp |>
    httr2::resp_body_string() |>
    jsonlite::parse_json()
  list(data = resp_list$data, page_count = resp_list$last_page)
}

make_url_args <- function(input, input_ids) {
  input_ids |>
    purrr::map(~ if (.x == "flower_color") { NULL } else { input[[.x]] }) |>
    purrr::set_names(input_ids) |>
    purrr::compact()
}

# The first response will only return page 1 of results. Get additional pages
query_addl_pages <- function(ids, page_count, url_args, addl_filters,
                             imgs = NULL, queried_pages = 1, pages_to_add = 4) {
  print("Queried another page...")
  if (page_count == 1) {
    return(NULL)
  } else {
    addl_pages <- setdiff(2:page_count, queried_pages)
    if (length(addl_pages) == 0) {
      return(NULL)
    }
    if (page_count >= pages_to_add + 1) {
      addl_pages <- sample(addl_pages, pages_to_add)
    }
  }

  species_list <- NULL
  for (page in addl_pages) {
    addl_response <-
      query_list_safe(url_args = c(url_args, page = page), addl_filters)
    if (identical(addl_response, "api_error")) {
      return("api_error")
    }
    species_list$data <- c(species_list$data, addl_response$data)
  }

  # Show plant list in random order
  ids <- sample(c(ids, purrr::map_vec(species_list$data, ~ .x$id)))

  # Sometimes an entry in the plant list will contain images but they're not
  # included in the details entry

  #if (length(species_list$data) > 0) browser()

  print(paste("Current images:", paste(names(imgs), collapse = " ")))
  print(paste(names(purrr::map(species_list$data, ~ .x$default_image)), collapse = " "))

  list(
    ids = ids,
    imgs = c(imgs, purrr::map(species_list$data, ~ .x$default_image)),
    queried_pages = c(queried_pages, addl_pages)
  )

}

get_details_batch <- function(ids) {

  # Try to get 12 valid records to display, but don't try too hard
  attempts <- 0
  details <- NULL
  while(all(attempts < length(ids), length(details) < 12, attempts < 20)) {
    id <- ids[attempts + 1]
    details_record <- query_details_safe(id)
    if (identical(details_record, "api_error")) {
      message(paste("API error occurred for id ", id))
      return("api_error")
    }
    if (identical(details_record, "invalid_record")) {
      message(paste("Invalid record for id ", id))
    } else {
      details[[paste0("plant_", id)]] <- details_record
    }
    attempts <- attempts + 1
  }

  list(data = details, rem_ids = setdiff(ids, ids[1:attempts]))

}


