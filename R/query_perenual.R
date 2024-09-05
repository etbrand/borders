#' @import jsonlite httr2
#TODO: Combine all pages?

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

query_details <- function(ID) {
  glue::glue("https://perenual.com/api/species/details/{ID}?",
             "key={Sys.getenv('PERENUAL_KEY')}") |>
    httr2::request() |>
    httr2::req_perform()
}

query_care_guide <- function(ID) {
  glue::glue("https://perenual.com/api/species-care-guide-list?species_id={ID}",
             "&key={Sys.getenv('PERENUAL_KEY')}") |>
    httr2::request() |>
    httr2::req_perform()
}

#TODO: Add error handling
get_care_guide <- function(id) {
  id |>
    query_care_guide() |>
    httr2::resp_body_string() |>
    jsonlite::parse_json() |>
    _$data[[1]]
}

resp_to_list <- function(resp) {
  resp_list <- resp |>
    httr2::resp_body_string() |>
    jsonlite::parse_json()
  list(data = resp_list$data, page_count = resp_list$last_page)
}

make_url_args <- function(input, input_ids) {
  input_ids |>
    purrr::map(~ if (.x == "hardiness") { hardiness_range(input[[.x]]) }
               else { input[[.x]] }) |>
    purrr::set_names(input_ids) |>
    purrr::compact()
}

# The first response will only return page 1 of results. Get additional pages
add_species_records <- function(species_list, url_args, pages_to_add = 4) {
  if (species_list$page_count == 1) {
    addl_pages <- NULL
  } else if (species_list$page_count < pages_to_add + 1) {
    addl_pages <- 2:species_list$page_count
  } else {
    addl_pages <- sample(2:species_list$page_count, pages_to_add)
  }

  for (page in addl_pages) {
    addl_response <- do.call(query_species_list, c(url_args, page = page)) |>
      resp_to_list()
    species_list$data <- c(species_list$data, addl_response$data)
  }

  # Remove trees and records without images
  species_list$data <- species_list$data |>
    purrr::discard(~ .x$id %in% 1:397 ||
                     is.null(.x$default_image$regular_url)) |>
    shuffle_list()

  IDs <- purrr::map(species_list$data, ~ paste0("plant_", .x$id))
  species_list$data <- purrr::set_names(species_list$data, IDs)

  species_list
}
