# Get plant search list for "find a plant" on the choose plants page

library(dplyr)
pkgload::load_all()

make_search_df <- function(resp_list) {
  data.frame(
    id = purrr::map_vec(resp_list$data, ~ .x$id),
    common_name = purrr::map_vec(resp_list$data, ~ .x$common_name),
    scientific_name =
      purrr::map_vec(resp_list$data, ~ paste(.x$scientific_name, collapse = ""))
  )
}

#TODO: Rewrite with new query functions; remove resp_to_list()
search_list <- NULL
for (zone in 3:11) {
  message(paste0("Fetching data for zone ", zone, "..."))
  page1 <- query_species_list(hardiness = hardiness_range(zone)) |>
    resp_to_list()
  zone_df <- make_search_df(page1)
  for (page in 2:page1$page_count) {
    message(paste0("Page ", page, "..."))
    zone_df <-
      query_species_list(hardiness = hardiness_range(zone), page = page) |>
      resp_to_list() |>
      make_search_df() |>
      bind_rows(zone_df)
    Sys.sleep(1)
  }
  search_list[[paste0("zone", zone)]] <- search_df |>
    dplyr::bind_rows(zone_df)
}

arrange_search_df <- function(search_df, name_type) {
  search_df <- search_df |>
    arrange(!!sym(name_type)) |>
    select(id, !!name_type)
  if (name_type == "common_name") {
    mutate(search_df, !!name_type := stringr::str_to_title(!!sym(name_type)))
  } else {
    search_df
  }
}

search_list <- search_list |>
  purrr::map(
    ~ list(
      by_common_name = arrange_search_df(.x, "common_name"),
      by_scientific_name = arrange_search_df(.x, "scientific_name")
    )
  )

usethis::use_data(search_list, overwrite = TRUE)
