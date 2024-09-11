pkgload::load_all()

# Get default data to populate the app on startup
get_zone_data <- function(zone) {
  message(paste0("Fetching data for zone ", zone, "..."))
  page1 <- query_list_safe(
    url_args = list(hardiness = hardiness_range(zone)),
    addl_filters = list(filter_no_img = TRUE, filter_trees = TRUE)
  )
  page1_ids <- purrr::map_vec(page1$data, ~ .x$id)

  if (length(page1$data) == 0) {
    message("No data in page 1")
  }

  id_list <- query_addl_pages(
    rem_pages = sample(2:page1$page_count),
    url_args = list(hardiness = hardiness_range(zone)),
    addl_filters = list(filter_no_img = TRUE, filter_trees = TRUE)
  )

  details <- query_details_batch(sample(id_list$ids))
  Sys.sleep(15)
  list(details = details, rem_pages = id_list$rem_pages)
}

hardiness_zones <- 3:11
default_zone_data <- hardiness_zones |>
  purrr::map(get_zone_data) |>
  purrr::set_names(paste0("zone", hardiness_zones))

usethis::use_data(default_zone_data, overwrite = TRUE)
