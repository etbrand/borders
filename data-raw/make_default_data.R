pkgload::load_all()

# Get default data to populate the app on startup
get_zone_data <- function(zone) {
  message(paste0("Fetching data for zone ", zone, "..."))
  page1 <- query_list_safe(hardiness = hardiness_range(zone))
  page1_ids <- purrr::map_vec(page1$data, ~ .x$id)
  id_list <- query_addl_pages(
    ids = page1_ids,
    page_count = page1$page_count,
    url_args = list(hardiness = hardiness_range(zone))
  )
  details <- get_details_batch(id_list$ids)
  Sys.sleep(15)
  list(details = details, queried_pages = id_list$queried_pages)
}

hardiness_zones <- 3:11
default_zone_data <- hardiness_zones |>
  purrr::map(get_zone_data) |>
  purrr::set_names(paste0("zone", hardiness_zones))

usethis::use_data(default_zone_data, overwrite = TRUE)
