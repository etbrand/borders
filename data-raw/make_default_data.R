# Make default data to populate the app on startup

pkgload::load_all()

get_zone_data <- function(zone) {

  message(paste0("Fetching data for ", zone, "..."))

  page1_resp <-
    do.call(req_species_list, list(hardiness = hardiness_range(zone))) |>
    httr2::req_perform()
  page1 <- page1_resp |>
    process_page_resp(list(include_no_img = FALSE, include_trees = FALSE))

  page_batch <- req_page_batch(
    rem_pages = sample(2:page1$page_count),
    url_args = list(hardiness = hardiness_range(zone)),
    addl_filters = list(include_no_img = FALSE, include_trees = FALSE),
    pages_to_add = 5
  )

  details <- req_details_batch(sample(page_batch$ok_ids))
  Sys.sleep(15)
  print(length(details$data))
  list(details = details, rem_pages = page_batch$rem_pages)
}

hardiness_zones <- paste0("zone", 3:11)
default_zone_data <- hardiness_zones |>
  purrr::map(get_zone_data) |>
  purrr::set_names(hardiness_zones)

usethis::use_data(default_zone_data, overwrite = TRUE)
