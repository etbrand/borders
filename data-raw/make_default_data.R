pkgload::load_all()

# Get default data to populate the app on startup
get_zone_data <- function(zone) {
  message(paste0("Fetching data for zone ", zone, "..."))
  resp <- query_species_list(hardiness = hardiness_range(zone))
  species_list <- resp_to_list(resp) |>
    add_species_records(list(hardiness = hardiness_range(zone)))
  species_list$data <- species_list$data[1:12]
  species_details <- species_list$data |>
    purrr::map(~ query_details(.x$id)) |>
    purrr::map(httr2::resp_body_json)
  Sys.sleep(15)
  list(species_list = species_list, species_details = species_details)
}

hardiness_zones <- 2:12
default_zone_data <- hardiness_zones |>
  purrr::map(get_zone_data) |>
  purrr::set_names(paste0("zone", hardiness_zones))

usethis::use_data(default_zone_data, overwrite = TRUE)
