# Create zone and city lookup by zipcode

library(dplyr)

read.csv3 <- function(file) read.csv(file, colClasses = "character")

zipcode_info <- read.csv3("data-raw/zip_code_database.csv")
contig_US_zones <- read.csv3("data-raw/phzm_us_zipcode_2023.csv")
alaska_zones <- read.csv3("data-raw/phzm_ak_zipcode_2023.csv")
hawaii_zones <- read.csv3("data-raw/phzm_hi_zipcode_2023.csv")

zipcode_info <- zipcode_info |>
  mutate(city = paste(primary_city, state, sep = ", ")) |>
  select(zip, city) |>
  rename(zipcode = "zip")

zone_by_zipcode <- contig_US_zones |>
  bind_rows(hawaii_zones) |>
  bind_rows(alaska_zones) |>
  mutate(zone = gsub("([0-9]+).*$", "\\1", zone)) |>
  select(zipcode, zone) |>
  left_join(zipcode_info, by = "zipcode")

usethis::use_data(zone_by_zipcode, overwrite = TRUE)
