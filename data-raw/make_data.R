label <- function(choices) {
  choices |>
    stringr::str_to_sentence() |>
    gsub("_", " ", x = _) |>
    gsub("-", " to ", x = _) |>
    setNames(choices, nm = _)
}

input_choices <- list(
  # Current there's an issue including zone 1 in requests
  #hardiness = 2:12,
  sunlight = label(c("full_shade", "part_shade", "sun-part_shade", "full_sun")),
  cycle = label(c("perennial", "annual", "biennial", "biannual")),
  watering = label(c("frequent", "average", "minimum", "none")),
  edible = c(Edible = 1, Nonedible = 0),
  poisonous = c(Poisonous = 1, "Non-poisonous" = 0)
)

usethis::use_data(input_choices, overwrite = TRUE)
