label <- function(choices) {
  choices |>
    gsub("part", "partial", x = _) |>
    stringr::str_to_sentence() |>
    gsub("_", " ", x = _) |>
    gsub("-", " to ", x = _) |>
    sub("No color", "Flowers insignificant", x = _) |>
    setNames(choices, nm = _)
}

filter_choices <- list(
  sunlight = label(c("full_shade", "part_shade", "part_sun", "full_sun")),
  cycle = label(c("perennial", "annual", "biennial", "biannual")),
  watering = label(c("frequent", "average", "minimum", "none")),
  edible = c(Edible = 1, Nonedible = 0),
  poisonous = c(Poisonous = 1, "Non-poisonous" = 0),
  flower_color = label(c("blue", "brown", "green", "orange", "pink", "purple",
                         "red", "white", "yellow", "no_color"))
)

usethis::use_data(filter_choices, overwrite = TRUE)
