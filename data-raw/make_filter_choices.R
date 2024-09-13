label <- function(choices) {
  choices |>
    gsub("part", "partial", x = _) |>
    stringr::str_to_sentence() |>
    gsub("_", " ", x = _) |>
    gsub("-", " to ", x = _) |>
    sub("No color", "Flowers insignificant", x = _) |>
    setNames(choices, nm = _)
}

filter_selects <- list(
  sunlight = label(c("full_shade", "part_shade", "part_sun", "full_sun")),
  cycle = label(c("perennial", "annual", "biennial", "biannual")),
  watering = label(c("frequent", "average", "minimum", "none")),
  flower_color = label(c("blue", "brown", "green", "orange", "pink", "purple",
                         "red", "white", "yellow", "no_color"))
)

filter_checkboxes <- list(
  edible_only = "Edible plants only",
  nonpoisonous_only = "Nonpoisonous plants only",
  include_trees = "Include trees"
)

usethis::use_data(filter_selects, filter_checkboxes, overwrite = TRUE)
