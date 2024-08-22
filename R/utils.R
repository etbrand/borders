multi_fn <- function(vect) paste0("^IN(", paste0(vect, collapse = ","), ")")

shuffle_list <- function(list) list[sample(1:length(list))]

# API responses are better behaved when hardiness is a range
hardiness_range <- function(zone) {
  zone <- as.numeric(zone)
  paste0(max(zone - 3, 2), "-", min(zone + 1, 13))
}

# Plant types are messy; this cleans them up
simplify_plant_type <- function(plant_type) {
  type_like <- function(pattern) grepl(pattern, plant_type, ignore.case = TRUE)
  if (type_like("flower")) {
    return("Flower")
  }
  if (type_like("shrub")) {
    return("Shrub")
  }
  if (type_like("tree|evergreen")) {
    return("Tree")
  }
  if (type_like("conifer")) {
    return("Conifer")
  }
  if (type_like("vine|climbing|creeping")) {
    return("Vine")
  }
  if (type_like("weed|invasive")) {
    return("Weed")
  }
  if (type_like("produce")) {
    return("Produce")
  }
  if (type_like("bulb")) {
    return("Bulb")
  }
  if (type_like("herb")) {
    return("Herb")
  }
  if (type_like("cactus")) {
    return("Cactus")
  }
  if (type_like("grain")) {
    return("Grain")
  }
  plant_type
}

hardiness_info <- function(hardiness) {
  if (is.null(hardiness$min) && is.null(hardiness$max)) {
    return(NULL)
  }
  if (hardiness$min == hardiness$max) {
    HTML(glue::glue("<span><b>Hardiness zone:</b> {hardiness$min}</span>"))
  } else {
    HTML(glue::glue("<span><b>Hardiness zones:</b> {hardiness$min} to {hardiness$max}</span>"))
  }
}

# Clean up messy sunlight entries
parse_sunlight <- function(sunlight) {
  sunlight <- sunlight |>
    tolower() |>
    strsplit("/") |>
    purrr::list_flatten() |>
    unlist() |>
    trimws()
  sunlight <- gsub("filtered shade", "part shade", sunlight)
  sunlight <- gsub("deep shade", "full shade", sunlight)
  valid_choices <- c("full shade", "part shade", "part sun", "full sun")
  print(sunlight[!(sunlight %in% valid_choices)])
  # Fill if any choices are missing in the middle
  idx <- which(valid_choices %in% sunlight)
  valid_choices[idx[1]:tail(idx, 1)]
}

get_sunlight_icon <- function(sunlight_entry) {
  switch(
    sunlight_entry,
    "full shade" = "noun-eclipse-5842793",
    "part shade" = "noun-eclipse-5819735",
    "part sun"   = "noun-eclipse-5819699",
    "full sun"   = "noun-eclipse-5842843"
  )
}

sunlight_info <- function(sunlight) {
  if (is.null(sunlight)) {
    return(NULL)
  }
  icon_files <- purrr::map(parse_sunlight(sunlight), get_sunlight_icon)
  sun_icons <- paste0("<img src=www/noun_icons/", icon_files,
                     ".svg height='13' width='13'>", collapse = "")
  HTML(paste0("<span><strong>Sunlight needs: </strong>", sun_icons, "</span>"))
}

watering_info <- function(water_needs) {
  if (is.null(water_needs)) {
    return(NULL)
  }
  water_needs <- if (water_needs == "Minimal") "Minimum" else water_needs
  n_drops <- switch(
    water_needs,
    "None"     = 1,
    "Minimum"  = 2,
    "Average"  = 3,
    "Frequent" = 4,
    0
  )
  print(water_needs)
  water_icon <-
    "<img src=www/noun_icons/noun-water-344559.svg height='13' width='13'>"
  HTML(paste0("<span style='padding-bottom: 10px;'><strong>Water needs: </strong>",
              paste(rep(water_icon, n_drops), collapse = ""), "</span>"))
}
