`%||%` <- function(x, y) if (is.null(x)) y else x

`%not_in%` <- Negate(`%in%`)

not_null <- Negate(is.null)

not_empty <- Negate(rlang::is_empty)

multi_fn <- function(vect) paste0("^IN(", paste0(vect, collapse = ","), ")")

pop_id <- function(list, id) { list[[id]] <- NULL; list }

to_api_ids <- function(plant_id) as.numeric(gsub("plant_", "", plant_id))

img_on_error <-
  "this.onerror=null; this.src='www/noun_icons/noun-no-image-3581362.svg'"

# Because sample's return is unexpected for length 1 vectors
sample2 <- function(x) {
  if (length(x) == 1) {
    return(x)
  }
  sample(x)
}

addClassAsis <- purrr::partial(shinyjs::addClass, asis = TRUE)
removeClassAsis <- purrr::partial(shinyjs::removeClass, asis = TRUE)

set_plant_names <- function(plant_data) {
  if (length(plant_data) == 0) {
    return(plant_data)
  }
  plant_data |>
    purrr::map(~ paste0("plant_", .x$id)) |>
    purrr::set_names(plant_data, nm = _)
}

# API responses are better behaved when hardiness is a range
hardiness_range <- function(zone) {
  zone <- as.numeric(substr(zone, 5, nchar(zone)))
  paste0(max(zone - 3, 2), "-", min(zone + 1, 13))
}

hardiness_info <- function(hardiness) {
  if (rlang::is_empty(hardiness$min) && rlang::is_empty(hardiness$max)) {
    return(HTML("<span><b>Hardiness range unavailable</b></span>"))
  }
  if (hardiness$min == hardiness$max) {
    HTML(glue::glue("<span><b>Hardiness zone:</b> {hardiness$min}</span>"))
  } else {
    HTML(glue::glue("<span><b>Hardiness zones:</b> {hardiness$min} to {hardiness$max}</span>"))
  }
}

# Clean up messy sunlight entries
# parse_sunlight <- function(sunlight) {
#   sunlight <- sunlight |>
#     tolower() |>
#     strsplit("/") |>
#     purrr::list_flatten() |>
#     unlist() |>
#     trimws()
#   sunlight <- gsub("filtered shade", "partial shade", sunlight)
#   sunlight <- gsub("part shade", "partial shade", sunlight)
#   sunlight <- gsub("part sun", "partial sun", sunlight)
#   sunlight <- gsub("deep shade", "full shade", sunlight)
#
#   sunlight
# }

parse_sunlight <- function(sunlight) {

  sunlight <- unlist(sunlight)
  valid_choices <- c("full shade", "partial shade", "partial sun", "full sun")

  if (all(tolower(sunlight) %in% valid_choices)) {
    sunlight <- tolower(sunlight)
  } else if (identical(tolower(sunlight), "shade")) {
    sunlight <- "full shade"
    # } else if (identical(tolower(sunlight), "part shade")) {
    #   sunlight <- "partial shade"
  } else if (identical(tolower(sunlight), "sun")) {
    sunlight <- "full sun"
  } else if (any(grepl("/|,", sunlight)) || length(sunlight) > 1) {
    sunlight <- sunlight |>
      tolower() |>
      strsplit("/") |>
      purrr::list_flatten() |>
      unlist() |>
      trimws()
  } else {
    #if (grepl("(?=[[:upper:]])", sunlight, perl = TRUE))) {
    sunlight <- sunlight |>
      strsplit("(?<=.)(?=[[:upper:]])", perl = TRUE) |>
      unlist() |>
      trimws() |>
      tolower()
  }

  sunlight <- gsub("filtered shade", "partial shade", sunlight)
  sunlight <- gsub("part shade", "partial shade", sunlight)
  sunlight <- gsub("part sun", "partial sun", sunlight)
  sunlight <- gsub("deep shade", "full shade", sunlight)

  if (!any(sunlight %in% valid_choices)) {
    message(paste("Sunlight entry '", sunlight, "' is not valid"))
  }

  # Reorder and fill if any choices are missing in the middle
  idx <- which(valid_choices %in% sunlight)
  valid_choices[idx[1]:tail(idx, 1)]
}

get_sunlight_icon <- function(sunlight_entry) {
  switch(
    sunlight_entry,
    "full shade"    = "noun-eclipse-5842793",
    "partial shade" = "noun-eclipse-5819735",
    "partial sun"   = "noun-eclipse-5819699",
    "full sun"      = "noun-eclipse-5842843"
  )
}

sunlight_info <- function(sunlight) {

  if (is.null(sunlight)) {
    return(NULL)
  }

  sunlight <- parse_sunlight(sunlight)

  icon_files <- purrr::map(sunlight, get_sunlight_icon)
  sun_icons <- paste0("<img src=www/noun_icons/", icon_files,
                      ".svg height='13' width='13'>", collapse = "")
  HTML(paste0("<span><strong>Sunlight needs: </strong>", sun_icons, "</span>"))
}

watering_info <- function(water_needs, as_int = FALSE) {
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

  if (n_drops == 0) {
    message(paste("Water needs entry '", water_needs, "' is not valid"))
  }

  if (as_int) {
    return(n_drops)
  }

  water_icon <- paste("<img src=www/noun_icons/noun-water-344559.svg",
                      "height='13' width='13'>")
  HTML(paste0("<span style='padding-bottom: 10px;'><strong>Water needs: </strong>",
              paste(rep(water_icon, n_drops), collapse = ""), "</span>"))
}

mix_colors <- function(cols) colorRampPalette(c(cols[[1]], cols[[2]]))(3)[2]

#TODO: Could parse "light/pale/dark" separately
get_flower_color <- function(color_record, to_hex = TRUE) {
  matches <- names(flower_colors) |>
    purrr::map_vec(~ regexpr(.x, color_record[[1]], ignore.case = TRUE))
  if (all(matches == -1)) {
    if (not_empty(color_record) && color_record[[1]] != "Non-flowering") {
      message(paste("Color not found for", color_record))
    }
    return(NULL)
  }
  idx <- which(matches == min(matches[matches != -1]))
  dominant_color <- names(flower_colors)[idx]
  if (to_hex) {
    list(hex_color = flower_colors[[dominant_color]])
  } else {
    dominant_color
  }
}

make_color_row <- function(plant_record) {
  data.frame(
    common_name = plant_record$common_name,
    scientific_name = unlist(plant_record$scientific_name),
    color = plant_record$hex_color
  )
}

arrange_by_color <- function(df) {
  df[TSP::solve_TSP(TSP::as.TSP(dist(t(col2rgb(df$color))))), ]
  #colors[order(colMeans(col2rgb(colors)[c("red", "blue"), ]))]
}

# Check that all functions used for a plant entry will not fail

no_error <- function(fct, id, ...) {
  rtrn <- tryCatch(fct(...), error = identity)
  if (inherits(rtrn, "error")) {
    message(paste0("Function '", substitute(fct), "' failed for plant ", id))
  }
  !inherits(rtrn, "error")
}

valid_plant_entry <- function(details_record) {
  functions_work <- c(
    no_error(simplify_plant_type, details_record$id, details_record$type),
    no_error(hardiness_info, details_record$id, details_record$hardiness),
    no_error(sunlight_info, details_record$id, details_record$sunlight),
    no_error(watering_info, details_record$id, details_record$watering)
  )
  all(functions_work)
}

bs_modal <- function(id, type, title, body_content) {
  title_icon <- switch(type,
                       error = "triangle-exclamation",
                       warning = "circle-exclamation")
  tags$div(
    id = id,
    class = "modal fade",
    `data-bs-backdrop` = "static",
    `data-bs-keyboard` = "false",
    tabindex = "-1",
    `aria-labelledby` = paste0(id, "_label"),
    `aria-hidden` = "true",
    tags$div(
      class = "modal-dialog",
      tags$div(
        class = "modal-content",
        tags$div(
          class = "modal-header",
          h5(
            id = paste0(id, "_label"),
            class = "modal-title",
            tags$span(icon(title_icon), title)
          ),
          tags$button(
            type = "button",
            class = "btn-close",
            `data-bs-dismiss` = "modal",
            `aria-label` = "Close"
          )
        ),
        tags$div(
          class = "modal-body",
          body_content
        ),
        tags$div(
          class = "modal-footer",
          tags$button(
            type = "button",
            class = "btn btn-hbs",
            `data-bs-dismiss` = "modal",
            "Got it"
          )
        )
      )
    )
  )
}

show_api_error_modal <- function() {
  shiny::showModal(
    shiny::modalDialog(
      size = "m",
      title = tags$span(icon("triangle-exclamation"), "API error"),
      bslib::card_body(
        paste(
          "There was an error connecting to the Perenual API. This could be",
          "because the API is down or because it received too many requests.",
          "Please try again later."
        )
      ),
      easyClose = TRUE
    )
  )
}

show_no_results_modal <- function() {
  shiny::showModal(
    shiny::modalDialog(
      size = "m",
      title = tags$span(icon("circle-exclamation"), "No results"),
      bslib::card_body(
        "Your filters did not return any results. Please expand your search."
      ),
      easyClose = TRUE
    )
  )
}

filters_applied <- function() {
  shinyjs::removeClass(id = "apply_filters_text", "apply-filters-busy")
  shinyjs::removeClass(id = "apply_filters", "btn-spinner")
  shinyjs::addClass(id = "apply_filters_text", "filters-applied")
}

enable_filters <- function() {
  shinyjs::enable("apply_filters")
  shinyjs::removeClass(id = "apply_filters_text", "filters-applied")
  shinyjs::removeClass(id = "apply_filters_text", "no-filters")
  shinyjs::addClass(id = "apply_filters_text", "apply-filters-ready")
  shinyjs::addClass(id = "apply_filters", "btn-spinner")
}

no_filters <- function() {
  shinyjs::removeClass(id = "apply_filters", "btn-spinner")
  shinyjs::removeClass(id = "apply_filters_text", "apply-filters-ready")
  shinyjs::removeClass(id = "apply_filters_text", "filters-applied")
  shinyjs::addClass(id = "apply_filters_text", "no-filters")
  shinyjs::disable("apply_filters")
}

update_plant_list <- function(ns, details, your_border, card_ids, search_ids) {
  if (length(details) > 0) {
    insert_plant_cards(ns, details)
    details |>
      purrr::iwalk(
        ~ plant_card_server(
          id = .y,
          details_record = .x,
          your_border = your_border,
          card_ids = card_ids,
          search_ids = search_ids
        )
      )
  }
}
