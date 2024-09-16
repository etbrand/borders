#' Make guide content
#'
#' Make content for the plant care guide tab
#' @param ns Namespacing function
#' @param care_guide The care guide data for the plant
#' @param details_record The details record for the plant
#' @param side The side of the page the text should appear on relative to the
#' image
#' @return The html for an entry in the plant guide
make_guide_content <- function(ns, url_prefix, care_guide, details_record,
                               side) {

  id <- paste(c(url_prefix, paste0("plant", care_guide$species_id)),
              collapse = "_")
  card_offset <- if (side == "right") "offset-md-1"
  card_order <- if (side == "left") "order-md-2 order-xs-1"
  plant_card <- tags$div(
    class = paste("col-md-3 align-self-start sticky-top", card_offset,
                  card_order),
    plant_card_UI(
      id = paste0("plant_", care_guide$species_id),
      details_record = details_record,
      simplified = TRUE
    )
  )
  left_class <- if (side == "left") "offset-md-1 order-md-1 order-xs-2"
  text_content <- tags$div(
    class = paste("col-md-6", left_class),
    tags$h2(
      id = id,
      class = "plant-guide-header",
      stringr::str_to_title(care_guide$common_name)
    ),
    tags$h3(id = paste0(id, "_description"), "Description"),
    tags$p(details_record$description),
    tags$h3(
      id = paste0(id, "_", care_guide$section[[1]]$type),
      stringr::str_to_title(care_guide$section[[1]]$type)
    ),
    tags$p(care_guide$section[[1]]$description),
    tags$h3(
      id = paste0(id, "_", care_guide$section[[2]]$type),
      stringr::str_to_title(care_guide$section[[2]]$type)
    ),
    tags$p(care_guide$section[[2]]$description),
    tags$h3(
      id = paste0(id, "_", care_guide$section[[3]]$type),
      stringr::str_to_title(care_guide$section[[3]]$type)
    ),
    tags$p(care_guide$section[[3]]$description)
  )

  tags$div(class = "row mt-2", plant_card, text_content)

}

#' Show no info modal
#'
#' Modal to show when no info is available
show_no_info_modal <- function() {
  shiny::showModal(
    shiny::modalDialog(
      size = "m",
      title = tags$span(icon("circle-exclamation"), "No info available"),
      bslib::card_body(
        paste(
          "No info is available for your border. Note that care guides are only",
          "available for some plants."
        )
      ),
      easyClose = TRUE
    )
  )
}

#' Show empty border modal
#'
#' Modal to show when border is empty
show_empty_border_modal <- function() {
  shiny::showModal(
    shiny::modalDialog(
      size = "m",
      title = tags$span(icon("circle-exclamation"), "Border is empty"),
      bslib::card_body(
        "Your border is empty. Choose plants and come back."
      ),
      easyClose = TRUE
    )
  )
}
