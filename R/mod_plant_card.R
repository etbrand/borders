plant_card_UI <- function(id, details_record, in_border = FALSE,
                          simplified = FALSE) {

  ns <- shiny::NS(id)

  #TODO: Check if this looks ok in guide
  # error_img <- "www/noun_icons/noun-no-image-3581362.svg"
  if (not_null(details_record$default_image$regular_url)) {
    img_path <- details_record$default_image$regular_url
  } else {
    img_path <- "www/noun_icons/noun-no-image-3581362.svg"
  }

  tags$div(
    class = if (!simplified) "flex-column col-lg-3 col-md-4 col-sm-6",
    bslib::card(
      id = ns("card"),
      class = if (simplified) "simple-plant-card" else "plant-card mx-2 my-2",
      if (!simplified) {
        plant_card_header(
          plant_type = details_record$type,
          common_name = details_record$common_name
        )
      },
      bslib::card_image(
        file = img_path,
        alt = details_record$common_name,
        onerror = img_on_error
      ),
      bslib::card_body(
        class = "plant-card-body",
        gap = 0,
        stringr::str_to_title(details_record$common_name),
        tags$em(details_record$scientific_name),
        if (!simplified) {
          tagList(
            hardiness_info(details_record$hardiness),
            sunlight_info(details_record$sunlight),
            watering_info(details_record$watering),
            tags$div(
              class = "d-flex justify-content-end",
              tags$button(
                id = ns("card_update"),
                class = if (in_border) "btn btn-discreet-md btn-remove-plant"
                else "btn btn-discreet-md btn-add-plant",
                tags$span(
                  id = ns("card_update_text"),
                  class =
                    if (in_border) "remove-plant-text" else "add-plant-text"
                )
              )
            )
          )
        }
      )
    )
  )
}

plant_card_server <- function(id, details_record, your_border, card_ids,
                              search_ids) {
  moduleServer(
    id,
    function(input, output, session) {
      shinyjs::onclick(
        id = "card_update",
        expr = update_border(
          id = id,
          details_record = details_record,
          your_border = your_border,
          card_ids = card_ids,
          search_ids = search_ids
        )
      )
    }
  )
}
