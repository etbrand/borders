plant_card_header <- function(plant_type, common_name) {

  header_icon <- switch(
    simplify_plant_type(plant_type),
    "Bulb" = "noun-onion-4623459",
    "Cactus" = "noun-cactus-2590999",
    "Conifer" = "noun-pine-5973913",
    "Flower" = "noun-flower-7088847",
    "Grain" = "noun-wheat-124955",
    "Herb" = "noun-herb-6363034",
    "Produce" = "noun-vegetable-7110902",
    "Shrub" = "noun-shrub-4416049",
    "Tree" = "noun-tree-7108880",
    "Vine" = "noun-vine-2247901",
    "noun-plant-5018755"
  )

  bslib::card_header(
    tags$span(
      tags$img(src = glue::glue("www/noun_icons/{header_icon}.svg"),
               height = "25", width = "25"),
      stringr::str_to_title(common_name)
    )
  )

}

plant_card_UI <- function(id, species_record, details_record) {
  ns <- NS(id)
  bslib::card(
    id = ns("card"),
    plant_card_header(details_record$type, species_record$common_name),
    bslib::card_image(
      file = species_record$default_image$regular_url,
      alt = species_record$common_name
    ),
    bslib::card_body(
      gap = 0,
      tags$em(details_record$scientific_name),
      hardiness_info(details_record$hardiness),
      sunlight_info(details_record$sunlight),
      watering_info(details_record$watering),
      # actionButton(
      #   inputId = ns("add_to_border"),
      #   label = "Add to border",
      #   class = "mt-auto"
      # )
      tags$div(
        shinyWidgets::prettyToggle(
          inputId = ns("add_to_border"),
          label_on = "In your border",
          label_off = "Add to border",
        ),
        class = "mt-auto"
      )
    )
  )
}

plant_card_server <- function(id, your_border) {
  moduleServer(
    id,
    function(input, output, session) {
      observeEvent(input$add_to_border, {
        plant_ID <- gsub("plant_card_", "", id)
        if (input$add_to_border) {
          your_border(c(your_border(), plant_ID))
        }
      }, ignoreInit = TRUE)
    }
  )
}
