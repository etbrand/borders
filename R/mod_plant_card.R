plant_card_header <- function(plant_type, common_name) {

  header_icon <- switch(
    simplify_plant_type(plant_type),
    "Bulb" = "noun-onion-4623459",
    "Cactus" = "noun-cactus-2590999",
    "Carnivorous" = "noun-venus-flytrap-27589",
    "Conifer" = "noun-pine-5973913",
    "Fern" = "noun-fern-4273283",
    "Flower" = "noun-flower-7088847",
    "Fruit" = "noun-fruit-6461708",
    "Grain" = "noun-wheat-124955",
    "Grass" = "noun-grass-6990187",
    "Herb" = "noun-herb-6363034",
    "Palm" = "noun-palm-5949874",
    "Produce" = "noun-vegetable-7110902",
    "Shrub" = "noun-shrub-4416049",
    "Tree" = "noun-tree-7108880",
    "Vine" = "noun-vine-2247901",
    "noun-plant-5018755"
  )

  bslib::card_header(
    class = "plant-card-header",
    tags$span(
      tags$img(src = glue::glue("www/noun_icons/{header_icon}.svg"),
               height = 20, width = 20),
      stringr::str_to_title(common_name)
    )
  )

}

plant_card_UI <- function(id, details_record, simplified = FALSE) {
  ns <- shiny::NS(id)
  #TODO: Check if this looks ok in guide
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
        file = details_record$default_image$regular_url,
        alt = details_record$common_name
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
              shinyjs::hidden(
                tags$div(
                  id = ns("checked"),
                  icon("check")
                )
              )
            )
          )
        }
      ),
      onclick = if (!simplified) paste0("toggleColor('", ns("card"), "');")
    )
  )
}

plant_card_server <- function(id, species_details, your_border) {
  moduleServer(
    id,
    function(input, output, session) {

      add_or_remove <- function(id, your_border) {
        if (id %in% names(your_border())) {
          your_border(pop_id(your_border(), id))
        } else {
          your_border(c(your_border(), rlang::list2(!!id := species_details)))
        }
        shinyjs::toggle("checked")
      }

      shinyjs::onclick("card", add_or_remove(id, your_border))

    }
  )
}
