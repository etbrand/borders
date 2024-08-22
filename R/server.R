server <- function(input, output) {

  species_info <- reactiveValues(
    species_list = default_zone_data$zone9$species_list,
    species_details = default_zone_data$zone9$species_details
  )

  species_response <- eventReactive(input$get_ideas, {
    url_args <- make_url_args(input, names(input_choices))
    initial_resp <- do.call(query_species_list, url_args)

    species_list <- resp_to_list(initial_resp) |>
      add_species_records(url_args)

    saveRDS(species_list, "species_list.RDS")
    species_list
  })

  species_preview <- reactive({
    species_response()$data[1:12]
  })

  details_preview <- reactive({
    resps <- purrr::map(species_preview(), ~ query_details(.x$id))
    details <- purrr::map(resps, httr2::resp_body_json)
    saveRDS(details, "details.RDS")
    details
  })

  #species_list <- readRDS("species_list.RDS")
  output$image_cards <- renderUI({
    # image_cards <- species_info$species_list$data |>
    #   purrr::map2(species_info$species_details, make_plant_card)
    # bslib::layout_column_wrap(!!!image_cards)

    image_cards <- species_info$species_list$data |>
      purrr::map2(species_info$species_details,
                  ~ plant_card_UI(paste0("plant_card_", .x$id), .x, .y))
    bslib::layout_column_wrap(!!!image_cards)



    # bslib::layout_column_wrap(
    #   fill = FALSE,
    #   #width = 1,
    #   #width = 1/3,
    #   #!!!purrr::map(species_response()$data, make_plant_card)
    #   !!!purrr::map(species_list$data, make_plant_card)
    # )
  })

  your_border <- reactiveVal()

  default_zone_data$zone9$species_list$data |>
    purrr::walk(~ plant_card_server(paste0("plant_card_", .x$id), your_border))

  # make_add_to_border_observer <- function(plant_ID) {
  #   ns <- NS(paste0("plant_card_", plant_ID))
  #   observeEvent(input[[ns("add_to_border")]], {
  #     your_border(c(your_border(), plant_ID))
  #   })
  # }

  output$your_border <- renderUI({
    if (is.null(your_border())) {
      "Nothing in your border. Choose plants from the plant list."
    } else {
      paste(your_border(), collapse = ",")
    }
  })

}
