options(r2d3.shadow = FALSE)

#' @import shiny
server <- function(input, output, session) {

  # Allow user to navigate directly to their zone page and skip the modal
  zone_ID <- gsub("#", "", isolate(session$clientData$url_hash))
  #TODO: Remove later
  #zone_ID <- "zone9"
  your_border <- reactiveVal()

  if (zone_ID %in% paste0("zone", 3:11)) {
    choose_plants_server("choose_plants", zone_ID, your_border)
    at_a_glance_server("at_a_glance", your_border)
    care_guide_server("care_guide", your_border)
  } else {
    shiny::showModal(
      shiny::modalDialog(
        size = "l",
        title = tags$span(
          tags$img(src = "www/logo-dark-gray.svg", height = "30"),
          "Welcome"),
        bslib::card_body(
          shiny::HTML(
            paste(
              "<span>Borders is a tool to help you choose plants for a garden ",
              "border. To get started, enter your ZIP Code to find your USDA",
              "hardiness zone. If you live outside the US and know your",
              "hardiness zone, you can enter it directly instead.</span>",
              "<span>Plant listings will also include species suitable for",
              "similar hardiness zones to yours.</span>"
            )
          ),
          tags$div(
            style = "align-self: center; padding-bottom: 40px; padding-top: 20px;",
            shiny::selectizeInput(
              inputId = "zip",
              label = "US ZIP Code:",
              choices = NULL
            ),
            tags$div(
              id = "location_text",
              uiOutput("location")
            ),
            shinyjs::hidden(
              shiny::selectInput(
                inputId = "zone",
                label = "Hardiness zone:",
                choices = 3:11
              )
            )
          ),
          tags$div(
            class = "d-flex justify-content-start",
            actionButton(
              inputId = "zip_or_zone",
              label = icon("chevron-right"),
              class = "btn-discreet-md search-zone"
            )
          )
        ),
        footer = actionButton("go", "Let's go", style = "width: 25%;")
      )
    )

    shiny::updateSelectizeInput(
      session = session,
      inputId = "zip",
      choices = zone_by_zipcode$zipcode,
      selected = "91105",
      server = TRUE
    )

    observeEvent(input$zip_or_zone, {
      if (input$zip_or_zone %% 2 == 1) {
        shinyjs::removeClass("zip_or_zone", "search-zone")
        shinyjs::addClass("zip_or_zone", "search-zip")
        shinyjs::show("zone")
        shinyjs::hide("zip")
        shinyjs::hide("location_text")
      } else {
        shinyjs::removeClass("zip_or_zone", "search-zip")
        shinyjs::addClass("zip_or_zone", "search-zone")
        shinyjs::show("zip")
        shinyjs::show("location_text")
        shinyjs::hide("zone")
      }
    })

    observe({
      req(input$zip_or_zone)
      if (input$zip_or_zone == "zip" && input$zip %||% "" == "") {
        zip_iv$enable()
        shinyjs::disable("go")
      } else {
        zip_iv$disable()
        shinyjs::enable("go")
      }
    })

    output$location <- renderUI({
      if (input$zip != "") {
        city <- zone_by_zipcode$city[zone_by_zipcode$zip == input$zip]
        zone <- zone_by_zipcode$zone[zone_by_zipcode$zip == input$zip]
        tags$em(paste(city, "- Zone", zone))
      }
    })

    zip_iv <- shinyvalidate::InputValidator$new()
    zip_iv$add_rule("zip", shinyvalidate::sv_required())

    observeEvent(input$go, {
      req(zip_iv$is_valid())
      if (input$zip_or_zone %% 2 == 0) {
        zone <- zone_by_zipcode$zone[zone_by_zipcode$zip == input$zip]
      } else {
        zone <- input$zone
      }
      zone_ID <- paste0("zone", zone)
      shinyjs::runjs(paste0("window.location.hash = '", zone_ID, "';"))
      choose_plants_server("choose_plants", paste0("zone", zone), your_border)
      at_a_glance_server("at_a_glance", your_border)
      care_guide_server("care_guide", your_border)
      shiny::removeModal()
    })

  }

  output$n_plants <- renderText({ length(your_border()) })

}

