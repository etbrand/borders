options(r2d3.shadow = FALSE)

#' @import shiny
server <- function(input, output, session) {

  # Allow user to navigate directly to their zone page and skip the modal
  zone_ID <- gsub("#", "", isolate(session$clientData$url_hash))
  #TODO: Remove later
  zone_ID <- "zone9"
  if (zone_ID %in% paste0("zone", 3:11)) {
    your_border <- choose_plants_server("choose_plants", zone_ID)
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
              "<span><strong>Borders</strong> is a tool to help you design a ",
              "garden border for your home. To get started, enter your ZIP Code",
              "to find your USDA hardiness zone and click <strong>Go</strong>.",
              "If you live outside the US and know your hardiness zone, you",
              "can enter it directly instead.</span><span>Plant listings will",
              "also include species suitable for similar hardiness zones to",
              "yours.</span>"
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
          shinyWidgets::radioGroupButtons(
            inputId = "zip_or_zone",
            status = "light",
            justified = TRUE,
            choices = list("Enter zipcode" = "zip",
                           "Select hardiness zone" = "zone"),
            selected = "zip",
            checkIcon = list(yes = icon("check"))
          )
        ),
        footer = actionButton("go", "Go")
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
      if (input$zip_or_zone == "zip") {
        shinyjs::show("zip")
        shinyjs::show("location_text")
        shinyjs::hide("zone")
      } else {
        shinyjs::show("zone")
        shinyjs::hide("zip")
        shinyjs::hide("location_text")
      }
    })

    observe({
      req(input$zip_or_zone)
      if (input$zip_or_zone == "zip" && input$zip %||% "" == "") {
        shinyjs::disable("go")
      } else {
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
    zip_iv$enable()

    observeEvent(input$go, {
      if (input$zip_or_zone == "zip") {
        zone <- zone_by_zipcode$zone[zone_by_zipcode$zip == input$zip]
      } else {
        zone <- input$zone
      }
      zone_ID <- paste0("zone", zone)
      shinyjs::runjs(paste0("window.location.hash = '", zone_ID, "';"))
      your_border <- choose_plants_server("choose_plants", paste0("zone", zone))
      at_a_glance_server("at_a_glance", your_border)
      care_guide_server("care_guide", your_border)
      shiny::removeModal()
    })

  }

  output$n_plants <- renderText({ length(your_border()) })

}

