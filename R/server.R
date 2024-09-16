#' @import shiny httr2
server <- function(input, output, session) {

  # Check url to see if it contains a zone, an active tab, or plants

  url <- strsplit(isolate(session$clientData$url_hash), "_")[[1]]

  if (length(url) > 0) {
    zone <- gsub("#", "", url[1])
    zone <- if (zone %in% paste0("zone", 3:11)) zone else NULL
    plants <- strsplit(url[3], "-")[[1]]
    plants <- plants[plants %in% as.character(1:10000)]
    bookmark <- list(zone = zone, plants = plants)
  } else {
    bookmark <- NULL
  }

  your_border <- reactiveVal()

  if (not_null(bookmark$zone)) {
    choose_plants_server("choose_plants", zone, your_border, bookmark$plants)
    border_info_server("border_info", your_border)
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
            style =
              "align-self: center; padding-bottom: 40px; padding-top: 20px;",
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
        footer = actionButton("go", "Find plants", style = "width: 25%;")
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

      url_prefix <- paste0("zone", zone, "_choose")

      shinyjs::runjs(paste0("window.location.hash = '", url_prefix, "';"))
      choose_plants_server("choose_plants", paste0("zone", zone), your_border,
                           saved_plants = NULL)
      border_info_server("border_info", your_border)
      shiny::removeModal()
    })

  }

  observeEvent(input$current_tab, {
    current_url <- session$clientData$url_hash
    split_url <- strsplit(current_url, "_")[[1]]
    split_url[2] <- input$current_tab
    if (input$current_tab == "info") {
      new_url <- paste(split_url, collapse = "_")
    } else {
      new_url <- paste(split_url[1:3], collapse = "_")
    }
    shinyjs::runjs(paste0("window.location.hash = '", new_url, "';"))
  }, ignoreInit = TRUE)

  output$n_plants <- renderText({ length(your_border()) })

}

