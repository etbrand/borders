make_plant_stub <- function(ns, plant_id, details_record) {
  tags$div(
    id = paste0(ns(plant_id), "_stub"),
    class = "border-stub d-flex",
    tags$div(
      class = "align-self-center me-2",
      tags$img(src = details_record$default_image$thumbnail,
               height = 35, width = 35)
    ),
    tags$div(
      class = "me-1",
      tags$div(stringr::str_to_title(details_record$common_name)),
      tags$div(tags$em(details_record$scientific_name))
    ),
    tags$button(
      class = paste("add-from-search btn btn-sm me-1 btn-outline-primary",
                    "align-self-center ms-auto"),
      id = ns(paste0("add_", plant_id, "_from_search")),
      shiny::icon("plus")
    )
  )
}

choose_plants_UI <- function(id) {
  ns <- NS(id)

  make_input <- function(id, ns) {
    shinyWidgets::pickerInput(
      inputId = ns(id),
      label = paste0(stringr::str_to_sentence(id), ":"),
      choices = input_choices[[id]],
      multiple = id != "hardiness"
    )
  }

  # bslib::page_sidebar(
  #   title = tags$div(
  #     style = "display: flex;",
  #     tags$div(
  #       style = "align-self: center;",
  #       tags$img(src = 'www/logo.svg', height = 30)
  #     ),
  #     tags$h1(class = "bslib-page-title navbar-brand", "Borders")
  #   ),
  #   theme = bslib::bs_theme(bootswatch = "journal", primary = "#494b4d"),
  #  sidebar = bslib::sidebar(
  #   actionButton(
  #     inputId = ns("get_ideas"),
  #     label = "Refresh plant list"
  #   ),
  #   bslib::accordion(
  #     bslib::accordion_panel(
  #       "Basic options",
  #       #icon = bsicons::bs_icon("menu-app"),
  #       purrr::map(names(input_choices), ~ make_input(.x, ns))
  #     ),
  #     bslib::accordion_panel(
  #       "Advanced options"
  #       #icon = bsicons::bs_icon("sliders"),
  #     )
  #   )
  # ),
  bslib::nav_panel(
    title = "Choose plants",
    tags$div(
      class = "container",
      #bslib::layout_column_wrap(
      #width = NULL,
      #style = bslib::css(grid_template_columns = "3fr 1fr"),
      #bslib::card(
      tags$div(
        class = "d-inline-flex gap-2 mt-2 mb-4 align-self-start sticky-top",
        shinyWidgets::dropdownButton(
          label = "Find a plant",
          status = "primary",
          inline = TRUE,
          icon = icon("magnifying-glass"),
          circle = FALSE,
          tags$div(
            class = "d-flex justify-content-end",
            actionButton(
              inputId = ns("switch_search"),
              label = icon("chevron-right"),
              class = "btn-discreet search-sn"
            )
          ),
          selectizeInput(
            inputId = ns("search_common_name"),
            label = "By common name:",
            choices = NULL
          ),
          shinyjs::hidden(
            selectizeInput(
              inputId = ns("search_scientific_name"),
              label = "By scientific name:",
              choices = NULL
            )
          ),
          tags$div(
            class = "d-flex justify-content-end mb-4",
            actionButton(
              class = "btn-primary btn-spinner btn-block w-100 h-100",
              inputId = ns("search"),
              label = tags$span(
                id = ns("search_text"),
                class = "search-ready"
              )
            )
          ),
          uiOutput(ns("search_results"))
        ),
        tags$button(
          class = "btn btn-primary",
          type = "button",
          `data-bs-toggle` = "offcanvas",
          `data-bs-target` = "#filters",
          `aria-controls` = "offcanvasExample",
          tags$span(icon("filter"), "Filter plant list")
        ),
        tags$div(
          id = "filters",
          class = "offcanvas offcanvas-start",
          tabIndex = "-1",
          `aria-labelledby` = "offcanvasExampleLabel",
          tags$div(
            class = "offcanvas-header",
            tags$h5(
              id = "offcanvasExampleLabel",
              class = "offcanvas-title",
              tags$span("Filters", class = "ms-2")
            ),
            tags$button(
              type = "button",
              class = "btn-close text-reset",
              `data-bs-dismiss` = "offcanvas",
              `aria-label` = "Close"
            )
          ),
          tags$div(
            class = "offcanvas-body",
            tags$div(
              "Some text as placeholder. In real life you can have the elements you have chosen. Like, text, images, lists, etc."
            )
          )
        )
      ),
      uiOutput(ns("image_cards"))
    )#,
    #bslib::card(
    #  bslib::card_header("Your border"),
    # tags$div(
    #   tags$div(
    #     id = ns("empty_border"),
    #     "Nothing in your border. Choose plants from the plant list."
    #   ),
    #   tags$div(id = ns("border_plants"))
    # )
  )
}

#' @import rlang
choose_plants_server <- function(id, zone, city = NULL) {
  moduleServer(
    id,
    function(input, output, session) {

      ns <- NS(id)

      updateSelectizeInput(
        session = session,
        inputId = "search_common_name",
        choices = unique(search_list[[zone]]$by_common_name$common_name),
        server = TRUE
      )

      updateSelectizeInput(
        session = session,
        inputId = "search_scientific_name",
        choices = search_list[[zone]]$by_scientific_name$scientific_name,
        server = TRUE
      )

      default_species_list <- default_zone_data[[zone]]$species_list$data
      default_species_details <- default_zone_data[[zone]]$species_details

      #TODO: Rename to plant_info and remove other ref?
      species_info <- reactiveValues(
        #api_ids = gsub("plant_", "", names(default_species_list)),
        species_list = default_species_list,
        species_details = default_species_details
      )

      search_type <- reactiveVal("by_common_name")
      observeEvent(input$switch_search, {
        if (input$switch_search %% 2 == 1) {
          shinyjs::removeClass("switch_search", "search-sn")
          shinyjs::addClass("switch_search", "search-cn")
          shinyjs::hide("search_common_name")
          shinyjs::show("search_scientific_name")
          search_type("by_scientific_name")
        } else {
          shinyjs::removeClass("switch_search", "search-cn")
          shinyjs::addClass("switch_search", "search-sn")
          shinyjs::hide("search_scientific_name")
          shinyjs::show("search_common_name")
          search_type("by_common_name")
        }
      })

      search_ids <- reactiveVal()
      observeEvent(input$search, {
        plant_name <- input[[sub("by_", "search_", search_type())]]
        col_name <- sub("by_", "", search_type())
        #species_info$api_ids <-
        new_search_ids <- search_list[[zone]][[search_type()]] |>
          dplyr::filter(!!rlang::sym(col_name) == plant_name) |>
          dplyr::pull(id)
        search_ids(new_search_ids)
      })

      output$search_results <- renderUI({
        req(search_ids())
        shinyjs::removeClass(id = "search_text", "search-ready")
        shinyjs::addClass(id = "search_text", "search-busy")
        shinyjs::disable("search")
        new_details <- NULL
        for (id in search_ids()) {
          resp <- tryCatch(query_details(id), error = identity)
          if (inherits(resp, "error") || resp$status_code != 200) {
            new_details[[paste0("plant_", id)]] <- "api_error"
          } else {
            new_details[[paste0("plant_", id)]] <- query_details(id) |>
              httr2::resp_body_string() |>
              jsonlite::parse_json()
          }
        }
        shinyjs::enable("search")
        shinyjs::removeClass(id = "search_text", "search-busy")
        shinyjs::addClass(id = "search_text", "search-ready")
        purrr::imap(new_details, ~ make_plant_stub(ns, .y, .x))
      })

      # observeEvent(species_info$api_ids, {
      #   new_details <- NULL
      #   for (id in species_info$api_ids) {
      #     resp <- tryCatch(query_details(id), error = identity)
      #     if (inherits(resp, "error") || resp$status_code != 200) {
      #       new_details[[paste0("plant_", id)]] <- "api_error"
      #     } else {
      #       new_details[[paste0("plant_", id)]] <- query_details(id) |>
      #         httr2::resp_body_string() |>
      #         jsonlite::parse_json()
      #     }
      #   }
      #   species_info$species_details <- new_details
      #}, ignoreInit = TRUE)

      species_response <- eventReactive(input$get_ideas, {
        url_args <- make_url_args(input, names(input_choices))
        initial_resp <- do.call(query_species_list, url_args)

        # TODO: Add error handling

        add_species_records(resp_to_list(initial_resp))
      })

      # TODO: Is this necessary?
      species_preview <- reactive({
        species_response()$data[1:12]
      })

      displayed_details <- reactiveVal(default_species_details)

      #details_preview <- reactive({
      #  resps <- purrr::map(species_preview(), ~ query_details(.x$id))

      #TODO: Add error handling

      #  purrr::map(resps, httr2::resp_body_json)
      #})
      #style = "grid-template-columns: repeat(auto-fit, minmax(min(100%, max(175px, 24%)), 1fr));"
      output$image_cards <- renderUI({
        image_cards <- species_info$species_details |>
          purrr::imap(~ plant_card_UI(ns(.y), .x))
        tags$div(
          class = "grid",
          style = "grid-template-columns: repeat(auto-fit, minmax(min(100%, max(175px, 20%)), 1fr));",
          #bslib::layout_column_wrap(
          # width = NULL,
          #  style = bslib::css(
          #    grid_template_columns =
          #      "repeat(auto-fit, minmax(min(100%, max(175px, 24%)), 1fr));"
          #  ),
          !!!unname(image_cards)
        )
      })

      your_border <- reactiveVal()

      default_species_details |>
        purrr::iwalk(~ plant_card_server(.y, .x, your_border))


      border_IDs <- reactiveVal()

      remove_plant_stub <- function(border_IDs, plant) {
        border_IDs(setdiff(border_IDs(), plant))
        shiny::removeUI(selector =  paste0("#", plant, "_stub"))
        if (rlang::is_empty(border_IDs())) {
          shinyjs::show("empty_border", asis = TRUE)
        }
      }

      observeEvent(your_border(), {
        added_plant <- setdiff(names(your_border()), border_IDs())
        removed_plant <- setdiff(border_IDs(), names(your_border()))
        if (not_empty(added_plant)) {
          if (rlang::is_empty(border_IDs())) {
            shinyjs::hide("empty_border", asis = TRUE)
          }
          plant_info <- your_border()[[added_plant]]
          shiny::insertUI(
            selector = "#border_plants",
            ui = tags$div(
              id = paste0(added_plant, "_stub"),
              class =
                "border-stub d-flex justify-content-between align-items-center",
              tags$span(
                tags$img(src = plant_info$default_image$thumbnail,
                         height = 35, width = 35),
                stringr::str_to_title(plant_info$common_name
                )
              ),
              tags$div(
                id = paste0("remove_", added_plant),
                class = "border-stub-x",
                shiny::icon("x")
              )
            )
          )
          shinyjs::onclick(
            paste0("remove_", added_plant),
            {
              remove_plant_stub(border_IDs, added_plant)
              your_border(pop_ID(your_border(), added_plant))
              shinyjs::toggle(paste0(added_plant, "-checked"))
              shinyjs::runjs(
                paste0("toggleColor('", ns(added_plant), "-card');")
              )
            },
            asis = TRUE
          )
          border_IDs(c(border_IDs(), added_plant))
        }
        if (not_empty(removed_plant)) {
          remove_plant_stub(border_IDs, removed_plant)
        }
      }, ignoreNULL = FALSE)

      # Return your_border with colors added
      reactive({
        if (not_empty(your_border())) {
          your_border() |>
            purrr::map(~ get_flower_color(.x$flower_color)) |>
            modifyList(your_border(), val = _)
        }
      })

    }
  )
}
