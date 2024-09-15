choose_plants_UI <- function(id) {
  ns <- NS(id)

  bslib::nav_panel(
    title = "Choose plants",
    tags$div(
      class = "container",
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
            id = ns("search_div"),
            class = "d-flex mb-4",
            actionButton(
              class = "btn-primary btn-spinner btn-block w-100 h-100",
              inputId = ns("search"),
              label = tags$span(
                id = ns("search_text"),
                class = "search-ready"
              )
            )
          )#,
          #uiOutput(ns("search_results"))
        ),
        tags$button(
          class = "btn btn-primary",
          type = "button",
          `data-bs-toggle` = "offcanvas",
          `data-bs-target` = paste0("#", ns("filters")),
          `aria-controls` = "offcanvasExample",
          tags$span(icon("filter"), "Filter plant list")
        ),
        tags$div(
          id = ns("filters"),
          class = "offcanvas offcanvas-start",
          tabIndex = "-1",
          `aria-labelledby` = ns("filters_header"),
          tags$div(
            class = "offcanvas-header",
            tags$h5(
              id = ns("filters_header"),
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
            class = "offcanvas-body d-flex justify-content-center",
            tags$div(
              purrr::map(names(filter_selects), ~ make_filters(.x, ns)),
              purrr::imap(
                filter_checkboxes,
                ~ tags$div(class = "mt-3", checkboxInput(ns(.y), .x))
              ),
              tags$div(
                class = "d-flex mt-4",
                shinyjs::disabled(
                  actionButton(
                    class = "btn-primary btn-block w-100 h-100",
                    inputId = ns("apply_filters"),
                    label = tags$span(
                      id = ns("apply_filters_text"),
                      class = "no-filters"
                    )
                  )
                )
              )
            )
          )
        )
      ),
      tags$div(
        class = "d-flex flex-wrap",
        tags$div(
          id = ns("show_more_div"),
          class = "d-flex flex-grow-1 justify-content-center align-items-center mb-4",
          shinyjs::hidden(
            actionButton(
              class = "btn-discreet-lg",
              inputId = ns("show_more"),
              label = tags$span(
                id = ns("show_more_text"),
                class = "show-more-ready"
              )
            )
          )
        )
      )
    )
  )
}

#' @import rlang
choose_plants_server <- function(id, zone, your_border, city = NULL) {
  moduleServer(
    id,
    function(input, output, session) {

      ns <- NS(id)


      default_details <- default_zone_data[[zone]]$details
      insert_plant_cards(ns, default_details$data, your_border)

      card_ids <- reactiveVal(names(default_details$data))
      search_ids <- reactiveVal()

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

      shinyjs::show("show_more")

      #TODO: Make
      plant_details <- reactiveVal(default_details$data)

      #TODO: This could just be a list?
      api_info <- reactiveValues(
        url_args = list(hardiness = hardiness_range(zone)),
        addl_filters = list(filter_no_img = TRUE, filter_trees = TRUE),
        rem_pages = default_zone_data[[zone]]$rem_pages,
        rem_ids = default_details$rem_ids,
        error = FALSE
      )

      observeEvent(input$show_more, {

        status <- "OK"
        new_details <- NULL
        rem_pages <- api_info$rem_pages
        rem_ids <- api_info$rem_ids

        shinyjs::addClass(id = "show_more", "btn-spinner")
        shinyjs::removeClass(id = "show_more_text", "show-more-ready")
        shinyjs::addClass(id = "show_more_text", "show-more-busy")
        shinyjs::disable("show_more")

        # If any remaining ids from the last page batch, start with those
        if (length(rem_ids) > 0) {
          details_batch <- query_details_batch(rem_ids)
          status <- details_batch$status
          new_details <- details_batch$data
          rem_ids <- details_batch$rem_ids
        }

        # If no ids from last page batch or still no results after using the
        # remaining ids, get more pages
        if (status == "OK" && length(new_details) == 0) {
          more_plants <- query_more_plants(
            url_args = api_info$url_args,
            addl_filters = api_info$addl_filters,
            rem_pages = api_info$rem_pages
          )
          status <- more_plants$status
          new_details <- more_plants$details
          rem_pages <- more_plants$rem_pages
          rem_ids <- more_plants$rem_ids
        }

        if (length(new_details) > 0) {
          card_ids(c(card_ids(), names(new_details)))
          update_plant_list(
            ns = ns,
            details = new_details,
            your_border = your_border,
            card_ids = card_ids,
            search_ids = search_ids
          )
          plant_details(c(plant_details(), new_details))
        } else if (status == "api_error") {
          show_api_error_modal()
        }

        api_info$rem_pages <- rem_pages
        api_info$rem_ids <- rem_ids

        enable_filters()
        update_show_more(rem_pages, rem_ids, TRUE)

      })

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

      current_search_ids <- NULL

      observeEvent(input$search, {
        print("SEARCH FIRED")
        plant_name <- input[[sub("by_", "search_", search_type())]]
        col_name <- sub("by_", "", search_type())
        new_search_ids <- search_list[[zone]][[search_type()]] |>
          dplyr::filter(!!rlang::sym(col_name) == plant_name) |>
          dplyr::pull(id)
        #if (!identical(new_search_ids, current_search_ids)) {
        #  current_search_ids <<- new_search_ids
          search_ids(paste0("plant_", new_search_ids))
        #}
      })

      observeEvent(search_ids(), {
        #req(!identical(search_ids(), current_search_ids))
        #current_search_ids <<- isolate(search_ids())
        shinyjs::removeClass(id = "search_text", "search-ready")
        shinyjs::addClass(id = "search_text", "search-busy")
        shinyjs::disable("search")

        removeUI(
          selector = ".border-stub:has([id ^=choose_plants])",
          multiple = TRUE,
          immediate = TRUE
        )

        details <- query_details_batch(to_api_ids(search_ids()))

        shinyjs::enable("search")
        shinyjs::removeClass(id = "search_text", "search-busy")
        shinyjs::addClass(id = "search_text", "search-ready")

        if (details$status == "api_error") {
          api_info$error <- TRUE
          print("updated from sr error")
          search_ids(NULL)
          show_api_error_modal()
        } else {
          #browser()
          search_results <- details$data |>
            purrr::imap(
              ~ make_plant_stub(
                id = .y,
                details_record = .x,
                your_border = your_border,
                card_ids = card_ids,
                search_ids = search_ids,
                ns = ns
              )
            )
          shiny::insertUI(
            selector = paste0("#", ns("search_div")),
            ui = search_results,
            where = "afterEnd"
          )
        }

      })

      # output$search_results <- renderUI({
      #   #req(!identical(search_ids(), current_search_ids))
      #   #current_search_ids <<- isolate(search_ids())
      #   shinyjs::removeClass(id = "search_text", "search-ready")
      #   shinyjs::addClass(id = "search_text", "search-busy")
      #   shinyjs::disable("search")
      #   details <- query_details_batch(search_ids())
      #
      #   shinyjs::enable("search")
      #   shinyjs::removeClass(id = "search_text", "search-busy")
      #   shinyjs::addClass(id = "search_text", "search-ready")
      #
      #   if (details$status == "api_error") {
      #     api_info$error <- TRUE
      #     print("updated from sr error")
      #     search_ids(NULL)
      #     show_api_error_modal()
      #   } else {
      #     details$data |>
      #       purrr::imap(
      #         ~ make_plant_stub(
      #           id = .y,
      #           details_record = .x,
      #           your_border = your_border,
      #           card_ids = card_ids,
      #           search_ids = search_ids,
      #           ns = ns
      #         )
      #       )
      #   }
      #
      # })

      filters <- reactive({
        filter_selects |>
          purrr::imap(~ if (identical(input[[.y]], unname(.x))) { NULL }
                      else { input[[.y]] }) |>
          purrr::compact()
      })

      observeEvent(filters(), {
        if (length(filters()) > 0) {
          enable_filters()
        } else {
          no_filters()
        }
      }, ignoreInit = TRUE)

      observeEvent(input$apply_filters, {

        status <- "OK"
        new_details <- rem_ids <- rem_pages <- NULL

        shinyjs::removeClass(id = "apply_filters_text", "apply-filters-ready")
        shinyjs::addClass(id = "apply_filters_text", "apply-filters-busy")
        shinyjs::disable("apply_filters")

        removeUI(
          selector = ".flex-column:has([id ^=choose_plants][id $=card])",
          multiple = TRUE,
          immediate = TRUE
        )

        url_args <- make_url_args(input, names(filter_selects))
        api_info$url_args <- url_args

        #TODO: Change to use inputs
        #TODO: Save to reactivevalues obj
        addl_filters <- list(filter_no_img = TRUE, filter_trees = TRUE)

        if (isTruthy(input$flower_color)) {
          addl_filters <- addl_filters |>
            c(list(flower_color = input$flower_color))
        }

        api_info$addl_filters <- addl_filters

        # Query the first page separately to get the results page_count
        page1_resp <- do.call(query_species_list, url_args) |>
          httr2::req_perform()

        if (page1_resp$status_code != 200) {
          status <- "api_error"
        }

        if (status == "OK") {

          page1 <- process_page_resp(page1_resp, addl_filters)

          #rem_pages <- c(45, 3, 75)#2:5
          page_batch <- query_page_batch(
            url_args = url_args,
            addl_filters = addl_filters,
            rem_pages = if (page1$page_count == 1) NULL else 2:page1$page_count
          )

          status <- page_batch$status
          rem_pages <- page_batch$rem_pages

          if (status == "OK") {
            # TODO: Do we even need data? / maybe just keep images
            combined_pages <- c(page1$data, page_batch$data)
            combined_ids <- names(page1$data) |>
              to_api_ids() |>
              c(page_batch$ok_ids) |>
              sample2()
            details_batch <- query_details_batch(sample2(combined_ids))
            status <- details_batch$status
            new_details <- details_batch$data
            rem_ids <- details_batch$rem_ids

            print(paste("Length new details:", length(new_details)))

            if (status == "OK" && length(new_details) == 0) {
              more_plants <- query_more_plants(
                url_args = url_args,
                addl_filters = addl_filters,
                rem_pages = page_batch$rem_pages
              )

              status <- more_plants$status
              new_details <- more_plants$details
              rem_pages <- more_plants$rem_pages
              rem_ids <- more_plants$rem_ids

            }
          }
        }

        # Only show error if no results could be recovered
        if (length(new_details) > 0) {
          card_ids(names(new_details))
          update_plant_list(
            ns = ns,
            details = new_details,
            your_border = your_border,
            card_ids = card_ids,
            search_ids = search_ids
          )
          filters_applied()
        } else {
          card_ids(NULL)
          if (status == "api_error") {
            show_api_error_modal()
          }
        }

        plant_details(new_details)

        api_info$rem_pages <- rem_pages
        api_info$rem_ids <- rem_ids

        update_show_more(rem_pages, rem_ids, length(new_details) > 0)

      })

      #border_ids <- reactiveVal()

      default_details$data |>
        purrr::iwalk(
          ~ plant_card_server(
            id = .y,
            details_record = .x,
            your_border = your_border,
            card_ids = card_ids,
            search_ids = search_ids
          )
        )

      # remove_plant_stub <- function(border_ids, plant) {
      #   border_ids <- setdiff(border_ids, plant)
      #   shiny::removeUI(selector =  paste0("#", plant, "_stub"))
      #   if (length(border_ids) == 0) {
      #     shinyjs::show("empty_border", asis = TRUE)
      #   }
      # }

      # observeEvent(your_border(), {
      #   added_plant <- setdiff(names(your_border()), border_ids())
      #   removed_plant <- setdiff(border_ids(), names(your_border()))
      #
      #
      #   shiny::removeUI(selector =  paste0("#", plant, "_stub"))
      #   if (length(border_ids) == 0) {
      #     shinyjs::show("empty_border", asis = TRUE)
      #   }
      #
      #   if (length(added_plant) == 1) {
      #     if (length(your_border()) == 1) {
      #       shinyjs::hide("empty_border", asis = TRUE)
      #     }
      #     details_record <- your_border()[[added_plant]]
      #     shiny::insertUI(
      #       selector = "#border_plants",
      #       ui = make_plant_stub(added_plant, details_record, in_border = TRUE)
      #     )
      #     # shinyjs::onclick(
      #     #   paste0("remove_", added_plant),
      #     #   {
      #     #     remove_plant_stub(border_ids, added_plant)
      #     #     your_border(pop_id(your_border(), added_plant))
      #     #     shinyjs::runjs(
      #     #       paste0("toggleColor('", ns(added_plant), "-card');")
      #     #     )
      #     #   },
      #     #   asis = TRUE
      #     # )
      #     #border_ids(c(border_ids(), added_plant))
      #   }
      #   if (not_empty(removed_plant)) {
      #     remove_plant_stub(border_ids, removed_plant)
      #   }
      # }, ignoreNULL = FALSE)

      # Return your_border with colors added
      # TODO: Remove
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
