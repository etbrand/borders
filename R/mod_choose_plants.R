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

insert_plant_cards <- function(ns, plants) {
  insertUI(
    paste0("#", ns("show_more_div")),
    where = "beforeBegin",
    ui = tagList(!!!unname(purrr::imap(plants, ~ plant_card_UI(ns(.y), .x))))
  )
}

make_filters <- function(id, ns) {
  tags$div(
    class = "mb-0",
    shinyWidgets::pickerInput(
      inputId = ns(id),
      label = tags$span(
        paste0(stringr::str_to_sentence(gsub("_", " ", id)), ":")
      ),
      choices = filter_choices[[id]],
      selected = filter_choices[[id]],
      multiple = TRUE,
      options = if (id == "flower_color") list(
        `actions-box` = TRUE,
        `select-all-text` = "Include all",
        `deselect-all-text` = "Remove all"
      )
    )
  )
}

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
            class = "d-flex mb-4",
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
              purrr::map(names(filter_choices), ~ make_filters(.x, ns)),
              tags$div(
                class = "d-flex mt-4",
                actionButton(
                  class = "btn-primary btn-spinner btn-block w-100 h-100",
                  inputId = ns("apply_filters"),
                  label = tags$span(
                    id = ns("apply_filters_text"),
                    class = "apply-filters-ready"
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
          class = "d-flex flex-grow-1 justify-content-center align-items-center",
          actionButton(
            class = "btn-primary btn-spinner",# btn-block w-100 h-100",
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

      default_details <- default_zone_data[[zone]]$details

      insert_plant_cards(ns, default_details$data)

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
        shinyjs::removeClass(id = "show_more_text", "show-more-ready")
        shinyjs::addClass(id = "show_more_text", "show-more-busy")
        shinyjs::disable("show_more")

        if (length(api_info$rem_ids) > 0) {
          details_batch <- query_details_batch(rem_ids)
        } else {
          id_list <- query_addl_pages(

          )
        }


        shinyjs::enable("show_more")
        shinyjs::removeClass(id = "show_more_text", "show-more-busy")
        shinyjs::addClass(id = "show_more_text", "show-more-ready")
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

      search_ids <- reactiveVal()
      observeEvent(input$search, {
        plant_name <- input[[sub("by_", "search_", search_type())]]
        col_name <- sub("by_", "", search_type())
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
        details <- query_details_batch(search_ids())
        shinyjs::enable("search")
        shinyjs::removeClass(id = "search_text", "search-busy")
        shinyjs::addClass(id = "search_text", "search-ready")
        purrr::imap(details$data, ~ make_plant_stub(ns, .y, .x))
      })

      observeEvent(input$apply_filters, {

        shinyjs::removeClass(id = "apply_filters_text", "apply-filters-ready")
        shinyjs::addClass(id = "apply_filters_text", "apply-filters-busy")
        shinyjs::disable("apply_filters")

        removeUI(
          selector = ".flex-column:has([id ^=choose_plants][id $=card])",
          multiple = TRUE,
          immediate = TRUE
        )

        url_args <- make_url_args(input, names(filter_choices))

        #TODO: Change to use inputs
        #TODO: Save to reactivevalues obj
        addl_filters <- list(filter_no_img = TRUE, filter_trees = TRUE)

        if (isTruthy(input$flower_color)) {
          addl_filters <-
            c(addl_filters, list(flower_color = input$flower_color))
        }

        page1 <- query_list_safe(url_args, addl_filters)

        if (identical(page1, "api_error")) {
          print("API ERROR")
          api_info$error <- TRUE
        } else {
          page1_ids <- purrr::map_vec(page1$data, ~ .x$id)

          # Start by querying four more pages to mix with the first
          id_list <- query_addl_pages(
            url_args = url_args,
            addl_filters = addl_filters,
            rem_pages = setdiff(sample(1:page1$page_count), 1)
          )
          print(names(id_list$imgs))
          rem_pages <- id_list$rem_pages

          if (length(id_list$ids) == 0) {
            details <- NULL
            successful_ids <- NULL
            rem_ids <- NULL
            imgs <- NULL
          } else {
            #TODO: test if this breaks after page successful
            first_batch <- query_details_batch(sample2(id_list$ids))
            details <- first_batch$data

            if (any(purrr::map_vec(first_batch$data, ~ is.null(.x$default_image$regular_url)))) {
              browser()
            }

            successful_ids <- to_api_ids(names(first_batch$data))
            rem_ids <- first_batch$rem_ids
            imgs <- id_list$imgs

          }

          if (identical(id_list, "api_error")) {
            print("API ERROR")
            api_info$error <- TRUE
          } else {
            # Query up to 7 more pages looking for filter matches
            attempts <- 1
            while (all(attempts <= 15, length(successful_ids) < 5,
                       length(rem_pages) != 0)) {

              # Get another page of results and apply filters
              id_list <- query_addl_pages(
                url_args = url_args,
                addl_filters = addl_filters,
                rem_pages = rem_pages,
                pages_to_add = 1
              )

              if (identical(id_list, "api_error")) {
                print("API ERROR")
                api_info$error <- TRUE
                break
              }

              rem_pages <- id_list$rem_pages

              if (length(id_list$ids) > 0) {
                print(paste("Images:", paste(names(id_list$imgs), collapse = "")))
                print(paste("Successful ids:", paste(id_list$ids, collapse = "")))
                imgs <- c(imgs, id_list$imgs)
                details_batch <- query_details_batch(id_list$ids)
                if (identical(details_batch, "api_error")) {
                  print("API ERROR")
                  api_info$error <- TRUE
                  break
                }

                if (length(details_batch$data) > 0) {
                  details <- c(details, details_batch$data)
                  print(paste("Length of details from while loop:", length(details)))
                  successful_ids <-
                    c(successful_ids, to_api_ids(names(details_batch$data)))
                  rem_ids <- details_batch$rem_ids
                  if (any(purrr::map_vec(details_batch$data,
                                         ~ is.null(.x$default_image$regular_url)))) {
                    browser()
                  }
                }
              }

              attempts <- attempts + 1
            }

            if (!identical(id_list, "api_error")) {
              print(paste("Length of details:", length(details)))
              print(names(details))
              cat(crayon::green("Updating species list\n"))
              api_info$rem_pages <- id_list$rem_pages
              api_info$rem_ids <- rem_ids
              # TODO: Shuffle
              insert_plant_cards(ns, details)
              purrr::iwalk(details, ~ plant_card_server(.y, .x, your_border))
            }

          }
        }

        shinyjs::enable("apply_filters")
        shinyjs::removeClass(id = "apply_filters_text", "apply-filters-busy")
        shinyjs::addClass(id = "apply_filters_text", "apply-filters-ready")

      })

      border_ids <- reactiveVal()
      your_border <- reactiveVal()

      default_details$data |>
        purrr::iwalk(~ plant_card_server(.y, .x, your_border))

      remove_plant_stub <- function(border_ids, plant) {
        border_ids(setdiff(border_ids(), plant))
        shiny::removeUI(selector =  paste0("#", plant, "_stub"))
        if (rlang::is_empty(border_ids())) {
          shinyjs::show("empty_border", asis = TRUE)
        }
      }

      observeEvent(your_border(), {
        added_plant <- setdiff(names(your_border()), border_ids())
        removed_plant <- setdiff(border_ids(), names(your_border()))
        if (not_empty(added_plant)) {
          if (rlang::is_empty(border_ids())) {
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
              remove_plant_stub(border_ids, added_plant)
              your_border(pop_id(your_border(), added_plant))
              shinyjs::toggle(paste0(added_plant, "-checked"))
              shinyjs::runjs(
                paste0("toggleColor('", ns(added_plant), "-card');")
              )
            },
            asis = TRUE
          )
          border_ids(c(border_ids(), added_plant))
        }
        if (not_empty(removed_plant)) {
          remove_plant_stub(border_ids, removed_plant)
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
