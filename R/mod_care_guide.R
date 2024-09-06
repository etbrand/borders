care_guide_UI <- function(id) {
  ns <- NS(id)
  bslib::nav_panel(
    title = "Care guide",
    tags$div(
      class = "d-flex gap-1 justify-content-between",
      tags$div(uiOutput(ns("content"))),
      tags$div(
        class = "mt-2 align-self-start sticky-top",
        uiOutput(ns("toc"))
      )
    )
  )
}

care_guide_server <- function(id, your_border) {
  moduleServer(
    id,
    function(input, output, session) {

      ns <- NS(id)

      toc_entry <- function(care_guide) {
        id <- ns(paste0("plant_", care_guide$species_id))
        tags$nav(
          tags$a(
            href = paste0("#", id),
            class = "nav-link toc-heading",
            stringr::str_to_title(care_guide$common_name)
          )
        )
      }

      make_guide_content <- function(care_guide, details_record, side) {
        id <- ns(paste0("plant_", care_guide$species_id))
        card_offset <- if (side == "right") "offset-md-1"
        card_order <- if (side == "left") "order-md-2 order-xs-1"
        plant_card <- tags$div(
          class = paste("col-md-3 align-self-start sticky-top", card_offset,
                        card_order),
          plant_card_UI(
            id = paste0("plant_", care_guide$species_id),
            details_record = details_record,
            simplified = TRUE
          )
        )
        left_class <- if (side == "left") "offset-md-1 order-md-1 order-xs-2"
        text_content <- tags$div(
          class = paste("col-md-6", left_class),
          tags$h2(
            id = id,
            class = "plant-guide-header",
            stringr::str_to_title(care_guide$common_name)
          ),
          tags$h3(id = paste0(id, "_description"), "Description"),
          tags$p(details_record$description),
          tags$h3(
            id = paste0(id, "_", care_guide$section[[1]]$type),
            stringr::str_to_title(care_guide$section[[1]]$type)
          ),
          tags$p(care_guide$section[[1]]$description),
          tags$h3(
            id = paste0(id, "_", care_guide$section[[2]]$type),
            stringr::str_to_title(care_guide$section[[2]]$type)
          ),
          tags$p(care_guide$section[[2]]$description),
          tags$h3(
            id = paste0(id, "_", care_guide$section[[3]]$type),
            stringr::str_to_title(care_guide$section[[3]]$type)
          ),
          tags$p(care_guide$section[[3]]$description)
        )

        tags$div(class = "row mt-2", plant_card, text_content)

      }

      # Care guides are only available for plants 1-3000; this avoids calling
      # the API again if other plants in the border change

      care_guide_ids <- reactiveVal()
      observeEvent(your_border(), {
        #TODO: Change this to a message when nothing in border
        req(your_border())
        api_ids <- as.numeric(gsub("plant_", "", names(your_border())))
        new_care_guide_ids <- sort(api_ids[api_ids <= 3000])
        if (!identical(new_care_guide_ids, care_guide_ids())) {
          care_guide_ids(new_care_guide_ids)
        }
      })

      care_guides <- reactive({
        new_care_guides <- care_guide_ids() |>
          purrr::map(get_care_guide) |>
          purrr::set_names(paste0("plant_", care_guide_ids()))
      })

      care_guide_data <- reactive({
        list(
          care_guides = care_guides(),
          plant_details = isolate(your_border()[names(care_guides())]),
          sides = ifelse(1:length(care_guides()) %% 2 == 1, "right", "left")
        )
      })

      output$content <- renderUI({
        req(care_guide_data())
        tags$div(
          `data-bs-spy` = "scroll",
          `data-bs-target` = paste0("#", ns("toc")),
          `data-bs-offset` = "0",
          tabindex = "0",
          height = "500",
          `overflow-y` = "scroll",
          style = "position: relative;",
          purrr::pmap(unname(care_guide_data()), make_guide_content)
        )
      })

      output$toc <- renderUI({
        tags$nav(
          role = "doc-toc",
          tags$h2(class = "toc-title", "Jump to:"),
          tags$ul(
            class = "toc-list",
            purrr::map(care_guide_data()$care_guides, toc_entry)
          )
        )
      })

    }
  )
}
