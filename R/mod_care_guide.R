care_guide_UI <- function(id) {
  ns <- NS(id)
  bslib::nav_panel(
    title = "Care guide",
    tags$div(
      class = "grid",
      tags$div(class = "g-col-11", uiOutput(ns("content"))),
      tags$div(
        class = "g-col-1 mt-2 me-2 align-self-start sticky-top",
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
        tags$li(
          tags$a(
            href = paste0("#", id),
            class = "nav-link",
            h3(class = "toc-heading",
               stringr::str_to_title(care_guide$common_name))
          )
        )
      }

      make_guide_content <- function(care_guide, details_record, side) {
        id <- ns(paste0("plant_", care_guide$species_id))
        sct1 <- care_guide$section[[1]]
        sct2 <- care_guide$section[[2]]
        sct3 <- care_guide$section[[3]]
        plant_card <- tags$div(
          class = "g-col-3 align-self-start sticky-top",
          plant_card_UI(
            id = paste0("plant_", care_guide$species_id),
            details_record = details_record,
            simplified = TRUE
          )
        )
        text_content <- tags$div(
          class = "g-col-9",
          tags$h2(
            id = id,
            class = "plant-guide-header",
            stringr::str_to_title(care_guide$common_name)
          ),
          tags$h3(id = paste0(id, "_description"), "Description"),
          tags$p(details_record$description),
          tags$h3(
            id = paste0(id, "_", sct1$type),
            stringr::str_to_title(sct1$type)
          ),
          tags$p(sct1$description),
          tags$h3(
            id = paste0(id, "_", sct2$type),
            stringr::str_to_title(sct2$type)
          ),
          tags$p(sct2$description),
          tags$h3(
            id = paste0(id, "_", sct3$type),
            stringr::str_to_title(sct3$type)
          ),
          tags$p(sct3$description)
        )

        if (side == "right") {
          tags$div(class = "grid mt-4 ms-4 me-4", plant_card, text_content)
        } else {
          tags$div(class = "grid mt-4 ms-4 me-4", text_content, plant_card)
        }

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
        purrr::pmap(unname(care_guide_data()), make_guide_content)
      })

      output$toc <- renderUI({
        tags$nav(
          role = "doc-toc",
          tags$h2(class = "toc-title", "Jump to:"),
          tags$ul(
            style = "list-style-type: none; padding-inline-start: 15px;",
            tagList(purrr::map(care_guide_data()$care_guides, toc_entry))
          )
        )
      })

    }
  )
}
