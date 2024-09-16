border_info_UI <- function(id) {
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

border_info_server <- function(id, your_border) {
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

      # Care guides are only available for plants 1-3000; this avoids calling
      # the API again if other plants in the border change

      care_guide_ids <- reactiveVal()
      observeEvent(your_border(), {
        #TODO: Change this to a message when nothing in border
        #req(your_border())
        api_ids <- to_api_ids(names(your_border()))
        print(paste("Api ids:", paste(api_ids, collapse = " ")))
        new_care_guide_ids <- sort(api_ids[api_ids <= 3000])
        print(paste("CG ids:", paste(new_care_guide_ids, collapse = " ")))
        if (!identical(new_care_guide_ids, care_guide_ids())) {
          print("Updated cg ids!")
          care_guide_ids(new_care_guide_ids)
        }
      })

      care_guides <- reactive({
        new_care_guides <- care_guide_ids() |>
          purrr::map(req_care_guide_safe) |>
          purrr::set_names(paste0("plant_", care_guide_ids()))
      })

      care_guide_data <- reactive({
        list(
          cg = care_guides(),
          pd = isolate(your_border()[names(care_guides())]),
          s = ifelse(1:length(care_guides()) %% 2 == 1, "right", "left")
        )
      })

      output$content <- renderUI({
        unname(care_guide_data()) |>
          purrr::pmap(\(cg, pd, s) make_guide_content(ns, cg, pd, s))
      })

      output$toc <- renderUI({
        tags$nav(
          role = "doc-toc",
          tags$h2(class = "toc-title", "Jump to:"),
          tags$ul(
            class = "toc-list",
            purrr::map(care_guide_data()$cg, toc_entry)
          )
        )
      })

    }
  )
}
