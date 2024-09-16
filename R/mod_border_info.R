border_info_UI <- function(id) {
  ns <- NS(id)
  bslib::nav_panel(
    title = "Border info",
    value = "info",
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

      care_guide_ids <- reactive({
        api_ids <- to_api_ids(names(your_border()))
        sort(api_ids[api_ids <= 3000])
      })

      care_guides <- reactive({
        req(length(care_guide_ids()) > 0)
        care_guide_ids() |>
          purrr::map(req_care_guide_safe) |>
          purrr::set_names(paste0("plant_", care_guide_ids()))
      })

      care_guide_data <- reactive({
        list(
          cg = care_guides(),
          pd = your_border()[names(care_guides())],
          s = ifelse(1:length(care_guides()) %% 2 == 1, "right", "left")
        )
      })

      output$content <- renderUI({

        if (length(your_border()) == 0) {
          show_empty_border_modal()
        } else if (length(care_guide_ids()) == 0) {
          show_no_info_modal()
        }

        url <- sub("#", "", update_url(session, your_border()))
        split_url <- strsplit(url, "_")[[1]]
        split_url[2] <- "info"
        url_prefix <- paste(split_url[1:3], collapse = "_")
        unname(care_guide_data()) |>
          purrr::pmap(\(cg, pd, s) make_guide_content(
            ns, url_prefix, cg, pd, s))
      })

      output$toc <- renderUI({

        toc_entry <- function(care_guide) {

          url <- sub("#", "", update_url(session, your_border()))
          split_url <- strsplit(url, "_")[[1]]
          split_url[2] <- "info"
          url_prefix <- paste(split_url[1:3], collapse = "_")

          id <- paste(c(url_prefix, paste0("plant", care_guide$species_id)),
                      collapse = "_")

          tags$nav(
            tags$a(
              href = paste0("#", id),
              class = "nav-link toc-heading",
              stringr::str_to_title(care_guide$common_name)
            )
          )
        }

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
