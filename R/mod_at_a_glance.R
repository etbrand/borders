at_a_glance_UI <- function(id) {
  ns <- NS(id)

  bslib::nav_panel(
    title = "At a glance",
    bslib::layout_column_wrap(
      bslib::card(
        max_height = "300px",
        gap = 0,
        bslib::card_header("Color palette"),
        # shinyWidgets::radioGroupButtons(
        #   inputId = ns("season"),
        #   status = "light",
        #   justified = TRUE,
        #   choices = c("Spring", "Summer", "Fall", "Winter"),
        #   selected = "Spring"
        # ),
        #class = "d-flex align-items-center justify-contents-center",
        bslib::card_body(
          h1("Flowers"),
          r2d3::d3Output(ns("border_colors"), height = "60px", width = "400px")
        )
      ),
      tags$div("Placeholder for other content")
    )
  )

}

at_a_glance_server <- function(id, your_border = NULL) {
  moduleServer(
    id,
    function(input, output, session) {

      ns <- NS(id)

      plant_color_data <- reactive({
        if (length(your_border()) > 0) {
          color_rows <- your_border() |>
            purrr::discard(~ is.null(.x$hex_color)) |>
            purrr::map(make_color_row)
          dplyr::bind_rows(!!!color_rows) |>
            arrange_by_color() |>
            dplyr::mutate(x = 400 * (dplyr::row_number() - 1) / length(color_rows))
        }
      })

      output$border_colors <- r2d3::renderD3({
        if (nrow(plant_color_data()) > 0) {
          r2d3::r2d3(
            data = plant_color_data(),
            script = "www/js/plantColorChart.js"
          )
        }
      })

      # observeEvent(your_border(), {
      #   if (not_null(your_border()$details)) {
      #     browser()
      #   }
      # })

    }
  )
}
