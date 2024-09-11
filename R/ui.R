ui <- function() {
  shiny::addResourcePath("www", system.file("www", package = "borders"))
  tagList(
    shinyjs::useShinyjs(),
    tags$head(
      tags$link(href = "www/custom.css", rel = "stylesheet", type = "text/css"),
      tags$link(rel = "shortcut icon", href = "www/favicon.svg"),
      tags$script(src = "www/js/toggleColor.js")
    ),
    bslib::page_navbar(
      title = tags$div(
        class = "d-inline-flex align-content-start;",
        tags$img(class = "align-self-center",
                 src = 'www/logo.svg', height = 30),
        tags$span(class = "navbar-brand", "Borders"),
        actionButton(
          inputId = "your_border",
          label = tags$span(
            "Your border",
            tags$span(
              class = "position-absolute translate-middle badge bg-secondary",
              style = "top: 0.25em; right: -1.5em;",
              textOutput("n_plants")
            )
          ),
          class = "btn-light align-self-center",
          style = "position: absolute; right: 5em;",
          `data-bs-toggle` = "offcanvas",
          `data-bs-target` = "#your_border_oc",
          `aria-controls` = "your_border_oc"
        )
      ),
      theme = bslib::bs_theme(
        bootswatch = "journal",
        primary = "#494b4d",
        secondary = "#4F7942"
      ),
      header = tags$div(
        id = "your_border_oc",
        class = "offcanvas offcanvas-end",
        tabIndex = "-1",
        `aria-labelledby` = "your_border_header",
        tags$div(
          class = "offcanvas-header",
          tags$h5(
            id = "your_border_header",
            class = "offcanvas-title",
            tags$span("Your border", class = "ms-2")
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
            tags$div(
              id = "empty_border",
              "Nothing in your border. Choose plants from the plant list."
            ),
            tags$div(id = "border_plants")
          )
        )
      ),
      getting_started_UI("getting_started"),
      choose_plants_UI("choose_plants"),
      at_a_glance_UI("at_a_glance"),
      care_guide_UI("care_guide")
    )
  )
}
