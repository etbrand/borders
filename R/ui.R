#TODO: Put this somewhere else?
make_input <- function(id) {
  shinyWidgets::pickerInput(
    inputId = id,
    label = paste0(stringr::str_to_sentence(id), ":"),
    choices = input_choices[[id]],
    multiple = id != "hardiness"
  )
}

ui <- function(request) {
  shiny::addResourcePath("www", system.file("www", package = "borders"))
  tagList(
    tags$head(
      tags$link(href = "www/custom.css", rel = "stylesheet", type = "text/css"),
      tags$link(rel = "shortcut icon", href = "www/favicon.svg")
    ),
    bslib::page_sidebar(
      title = tags$div(
        style = "display: flex;",
        tags$div(
          style = "align-self: center;",
          tags$img(src = 'www/logo.svg', height = 30)
        ),
        tags$h1(class = "bslib-page-title navbar-brand", "Borders")
      ),
      theme = bslib::bs_theme(bootswatch = "journal", primary = "#494b4d"),
      sidebar = bslib::sidebar(
        actionButton(
          inputId = "get_ideas",
          label = "Refresh plant list"
        ),
        bslib::accordion(
          bslib::accordion_panel(
            "Basic options",
            #icon = bsicons::bs_icon("menu-app"),
            purrr::map(names(input_choices), make_input)
          ),
          bslib::accordion_panel(
            "Advanced options"
            #icon = bsicons::bs_icon("sliders"),
          )
        )
      ),
      bslib::layout_column_wrap(
        width = NULL,
        style = bslib::css(grid_template_columns = "3fr 1fr"),
        bslib::card(
          width = 1/2,
          bslib::card_header("Explore border plants"),
          uiOutput("image_cards")
        ),
        bslib::card(
          bslib::card_header("Your border"),
          uiOutput("your_border")
        )
      )
    )
  )
}
