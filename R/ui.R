#TODO: Put this somewhere else?
# make_input <- function(id) {
#   shinyWidgets::pickerInput(
#     inputId = id,
#     label = paste0(stringr::str_to_sentence(id), ":"),
#     choices = input_choices[[id]],
#     multiple = id != "hardiness"
#   )
# }

ui <- function() {
  shiny::addResourcePath("www", system.file("www", package = "borders"))
  tagList(
    shinyjs::useShinyjs(),
    tags$head(
      tags$link(href = "www/custom.css", rel = "stylesheet", type = "text/css"),
      tags$link(rel = "shortcut icon", href = "www/favicon.svg"),
      tags$script(src = "www/js/toggleColor.js")#,
      #tags$script(src = "www/js/scrollToc.js"),
    ),
    bslib::page_navbar(
      title = tags$div(
        class = "d-inline-flex align-content-start;",
        tags$img(class = "align-self-center",
                 src = 'www/logo.svg', height = 30),
        tags$span(class = "navbar-brand", "Borders")
      ),
      theme = bslib::bs_theme(bootswatch = "journal", primary = "#494b4d"),
      getting_started_UI("getting_started"),
      choose_plants_UI("choose_plants"),
      at_a_glance_UI("at_a_glance"),
      care_guide_UI("care_guide"),
      bslib::nav_spacer(),
      tags$div(
        actionButton("hi", "Hello!")
      )
    )
  )
}
