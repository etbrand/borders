getting_started_UI <- function(id) {
  ns <- NS(id)

  bslib::nav_panel(
    title = "Getting started",
    tags$iframe(
      src = "www/getting_started/getting_started.html",
      width = "100%",
      height = "100%"
    )
  )

}
