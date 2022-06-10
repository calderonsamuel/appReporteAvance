#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  mod_form_page_server("form_page_1")
  mod_tasks_server("tasks_1")
  mod_progress_server("progress_1")
}
