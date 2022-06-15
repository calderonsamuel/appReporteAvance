#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  ns <- session$ns

  f <- firebase::FirebaseUI$
    new()$ # instantiate
    set_providers( # define providers
      # email = TRUE,
      google = TRUE
    )$
    launch()

  s <- firebase::Storage$new()

  user_iniciado <- reactive(f$get_signed_in()$response$email)


  output$my_ui <- renderUI(mod_secure_ui(ns("secure_1")))

  mod_secure_server("secure_1", user_iniciado)

}
