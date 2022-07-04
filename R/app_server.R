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
  privileges <- reactive(user_get_privileges(user_iniciado()))


  output$my_ui <- renderUI({
    f$req_sign_in()
    mod_secure_ui(ns("secure_1"), privileges = privileges())
  })

  mod_secure_server("secure_1", user_iniciado)

}
