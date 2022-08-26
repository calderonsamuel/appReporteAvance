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

  rv <- reactiveValues(
      user_iniciado = character(),
      privileges = character()
  )

  output$my_ui <- renderUI({
    f$req_sign_in()

      rv$user_iniciado <- f$get_signed_in()$response$email

      if (!user_is_registered(rv$user_iniciado)) {

        tagList(
            fluidPage(
                tags$h3("No tiene permiso para usar esta aplicación"),
                tags$p("Comuníquese con el administrador para subsanar su registro")
            )
        )
      } else {

        rv$privileges <- user_get_privileges(rv$user_iniciado)
        glue::glue("sesion iniciada de {user}", user = rv$user_iniciado) |>
          message()

        mod_secure_ui("secure_1", privileges = rv$privileges, user_iniciado = rv$user_iniciado)
      }

    # mod_secure_ui("secure_1", privileges = rv$privileges)
    # mod_secure_ui(ns("secure_1"), privileges = rv$privileges)
  })

  observe({
      if (!user_is_registered(rv$user_iniciado)) {
          NULL
      } else {
          mod_secure_server("secure_1", user_iniciado = rv$user_iniciado)
      }

  }) |>
      bindEvent(f$req_sign_in())

}
