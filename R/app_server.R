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
      user_display_name = character(),
      privileges = character()
  )

  output$my_ui <- renderUI({
    # f$req_sign_in() # https://firebase.google.com/docs/reference/rest/auth#section-sign-in-with-oauth-credential

      rv$user_id <- f$get_signed_in()$response$email
      rv$user_display_name <- f$get_signed_in()$response$displayName
      # 
      # print(f$get_signed_in()$response)

      if (!user_is_registered(rv$user_id)) {

        tagList(
            fluidPage(
                tags$h3("No tiene permiso para usar esta aplicación"),
                tags$p("Comuníquese con el administrador para subsanar su registro")
            )
        )
      } else {

        rv$privileges <- user_get_privileges(rv$user_id)
        glue::glue("sesion iniciada de {user}", user = rv$user_id) |>
          message()

        mod_secure_ui("secure_1", rv)
      }

    # mod_secure_ui("secure_1", privileges = rv$privileges)
    # mod_secure_ui(ns("secure_1"), privileges = rv$privileges)
  }) |> 
      bindEvent(f$req_sign_in())

  observe({
      if (!user_is_registered(rv$user_id)) {
          NULL
      } else {
          mod_secure_server("secure_1", rv)
      }

  }) |>
      bindEvent(f$req_sign_in())

}
