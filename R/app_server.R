#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
    # Your application server logic
    ns <- session$ns
    
    shinyWidgets::sendSweetAlert(
        session = session,
        title = "Sistema de reportes",
        text = tags$div(firebase::firebaseUIContainer()),
        html = TRUE,
        btn_labels = NA,
        closeOnClickOutside = FALSE
    )
    
    
    f <- firebase::FirebaseUI$new()$# instantiate
        set_providers(# define providers
            # email = TRUE,
            google = TRUE)$launch()
    
    myapp_data <- reactive(AppData$new(f$get_signed_in()$response$email)) |> bindEvent(f$req_sign_in())

  output$my_ui <- renderUI({
      # f$req_sign_in() # https://firebase.google.com/docs/reference/rest/auth#section-sign-in-with-oauth-credential
      
      mod_secure_ui("secure_1", myapp_data())
  }) |>
      bindEvent(f$req_sign_in())
  
  observe({
      shinyWidgets::closeSweetAlert(session)
      mod_secure_server("secure_1", myapp_data())
  }) |>
      bindEvent(f$req_sign_in())
  
}
