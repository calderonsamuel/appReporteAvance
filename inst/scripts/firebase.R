library(shiny)
library(firebase)

ui <- fluidPage(
    useFirebase(),
    firebaseUIContainer(),
    reqSignin(
        uiOutput("my_ui")
    )
)

server <- function(input, output, session) {
    f <- firebase::FirebaseUI$new()$# instantiate
        set_providers(# define providers
            # email = TRUE,
            google = TRUE)$launch()
    
    output$my_ui <- renderUI({
        # f$req_sign_in() # https://firebase.google.com/docs/reference/rest/auth#section-sign-in-with-oauth-credential
        tagList(
            h1("hello"),
            verbatimTextOutput("debug")
        )
        
    }) #|>
        # bindEvent(f$req_sign_in())
    
    output$debug <- renderPrint(f$get_signed_in()$response)
}

shinyApp(ui, server)
