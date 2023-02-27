custom_shiny_btn_dep <- function() {
    htmltools::htmlDependency(
        name = "multiBtnShinyInput", 
        version = "1.0",
        package = "appReporteAvance",
        src = "js",
        script = "multiBtnShinyInput.js"
    )
}

custom_shiny_btn <- function(
    inputId, label, 
    selectorClass, 
    idForSelection = "",
    colorClass = ""
    ) {
    input_tag <-  htmltools::tags$button(
        id = inputId, 
        type = "button",
        label, 
        `id-for-selection` = idForSelection,
        class = "btn",
        class = colorClass,
        class = selectorClass
    )
    
    tagList(custom_shiny_btn_dep(), input_tag)
}

ns_safe <- function(id, ns = NULL) if (is.null(ns)) id else ns(id)

admin_user_toolbar <- function(inputId, ns = NULL, class = "float-right") {
    div(
        class = "btn-group float-right", 
        role = "group",
        custom_shiny_btn(
            inputId = ns_safe(inputId, ns),
            label = fontawesome::fa("fas fa-pencil"),
            idForSelection = inputId,
            colorClass = "btn-warning",
            selector = "user-edit"
        ),
        custom_shiny_btn(
            inputId = ns_safe(inputId, ns),
            label = fontawesome::fa("fas fa-trash"),
            idForSelection = inputId,
            colorClass = "btn-danger",
            selector = "user-delete"
        )
    )
}

custom_input_UI <- function(id) {
  ns <- NS(id)
  tagList(
      lapply(ids::random_id(4), \(x) {
          shiny::fluidRow(
              style = "max-width: 350px;",
              column("Nombre", x, width = 8),
              column(admin_user_toolbar(x, ns), width = 4)
          )
      }),
      shiny::verbatimTextOutput(ns("info"))
  )
}

custom_input_Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
        ns <- session$ns
        
        output$info <- shiny::renderPrint({
            list(
                edit = input$userToEdit,
                delete = input$userToDelete
            )
        })
    }
  )
}

ui <- shiny::fluidPage(
    theme = bslib::bs_theme(version = 5),
    # custom_input_UI("test")
    shiny::uiOutput("ui")
)

server <- function(input, output, session) {
    output$ui <- shiny::renderUI(custom_input_UI("test"))
    custom_input_Server("test")
}

shiny::shinyApp(ui, server)
