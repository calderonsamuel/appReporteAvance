ns <- shiny::NS("test")

custom_shiny_btn_dep <- function() {
    htmltools::htmlDependency(
        name = "user-delete", 
        version = "1.0",
        package = "appReporteAvance",
        src = "js",
        script = "user-delete.js"
    )
}

custom_shiny_btn <- function(
    inputId, label, selectorClass, 
    idForSelection = "",
    colorClass = ""
    ) {
    input_tag <-  htmltools::tags$button(
        id = inputId, 
        type = "button",
        label, 
        idForSelection = idForSelection,
        class = "btn",
        class = colorClass,
        class = selectorClass
    )
    
    tagList(custom_shiny_btn_dep(), input_tag)
}

custom_input_UI <- function(id) {
  ns <- NS(id)
  tagList(
      shiny::tagList(
          lapply(ids::random_id(4), \(x) {
              custom_shiny_btn(
                  inputId = ns(x),
                  label = fontawesome::fa("fas fa-trash"),
                  idForSelection = x,
                  colorClass = "btn-danger",
                  selector = "user-delete"
              )
          })
      ),
      
      shiny::verbatimTextOutput(ns("info"))
  )
}

custom_input_Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
        output$info <- shiny::renderPrint({
            input[["isMac"]]
        })
    }
  )
}

ui <- shiny::fluidPage(
    custom_input_UI("test")
)

server <- function(input, output, session) {
    custom_input_Server("test")
}

shiny::shinyApp(ui, server)
