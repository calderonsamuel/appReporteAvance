#' role_transfer UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_role_transfer_ui <- function(id) {
    ns <- NS(id)
    tagList(
        
    )
}

#' role_transfer Server Functions
#'
#' @noRd
mod_role_transfer_server <- function(id, AppData, trigger, config) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns
        
        groups <- AppData$groups |> 
            purrr::keep(~.x$group_role == "admin")
        
        observe({
            showModal(modalDialog(
                h1("Transferir cargo"),
                
                selectInput(
                    inputId = ns("user"),
                    label = "Seleccione usuario",
                    choices = get_user_choices(AppData, config$group_selected())
                ),
                
                footer = tagList(
                    modalButton("Cancelar"),
                    btn_guardar(ns("save"))
                )
            ))
        }) |> 
            bindEvent(trigger())
        
        observe({
            tryCatch({
                removeModal(session)
            }, error = \(e) alert_error(session, e))
        }) |> 
            bindEvent(input$save)
        
        
        
    })
}

## To be copied in the UI
# mod_role_transfer_ui("role_transfer_1")

## To be copied in the server
# mod_role_transfer_server("role_transfer_1")
