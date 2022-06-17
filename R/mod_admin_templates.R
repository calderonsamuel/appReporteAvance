#' admin_templates UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_admin_templates_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' admin_templates Server Functions
#'
#' @noRd 
mod_admin_templates_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_admin_templates_ui("admin_templates_1")
    
## To be copied in the server
# mod_admin_templates_server("admin_templates_1")
