#' group_users UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_group_users_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' group_users Server Functions
#'
#' @noRd 
mod_group_users_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_group_users_ui("group_users_1")
    
## To be copied in the server
# mod_group_users_server("group_users_1")
