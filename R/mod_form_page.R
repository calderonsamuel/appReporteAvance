#' form_page UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_form_page_ui <- function(id){
  ns <- NS(id)
  tagList(
    mod_metadata_ui(ns("metadata_1")),
    mod_reporte_ui(ns("reporte_1"))
  )
}

#' form_page Server Functions
#'
#' @noRd
mod_form_page_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    mod_metadata_server("metadata_1")
    mod_reporte_server("reporte_1")
  })
}

## To be copied in the UI
# mod_form_page_ui("form_page_1")

## To be copied in the server
# mod_form_page_server("form_page_1")
