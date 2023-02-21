#' group_selection UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_group_selection_ui <- function(id, AppData) {
    ns <- NS(id)
    tagList(
        selectInput(
            inputId = ns("orgs"),
            label = "Seleccione organización",
            choices = get_org_choices(AppData)
        ),
        selectInput(
            inputId = ns("groups"),
            label = "Seleccione equipo",
            choices = get_group_choices(
                AppData, 
                org_id = AppData$orgs[[1]]$org_id # TODO: change when AppData handles org_selected
            ),
            selected = AppData$group_selected
        )
    )
}

#' group_selection Server Functions
#'
#' @noRd
mod_group_selection_server <- function(id, AppData) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns
        
        observe({
            updateSelectInput(
                session = session,
                inputId = "groups",
                choices = get_group_choices(AppData, input$orgs)
            )
        }) |> 
            bindEvent(input$orgs)
        
        observe({
            AppData$group_select(input$groups)
        }, priority = 100) |> 
            bindEvent(input$groups)
        
        ## output ----
        list(
            org_selected = reactive(input$orgs),
            group_selected = reactive(input$groups)
        )
    })
}

## To be copied in the UI
# mod_group_selection_ui("group_selection_1")

## To be copied in the server
# mod_group_selection_server("group_selection_1")

mod_group_selection_apptest <- function(user = Sys.getenv("REPORTES_EMAIL")) {
    AppData <- AppData$new(user)
    
    quick_bs4dash(
        modUI = mod_group_selection_ui("test", AppData),
        modServer = mod_group_selection_server("test", AppData)
    )
}
