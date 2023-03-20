#' group_selection UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_group_selection_ui <- function(id, app_data) {
    ns <- NS(id)
    tagList(
        selectInput(
            inputId = ns("orgs"),
            label = "Seleccione organización",
            choices = get_org_choices(app_data)
        ),
        selectInput(
            inputId = ns("groups"),
            label = "Seleccione equipo",
            choices = get_group_choices(
                app_data, 
                org_id = app_data$orgs[[1]]$org_id # TODO: change when app_data handles org_selected
            ),
            selected = app_data$group_selected
        )
    )
}

#' group_selection Server Functions
#'
#' @noRd
mod_group_selection_server <- function(id, app_data) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns
        
        observe({
            updateSelectInput(
                session = session,
                inputId = "groups",
                choices = get_group_choices(app_data, input$orgs)
            )
        }) |> 
            bindEvent(input$orgs)
        
        observe({
            app_data$group_select(input$groups)
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
    app_data <- AppData$new(user)
    
    quick_bs4dash(
        modUI = mod_group_selection_ui("test", app_data),
        modServer = mod_group_selection_server("test", app_data)
    )
}
