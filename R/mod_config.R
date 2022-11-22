#' config UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_config_ui <- function(id, AppData) {
    ns <- NS(id)
    
    org_choices <- get_org_choices(AppData)
    group_choices <- get_group_choices(AppData, org_choices[1])
    
    tagList(
        selectInput(
            inputId = ns("orgs"), 
            label = "Seleccione organización", 
            choices = org_choices
        ),
        selectInput(
            inputId = ns("groups"),
            label = "Seleccione equipo",
            choices = group_choices
        )
    )
}

#' config Server Functions
#'
#' @noRd
mod_config_server <- function(id, AppData, trigger) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns
        
        observe({
            updateSelectInput(
                session = session,
                inputId = "orgs",
                choices = get_org_choices(AppData)
            )
        }) |> 
            bindEvent(trigger())
        
        observe({
            updateSelectInput(
                session = session,
                inputId = "groups",
                choices = get_group_choices(AppData, input$orgs)
            )
        }) |> 
            bindEvent(input$orgs)
        
        
        # output ----
        
        list(
            org_selected = reactive(input$orgs),
            group_selected = reactive(input$groups),
            is_group_admin = reactive(AppData$groups[[input$groups]]$group_role == "admin")
        )
        
    })
}

## To be copied in the UI
# mod_config_ui("config_1")

## To be copied in the server
# mod_config_server("config_1")

get_org_choices <- function(AppData) {
    ids <- AppData$orgs |> purrr::map_chr("org_id")
    titles <- AppData$orgs |> purrr::map_chr("org_title")
    setNames(ids, titles)
}

get_group_choices <- function(AppData, org_id) {
    groups <- AppData$groups |> 
        purrr::keep(~.x$org_id == org_id)
    ids <- groups |> purrr::map_chr("group_id")
    titles <- groups |> purrr::map_chr("group_title")
    setNames(ids, titles)
}

get_user_choices <- function(AppData, group_id) {
    is_admin <- AppData$groups[[group_id]]$group_role == "admin"
    if (is_admin) {
        users <- AppData$group_users[[group_id]]
        ids <- users |> purrr::map_chr("user_id")
        titles <- users |> purrr::map_chr(~paste(.x$name, .x$last_name))
        setNames(ids, titles)
    } else {
        setNames(object = AppData$user$user_id, 
                 nm = paste(AppData$user$name, AppData$user$last_name))
    }
    
}

is_group_admin <- function(AppData, group_id) AppData$groups[[group_id]]$group_role == "admin"

