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
        ),
        div(
            id = ns("div_transfer"),
            # mod_groupAdmin_ui(ns("groupAdmin_1"))
            h5("Transferencia de cargo"),
            selectInput(
                inputId = ns("users"),
                label = "Seleccione usuario",
                choices = get_user_choices(AppData, group_choices[1] |> unname())
            ),
            btn_guardar(ns("save"))
        )
    )
}

#' config Server Functions
#'
#' @noRd
mod_config_server <- function(id, AppData, trigger) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns
        
        is_group_admin <- reactive(verify_group_admin(AppData, input$groups))
        
        mod_groupAdmin_server("groupAdmin_1", AppData, config_output)
        
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
        
        observe({
            if (is_group_admin()) {
                shinyjs::show("div_transfer")
                updateSelectInput(
                    session = session,
                    inputId = "users",
                    choices = get_user_choices(AppData, input$groups)
                )
            } else {
                shinyjs::hide("div_transfer")
                
            }
        }) |> 
            bindEvent(input$groups)
        
        observe({
            shinyWidgets::ask_confirmation(
                inputId = ns("confirm_transfer"),
                title = "Transfiriendo cargo",
                text = "Una vez transferido el cargo ya no tendrá privilegios de responsable de equipo",
                type = "warning",
                btn_labels = c("Cancelar", "Confirmar"),
                btn_colors = c("#6e7d88", "#ff5964")
            )
        }) |> 
            bindEvent(input$save)
        
        observe({
            tryCatch(expr = {
                if(isTRUE(input$confirm_transfer)) {
                    # Name new admin
                    AppData$group_user_edit(
                        org_id = input$orgs,
                        group_id = input$groups,
                        user_id = input$users,
                        group_role = "admin"
                    )
                    # Get user role
                    AppData$group_user_edit(
                        org_id = input$orgs,
                        group_id = input$groups,
                        user_id = AppData$user$user_id,
                        group_role = "user"
                    )
                    
                    alert_info(session, "Se transfirió el cargo")
                    
                    session$reload()
                    
                }
            }, error = \(e) alert_error(session, e))
            
        }) |> 
            bindEvent(input$confirm_transfer)
        
        
        # output ----
        
        config_output <- list(
            org_selected = reactive(input$orgs),
            group_selected = reactive(input$groups),
            is_group_admin = is_group_admin
        )
        
        config_output
        
    })
}

## To be copied in the UI
# mod_config_ui("config_1")

## To be copied in the server
# mod_config_server("config_1")

mod_config_apptest <- function(id = "test", user = Sys.getenv("REPORTES_EMAIL")) {
    
    AppData <- AppData$new(user)
    trigger <- isolate(reactive(1))
    
    quick_bs4dash(
        modUI = mod_config_ui(id, AppData),
        modServer = mod_config_server(id, AppData, trigger = trigger)
    )
}

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

verify_group_admin <- function(AppData, group_id) AppData$groups[[group_id]]$group_role == "admin"

