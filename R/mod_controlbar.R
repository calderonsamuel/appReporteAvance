#' controlbar UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_controlbar_ui <- function(id, AppData) {
    ns <- NS(id)
    
    menu <- bs4Dash::controlbarMenu(
        type = "pills",
        bs4Dash::controlbarItem(
            title = fontawesome::fa("people-roof"),
            id = ns("section-group-selection"),
            mod_group_selection_ui(ns("group_selection_1"), AppData)
        ),
        bs4Dash::controlbarItem(
            title = fontawesome::fa("user-pen"),
            id = ns("section-group-users"),
            mod_group_users_ui(ns("group_users_1"))
        ),
        bs4Dash::controlbarItem(
            title = fontawesome::fa("temperature-low"),
            id = ns("section-group-units"),
            mod_group_units_ui(ns("group_units_1"))
        ) 
    )
    
    # Add class to allow shinyjs to hide/show controlbarItems in the server
    htmltools::tagQuery(menu)$
        find(".nav-item")$
        filter(\(x, i) i > 1)$
        addClass("nav-item-hideable")$
        allTags()
}

#' controlbar Server Functions
#'
#' @noRd
mod_controlbar_server <- function(id, AppData) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns
        
        group_selection <- mod_group_selection_server("group_selection_1", AppData)
        users_admin <- mod_group_users_server("group_users_1", AppData, group_selection)
        units_admin <- mod_group_units_server("group_units_1", AppData, group_selection)
        
        group_info <- reactive({
            AppData$groups[[group_selection$group_selected()]]
        }) |> 
            bindEvent(
                group_selection$group_selected()
            )
        
        is_admin <- reactive(group_info()$group_role == "admin")
        
        observe({
            if (is_admin()) {
                shinyjs::show(selector = ".nav-item-hideable")
            } else {
                shinyjs::hide(selector = ".nav-item-hideable")
            }
        }) |> 
            bindEvent(is_admin())
        
       
        
        # output ----
        
        list(
            org_selected = group_selection$org_selected,
            group_selected = group_selection$group_selected,
            group_colors_modified = reactive(users_admin$group_colors_modified),
            is_admin = is_admin
        )
        
    })
}

## To be copied in the UI
# mod_controlbar_ui("controlbar_1")

## To be copied in the server
# mod_controlbar_server("controlbar_1")

mod_controlbar_apptest <- function(user = Sys.getenv("REPORTES_EMAIL")) {
    AppData <- AppData$new(user)
    
    quick_bs4dash(
        controlbar = bs4Dash::dashboardControlbar(
            id = "controbar",
            mod_controlbar_ui("test", AppData)
        ), 
        modServer = mod_controlbar_server("test", AppData)
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