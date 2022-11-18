task_box <- function(task, ns = NULL, is_group_admin = FALSE) {
    id <- ns_safe(task$task_id, ns)
    
    dropdown <- task_dropdown(id, task$status_current, is_group_admin)
        
    bs4Dash::box(
        id = id,
        title = task$task_title,
        width = 12,
        collapsed = TRUE,
        headerBorder = FALSE,
        background = task$user_color,
        label = bs4Dash::boxLabel(
            text = glue::glue("{task$output_current}/{task$output_goal}"), 
            status = "primary",
            tooltip = task$output_unit
        ),
        dropdownMenu = dropdown,
        tags$p(task$task_description),
        tags$div(
            tags$span(fontawesome::fa("far fa-user"), glue::glue("{task$assignee_name} {task$assignee_last_name}"))
        ),
        tags$div(
            tags$span(fontawesome::fa("far fa-thumbs-up"), glue::glue("{task$assigned_by_name} {task$assigned_by_last_name}"))
        ),
        tags$div(
            tags$span(fontawesome::fa("far fa-calendar"), format(task$time_due, "%d/%m/%Y")),
            tags$span(fontawesome::fa("far fa-clock"), format(task$time_due, "%H:%M:%S"))
        )
    )
}

ns_safe <- function(id, ns = NULL) if (is.null(ns)) id else ns(id)

task_dropdown <- function(id, status, is_group_admin) {
    item_avanzar <- 
        bs4Dash::boxDropdownItem("Avanzar", 
            id = paste0(id, "-task-report"), 
            icon = fontawesome::fa("fas fa-forward"))
    
    item_editar <- 
        bs4Dash::boxDropdownItem("Editar", 
            id = paste0(id, "-task-edit"), 
            icon = fontawesome::fa("fas fa-pen-to-square"))
    
    item_eliminar <- 
        bs4Dash::boxDropdownItem(
            tags$span(fontawesome::fa("fas fa-trash", fill = "#cf222e"), 
                      "Eliminar", style = "color: #cf222e;"),
            id = paste0(id, "-task-delete"))
    
    item_historia <- 
        bs4Dash::boxDropdownItem("Ver historia", 
            id = paste0(id, "-task-history"), 
            icon = fontawesome::fa("fas fa-clock-rotate-left"))
    
    item_revisar <- 
        bs4Dash::boxDropdownItem("Revisar", 
            id = paste0(id, "-task-report"), 
            icon = fontawesome::fa("fas fa-book-open-reader"))
    
    if (status %in% c("Pendiente", "En proceso", "Pausado")) {
        bs4Dash::boxDropdown(
            icon = fontawesome::fa("fas fa-ellipsis"),
            item_avanzar, item_editar, item_eliminar, item_historia
        )
    } else if (status == "Observado") {
        bs4Dash::boxDropdown(
            icon = fontawesome::fa("fas fa-ellipsis"), item_avanzar
        )
    } else if (status == "En revisión" && is_group_admin) {
        bs4Dash::boxDropdown(
            icon = fontawesome::fa("fas fa-ellipsis"), item_revisar
        )
    } else NULL
}
