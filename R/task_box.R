task_box <- function(task, ns = NULL, is_group_admin = FALSE) {
    id <- ns_safe(task$task_id, ns)
    
    dropdown <- task_dropdown(id, task$status_current, is_group_admin)
    
    box_tag <- bs4Dash::box(
        id = id,
        title = task$task_title,
        width = 12,
        collapsed = TRUE,
        headerBorder = FALSE,
        # background = task$user_color,
        label = bs4Dash::boxLabel(
            text = format(task$time_due, "%d %b"), 
            status = task_status_from_time_due(task$time_due),
            tooltip = task$output_unit
        ),
        dropdownMenu = dropdown,
        tags$p(task$task_description),
        task_assignee_div(task),
        tags$div(
            tags$span(fontawesome::fa("far fa-clock"), format(task$time_due, "%H:%M:%S")),
            tags$span(fontawesome::fa("fas fa-bullseye"), glue::glue("{task$output_current}/{task$output_goal} {task$output_unit}"), style = "float: right;")
        )
    )
    
    # This hacks the validation that bs4Dash makes on its background colors. 
    # Allows to use all the colors specified in fresh::create_theme()
    box_tag |> 
        tagAppendAttributes(class = paste0("bg-", task$user_color), .cssSelector = ".bs4Dash")
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
    
    dd_items <- 
        if (status %in% c("Pendiente", "En proceso", "Pausado")) {
            tagList(
                item_avanzar, item_editar, item_historia, item_eliminar
            )
        } else if (status == "Observado") {
            tagList(
                item_avanzar, item_historia
            )
        } else if (status == "En revisión" && is_group_admin) {
            tagList(
                item_revisar, item_historia
            )
        } else {
            tagList(
                item_historia
            )
        }

    bs4Dash::boxDropdown(
        icon = fontawesome::fa("fas fa-ellipsis"), dd_items
        
    )
}

task_status_from_time_due <- function(time_due) {
    if (as.Date(time_due) > lubridate::today("America/Lima")) {
        "success"
    } else if (as.Date(time_due) == lubridate::today("America/Lima")) {
        "warning"
    } else  "danger"
}

task_assignee_div <- function(task) {
    if (task$assignee == task$assigned_by) {
        tags$div(
            tags$span(fontawesome::fa("far fa-user"), fontawesome::fa("far fa-thumbs-up"), glue::glue("{task$assignee_name} {task$assignee_last_name}"))
        )
    } else {
        tagList(
            tags$div(
                tags$span(fontawesome::fa("far fa-user"), glue::glue("{task$assignee_name} {task$assignee_last_name}"))
            ),
            tags$div(
                tags$span(fontawesome::fa("far fa-thumbs-up"), glue::glue("{task$assigned_by_name} {task$assigned_by_last_name}"))
            )
        )
    }
}
