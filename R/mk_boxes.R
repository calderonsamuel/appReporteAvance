mk_box <- function(icon = NULL, use_ddMenu = TRUE) {
    function(task, label = NULL, ns = NULL) {
        if (!isTruthy(task)) return(NULL)

        task_id_ns <- ns_safe(task$task_id, ns)
        background <- box_bg_by_assignee(task$task_assignee_id)
        dropdownMenu <- if(use_ddMenu) box_ddMenu(task_id_ns) else NULL
        bs4Dash::box(
            title = task$task_description,
            dropdownMenu = dropdownMenu,
            footer = NULL,
            status = NULL,
            solidHeader = FALSE,
            background = background,
            width = 12, # default is 6´
            height = NULL,
            collapsible = TRUE,
            collapsed = TRUE, # default is FALSE
            closable = FALSE,
            maximizable = FALSE,
            icon = icon,
            gradient = FALSE,
            boxToolSize = "sm",
            elevation = NULL,
            headerBorder = TRUE,
            label = label,
            sidebar = NULL,
            id = NULL,

            box_interior(
                assignee_name = task$task_assignee_names,
                reviewer_name = task$task_creator_names,
                template_description = task$template_description
            )
        )
    }
}

box_bg_by_assignee <- function(user_id) {
    if (grepl("^team", user_id)) "olive" else "teal"
    switch (user_id,
        "team-politicas" = "olive",
        "wolivos@mininter.gob.pe" = "orange",
        "dgco84@mininter.gob.pe" = "teal",
        "dgco93@mininter.gob.pe" = "lime",
        "dgco80@mininter.gob.pe" = "purple"
    )
}

ns_safe <- function(id, ns = NULL) {
    if (is.null(ns)) id else ns(id)
}

box_interior <- function(assignee_name, reviewer_name, template_description){
    tagList(
        h6(glue::glue("Responsable: {assignee_name}")),
        h6(glue::glue("Asignado por: {reviewer_name}")),
        h6(glue::glue("Plantilla: {template_description}"))
    )
}

box_ddMenu <- function(item_id) {
    bs4Dash::boxDropdown(
        bs4Dash::boxDropdownItem("Modificar", id = item_id)
    )
}

box_dd_yes <- mk_box(use_ddMenu = TRUE)
box_dd_no <- mk_box(use_ddMenu = FALSE)

boxHidden <- function(
        ...,
        title = NULL,
        footer = NULL,
        status = NULL,
        solidHeader = FALSE,
        background = NULL,
        width = 6,
        height = NULL,
        collapsible = TRUE,
        collapsed = FALSE,
        closable = FALSE,
        maximizable = FALSE,
        icon = NULL,
        gradient = FALSE,
        boxToolSize = "sm",
        elevation = NULL,
        headerBorder = TRUE,
        label = NULL,
        dropdownMenu = NULL,
        sidebar = NULL,
        id = NULL,
        hidden = TRUE) {
    
    myBox <- bs4Dash::box(
        ...,
        title = title, 
        footer = footer, 
        status = status, 
        solidHeader = solidHeader, 
        background = background, 
        width = width, 
        height = height, 
        collapsible = collapsible, 
        collapsed = collapsed, 
        closable = closable, 
        maximizable = maximizable, 
        icon = icon, 
        gradient = gradient, 
        boxToolSize = boxToolSize, 
        elevation = elevation, 
        headerBorder = headerBorder, 
        label = label, 
        dropdownMenu = dropdownMenu, 
        sidebar = sidebar, 
        id = id)
    
    if (hidden) boxHide(myBox) else myBox
}

task_box <- function(task, ns = NULL) {
    id <- ns_safe(task$task_id, ns)
    
    dropdown <- if (task$status_current != "Terminado") {
        bs4Dash::boxDropdown(
            icon = fontawesome::fa("fas fa-ellipsis"),
            bs4Dash::boxDropdownItem("Avance", 
                                     id = paste0(id, "-task-report"), 
                                     icon = fontawesome::fa("fas fa-forward")),
            bs4Dash::boxDropdownItem("Editar", 
                                     id = paste0(id, "-task-edit"), 
                                     icon = fontawesome::fa("fas fa-pen-to-square")),
            bs4Dash::boxDropdownItem("Eliminar", 
                                     id = paste0(id, "-task-delete"),
                                     icon = fontawesome::fa("fas fa-trash"))
        )
    } else NULL
        
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
