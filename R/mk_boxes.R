mk_box <- function(icon = NULL, background = NULL) {
    function(task, ..., label = NULL, dropdownMenu = NULL, ns = NULL) {
        if (!isTruthy(task)) return(NULL)

        task_id_ns <- ns_safe(task$task_id, ns)
        background <- if (task_is_from_group(task$task_id)) "olive" else background
        bs4Dash::box(
            title = task$task_description,
            dropdownMenu = bs4Dash::boxDropdown(
              bs4Dash::boxDropdownItem("Modificar", id = task_id_ns)
            ),
            box_interior(
                assignee_name = task$assignee$user_name,
                reviewer_name = task$reviewer$user_name,
                template_description = task$template$template_description
            ),
            ...,
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
            id = NULL
        )
    }
}

box_bg_by_assignee <- function(task_id) {

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

box_pendientes <- mk_box(icon = icon("calendar"))
box_pendientes_user <- mk_box(background = "olive")
box_pendientes_group <- mk_box(background = "teal")

box_user <- mk_box(background = "olive")
box_group <- mk_box(background = "teal")
