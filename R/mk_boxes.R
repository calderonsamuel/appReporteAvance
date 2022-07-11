mk_box <- function(icon = NULL, background = NULL) {
  function(..., task, inputId = NULL, task_description = NULL, label = NULL, dropdownMenu = NULL
           #user_id, reviewer, template_id
           ) {
      if (nrow(task) == 0) return(NULL)

    bs4Dash::box(
      title = task$task_description,
      h6(paste0("Responsable: ", task$user_id)),
      h6(paste0("Asignado por: ", task$reviewer)),
      h6(paste0("Plantilla: ", task$template_id)),
      dropdownMenu = bs4Dash::boxDropdown(
          bs4Dash::boxDropdownItem("Modificar", id = task$task_id)
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

box_pendientes <- mk_box(icon = icon("calendar"))
box_pendientes_user <- mk_box(background = "olive")
box_pendientes_group <- mk_box(background = "teal")

box_user <- mk_box(background = "olive")
box_group <- mk_box(background = "teal")
