mk_box <- function(icon = NULL) {
  function(title = NULL, ...) {
    bs4Dash::box(
      ...,
      title = title,
      footer = NULL,
      status = NULL,
      solidHeader = FALSE,
      background = NULL,
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
      label = NULL,
      dropdownMenu = NULL,
      sidebar = NULL,
      id = NULL
    )
  }
}

box_pendientes <- mk_box(icon = shiny::icon("calendar"))
