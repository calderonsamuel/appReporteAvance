#' @example icon_picker("picker", "Ícono", icon = fontawesome::fa("fas fa-icons"))
#' 
icon_picker <- function(inputId, label, icon = NULL, class = NULL, ...) {
  icon_names <- reportes_icon_names()

  tagList(
    div(
      class = "dropdown",
      tags$button(
        class = "btn dropdown-toggle",
        class = class,
        type = "button",
        id = paste0(inputId, "-dropdown"),
        `data-toggle` = "dropdown",
        `aria-haspopup` = "true",
        `aria-expanded` = "false",
        icon,
        label
      ),
      div(
        class = "dropdown-menu p-2",
        `aria-labelledby` = paste0(inputId, "-dropdown"),
        style = "width: 300px; font-size: large; text-align: center;",
        purrr::map(icon_names, ~{
          span(
            style = "min-width: 25px; display: inline-block;",
            tags$a(
              `id-for-selection` = inputId,
              `multi-value` = .x,
              class = "multi-btn icon-picker text-muted",
              fontawesome::fa(paste0("fas fa-", .x))
            )
          ) 
        })
      )
    ),
    multiBtnInput_dep(),
    iconPicker_dep()
  )
}

reportes_icon_names <- function() {
  fontawesome::fa_metadata()$icon_names_fas |> sample(100)
}


iconPicker_dep <- function() {
  htmltools::htmlDependency(
        name = "iconPicker", 
        version = "1.0",
        package = "appReporteAvance",
        src = "js",
        script = "iconPicker.js"
    )
}