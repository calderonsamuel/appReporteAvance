#' @example icon_picker("picker", "Ícono", icon = fontawesome::fa("fas fa-icons"))
#' 
icon_picker <- function(inputId, label, selected = NULL, btn_class = NULL, ...) {
  icon_names <- reportes_icon_names()

  tagList(
    div(
      class = "dropdown",
      tags$button(
        class = "btn dropdown-toggle",
        class = btn_class,
        type = "button",
        id = paste0(inputId, "-dropdown"),
        `data-bs-toggle` = "dropdown",
        `data-toggle` = "dropdown",
        `aria-haspopup` = "true",
        `aria-expanded` = "false",
        fontawesome::fa(selected) %||% NULL,
        label
      ),
      div(
        class = "dropdown-menu p-1",
        `aria-labelledby` = paste0(inputId, "-dropdown"),
        style = htmltools::css(
          width = "310px",
          `font-size` = "large",
          `text-align` = "center",
          `max-height` = "200px",
          `overflow-y` = "scroll"
        ),
        purrr::map(icon_names, ~{
          span(
            style = "min-width: 25px; display: inline-block;",
            tags$a(
              `id-for-selection` = inputId,
              `multi-value` = .x,
              class = "multi-btn icon-picker",
              class = if (.x == selected) "text-primary icon-last-clicked" else "text-muted",
              fontawesome::fa(.x)
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
  fontawesome::fa_metadata()$icon_names_full_fas
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