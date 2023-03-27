#' Icon Picker
#'
#' @description Create an icon picker element.
#'
#' @param inputId chr: the ID of the input element.
#' @param label chr: the label of the icon picker's dropdown button.
#' @param selected chr: the name of the selected fontawesome icon. Must have the form 'fas fa-name'.
#' @param btn_class chr: the class of the button element.
#'
#' @return a dropdown list with icons and a hidden input to hold their values.
#'
#' @importFrom fontawesome fa fa_metadata
#' @importFrom rlang %||%
#' @import purrr
#' @importFrom htmltools tagList div tags
#'
input_icon_picker <- function(inputId, label,
                              selected = NULL, btn_class = NULL) {
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
        lapply(icon_names, \(.x) {
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

#' Get Fontawesome Icons
#'
#' @description Get the available solid style icons in the fontawesome package.
#'
#' @return a character vector with the names of the available icons.
#'
reportes_icon_names <- function() {
  fontawesome::fa_metadata()$icon_names_full_fas
}

#' Icon Picker Dependency
#'
#' @description Create a dependency for the icon picker element.
#'
#' @return a html dependency named "iconPicker".
#'
iconPicker_dep <- function() {
  htmltools::htmlDependency(
        name = "iconPicker", 
        version = "1.0",
        package = "appReporteAvance",
        src = "js",
        script = "iconPicker.js"
    )
}