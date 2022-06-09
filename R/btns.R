
btn_x <- function(color, label) {
  function(inputId, icon = NULL, size = "md", block = FALSE, no_outline = TRUE) {
    shinyWidgets::actionBttn(
      inputId = inputId,
      label = label,
      style = "jelly",
      color = color,
      icon = icon,
      size = size,
      block = block,
      no_outline = no_outline
    )
  }
}

btn_add <- btn_x(color = "success", label = icon("plus"))

btn_trash <- btn_x(color = "danger", label = icon("trash"))
