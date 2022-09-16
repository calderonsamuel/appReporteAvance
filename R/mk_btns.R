#' Pretty action buttons factory
#'
#' @inheritParams shinyWidgets::actionBttn
#'
#' @return A function to generate action buttons with a particular styling
#'
mk_btn <- function(color, label, style = "jelly") {
  function(inputId, icon = NULL, size = "md", block = FALSE,
           no_outline = TRUE) {
    shinyWidgets::actionBttn(
      inputId = inputId,
      label = label,
      style = style,
      color = color,
      icon = icon,
      size = size,
      block = block,
      no_outline = no_outline
    )
  }
}

#' Pre styled pretty action buttons
#'
#' These functions have predefined styling and icons for labels.
#' The defaults are inherited from [shinyWidgets::actionBttn()].
#'
#' @inheritParams shinyWidgets::actionBttn
#' @name buttons
#' @return HTML code for pretty action buttons
#' @seealso [shinyWidgets::actionBttn()]
#'
NULL
#> NULL

#' @rdname buttons
btn_add <- mk_btn(color = "success", label = icon("plus"))

#' @rdname buttons
btn_trash <- mk_btn(color = "danger", label = icon("trash"))

#' @rdname buttons
btn_agregar <- mk_btn(color = "success", label = "Agregar")

#' @rdname buttons
btn_guardar <- mk_btn(color = "success", label = "Guardar ", style = "simple")

#' @rdname buttons
btn_cancelar <- mk_btn(color = "danger", label = "Cancelar", style = "simple")

#' @rdname buttons
btn_eliminar <- mk_btn(color = "danger", label = "Eliminar")

#' @rdname buttons
btn_modificar <- mk_btn(color = "primary", label = "Modificar")

#' @rdname buttons
btn_minus <- mk_btn(color = "danger", label = icon("minus"))

#' @rdname buttons
btn_refresh <- mk_btn(color = "royal", label = icon("redo"))

#' @rdname buttons
btn_expand <- mk_btn(color = "royal", label = "Expandir")

#' @rdname buttons
btn_user_add <- mk_btn(color = "success", label = icon("user-plus"))

#' @rdname buttons
btn_user_remove <- mk_btn(color = "danger", label = icon("user-minus"))

#' @rdname buttons
btn_user_edit <- mk_btn(color = "warning", label = icon("user-edit"))

#' @rdname buttons
btn_editar <- mk_btn(color = "warning", label = "Editar") 
