#' Sweet alerts factory
#'
#' @inheritParams shinyWidgets::sendSweetAlert
#'
#' @return A function to generate a sweet alert with a particular title and type.
#'
mk_alert <- function(title = "Title", type = NULL) {
  function(session = getDefaultReactiveDomain(),
           text = NULL, btn_labels = "Ok", btn_colors = "#3085d6",
           html = FALSE, closeOnClickOutside = TRUE, showCloseButton = FALSE,
           width = NULL, ...) {
    shinyWidgets::sendSweetAlert(
      session = session,
      title = title,
      text = text,
      type = type,
      btn_labels = btn_labels,
      btn_colors = btn_colors,
      html = html,
      closeOnClickOutside = closeOnClickOutside,
      showCloseButton = showCloseButton,
      width = width,
      ...
    )
  }
}

#' Pre styled sweet alerts
#'
#' These functions are convenient wrappers for [shinyWidgets::sendSweetAlert()].
#'
#' @inheritParams shinyWidgets::sendSweetAlert
#' @name alerts
#' @return HTML code for sweet alerts
#' @seealso [shinyWidgets::sendSweetAlert()]
#'
NULL
#> NULL

#' @rdname alerts
alert_success <- mk_alert(title = "¡Hecho!", type = "success")

#' @rdname alerts
alert_error <- mk_alert(title = "Error...", type = "error")

#' @rdname alerts
alert_info <- mk_alert(title = "Atención", type = "info")

#' @rdname alerts
alert_warning <- mk_alert(title = "¡Cuidado!", type = "warning")

