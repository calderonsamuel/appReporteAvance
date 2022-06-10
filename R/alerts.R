
alert_x <- function(title = "Title", type = NULL) {
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

alert_success <- alert_x(title = "¡Hecho!", type = "success")

alert_error <- alert_x(title = "Error...", type = "error")

alert_info <- alert_x(title = "Atención", type = "info")

alert_warning <- alert_x(title = "¡Cuidado!", type = "warning")

