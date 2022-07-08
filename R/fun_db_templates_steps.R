create_reporte_templates_steps <- function() {
  con <- db_connect()

  if (!DBI::dbExistsTable(con, "templates_steps")) {
    fields_list <- data.frame(
      user_id = strrep(" ", 64),
      template_id = strrep(" ", 64),
      step_id = strrep(" ", 10),
      step_description = strrep(" ", 256)
    )

    DBI::dbWriteTable(con, "templates_steps", fields_list)
    step_remove(template_id = strrep(" ", 64), with_print = FALSE)
    message("created table 'templates_steps'")
  }

  DBI::dbDisconnect(con)
}

step_get_all <- function() {
  con <- db_connect()
  data <- DBI::dbReadTable(con, "templates_steps")
  DBI::dbDisconnect(con)
  return(data)
}

step_insert <- function(field_list, with_print = TRUE) {
  con <- db_connect()
  DBI::dbWriteTable(con, "templates_steps", field_list, append = TRUE)
  DBI::dbDisconnect(con)
  if (with_print) {
    message(sprintf("inserted steps %s in template with id %s", field_list$step_id, field_list$template_id))
  }
}

step_remove <- function(template_id, with_print = TRUE) {
  con <- db_connect()
  DBI::dbExecute(con, sprintf("DELETE FROM templates_steps WHERE (template_id = '%s')", template_id))
  DBI::dbDisconnect(con)
  if (with_print) message(sprintf("deleted template steps with id %s", template_id))
}

# create_reporte_templates_steps()
# db_remove_table("templates_steps")
#

# readxl::read_excel("inst/data-raw/template_example.xlsx") |>
#   janitor::clean_names() |>
#   subset(unidad_de_organizacion == "DDC", select = c(no, descripcion_de_la_actividad)) |>
#   setNames(c("step_id", "step_description")) |>
#   dplyr::mutate(
#     step_id = sprintf("step_%02i", step_id),
#     user_id = "team-politicas",
#     template_id = "M02.01.01.00.02"
#   ) |>
#   step_insert()

