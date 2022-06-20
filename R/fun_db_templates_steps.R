create_reporte_templates_steps <- function() {
  con <- db_connect()

  if (!DBI::dbExistsTable(con, "templates_steps")) {
    fields_list <- data.frame(
      template_id = strrep(" ", 64),
      step_id = strrep(" ", 64),
      step_description = strrep(" ", 64)
    )

    DBI::dbWriteTable(con, "templates_steps", fields_list)
    delete_template_steps(template_id = strrep(" ", 64), with_print = FALSE)
    print("created table 'templates_steps'")
  }

  DBI::dbDisconnect(con)
}

get_template_steps <- function() {
  con <- db_connect()
  data <- DBI::dbReadTable(con, "templates_steps")
  DBI::dbDisconnect(con)
  return(data)
}

insert_template_steps <- function(field_list, with_print = TRUE) {
  con <- db_connect()
  DBI::dbWriteTable(con, "templates_steps", field_list, append = TRUE)
  DBI::dbDisconnect(con)
  if (with_print) {
    print(sprintf("inserted template steps with id %s", field_list$template_id))
  }
}

delete_template_steps <- function(template_id, with_print = TRUE) {
  con <- db_connect()
  DBI::dbExecute(con, sprintf("DELETE FROM templates_steps WHERE (template_id = '%s')", template_id))
  DBI::dbDisconnect(con)
  if (with_print) print(sprintf("deleted template steps with id %s", template_id))
}

# create_reporte_templates_steps()
# remove_table_from_reporte("templates_steps")
#
