create_reporte_templates <- function() {
  con <- db_connect()

  if (!DBI::dbExistsTable(con, "templates")) {
    fields_list <- data.frame(
      template_id = strrep(" ", 64),
      template_description = strrep(" ", 64),
      step_id = strrep(" ", 64),
      step_description = strrep(" ", 64)
    )

    DBI::dbWriteTable(con, "templates", fields_list)
    delete_template(template_id = strrep(" ", 64), with_print = FALSE)
    print("created table 'templates'")
  }

  DBI::dbDisconnect(con)
}

get_templates <- function() {
  con <- db_connect()
  data <- DBI::dbReadTable(con, "templates")
  DBI::dbDisconnect(con)
  return(data)
}

insert_template <- function(field_list, with_print = TRUE) {
  con <- db_connect()
  DBI::dbWriteTable(con, "templates", field_list, append = TRUE)
  DBI::dbDisconnect(con)
  if (with_print) print(sprintf("inserted template with id %s", field_list$template_id))
}

delete_template <- function(template_id, with_print = TRUE) {
  con <- db_connect()
  DBI::dbExecute(con, sprintf("DELETE FROM templates WHERE (template_id = '%s')", template_id))
  DBI::dbDisconnect(con)
  if (with_print) print(sprintf("deleted template with id %s", template_id))
}

# create_reporte_templates()
# remove_table_from_reporte("templates")
