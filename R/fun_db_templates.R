create_reporte_templates <- function() {
  con <- db_connect()

  if (!DBI::dbExistsTable(con, "templates")) {
    fields_list <- data.frame(
      template_id = strrep(" ", 64),
      template_description = strrep(" ", 256),
      user_id = strrep(" ", 64)
    )

    DBI::dbWriteTable(con, "templates", fields_list)
    delete_template(template_id = strrep(" ", 64), with_print = FALSE)
    message("created table 'templates'")
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
  if (with_print) {
    message(sprintf("inserted template with id %s", field_list$template_id))
  }
}

delete_template <- function(template_id, with_print = TRUE) {
  con <- db_connect()
  DBI::dbExecute(con, sprintf("DELETE FROM templates WHERE (template_id = '%s')", template_id))
  DBI::dbDisconnect(con)
  if (with_print) message(sprintf("deleted template with id %s", template_id))
}

get_templates_from_user <- function(user_id) {
  con <- db_connect()
  data <- DBI::dbGetQuery(con, sprintf("SELECT * FROM templates WHERE (user_id = '%s')", user_id))
  DBI::dbDisconnect(con)
  return(data)
}

# create_reporte_templates()
# remove_table_from_reporte("templates")
#

# data.frame(
#   template_id = "M02.01.01.00.02",
#   template_description = "Diseño de políticas y estrategias para el control de drogas y cultivos ilegales",
#   user_id = "team-politicas"
# ) |>
#   insert_template()
