create_reporte_templates <- function() {
  con <- db_connect()

  if (!DBI::dbExistsTable(con, "templates")) {
    fields_list <- data.frame(
      template_id = strrep(" ", 64),
      template_description = strrep(" ", 256),
      user_id = strrep(" ", 64)
    )

    DBI::dbWriteTable(con, "templates", fields_list)
    template_remove(template_id = strrep(" ", 64))
    message("created table 'templates'")
  }

  DBI::dbDisconnect(con)
}

template_get_all <- function() {
    query <- "SELECT * FROM templates"
    data <- db_get_query(query)
    return(data)
}

template_insert <- function(field_list) {
  con <- db_connect()
  DBI::dbWriteTable(con, "templates", field_list, append = TRUE)
  DBI::dbDisconnect(con)
  glue::glue("inserted template with id {template_id}", template_id = field_list$template_id) |>
      message()
}

template_remove <- function(template_id) {
  statement <- "DELETE
                FROM templates
                WHERE (template_id = {template_id})"
  db_execute_statement(statement, template_id = template_id)
  glue::glue("deleted template with id {template_id}") |> message()
}

template_get_from_user <- function(user_id) {
    query <-    "SELECT *
                FROM templates
                WHERE (user_id IN ({vals*}))"
  data <- db_get_query(query, vals = user_id)
  return(data)
}

template_get_description <- function(template_id) {
    if (is.na(template_id)) return("Sin plantilla")
    query <- "SELECT template_description
              FROM templates
              WHERE (template_id IN ({vals*}))"
    data <- db_get_query(query, vals = template_id)
    return(data$template_description)
}

# create_reporte_templates()
# db_remove_table("templates")
#

# data.frame(
#   template_id = "M02.01.01.00.02",
#   template_description = "Diseño de políticas y estrategias para el control de drogas y cultivos ilegales",
#   user_id = "team-politicas"
# ) |>
#   template_insert()
