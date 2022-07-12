create_reporte_templates <- function() {
  con <- db_connect()

  if (!DBI::dbExistsTable(con, "templates")) {
    fields_list <- data.frame(
      template_id = strrep(" ", 64),
      template_description = strrep(" ", 256),
      user_id = strrep(" ", 64)
    )

    DBI::dbWriteTable(con, "templates", fields_list)
    template_remove(template_id = strrep(" ", 64), with_print = FALSE)
    message("created table 'templates'")
  }

  DBI::dbDisconnect(con)
}

template_get_all <- function() {
  con <- db_connect()
  data <- DBI::dbReadTable(con, "templates")
  DBI::dbDisconnect(con)
  return(data)
}

template_insert <- function(field_list, with_print = TRUE) {
  con <- db_connect()
  DBI::dbWriteTable(con, "templates", field_list, append = TRUE)
  DBI::dbDisconnect(con)
  if (with_print) {
      template_id <- field_list$template_id
      glue::glue("inserted template with id {template_id}") |>
          message()
  }
}

template_remove <- function(template_id, with_print = TRUE) {
  con <- db_connect()
  statement <- glue::glue_sql("DELETE
                              FROM templates
                              WHERE (template_id = {template_id})",
                              .con = con)
  DBI::dbExecute(con, statement)
  DBI::dbDisconnect(con)
  if (with_print) glue::glue("deleted template with id {template_id}") |> message()
}

template_get_from_user <- function(user_id) {
  con <- db_connect()
  query <- glue::glue_sql("SELECT *
                          FROM templates
                          WHERE (user_id IN ({vals*}))",
                          vals = user_id,
                          .con = con)
  data <- DBI::dbGetQuery(con, query)
  DBI::dbDisconnect(con)
  return(data)
}

template_get_description <- function(template_id) {
    if (is.na(template_id)) return("Sin plantilla")
    con <- db_connect()
    query <- glue::glue_sql("SELECT template_description
                          FROM templates
                          WHERE (template_id IN ({vals*}))",
                          vals = template_id,
                          .con = con)
    data <- DBI::dbGetQuery(con, query)
    DBI::dbDisconnect(con)
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
