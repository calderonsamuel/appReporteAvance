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
      step_id <- field_list$step_id
      template_id <- field_list$template_id
      glue::glue("inserted steps {step_id} in template with id {template_id}") |> message()
  }
}

step_remove <- function(template_id, with_print = TRUE) {
  con <- db_connect()
  statement <- glue::glue_sql("DELETE
                              FROM templates_steps
                              WHERE (template_id = {template_id})",
                              .con = con)
  DBI::dbExecute(con, statement)
  DBI::dbDisconnect(con)
  if (with_print) glue::glue("deleted template steps with id {template_id}") |> message()
}

step_get_from_template <- function(template_id) {
    if(!isTruthy(template_id)) {
        data <- list(step_id = "step_01",
                     step_description = "Actividad única")
        return(data)
    }
    con <- db_connect()
    query <- glue::glue_sql("SELECT *
                            FROM templates_steps
                            WHERE (template_id = {template_id})",
                            .con = con)
    data <- DBI::dbGetQuery(con, query)
    DBI::dbDisconnect(con)
    return(data)
}

step_get_description <- function(template_id, step_id) {
    con <- db_connect()
    query <- glue::glue_sql("SELECT step_description
                            FROM templates_steps
                            WHERE (template_id = {template_id}
                            AND step_id = {step_id})",
                            .con = con)
    data <- DBI::dbGetQuery(con, query)
    DBI::dbDisconnect(con)
    return(data$step_description)
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

