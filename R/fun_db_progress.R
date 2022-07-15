create_reporte_progress <- function() {
  con <- db_connect()

  if (!DBI::dbExistsTable(con, "progress")) {
    fields_list <- data.frame(
      task_id = strrep(" ", 64),
      status_id = strrep(" ", 64),
      step_id = strrep(" ", 64),
      reported_by = strrep(" ", 64),
      status = strrep(" ", 64),
      time = strrep(" ", 64),
      explain = strrep(" ", 64)
    )

    DBI::dbWriteTable(con, "progress", fields_list)
    progress_remove(task_id = strrep(" ", 64), status_id = strrep(" ", 64), with_print = FALSE)
    message("created table 'progress'")
  }

  DBI::dbDisconnect(con)
}

progress_get_all <- function() {
    query <- "SELECT * FROM progress"
    db_get_query(query)
}

progress_insert <- function(field_list, with_print = TRUE) {
  con <- db_connect()
  DBI::dbWriteTable(con, "progress", field_list, append = TRUE)
  DBI::dbDisconnect(con)
  if (with_print) {
      status_id <- field_list$status_id
      glue::glue("inserted status with id {status_id}") |> message()
  }
}

progress_remove <- function(task_id, status_id, with_print = TRUE) {
    statement <-
        "DELETE
        FROM progress
        WHERE (status_id = {status_id} AND task_id = {task_id})"
    db_execute_statement(statement, status_id = status_id, task_id = task_id)

    glue::glue("deleted status with id '{status_id}' from '{task_id}'") |> message()
}

progress_get_step_status <- function(task_id, step_id) {
    if (is.na(task_id)) return("Pendiente")
    query <-
        "SELECT status
        FROM progress
        WHERE (task_id = {task_id}
        AND step_id = {step_id})
        ORDER BY time DESC
        LIMIT 1"
    data <- db_get_query(query, task_id = task_id, step_id = step_id)
    return(data$status)
}

progress_status_choices <- function(status) {
    status_str <- str(status) |> as.character()
    # message(status_str)
    status <- if (!isTruthy(status)) "Pendiente" else status
    # paste0("status is '", status, "'") |> message()
    if (status == "Pendiente") {
        c("En proceso", "Pausado")
    } else if (status == "En proceso") {
        c("Pausado", "En revisión")
    } else if (status == "Pausado") {
        c("En proceso", "En revisión")
    } else if (status == "En revisión") {
        c("En proceso", "Terminado")
    }
}

# create_reporte_progress()
# db_remove_table("progress")
