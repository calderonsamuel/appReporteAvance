create_reporte_progress <- function() {
  con <- db_connect()

  if (!DBI::dbExistsTable(con, "progress")) {
    fields_list <- data.frame(
      task_id = strrep(" ", 64),
      status_id = strrep(" ", 64),
      status = strrep(" ", 64),
      time = strrep(" ", 64),
      explain = strrep(" ", 64)
    )

    DBI::dbWriteTable(con, "progress", fields_list)
    delete_status(status_id = strrep(" ", 64), with_print = FALSE)
    print("created table 'progress'")
  }

  DBI::dbDisconnect(con)
}

get_progress <- function() {
  con <- db_connect()
  data <- DBI::dbReadTable(con, "progress")
  DBI::dbDisconnect(con)
  return(data)
}

insert_status <- function(field_list, with_print = TRUE) {
  con <- db_connect()
  DBI::dbWriteTable(con, "progress", field_list, append = TRUE)
  DBI::dbDisconnect(con)
  if (with_print) print(sprintf("inserted status with id %s", field_list$status_id))
}

delete_status <- function(status_id, with_print = TRUE) {
  con <- db_connect()
  DBI::dbExecute(con, sprintf("DELETE FROM progress WHERE (status_id = '%s')", status_id))
  DBI::dbDisconnect(con)
  if (with_print) print(sprintf("deleted status with id %s", task_id))
}

# create_reporte_progress()
# remove_table_from_reporte("progress")
