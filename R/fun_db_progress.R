create_reporte_progress <- function() {
  con <- RSQLite::dbConnect(RSQLite::SQLite(), "reporte_avance.db")

  if (!RSQLite::dbExistsTable(con, "progress")) {
    fields_list <- data.frame(
      task_id = character(),
      status_id = character(),
      status = character(),
      time = character(),
      explain = character()
    )

    RSQLite::dbWriteTable(con, "progress", fields_list)
    print("created table 'progress'")
  }

  RSQLite::dbDisconnect(con)
}

get_progress <- function() {
  con <- RSQLite::dbConnect(RSQLite::SQLite(), "reporte_avance.db")
  data <- RSQLite::dbReadTable(con, "progress")
  RSQLite::dbDisconnect(con)
  return(data)
}

insert_status <- function(field_list) {
  con <- RSQLite::dbConnect(RSQLite::SQLite(), "reporte_avance.db")
  RSQLite::dbWriteTable(con, "progress", field_list, append = TRUE)
  RSQLite::dbDisconnect(con)
  print(sprintf("inserted status with id %s", field_list$status_id))
}

delete_status <- function(status_id) {
  con <- RSQLite::dbConnect(RSQLite::SQLite(), "reporte_avance.db")
  RSQLite::dbExecute(con, sprintf("DELETE FROM progress WHERE (status_id = '%s')", status_id))
  RSQLite::dbDisconnect(con)
  print(sprintf("deleted status with id %s", task_id))
}
