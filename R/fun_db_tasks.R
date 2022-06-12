create_reporte_tasks <- function() {
  con <- RSQLite::dbConnect(RSQLite::SQLite(), "reporte_avance.db")

  if (!RSQLite::dbExistsTable(con, "tasks")) {
    task_list <- data.frame(
      reviewer = character(),
      user = character(),
      task_id = character(),
      task_description = character(),
      status = character()
    )

    RSQLite::dbWriteTable(con, "tasks", task_list)
    print("created table 'tasks'")
  }

  RSQLite::dbDisconnect(con)
}

get_tasks <- function() {
  con <- RSQLite::dbConnect(RSQLite::SQLite(), "reporte_avance.db")
  data <- RSQLite::dbReadTable(con, "tasks")
  RSQLite::dbDisconnect(con)
  return(data)
}

insert_task <- function(task_list, with_print = TRUE) {
  con <- RSQLite::dbConnect(RSQLite::SQLite(), "reporte_avance.db")
  RSQLite::dbWriteTable(con, "tasks", task_list, append = TRUE)
  RSQLite::dbDisconnect(con)
  if(with_print) print(sprintf("inserted task with id %s", task_list$task_id))
}

delete_task <- function(task_id, with_print = TRUE) {
  con <- RSQLite::dbConnect(RSQLite::SQLite(), "reporte_avance.db")
  RSQLite::dbExecute(con, sprintf("DELETE FROM tasks WHERE (task_id = '%s')", task_id))
  RSQLite::dbDisconnect(con)
  if(with_print) print(sprintf("deleted task with id %s", task_id))
}

get_task_from_id <- function(task_id) {
  con <- RSQLite::dbConnect(RSQLite::SQLite(), "reporte_avance.db")
  data <- RSQLite::dbGetQuery(con, sprintf("SELECT * FROM tasks WHERE (task_id = '%s')", task_id))
  RSQLite::dbDisconnect(con)
  return(data)
}

mdf_task_status <- function(task_id, new_status, with_print = TRUE) {
  con <- RSQLite::dbConnect(RSQLite::SQLite(), "reporte_avance.db")
  task <- get_task_from_id(task_id)
  task$status <- new_status
  delete_task(task_id, with_print = FALSE)
  insert_task(task, with_print = FALSE)
  RSQLite::dbDisconnect(con)
  if(with_print) print(sprintf("modified task with id %s", task$task_id))
}

get_task_from_user <- function(user) {
  con <- RSQLite::dbConnect(RSQLite::SQLite(), "reporte_avance.db")
  data <- RSQLite::dbGetQuery(con, sprintf("SELECT * FROM tasks WHERE (user = '%s')", user))
  RSQLite::dbDisconnect(con)
  return(data)
}

# create_reporte_tasks()
# remove_table_from_reporte("tasks")
# get_tasks()

# data.frame(
#   reviewer = "dgco93",
#   user = "dgco90",
#   task_id = "AOI001",
#   task_description = "Una tarea importate",
#   status = "Pendiente"
# ) |> insert_task()

# get_tasks()
# delete_task("0905002")
# get_tasks()
