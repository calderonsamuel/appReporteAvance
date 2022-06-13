create_reporte_tasks <- function() {
  con <- db_connect()

  if (!DBI::dbExistsTable(con, "tasks")) {
    task_list <- data.frame(
      reviewer = strrep(" ", 64),
      user = strrep(" ", 64),
      task_id = strrep(" ", 64),
      task_description = strrep(" ", 64),
      status = strrep(" ", 64)
    )

    DBI::dbWriteTable(con, "tasks", task_list)
    delete_task(task_id = strrep(" ", 64), with_print = FALSE)
    print("created table 'tasks'")
  }

  DBI::dbDisconnect(con)
}

get_tasks <- function() {
  con <- db_connect()
  data <- DBI::dbReadTable(con, "tasks")
  DBI::dbDisconnect(con)
  return(data)
}

insert_task <- function(task_list, with_print = TRUE) {
  con <- db_connect()
  DBI::dbWriteTable(con, "tasks", task_list, append = TRUE)
  DBI::dbDisconnect(con)
  if(with_print) print(sprintf("inserted task with id %s", task_list$task_id))
}

delete_task <- function(task_id, with_print = TRUE) {
  con <- db_connect()
  DBI::dbExecute(con, sprintf("DELETE FROM tasks WHERE (task_id = '%s')", task_id))
  DBI::dbDisconnect(con)
  if(with_print) print(sprintf("deleted task with id %s", task_id))
}

get_task_from_id <- function(task_id) {
  con <- db_connect()
  data <- DBI::dbGetQuery(con, sprintf("SELECT * FROM tasks WHERE (task_id = '%s')", task_id))
  DBI::dbDisconnect(con)
  return(data)
}

mdf_task_status <- function(task_id, new_status, with_print = TRUE) {
  con <- db_connect()
  task <- get_task_from_id(task_id)
  task$status <- new_status
  delete_task(task_id, with_print = FALSE)
  insert_task(task, with_print = FALSE)
  DBI::dbDisconnect(con)
  if(with_print) print(sprintf("modified task with id %s", task$task_id))
}

get_task_from_user <- function(user) {
  con <- db_connect()
  data <- DBI::dbGetQuery(con, sprintf("SELECT * FROM tasks WHERE (user = '%s')", user))
  DBI::dbDisconnect(con)
  return(data)
}

# create_reporte_tasks()
# remove_table_from_reporte("tasks")
# get_tasks()
#
# data.frame(
#   reviewer = "dgco93",
#   user = "dgco90",
#   task_id = "123",
#   task_description = "Una tarea importate",
#   status = "Pendiente"
# ) |> insert_task()
#
# get_tasks()
# delete_task("AOI001")
# get_tasks()
