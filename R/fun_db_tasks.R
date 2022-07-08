create_reporte_tasks <- function() {
  con <- db_connect()

  if (!DBI::dbExistsTable(con, "tasks")) {
    task_list <- data.frame(
      reviewer = strrep(" ", 64),
      user_id = strrep(" ", 64),
      task_id = strrep(" ", 64),
      task_description = strrep(" ", 64),
      status = strrep(" ", 64)
    )

    DBI::dbWriteTable(con, "tasks", task_list)
    delete_task(task_id = strrep(" ", 64), with_print = FALSE)
    message("created table 'tasks'")
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
  if(with_print) message(sprintf("inserted task with id %s", task_list$task_id))
}

delete_task <- function(task_id, with_print = TRUE) {
  con <- db_connect()
  DBI::dbExecute(con, sprintf("DELETE FROM tasks WHERE (task_id = '%s')", task_id))
  DBI::dbDisconnect(con)
  if(with_print) message(sprintf("deleted task with id %s", task_id))
}

get_task_from_id <- function(task_id) {
  con <- db_connect()
  query <- glue::glue_sql(
      "SELECT *
      FROM tasks
      WHERE (task_id IN ({vals*}))",
      vals = task_id,
      .con = con
  )
  data <- DBI::dbGetQuery(con, query)
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
  if(with_print) message(sprintf("modified task with id %s", task$task_id))
}

get_task_from_user <- function(user) {
  con <- db_connect()
  data <- DBI::dbGetQuery(con, sprintf("SELECT * FROM tasks WHERE (user_id = '%s')", user))
  DBI::dbDisconnect(con)
  return(data)
}

mk_task_getter <- function(status) {
  function(user_id) {
    con <- db_connect()
    query <- glue::glue_sql(
        "SELECT *
        FROM tasks
        WHERE (user_id IN ({vals*}) and status = {status})",
        vals = user_id,
        .con = con
    )
    data <- DBI::dbGetQuery(con, query) |> try(silent = TRUE)
    DBI::dbDisconnect(con)
    return(data)
  }
}

get_tasks_pendientes <- mk_task_getter("Pendiente")
get_tasks_en_proceso <- mk_task_getter("En proceso")
get_tasks_pausado <- mk_task_getter("Pausado")
get_tasks_en_revision <- mk_task_getter("En revisión")
get_tasks_terminado <- mk_task_getter("Terminado")

# create_reporte_tasks()
# remove_table_from_reporte("tasks")
# get_tasks()
#
# data.frame(
#   reviewer = "dgco93",
#   user_id = "dgco90",
#   task_id = "123",
#   task_description = "Una tarea importate",
#   status = "Pendiente"
# ) |> insert_task()
#
# get_tasks()
# delete_task("AOI001")
# get_tasks()

