create_reporte_tasks <- function() {
  con <- db_connect()

  if (!DBI::dbExistsTable(con, "tasks")) {
    task_list <- data.frame(
      reviewer = strrep(" ", 64),
      user_id = strrep(" ", 64),
      task_id = strrep(" ", 64),
      task_description = strrep(" ", 64),
      template_id = strrep(" ", 64),
      status = strrep(" ", 64)
    )

    DBI::dbWriteTable(con, "tasks", task_list)
    task_remove(task_id = strrep(" ", 64), with_print = FALSE)
    message("created table 'tasks'")
  }

  DBI::dbDisconnect(con)
}

task_get_all <- function() {
  con <- db_connect()
  data <- DBI::dbReadTable(con, "tasks")
  DBI::dbDisconnect(con)
  return(data)
}

task_insert <- function(task_list, with_print = TRUE) {
  con <- db_connect()
  DBI::dbWriteTable(con, "tasks", task_list, append = TRUE)
  DBI::dbDisconnect(con)
  if(with_print) {
      task_id <- task_list$task_id
      glue::glue("inserted task with id {task_id}") |> message()
  }
}

task_remove <- function(task_id, with_print = TRUE) {
  con <- db_connect()
  statement <- glue::glue_sql("DELETE
                              FROM tasks
                              WHERE (task_id = {task_id})",
                              .con = con)
  DBI::dbExecute(con, statement)
  DBI::dbDisconnect(con)
  if(with_print) glue::glue("deleted task with id {task_id}") |> message()
}

task_get_from_id <- function(task_id) {
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

task_modify_status <- function(task_id, new_status, with_print = TRUE) {
    con <- db_connect()
    statement <- glue::glue_sql("UPDATE tasks
                                SET status = {new_status}
                                WHERE task_id = {task_id}",
                                .con = con)
    DBI::dbExecute(con, statement)
    DBI::dbDisconnect(con)
    if(with_print) glue::glue("modified task with id {task_id}") |> message()
}

task_get_from_user <- function(user_id) {
  con <- db_connect()
  query <- glue::glue_sql("SELECT task_id, task_description, status,
                          user_id, reviewer, template_id
                          FROM tasks
                          WHERE (user_id IN ({vals*}))",
                          vals = user_id,
                          .con = con)
  data <- DBI::dbGetQuery(con, query)
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

task_status_pendientes <- mk_task_getter("Pendiente")
task_status_en_proceso <- mk_task_getter("En proceso")
task_status_pausado <- mk_task_getter("Pausado")
task_status_en_revision <- mk_task_getter("En revisión")
task_status_terminado <- mk_task_getter("Terminado")

# create_reporte_tasks()
# db_remove_table("tasks")
# task_get_all()
#
# data.frame(
#   reviewer = "dgco93",
#   user_id = "dgco90",
#   task_id = "123",
#   task_description = "Una tarea importate",
#   status = "Pendiente"
# ) |> task_insert()
#
# task_get_all()
# task_remove("AOI001")
# task_get_all()

