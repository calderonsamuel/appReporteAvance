create_reporte_tasks <- function() {
  con <- db_connect()

  if (!DBI::dbExistsTable(con, "tasks")) {
    task_list <- data.frame(
      reviewer = strrep(" ", 64),
      user_id = strrep(" ", 64),
      task_id = strrep(" ", 512),
      task_description = strrep(" ", 64),
      template_id = strrep(" ", 64),
      status = strrep(" ", 64)
    )

    DBI::dbWriteTable(con, "tasks", task_list)
    task_remove(task_id = strrep(" ", 64))
    message("created table 'tasks'")
  }

  DBI::dbDisconnect(con)
}

task_get_all <- function() {
    data <- db_get_query("SELECT * FROM tasks")
    return(data)
}

task_insert <- function(task_list) {
    con <- db_connect()
    DBI::dbWriteTable(con, "tasks", task_list, append = TRUE)
    DBI::dbDisconnect(con)
    glue::glue("inserted task with id {task_id}", task_id = task_list$task_id) |>
      message()
}

task_remove <- function(task_id) {
    statement <- "DELETE
                  FROM tasks
                  WHERE (task_id = {task_id})"
    db_execute_statement(statement, task_id = task_id)
    glue::glue("deleted task with id {task_id}") |> message()
}

task_get_from_id <- function(task_id) {
    query <- "SELECT task_id, task_description, status,
              user_id, reviewer, template_id
              FROM tasks
              WHERE (task_id IN ({vals*}))"
    data <- db_get_query(query, vals = task_id)
    return(data)
}

task_modify_status <- function(task_id, new_status) {
    statement <- "UPDATE tasks
                SET status = {new_status}
                WHERE task_id = {task_id}"
    db_execute_statement(statement, new_status = new_status, task_id = task_id)
    glue::glue("modified task with id {task_id}") |> message()
}

task_get_from_user <- function(user_id) {
    query <- "SELECT task_id, task_description, status,
              user_id, reviewer, template_id
              FROM tasks
              WHERE (user_id IN ({vals*}))"
    data <- db_get_query(query, vals = user_id)
    return(data)
}

task_get_from_user2 <- function(user_id) {
    query <- "SELECT task_id
              FROM tasks
              WHERE (user_id IN ({vals*}))"
    data <- db_get_query(query, vals = user_id)
    return(data$task_id)
}

task_get_status <- function(task_id) {
    query <- "SELECT status
              FROM tasks
              WHERE (task_id IN ({task_id}))"
    data <- db_get_query(query, task_id = task_id)
    return(data$status)
}

task_get_from_reviewer <- function(reviewer) {
    query <- "SELECT task_id
              FROM tasks
              WHERE (reviewer IN ({vals*}))"
    data <- db_get_query(query, vals = reviewer)
    return(data$task_id)
}

mk_task_getter <- function(status) {
    function(user_id) {
        query <- "SELECT *
                FROM tasks
                WHERE (user_id IN ({vals*}) and status = {status})"
        data <- db_get_query(query, vals = user_id, status = status)
        return(data)
    }
}

task_status_pendientes <- mk_task_getter("Pendiente")
task_status_en_proceso <- mk_task_getter("En proceso")
task_status_pausado <- mk_task_getter("Pausado")
task_status_en_revision <- mk_task_getter("En revisión")
task_status_terminado <- mk_task_getter("Terminado")

task_is_from_group <- function(task_id) {
    query <- "SELECT user_id
            FROM tasks
            WHERE task_id = {task_id}"
    data <- db_get_query(query, task_id = task_id)
    return(grepl("^team", data$user_id))
}

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

