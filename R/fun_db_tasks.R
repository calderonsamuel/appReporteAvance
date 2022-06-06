create_reporte_tasks <- function() {
  con <- RSQLite::dbConnect(RSQLite::SQLite(), "reporte_avance.db")

  if (!RSQLite::dbExistsTable(con, "tasks")) {
    task_list <- data.frame(
      reviewer = character(),
      user = character(),
      task_id = character(),
      unique_id = character()
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

insert_task <- function(task_list) {
  con <- RSQLite::dbConnect(RSQLite::SQLite(), "reporte_avance.db")
  RSQLite::dbWriteTable(con, "tasks", task_list, append = TRUE)
  RSQLite::dbDisconnect(con)
  print(sprintf("inserted task with id %s", task_list$unique_id))
}

delete_task <- function(unique_id) {
  con <- RSQLite::dbConnect(RSQLite::SQLite(), "reporte_avance.db")
  RSQLite::dbExecute(con, sprintf("DELETE FROM tasks WHERE (unique_id = '%s')", unique_id))
  RSQLite::dbDisconnect(con)
  print(sprintf("deleted task with id %s", unique_id))
}

# create_reporte_tasks()
# get_tasks()

# task_1 <- data.frame(
#   reviewer = "dgco93",
#   user = "dgco90",
#   task_id = "AOI001",
#   unique_id = "0905002"
# )

# insert_task(task_1)
# get_tasks()
# delete_task("0905002")
# get_tasks()
